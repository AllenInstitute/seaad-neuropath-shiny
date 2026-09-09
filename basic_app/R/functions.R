# =============================================================================
# functions.R — pure helper functions used by global.R and server.R
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# Manifest parsing
# ---------------------------------------------------------------------------

s3_to_https <- function(s3_uri) {
  parts  <- sub("^s3://", "", s3_uri)
  bucket <- sub("/.*$", "", parts)
  key    <- sub("^[^/]+/", "", parts)
  paste0("https://", bucket, ".s3.amazonaws.com/", key)
}

# The JSON entries don't carry explicit "region"/"donor" fields, but both are
# embedded in every s3_uri's path: <region>/<donor>/<stain-subfolder>/<file>.
extract_region_donor <- function(s3_uri) {
  key  <- sub("^s3://[^/]+/", "", s3_uri)
  segs <- strsplit(key, "/")[[1]]
  list(region = segs[1], donor = segs[2])
}

# Matches file_type values like "SUBREGION_ANNOTATIONS_XML" (also tolerates
# "ANNOTATION_XML" in case naming varies across manifests).
is_annotation_xml <- function(file_type) {
  grepl("ANNOTATIONS?_XML$", file_type, ignore.case = TRUE)
}

# Parse one donor+region JSON manifest (local path or https URL both work).
parse_manifest_json <- function(path_or_url) {
  jsonlite::fromJSON(path_or_url, simplifyDataFrame = FALSE)
}

# Build the full nested manifest: donor -> region -> stain -> {
#   primary_dzi, annotation_dzi, annotation_files (character vector),
#   svs_width, svs_height
# }
# `manifest_sources` is a vector of JSON file paths/URLs, one per donor+region.
build_donor_manifest <- function(manifest_sources) {
  manifest <- list()
  
  for (src in manifest_sources) {
    entries <- tryCatch(parse_manifest_json(src), error = function(e) {
      warning(paste("Could not parse manifest:", src, "-", e$message))
      NULL
    })
    if (is.null(entries)) next
    
    for (entry in entries) {
      rd     <- extract_region_donor(entry$s3_uri)
      region <- rd$region
      donor  <- rd$donor
      stain  <- entry$stain_type
      if (is.null(stain) || length(stain) == 0 || is.na(stain)) next
      
      if (is.null(manifest[[donor]])) manifest[[donor]] <- list()
      if (is.null(manifest[[donor]][[region]])) manifest[[donor]][[region]] <- list()
      if (is.null(manifest[[donor]][[region]][[stain]])) {
        manifest[[donor]][[region]][[stain]] <- list(
          primary_dzi      = NULL,
          annotation_dzi   = NULL,
          annotation_files = character(0),
          svs_width        = NA_real_,
          svs_height       = NA_real_
        )
      }
      
      slot <- manifest[[donor]][[region]][[stain]]
      url  <- s3_to_https(entry$s3_uri)
      ft   <- entry$file_type
      
      if (identical(ft, "RAW_IMAGE_DEEPZOOM")) {
        slot$primary_dzi <- url
      } else if (identical(ft, "HALO_ANALYSIS_IMAGE_DEEPZOOM")) {
        slot$annotation_dzi <- url
      } else if (identical(ft, "RAW_IMAGE")) {
        slot$svs_width  <- suppressWarnings(as.numeric(entry$width))
        slot$svs_height <- suppressWarnings(as.numeric(entry$height))
      } else if (is_annotation_xml(ft)) {
        slot$annotation_files <- c(slot$annotation_files, url)
      }
      # Other file types (ANNOTATIONS_SVG, HALO_ANALYSIS_IMAGE_SUBREGION_CROPPED)
      # are intentionally not tracked — not needed by the viewer.
      
      manifest[[donor]][[region]][[stain]] <- slot
    }
  }
  
  manifest
}

# --- Region / stain lookup helpers -----------------------------------------

get_regions_for_donor <- function(donor) {
  regions <- donor_manifest[[donor]]
  if (is.null(regions)) return(character(0))
  names(regions)
}

# Stains available for one specific donor+region pair (exact, no flattening).
get_stain_choices_for_donor_region <- function(donor, region) {
  slot <- donor_manifest[[donor]][[region]]
  if (is.null(slot)) return(character(0))
  names(slot)
}

# A donor's stains flattened across ALL of that donor's regions — used only
# by the "compare one stain across donors" mode, which doesn't ask for a
# region. If the same stain name exists in more than one region for a donor,
# the first region encountered wins.
get_stain_choices_for_donor <- function(donor) {
  regions <- donor_manifest[[donor]]
  if (is.null(regions)) return(character(0))
  unique(unlist(lapply(regions, names), use.names = FALSE))
}

get_stain_slot <- function(donor, stain) {
  regions <- donor_manifest[[donor]]
  if (is.null(regions)) return(NULL)
  for (region in names(regions)) {
    if (!is.null(regions[[region]][[stain]])) return(regions[[region]][[stain]])
  }
  NULL
}

# All stains present for ANY donor, across the whole manifest — used to
# populate the stain picker for the "compare across donors" mode.
get_all_stains <- function() {
  unique(unlist(lapply(names(donor_manifest), get_stain_choices_for_donor), use.names = FALSE))
}

# Sanitized identifier safe for use as an HTML element id / JS key. `region`
# is optional — needed when comparing the same donor+stain across multiple
# regions, where donor+stain alone would collide.
safe_id <- function(donor, stain, region = NULL) {
  parts <- c(donor, region, stain)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  gsub("[^A-Za-z0-9]+", "_", paste(parts, collapse = "_"))
}

# ---------------------------------------------------------------------------
# HALO annotation XML parsing (lazy + cached — see server.R's handling of
# input$request_annotations for where this actually gets called)
# ---------------------------------------------------------------------------

# HALO/ImageScope-style LineColor is a decimal-packed BGR integer.
bgr_dec_to_hex <- function(dec) {
  dec <- suppressWarnings(as.integer(dec))
  if (is.na(dec)) return("#FF0000")
  r <- bitwAnd(dec, 255)
  g <- bitwAnd(bitwShiftR(dec, 8), 255)
  b <- bitwAnd(bitwShiftR(dec, 16), 255)
  sprintf("#%02X%02X%02X", r, g, b)
}

# Parse ONE HALO .annotations XML file into a list of polygons (normalized
# viewport-coordinate point strings). `ref_width` should be the true .svs
# width for the image this annotation was drawn against.
parse_halo_annotations <- function(ann_url, ref_width, skip_hidden = TRUE) {
  doc <- xml2::read_xml(ann_url)
  xml2::xml_ns_strip(doc)
  
  annotation_nodes <- xml2::xml_find_all(doc, "//Annotation")
  if (length(annotation_nodes) == 0) {
    stop("No <Annotation> nodes found in ", ann_url)
  }
  
  polygons <- list()
  
  for (ann in annotation_nodes) {
    ann_name  <- xml2::xml_attr(ann, "Name")
    visible   <- xml2::xml_attr(ann, "Visible")
    color_hex <- bgr_dec_to_hex(xml2::xml_attr(ann, "LineColor"))
    
    if (skip_hidden && !is.na(visible) && identical(tolower(visible), "false")) next
    
    regions <- xml2::xml_find_all(ann, ".//Region")
    for (reg in regions) {
      verts <- xml2::xml_find_all(reg, ".//V")
      if (length(verts) < 3) next
      
      xs <- as.numeric(xml2::xml_attr(verts, "X")) / ref_width
      ys <- as.numeric(xml2::xml_attr(verts, "Y")) / ref_width
      
      polygons[[length(polygons) + 1]] <- list(
        name   = ann_name,
        color  = color_hex,
        points = paste(sprintf("%f,%f", xs, ys), collapse = " ")
      )
    }
  }
  
  polygons
}

# Derive a short, human-readable label from an annotation file's name, e.g.
# ".../H19.33.004-A06-NeuN_Layer5-6_analysis.annotations" -> "Layer5-6"
# ".../H19.33.004-A06-NeuN_STG_analysis.annotations" -> "STG"
# Falls back to the filename (minus extension) if the pattern doesn't match.
annotation_label_from_url <- function(url) {
  fname <- basename(url)
  label <- sub("^.*_([^_]+)_analysis\\..*$", "\\1", fname, ignore.case = TRUE)
  if (identical(label, fname)) {
    label <- sub("\\.[^.]+$", "", fname)
  }
  label
}

# In-memory cache so repeatedly toggling the same annotation on/off — or
# loading the same file across multiple comparisons in one session — only
# ever parses it once.
.annotation_cache <- new.env(parent = emptyenv())

parse_halo_annotations_cached <- function(url, ref_width) {
  key <- paste(url, ref_width, sep = "::")
  if (exists(key, envir = .annotation_cache, inherits = FALSE)) {
    return(get(key, envir = .annotation_cache, inherits = FALSE))
  }
  result <- tryCatch(
    parse_halo_annotations(url, ref_width),
    error = function(e) {
      warning(paste("Could not parse", basename(url), "-", e$message))
      list()
    }
  )
  assign(key, result, envir = .annotation_cache)
  result
}

# ---------------------------------------------------------------------------
# Donor metadata — spec-driven so adding/removing a field only means editing
# METADATA_FIELDS, not touching the UI/generation/filtering code separately.
# type "range"  -> rendered as a slider, filtered as an inclusive [min,max].
# type "select" -> rendered as a multi-select, filtered as %in% (no
#                  selection = no filter applied for that field).
# ---------------------------------------------------------------------------

METADATA_FIELDS <- list(
  list(id = "age_at_death",    label = "Age at death",      type = "range",  min = 65, max = 102),
  list(id = "sex",              label = "Sex",               type = "select", choices = c("Female", "Male")),
  list(id = "apoe_genotype",    label = "APOE genotype",     type = "select",
       choices = c("2/2", "2/3", "2/4", "3/3", "3/4", "4/4")),
  list(id = "cog_status",       label = "Cognitive status",  type = "select",
       choices = c("Dementia", "No dementia")),
  list(id = "adnc",             label = "ADNC",               type = "select",
       choices = c("Not AD", "Low", "Intermediate", "High")),
  list(id = "thal_phase",       label = "Thal phase",         type = "select", choices = as.character(0:5)),
  list(id = "braak_stage",      label = "Braak stage",        type = "select",
       choices = c("0", "I", "II", "III", "IV", "V", "VI")),
  list(id = "cerad_score",      label = "CERAD score",        type = "select",
       choices = c("Absent", "Sparse", "Moderate", "Frequent")),
  list(id = "lbd_path",         label = "LBD pathology",      type = "select",
       choices = c("None", "Olfactory Bulb Only", "Amygdala-Predominant",
                   "Brainstem-Predominant", "Limbic", "Neocortical", "Not Assessed")),
  list(id = "years_education", label = "Years of education", type = "range",  min = 12, max = 21),
  list(id = "cps",              label = "Continuous Pseudo-progression Score (CPS)",
       type = "range", min = 0, max = 1)
)

# ---------------------------------------------------------------------------
# Loading real specimen metadata from a CSV (see global.R for where this is
# actually invoked). Column names are mapped by exact match against the
# headers you provided; anything not listed here is dropped.
# ---------------------------------------------------------------------------

SPECIMEN_CSV_COLUMN_MAP <- c(
  "Donor ID"                              = "donor",
  "Age at death (years)"                  = "age_at_death",
  "Sex"                                    = "sex",
  "APOE genotype"                         = "apoe_genotype",
  "Cognitive status"                      = "cog_status",
  "ADNC"                                   = "adnc",
  "Thal phase"                             = "thal_phase",
  "Braak stage"                            = "braak_stage",
  "CERAD score"                            = "cerad_score",
  "Years of education (years)"            = "years_education",
  "Continuous Pseudo-progression Score"   = "cps"
)

# Excel silently reinterprets genotype strings like "3/3" as dates and
# re-serializes them as e.g. "3-Mar" (day-month abbreviation). Since a US
# locale reads "M/D" as month/day, the original fraction is recoverable:
# "3-Mar" -> month=Mar(3), day=3 -> "3/3". "4-Mar" -> month=3, day=4 -> "3/4".
# "3-Feb" -> month=2, day=3 -> "2/3". "4-Apr" -> month=4, day=4 -> "4/4".
# Alleles are sorted ascending for a canonical "lower/higher" display.
decode_apoe_genotype <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (grepl("^[0-9]/[0-9]$", v)) return(v)  # already in the correct format
    m <- regmatches(v, regexec("^([0-9]+)-([A-Za-z]{3})$", v))[[1]]
    if (length(m) != 3) return(v)  # unrecognized format — leave untouched
    day   <- as.integer(m[2])
    month <- match(tolower(m[3]), tolower(month.abb))
    if (is.na(month)) return(v)
    paste(sort(c(month, day)), collapse = "/")
  }, character(1), USE.NAMES = FALSE)
}

# Reads the specimen metadata CSV and renames/cleans columns into our
# internal field ids. Strips "Thal "/"Braak " prefixes and decodes the
# Excel-mangled APOE genotype strings.
load_specimen_metadata_csv <- function(path) {
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  missing_cols <- setdiff(names(SPECIMEN_CSV_COLUMN_MAP), names(df))
  if (length(missing_cols) > 0) {
    warning("Specimen CSV is missing expected columns: ", paste(missing_cols, collapse = ", "))
  }
  
  keep <- intersect(names(SPECIMEN_CSV_COLUMN_MAP), names(df))
  df <- df[, keep, drop = FALSE]
  names(df) <- SPECIMEN_CSV_COLUMN_MAP[keep]
  
  if ("apoe_genotype" %in% names(df)) df$apoe_genotype <- decode_apoe_genotype(df$apoe_genotype)
  if ("thal_phase" %in% names(df))    df$thal_phase   <- sub("^Thal\\s*", "", df$thal_phase, ignore.case = TRUE)
  if ("braak_stage" %in% names(df))   df$braak_stage  <- sub("^Braak\\s*", "", df$braak_stage, ignore.case = TRUE)
  
  for (numeric_field in c("age_at_death", "years_education", "cps")) {
    if (numeric_field %in% names(df)) {
      df[[numeric_field]] <- suppressWarnings(as.numeric(df[[numeric_field]]))
    }
  }
  
  df
}

# Rebuilds each field's bounds/choices from REAL data instead of the
# hardcoded placeholders above: range fields get min/max from the data,
# select fields get their choices from the data's actual distinct values.
# A field whose column isn't present in `data` keeps its placeholder as-is.
derive_metadata_fields <- function(fields, data) {
  lapply(fields, function(f) {
    if (!(f$id %in% names(data))) return(f)
    vals <- data[[f$id]]
    if (f$type == "range") {
      rng <- range(vals, na.rm = TRUE)
      f$min <- floor(rng[1])
      f$max <- ceiling(rng[2])
    } else {
      f$choices <- sort(unique(vals[!is.na(vals) & nzchar(as.character(vals))]))
    }
    f
  })
}

generate_dummy_metadata <- function(donors, seed = 42) {
  if (length(donors) == 0) {
    df <- data.frame(donor = character(0), stringsAsFactors = FALSE)
    for (f in METADATA_FIELDS) df[[f$id]] <- if (f$type == "range") numeric(0) else character(0)
    return(df)
  }
  set.seed(seed)
  df <- data.frame(donor = donors, stringsAsFactors = FALSE)
  for (f in METADATA_FIELDS) {
    df[[f$id]] <- if (f$type == "range") {
      sample(f$min:f$max, length(donors), replace = TRUE)
    } else {
      sample(f$choices, length(donors), replace = TRUE)
    }
  }
  df
}

# Reads all the meta_<id>_range / meta_<id>_sel Shiny inputs directly and
# applies each as a filter; fields the user hasn't touched impose no
# restriction. Returns the vector of matching donor IDs.
filter_donors_by_metadata <- function(metadata, input) {
  df <- metadata
  for (f in METADATA_FIELDS) {
    if (f$type == "range") {
      val <- input[[paste0("meta_", f$id, "_range")]]
      if (!is.null(val)) df <- df[df[[f$id]] >= val[1] & df[[f$id]] <= val[2], ]
    } else {
      val <- input[[paste0("meta_", f$id, "_sel")]]
      if (!is.null(val) && length(val) > 0) df <- df[df[[f$id]] %in% val, ]
    }
  }
  df$donor
}