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

# A donor's stains flattened across all of that donor's regions (the viewer
# doesn't expose a region picker). If the same stain name exists in more than
# one region for a donor, the first region encountered wins.
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

# Sanitized identifier safe for use as an HTML element id / JS key.
safe_id <- function(donor, stain) {
  gsub("[^A-Za-z0-9]+", "_", paste(donor, stain, sep = "_"))
}

# ---------------------------------------------------------------------------
# HALO annotation XML parsing
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

# Parse MULTIPLE annotation files (e.g. one per subregion/layer) and combine
# into a single polygon list. A failure on one file doesn't block the others.
parse_multiple_annotations <- function(urls, ref_width) {
  all_polys <- list()
  for (url in urls) {
    polys <- tryCatch(
      parse_halo_annotations(url, ref_width),
      error = function(e) {
        warning(paste("Could not parse", basename(url), "-", e$message))
        list()
      }
    )
    all_polys <- c(all_polys, polys)
  }
  all_polys
}

# ---------------------------------------------------------------------------
# Donor metadata (dummy data for now)
# ---------------------------------------------------------------------------

generate_dummy_metadata <- function(donors, seed = 42) {
  if (length(donors) == 0) {
    return(data.frame(
      donor = character(0), age = numeric(0),
      cerad_score = numeric(0), thal_score = numeric(0), braak_score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  set.seed(seed)
  data.frame(
    donor       = donors,
    age         = sample(60:100, length(donors), replace = TRUE),
    cerad_score = sample(0:3, length(donors), replace = TRUE),   # 0=none .. 3=frequent
    thal_score  = sample(0:5, length(donors), replace = TRUE),   # Thal phase 0-5
    braak_score = sample(0:6, length(donors), replace = TRUE),   # Braak stage 0-6
    stringsAsFactors = FALSE
  )
}

# Returns the vector of donor IDs matching all supplied thresholds.
# Any argument left NULL is not applied as a filter.
filter_donors_by_metadata <- function(metadata, age_range = NULL,
                                      cerad_min = NULL, thal_min = NULL, braak_min = NULL) {
  df <- metadata
  if (!is.null(age_range))  df <- df[df$age >= age_range[1] & df$age <= age_range[2], ]
  if (!is.null(cerad_min))  df <- df[df$cerad_score >= cerad_min, ]
  if (!is.null(thal_min))   df <- df[df$thal_score  >= thal_min, ]
  if (!is.null(braak_min))  df <- df[df$braak_score >= braak_min, ]
  df$donor
}