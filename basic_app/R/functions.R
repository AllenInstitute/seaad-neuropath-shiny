# =============================================================================
# functions.R — pure helper functions used by global.R and server.R
# =============================================================================

library(dplyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# manifest parsing — a single csv covering any number of donors is read into
# a flat list of "entries" before being turned into the nested manifest.
# new donors/regions/stains need no code changes at all: they just need to
# appear as a new row in that csv.
# ---------------------------------------------------------------------------

s3_to_https <- function(s3_uri) {
  parts  <- sub("^s3://", "", s3_uri)
  bucket <- sub("/.*$", "", parts)
  key    <- sub("^[^/]+/", "", parts)
  paste0("https://", bucket, ".s3.amazonaws.com/", key)
}

is_annotation_xml <- function(file_type) {
  grepl("ANNOTATIONS?_XML$", file_type, ignore.case = TRUE)
}

# safely reads an OPTIONAL numeric field from an entry — returns NA if the
# column doesn't exist at all (entry[[field]] is NULL) as well as if it
# exists but is blank/unparseable. distinguishing "absent" from "blank"
# doesn't matter for our purposes, both just mean "go derive it instead".
get_numeric_field <- function(entry, field) {
  val <- entry[[field]]
  if (is.null(val) || length(val) == 0) return(NA_real_)
  suppressWarnings(as.numeric(val))
}

# fetches just `n` bytes starting at `offset` (0-indexed) from a URL via an
# HTTP Range request — used to read TIFF headers without downloading the
# whole (often multi-GB) file.
http_range_bytes <- function(url, offset, n) {
  resp <- httr::GET(url, httr::add_headers(Range = sprintf("bytes=%d-%d", offset, offset + n - 1)))
  httr::content(resp, as = "raw")
}

# reads ImageWidth/ImageLength (TIFF tags 256/257) out of IFD 0 of a
# CLASSIC (non-BigTIFF) TIFF-based file — this covers .svs files (and the
# mislabeled annotation.svg TIFFs), which conventionally store the
# full-resolution level as their first image directory. Used as a fallback
# when a manifest row's width/height columns are blank.
read_tiff_dimensions <- function(url) {
  header <- http_range_bytes(url, 0, 8)
  byte_order <- rawToChar(header[1:2])
  endian <- if (byte_order == "II") "little" else "big"
  magic <- readBin(header[3:4], "integer", size = 2, endian = endian, signed = FALSE)
  if (!(magic %in% c(42))) stop("not a classic TIFF (or is BigTIFF) — use vipsheader instead")
  ifd_offset <- readBin(header[5:8], "integer", size = 4, endian = endian, signed = FALSE)
  
  # entry count (2 bytes) + up to 64 entries (12 bytes each) + next-ifd offset (4 bytes)
  ifd_bytes <- http_range_bytes(url, ifd_offset, 2 + 64 * 12 + 4)
  n_entries <- readBin(ifd_bytes[1:2], "integer", size = 2, endian = endian, signed = FALSE)
  
  width <- NA_real_
  height <- NA_real_
  for (i in seq_len(min(n_entries, 64))) {
    start <- 2 + (i - 1) * 12 + 1
    entry <- ifd_bytes[start:(start + 11)]
    tag   <- readBin(entry[1:2], "integer", size = 2, endian = endian, signed = FALSE)
    type  <- readBin(entry[3:4], "integer", size = 2, endian = endian, signed = FALSE)
    value <- if (type == 3) {
      readBin(entry[9:10], "integer", size = 2, endian = endian, signed = FALSE)
    } else {
      readBin(entry[9:12], "integer", size = 4, endian = endian, signed = FALSE)
    }
    if (tag == 256) width  <- value
    if (tag == 257) height <- value
  }
  
  list(width = width, height = height)
}

# reads a single consolidated csv (covering any number of donors) into a
# list of row-entries, one list per row, each with the manifest's columns
# (file_type, stain_type, donor, region, s3_uri, width, height,
# annotation_name/subregion) as named elements.
read_manifest_csv_entries <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

# resolves an annotation file's display name from explicit columns only —
# `annotation_name` if present, else `subregion` (kept for backwards
# compatibility with older manifests) — falling back to a generic counter
# ONLY if neither column has a value. nothing is ever parsed from a url.
resolve_annotation_name <- function(entry, fallback_index) {
  candidates <- list(entry$annotation_name, entry$subregion)
  for (cand in candidates) {
    if (!is.null(cand) && !is.na(cand) && nzchar(as.character(cand))) return(as.character(cand))
  }
  paste0("Annotation ", fallback_index)
}

# builds the full nested manifest: donor -> region -> stain -> {
#   primary_dzi, annotation_dzi, annotation_files, svs_width, svs_height
# } from a combined list of entries, whatever their original source.
#
# donor, region, stain_type, and (for annotation rows) an explicit name are
# all REQUIRED COLUMNS on each entry — none of them are ever parsed out of a
# filename or s3_uri path. an entry missing donor/region/stain_type is
# skipped with a warning rather than guessed at.
build_donor_manifest_from_entries <- function(entries) {
  manifest <- list()
  
  is_blank <- function(x) is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))
  
  for (entry in entries) {
    if (is_blank(entry$s3_uri)) next
    
    donor  <- entry$donor
    region <- entry$region
    stain  <- entry$stain_type
    
    if (is_blank(donor) || is_blank(region) || is_blank(stain)) {
      warning("skipping entry missing donor/region/stain_type: ", entry$s3_uri)
      next
    }
    
    if (is.null(manifest[[donor]])) manifest[[donor]] <- list()
    if (is.null(manifest[[donor]][[region]])) manifest[[donor]][[region]] <- list()
    if (is.null(manifest[[donor]][[region]][[stain]])) {
      manifest[[donor]][[region]][[stain]] <- list(
        primary_dzi      = NULL,
        annotation_dzi   = NULL,
        annotation_files = list(),  # list of {url, name} — see resolve_annotation_name()
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
      # width/height are optional manifest columns, populated by the
      # separate precompute_manifest_dimensions.R script (run once, offline,
      # against the .svs files). if present, use them directly — no network
      # call needed. if absent/blank, fall back to reading the .svs file's
      # own TIFF header live (see read_tiff_dimensions() below).
      w <- get_numeric_field(entry, "width")
      h <- get_numeric_field(entry, "height")
      if (is.na(w) || is.na(h)) {
        dims <- tryCatch(read_tiff_dimensions(url), error = function(e) {
          warning("could not read TIFF header for ", url, ": ", e$message)
          list(width = NA_real_, height = NA_real_)
        })
        if (is.na(w)) w <- dims$width
        if (is.na(h)) h <- dims$height
      }
      slot$svs_width  <- w
      slot$svs_height <- h
    } else if (is_annotation_xml(ft)) {
      idx <- length(slot$annotation_files) + 1
      slot$annotation_files <- c(slot$annotation_files, list(list(
        url  = url,
        name = resolve_annotation_name(entry, idx)
      )))
    }
    # other file types (annotations_svg, halo_analysis_image_subregion_cropped)
    # are intentionally not tracked — not needed by the viewer.
    
    manifest[[donor]][[region]][[stain]] <- slot
  }
  
  manifest
}

# --- region / stain / donor lookup helpers ----------------------------------
# all derived live from donor_manifest's actual keys — adding a new
# donor/region/stain to the data source is all that's needed for these (and
# every dropdown built from them) to pick it up.

get_regions_for_donor <- function(donor) {
  regions <- donor_manifest[[donor]]
  if (is.null(regions)) return(character(0))
  names(regions)
}

get_all_regions <- function() {
  unique(unlist(lapply(names(donor_manifest), get_regions_for_donor), use.names = FALSE))
}

# stains available for one specific donor+region pair (exact, no flattening).
get_stain_choices_for_donor_region <- function(donor, region) {
  slot <- donor_manifest[[donor]][[region]]
  if (is.null(slot)) return(character(0))
  names(slot)
}

# a donor's stains flattened across all of that donor's regions.
get_stain_choices_for_donor <- function(donor) {
  regions <- donor_manifest[[donor]]
  if (is.null(regions)) return(character(0))
  unique(unlist(lapply(regions, names), use.names = FALSE))
}

# all stains present for any donor, across the whole manifest.
get_all_stains <- function() {
  unique(unlist(lapply(names(donor_manifest), get_stain_choices_for_donor), use.names = FALSE))
}

# regions where at least one donor has the given stain.
get_regions_for_stain <- function(stain) {
  donors <- names(donor_manifest)
  regs <- unlist(lapply(donors, function(d) {
    dr <- get_regions_for_donor(d)
    dr[vapply(dr, function(r) !is.null(donor_manifest[[d]][[r]][[stain]]), logical(1))]
  }), use.names = FALSE)
  unique(regs)
}

# regions of ONE donor that actually have the given stain.
get_regions_with_stain_for_donor <- function(donor, stain) {
  regs <- get_regions_for_donor(donor)
  regs[vapply(regs, function(r) !is.null(donor_manifest[[donor]][[r]][[stain]]), logical(1))]
}

# donors that actually have a valid image for a given stain+region pair —
# used to keep the "select specific donors" list free of dead-end choices.
get_donors_with_stain_region <- function(stain, region) {
  donors <- names(donor_manifest)
  donors[vapply(donors, function(d) !is.null(donor_manifest[[d]][[region]][[stain]]), logical(1))]
}

# sanitized identifier safe for use as an html element id / js key.
safe_id <- function(donor, stain, region = NULL) {
  parts <- c(donor, region, stain)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  gsub("[^A-Za-z0-9]+", "_", paste(parts, collapse = "_"))
}

# turns a slug like "middle-temporal-gyrus-and-superior-temporal-gyrus" into
# "Middle Temporal Gyrus And Superior Temporal Gyrus" for display.
prettify_region <- function(region) {
  if (is.null(region) || is.na(region) || !nzchar(region)) return(region)
  words <- strsplit(gsub("-", " ", region), " ")[[1]]
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
}

# prepends a blank "placeholder" choice so a single-select dropdown starts
# genuinely empty instead of defaulting to its first real option.
with_placeholder <- function(choices, label = "Select...") {
  if (length(choices) == 0) return(setNames("", label))
  c(setNames("", label), setNames(choices, choices))
}

# ---------------------------------------------------------------------------
# halo annotation xml parsing (lazy + cached — see server.r's handling of
# input$request_annotations for where this actually gets called)
# ---------------------------------------------------------------------------

# halo/imagescope-style linecolor is a decimal-packed bgr integer.
bgr_dec_to_hex <- function(dec) {
  dec <- suppressWarnings(as.integer(dec))
  if (is.na(dec)) return("#FF0000")
  r <- bitwAnd(dec, 255)
  g <- bitwAnd(bitwShiftR(dec, 8), 255)
  b <- bitwAnd(bitwShiftR(dec, 16), 255)
  sprintf("#%02X%02X%02X", r, g, b)
}

# parse one halo .annotations xml file into a list of polygons (normalized
# viewport-coordinate point strings).
parse_halo_annotations <- function(ann_url, ref_width, skip_hidden = TRUE) {
  doc <- xml2::read_xml(ann_url)
  xml2::xml_ns_strip(doc)
  
  annotation_nodes <- xml2::xml_find_all(doc, "//Annotation")
  if (length(annotation_nodes) == 0) {
    stop("no <Annotation> nodes found in ", ann_url)
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

# NOTE: annotation names are resolved from explicit manifest columns only —
# see resolve_annotation_name() above. nothing is parsed from filenames.

# deterministically assigns a color to an annotation label from
# annotation_color_palette (global.r) — same label always maps to the same
# color (within one palette), via a simple string hash, so no per-label
# bookkeeping is needed as new labels show up. returns NULL (meaning "keep
# each file's original HALO-authored color") if the palette is empty.
assign_annotation_color <- function(label) {
  if (length(annotation_color_palette) == 0) return(NULL)
  idx <- (sum(utf8ToInt(label)) %% length(annotation_color_palette)) + 1
  annotation_color_palette[idx]
}

# applies the palette-assigned color to every polygon parsed from a file.
apply_annotation_color_override <- function(label, polygons) {
  override <- assign_annotation_color(label)
  if (is.null(override)) return(polygons)
  lapply(polygons, function(p) { p$color <- override; p })
}

# in-memory cache so repeatedly toggling/loading the same file only parses it once.
.annotation_cache <- new.env(parent = emptyenv())

parse_halo_annotations_cached <- function(url, ref_width) {
  key <- paste(url, ref_width, sep = "::")
  if (exists(key, envir = .annotation_cache, inherits = FALSE)) {
    return(get(key, envir = .annotation_cache, inherits = FALSE))
  }
  result <- tryCatch(
    parse_halo_annotations(url, ref_width),
    error = function(e) {
      warning(paste("could not parse", basename(url), "-", e$message))
      list()
    }
  )
  assign(key, result, envir = .annotation_cache)
  result
}

# ---------------------------------------------------------------------------
# donor metadata — spec-driven (metadata_fields, defined in global.r) so
# adding a field only means adding one list() entry there. type "range" ->
# histoslider, always DERIVED min/max (never hardcoded — see
# derive_metadata_fields()). type "select" -> count-bar checkboxes; choices
# are derived from data UNLESS the field already hardcodes them in global.r.
# ---------------------------------------------------------------------------

specimen_csv_column_map <- c(
  "Donor ID"                             = "donor",
  "Age at death (years)"                 = "age_at_death",
  "Sex"                                   = "sex",
  "APOE genotype"                        = "apoe_genotype",
  "Cognitive status"                     = "cog_status",
  "ADNC"                                  = "adnc",
  "Thal phase"                            = "thal_phase",
  "Braak stage"                           = "braak_stage",
  "CERAD score"                           = "cerad_score",
  "Years of education (years)"           = "years_education",
  "Continuous Pseudo-progression Score"  = "cps"
)

# excel silently reinterprets genotype strings like "3/3" as dates ("3-mar").
# since a us locale reads "m/d" as month/day, the original fraction is
# recoverable: "3-mar" -> month=mar(3), day=3 -> "3/3"; "4-mar" -> "3/4"; etc.
# alleles sorted ascending for a canonical "lower/higher" display.
decode_apoe_genotype <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (grepl("^[0-9]/[0-9]$", v)) return(v)
    m <- regmatches(v, regexec("^([0-9]+)-([A-Za-z]{3})$", v))[[1]]
    if (length(m) != 3) return(v)
    day   <- as.integer(m[2])
    month <- match(tolower(m[3]), tolower(month.abb))
    if (is.na(month)) return(v)
    paste(sort(c(month, day)), collapse = "/")
  }, character(1), USE.NAMES = FALSE)
}

# reads the specimen metadata csv, renaming/cleaning columns into our
# internal field ids (see specimen_csv_column_map).
load_specimen_metadata_csv <- function(path) {
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  missing_cols <- setdiff(names(specimen_csv_column_map), names(df))
  if (length(missing_cols) > 0) {
    warning("specimen csv is missing expected columns: ", paste(missing_cols, collapse = ", "))
  }
  
  keep <- intersect(names(specimen_csv_column_map), names(df))
  df <- df[, keep, drop = FALSE]
  names(df) <- specimen_csv_column_map[keep]
  
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

# fills in each field's bounds/choices from real data — WITHOUT overwriting
# anything already set explicitly in global.r:
#   - range fields: min/max are ALWAYS (re)computed from the data. these are
#     never hardcoded/guessed, since a stale guess could silently clip real
#     values out of the slider's range.
#   - select fields: choices are only derived from data if the field's
#     global.r spec doesn't already hardcode them (f$choices is NULL).
#     hardcoding stays authoritative and is never overwritten.
derive_metadata_fields <- function(fields, data) {
  lapply(fields, function(f) {
    if (!(f$id %in% names(data))) return(f)
    vals <- data[[f$id]]
    if (f$type == "range") {
      rng <- range(vals, na.rm = TRUE)
      f$min <- floor(rng[1])
      f$max <- ceiling(rng[2])
    } else if (is.null(f$choices)) {
      f$choices <- sort(unique(vals[!is.na(vals) & nzchar(as.character(vals))]))
    }
    f
  })
}

# normalizes histoslider's selection value into a simple c(min, max).
histoslider_range <- function(val) {
  if (is.null(val)) return(NULL)
  if (is.list(val) && !is.null(val$start) && !is.null(val$end)) return(c(val$start, val$end))
  as.numeric(val)
}

# reads the meta_* inputs for a given page prefix and filters donor_metadata
# down with dplyr. returns the vector of matching donor ids.
filter_donors_by_metadata <- function(metadata, input, prefix) {
  df <- metadata
  for (f in metadata_fields) {
    if (f$type == "range") {
      val <- histoslider_range(input[[paste0(prefix, "_meta_", f$id, "_range")]])
      if (!is.null(val)) {
        df <- df %>% dplyr::filter(.data[[f$id]] >= val[1], .data[[f$id]] <= val[2])
      }
    } else {
      val <- input[[paste0(prefix, "_meta_", f$id, "_sel")]]
      if (!is.null(val) && length(val) > 0) {
        df <- df %>% dplyr::filter(.data[[f$id]] %in% val)
      }
    }
  }
  df$donor
}

# one bslib accordion_panel per metadata field: numeric fields get a
# histoslider (histogram + range filter combined in one widget); categorical
# fields get a plain checkboxGroupInput (label immediately next to each
# checkbox, as usual) plus a separate histogram registered server-side via
# register_metadata_histograms(). widget ids are prefixed per page.
build_metadata_accordion <- function(prefix, data) {
  panels <- lapply(metadata_fields, function(f) {
    body <- if (f$type == "range") {
      histoslider::input_histoslider(paste0(prefix, "_meta_", f$id, "_range"), NULL, data[[f$id]])
    } else {
      shiny::tagList(
        shiny::checkboxGroupInput(
          paste0(prefix, "_meta_", f$id, "_sel"), label = NULL,
          choices = f$choices, selected = character(0)
        ),
        shiny::plotOutput(paste0(prefix, "_hist_", f$id), height = "150px")
      )
    }
    bslib::accordion_panel(title = f$label, body)
  })
  do.call(bslib::accordion, c(list(id = paste0(prefix, "_metadata_accordion"), open = FALSE), panels))
}

# registers the renderPlot output for every categorical field's histogram
# under a page's prefix. call once per page (outside any observer) that
# includes a metadata accordion. shows the OVERALL distribution across all
# donors, not a live-filtered one.
register_metadata_histograms <- function(output, prefix, data) {
  for (f in metadata_fields) {
    if (f$type != "select") next
    local({
      fld <- f
      output[[paste0(prefix, "_hist_", fld$id)]] <- shiny::renderPlot({
        counts <- table(data[[fld$id]])
        barplot(counts, main = NULL, col = "#7952b3", border = NA, las = 2, cex.names = 0.8)
      })
    })
  }
}

# ---------------------------------------------------------------------------
# shared multi-image rendering/payload builders.
# `label_field` is whichever field actually VARIES on a given page — the
# other two are fixed and shown once in a constraint card instead.
# ---------------------------------------------------------------------------

render_viewer_grid_ui <- function(entries, label_field = c("stain", "donor", "region")) {
  label_field <- match.arg(label_field)
  if (length(entries) == 0) return(NULL)  # blank until something is actually loaded
  col_width <- if (length(entries) == 1) 12 else 6
  shiny::tagList(shiny::fluidRow(lapply(entries, function(e) {
    cid   <- paste0("osd-", safe_id(e$donor, e$stain, e$region))
    label <- if (label_field == "region") prettify_region(e$region) else e[[label_field]]
    shiny::column(
      width = col_width,
      shiny::h5(label),
      shiny::tags$div(
        id = cid,
        style = "width:100%; height:450px; background:#000; border:1px solid #ccc; position:relative; margin-bottom:6px;"
      ),
      shiny::tags$hr()
    )
  })))
}

render_annotation_master_ui <- function(entries) {
  if (length(entries) == 0) return(NULL)
  
  all_labels <- character(0)
  for (e in entries) {
    if (length(e$slot$annotation_files) > 0) {
      all_labels <- c(all_labels, vapply(e$slot$annotation_files, function(f) f$name, character(1)))
    }
  }
  all_labels <- sort(unique(all_labels))
  if (length(all_labels) == 0) return(NULL)
  
  shiny::tagList(
    shiny::strong("Annotations:"),
    shiny::div(
      style = "display:flex; flex-wrap:wrap; gap:14px; margin-top:6px;",
      lapply(all_labels, function(lab) {
        shiny::tags$label(
          shiny::tags$input(type = "checkbox", onclick = sprintf("toggleAnnotationLabel('%s', this.checked)", lab)),
          paste0(" ", lab)
        )
      })
    )
  )
}

# builds the json-ready payload for the 'loadImages' custom message.
# `show_overlay` (per-page checkbox) blanks overlayUrl entirely when off.
build_images_payload <- function(entries, overlay_opacity, show_overlay = TRUE) {
  lapply(entries, function(e) {
    ann_files <- e$slot$annotation_files
    if (length(ann_files) > 0) {
      ann_files <- ann_files[order(vapply(ann_files, function(f) f$name, character(1)))]
    }
    groups <- lapply(ann_files, function(f) {
      list(label = f$name, url = f$url, refWidth = e$slot$svs_width)
    })
    list(
      id               = safe_id(e$donor, e$stain, e$region),
      dziUrl           = e$slot$primary_dzi,
      overlayUrl       = if (isTRUE(show_overlay)) (e$slot$annotation_dzi %||% "") else "",
      overlayOpacity   = overlay_opacity,
      annotationGroups = groups
    )
  })
}