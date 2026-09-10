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
  # readBin only supports signed=FALSE for 1-2 byte integers, not 4 — these
  # offsets/values are always well under 2^31 in practice, so plain signed
  # reads are equivalent and avoid an R warning on every call.
  ifd_offset <- readBin(header[5:8], "integer", size = 4, endian = endian)
  
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
      readBin(entry[9:12], "integer", size = 4, endian = endian)
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
  sort(names(regions))
}

get_all_regions <- function() {
  sort(unique(unlist(lapply(names(donor_manifest), get_regions_for_donor), use.names = FALSE)))
}

# stains available for one specific donor+region pair (exact, no flattening).
get_stain_choices_for_donor_region <- function(donor, region) {
  slot <- donor_manifest[[donor]][[region]]
  if (is.null(slot)) return(character(0))
  sort(names(slot))
}

# a donor's stains flattened across all of that donor's regions.
get_stain_choices_for_donor <- function(donor) {
  regions <- donor_manifest[[donor]]
  if (is.null(regions)) return(character(0))
  sort(unique(unlist(lapply(regions, names), use.names = FALSE)))
}

# all stains present for any donor, across the whole manifest.
get_all_stains <- function() {
  sort(unique(unlist(lapply(names(donor_manifest), get_stain_choices_for_donor), use.names = FALSE)))
}

# regions where at least one donor has the given stain.
get_regions_for_stain <- function(stain) {
  donors <- names(donor_manifest)
  regs <- unlist(lapply(donors, function(d) {
    dr <- get_regions_for_donor(d)
    dr[vapply(dr, function(r) !is.null(donor_manifest[[d]][[r]][[stain]]), logical(1))]
  }), use.names = FALSE)
  sort(unique(regs))
}

# regions of ONE donor that actually have the given stain.
get_regions_with_stain_for_donor <- function(donor, stain) {
  regs <- get_regions_for_donor(donor)
  sort(regs[vapply(regs, function(r) !is.null(donor_manifest[[donor]][[r]][[stain]]), logical(1))])
}

# donors that actually have a valid image for a given stain+region pair —
# used to keep the "select specific donors" list free of dead-end choices.
get_donors_with_stain_region <- function(stain, region) {
  donors <- names(donor_manifest)
  sort(donors[vapply(donors, function(d) !is.null(donor_manifest[[d]][[region]][[stain]]), logical(1))])
}

# when the "Fetch annotations" checkbox is off, strips each entry's
# annotation_files so nothing gets parsed and no annotation checklist shows
# up for that load — used by the three comparison pages (Home always
# fetches, since a single image is cheap regardless).
maybe_skip_annotations <- function(entries, fetch_annotations) {
  if (isTRUE(fetch_annotations)) return(entries)
  lapply(entries, function(e) { e$slot$annotation_files <- list(); e })
}

# sorts a set of loaded entries alphabetically by whichever field varies on
# the page they're shown on (stain/donor/region) — used so images always
# appear in a predictable left-to-right/top-to-bottom order.
sort_entries_by <- function(entries, field) {
  if (length(entries) == 0) return(entries)
  entries[order(vapply(entries, function(e) e[[field]], character(1)))]
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
# halo annotation xml parsing (eager + cached — called directly from
# build_images_payload() at Load/Compare time; the cache means repeat loads
# of the same file across different comparisons only ever parse it once)
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

# all unique annotation labels across a set of entries, sorted — this is the
# actual "number of annotations" build_annotation_color_map() sizes its
# palette to, and also what the checklist (render_annotation_master_ui) and
# the initial payload (build_images_payload) both derive their colors from,
# so all three always agree on the same label -> color mapping for one load.
get_unique_annotation_labels <- function(entries) {
  labels <- character(0)
  for (e in entries) {
    if (length(e$slot$annotation_files) > 0) {
      labels <- c(labels, vapply(e$slot$annotation_files, function(f) f$name, character(1)))
    }
  }
  sort(unique(labels))
}

# picks a colorblind-friendly qualitative palette SIZED to how many distinct
# labels actually need a color, using khroma's Paul Tol schemes:
#   n <  10  -> "muted"    (max 9)
#   n in 10-11 -> "sunset"    (max 11)
#   n in 12-17 -> "nightfall" (max 17; also the fallback ceiling if n > 17,
#                              which shouldn't happen in practice)
# returns a named list: label -> hex color.
build_annotation_color_map <- function(labels) {
  if (length(labels) == 0) return(list())
  n <- length(labels)
  
  palette <- if (n < 10) {
    as.character(khroma::colour("muted")(n))
  } else if (n <= 11) {
    as.character(khroma::colour("sunset")(n))
  } else {
    as.character(khroma::colour("nightfall")(min(n, 17)))
  }
  
  # recycle if n somehow exceeds 17 (shouldn't happen) rather than erroring
  idx <- ((seq_len(n) - 1) %% length(palette)) + 1
  stats::setNames(as.list(palette[idx]), labels)
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
# donor metadata — fully spec-driven by metadata_fields (defined in
# global.r). Each field declares its own `csv_column` (the exact header text
# in the specimen CSV); load_specimen_metadata_csv() only ever reads those
# declared columns, so any OTHER column present in the file is silently
# ignored. Adding a metadata field is entirely a global.r edit — no changes
# needed here.
# ---------------------------------------------------------------------------

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

# reads the specimen metadata csv. `donor_id_column` identifies the donor-id
# column (not itself a metadata_fields entry); every OTHER column read is
# whatever metadata_fields declares via its `csv_column` — so a column in
# the file that isn't referenced by any field is simply never selected.
load_specimen_metadata_csv <- function(path, donor_id_column = "Donor ID") {
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  if (!(donor_id_column %in% names(df))) {
    stop("specimen csv is missing the donor id column: '", donor_id_column, "'")
  }
  out <- data.frame(donor = df[[donor_id_column]], stringsAsFactors = FALSE)
  
  for (f in metadata_fields) {
    col <- f$csv_column
    if (is.null(col) || !(col %in% names(df))) {
      warning("specimen csv is missing column '", col, "' for metadata field '", f$id, "'")
      out[[f$id]] <- NA
      next
    }
    out[[f$id]] <- df[[col]]
  }
  
  if ("apoe_genotype" %in% names(out)) out$apoe_genotype <- decode_apoe_genotype(out$apoe_genotype)
  if ("thal_phase" %in% names(out))    out$thal_phase   <- sub("^Thal\\s*", "", out$thal_phase, ignore.case = TRUE)
  if ("braak_stage" %in% names(out))   out$braak_stage  <- sub("^Braak\\s*", "", out$braak_stage, ignore.case = TRUE)
  
  for (numeric_field in c("age_at_death", "years_education", "cps")) {
    if (numeric_field %in% names(out)) {
      out[[numeric_field]] <- suppressWarnings(as.numeric(out[[numeric_field]]))
    }
  }
  
  out
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
#
# type "range"   -> a [min,max] filter on the raw numeric column.
# type "ordinal" -> categorical values with a meaningful order (declared via
#                   `choices`, in order), filtered via a sliderTextInput —
#                   input$... is a length-2 character vector of the selected
#                   labels themselves (not positions), so this just expands
#                   that into the corresponding subset of `choices`.
# type "select"  -> an unordered %in% filter from the checkbox group.
filter_donors_by_metadata <- function(metadata, input, prefix) {
  df <- metadata
  for (f in metadata_fields) {
    if (f$type == "range") {
      val <- histoslider_range(input[[paste0(prefix, "_meta_", f$id, "_range")]])
      if (!is.null(val)) {
        df <- df %>% dplyr::filter(.data[[f$id]] >= val[1], .data[[f$id]] <= val[2])
      }
    } else if (f$type == "ordinal") {
      val <- input[[paste0(prefix, "_meta_", f$id, "_range")]]
      if (!is.null(val) && length(val) == 2) {
        lo <- min(match(val, f$choices), na.rm = TRUE)
        hi <- max(match(val, f$choices), na.rm = TRUE)
        allowed <- f$choices[lo:hi]
        df <- df %>% dplyr::filter(.data[[f$id]] %in% allowed)
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

# wraps input_histoslider() with the shared metadata_chart_color (global.r).
# NOTE: histoslider's documented API doesn't expose a color option — the
# `options` argument here is unverified against the package's actual JS
# component and may not visibly change anything. If the bar color still
# doesn't match, the CSS override in ui.R's <style> block is the more
# reliable lever (target whatever class the rendered bars actually use —
# inspect one with your browser's dev tools to confirm the selector).
build_histoslider <- function(id, values) {
  tryCatch(
    histoslider::input_histoslider(id, NULL, values, options = list(color = metadata_chart_color)),
    error = function(e) histoslider::input_histoslider(id, NULL, values)
  )
}

# one bslib accordion_panel per metadata field:
#   "range"   -> a histoslider (histogram + range filter combined).
#   "ordinal" -> ordered categorical values, via shinyWidgets::sliderTextInput
#                (NOT histoslider — histoslider only supports numeric/date/
#                datetime axes, so it can't show category labels as ticks;
#                sliderTextInput natively shows every one of `choices` as a
#                labeled tick on the track, which is what was asked for here,
#                at the cost of no histogram-bar visualization for this type).
#   "select"  -> a plain checkboxGroupInput (label immediately next to each
#                checkbox) plus a separate histogram registered server-side
#                via register_metadata_histograms().
# widget ids are prefixed per page.
build_metadata_accordion <- function(prefix, data) {
  panels <- lapply(metadata_fields, function(f) {
    body <- if (f$type == "range") {
      build_histoslider(paste0(prefix, "_meta_", f$id, "_range"), data[[f$id]])
      
    } else if (f$type == "ordinal") {
      shinyWidgets::sliderTextInput(
        paste0(prefix, "_meta_", f$id, "_range"), label = NULL,
        choices = f$choices, selected = c(f$choices[1], f$choices[length(f$choices)]),
        grid = TRUE
      )
      
    } else {
      shiny::tagList(
        shiny::checkboxGroupInput(
          paste0(prefix, "_meta_", f$id, "_sel"), label = NULL,
          choices = f$choices, selected = character(0)
        ),
        shiny::plotOutput(paste0(prefix, "_hist_", f$id), height = "170px")
      )
    }
    bslib::accordion_panel(title = f$label, body)
  })
  do.call(bslib::accordion, c(list(id = paste0(prefix, "_metadata_accordion"), open = FALSE), panels))
}

# registers the renderPlot output for every categorical ("select") field's
# histogram under a page's prefix. call once per page (outside any
# observer) that includes a metadata accordion. shows the OVERALL
# distribution across all donors, not a live-filtered one. no y-axis (counts
# are labeled directly on top of each bar instead), x-axis labels angled.
register_metadata_histograms <- function(output, prefix, data) {
  for (f in metadata_fields) {
    if (f$type != "select") next
    local({
      fld <- f
      output[[paste0(prefix, "_hist_", fld$id)]] <- shiny::renderPlot({
        counts <- table(data[[fld$id]])
        graphics::par(mar = c(6, 1, 2, 1))
        bp <- graphics::barplot(
          counts, col = metadata_chart_color, border = NA,
          yaxt = "n", xaxt = "n", ylim = c(0, max(counts) * 1.15)
        )
        graphics::text(x = bp, y = counts, labels = counts, pos = 3, cex = 0.8, xpd = TRUE)
        usr <- graphics::par("usr")
        graphics::text(
          x = bp, y = usr[3] - 0.04 * (usr[4] - usr[3]), labels = names(counts),
          srt = 45, adj = 1, xpd = TRUE, cex = 0.8
        )
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
  
  all_labels <- get_unique_annotation_labels(entries)
  if (length(all_labels) == 0) return(NULL)
  
  color_map <- build_annotation_color_map(all_labels)
  
  shiny::tagList(
    shiny::strong("Annotations:"),
    shiny::div(
      style = "display:flex; flex-wrap:wrap; gap:40px; margin-top:12px;",
      lapply(all_labels, function(lab) {
        shiny::tags$label(
          shiny::tags$input(type = "checkbox", onclick = sprintf("toggleAnnotationLabel('%s', this.checked)", lab)),
          paste0(" ", lab),
          shiny::tags$span(style = sprintf(
            "display:inline-block; width:12px; height:12px; margin-left:6px; border-radius:2px; background:%s; vertical-align:middle;",
            color_map[[lab]]
          ))
        )
      })
    )
  )
}

# builds the json-ready payload for the 'loadImages' custom message.
# `show_overlay` (per-page checkbox) blanks overlayUrl entirely when off.
#
# EAGER annotation loading: every annotation file for every entry is parsed
# right here (via the cache, so repeat loads of the same file are instant)
# before the message is even sent — by the time an image appears, every one
# of its layers' polygons is already sitting in the browser, so checking a
# box just toggles visibility with no fetch delay. This trades a longer
# wait at Load/Compare time for annotations that are always instantly ready
# once the wait is over, which is the behavior actually being asked for
# here — the cost is that a comparison spanning many donors/layers can take
# a while up front, especially the first time each file is touched.
build_images_payload <- function(entries, overlay_opacity, show_overlay = TRUE) {
  color_map <- build_annotation_color_map(get_unique_annotation_labels(entries))
  
  lapply(entries, function(e) {
    ann_files <- e$slot$annotation_files
    if (length(ann_files) > 0) {
      ann_files <- ann_files[order(vapply(ann_files, function(f) f$name, character(1)))]
    }
    groups <- lapply(ann_files, function(f) {
      polys <- parse_halo_annotations_cached(f$url, e$slot$svs_width)
      color <- color_map[[f$name]]
      if (!is.null(color)) polys <- lapply(polys, function(p) { p$color <- color; p })
      list(label = f$name, polygons = polys)
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

# shows one donor's metadata as separate cards, grouped per
# metadata_display_groups (global.r) — edit that list to add/remove fields
# or reorder/regroup them; this function just renders whatever it's given.
render_donor_metadata_card <- function(donor_id) {
  row <- donor_metadata[donor_metadata$donor == donor_id, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  
  field_by_id <- stats::setNames(metadata_fields, vapply(metadata_fields, function(f) f$id, character(1)))
  
  cards <- lapply(names(metadata_display_groups), function(group_name) {
    field_ids <- metadata_display_groups[[group_name]]
    bslib::card(
      style = "min-width:220px;",
      bslib::card_header(group_name),
      bslib::card_body(
        shiny::tagList(lapply(field_ids, function(fid) {
          f <- field_by_id[[fid]]
          if (is.null(f)) return(NULL)
          shiny::tags$div(
            style = "margin-bottom:6px;",
            shiny::tags$strong(paste0(f$label, ": ")), as.character(row[[fid]])
          )
        }))
      )
    )
  })
  
  shiny::tagList(
    shiny::tags$hr(),
    shiny::strong("Donor metadata:"),
    shiny::div(style = "display:flex; flex-wrap:wrap; gap:16px; margin-top:8px;", cards)
  )
}