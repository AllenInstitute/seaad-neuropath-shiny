# =============================================================================
# functions.R — pure helper functions used by global.R and server.R
# =============================================================================

library(dplyr)

# true if a single-select input actually has a value chosen (not NULL and
# not the blank placeholder from with_placeholder()). used to gate the
# Load/Compare buttons via shinyjs — see server.r.
is_selected <- function(x) !is.null(x) && length(x) == 1 && nzchar(x)

# normalizes a donor id read from ANY csv (manifest, specimen metadata, QNP
# metadata) so the same donor always compares equal across all three
# sources, even if one file has leading/trailing whitespace or different
# letter casing than another — this is exactly the kind of mismatch that
# makes filter_donors_by_metadata()/filter_donors_identify_page() intersect
# against nothing and every donor go blank.
normalize_donor_id <- function(x) trimws(toupper(as.character(x)))

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
    donor <- normalize_donor_id(donor)
    
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

# no-op passthrough — manifest region values are already human-readable
# (e.g. "Dorsolateral Prefrontal Cortex (DLPFC)"), kept as a named hook in
# case that ever changes.
prettify_region <- function(region) {
  region
}

# prepends a blank "placeholder" choice so a single-select dropdown starts
# genuinely empty instead of defaulting to its first real option.
with_placeholder <- function(choices, label = "Select...") {
  if (length(choices) == 0) return(setNames("", label))
  c(setNames("", label), setNames(choices, choices))
}

# appends a "(n)" count of unique options to a dropdown's base label, per
# request — n is the count of REAL selectable choices, not counting the
# "Select..." placeholder with_placeholder() adds on top.
dropdown_label <- function(base_label, choices) {
  shiny::tagList(
    shiny::span(class = "filter-name-regular", base_label),
    shiny::span(class = "filter-count-light", sprintf(" (%d)", length(choices)))
  )
}

# lowercases text except for tokens that are ALREADY fully uppercase in
# the original (any uppercase letters, zero lowercase ones — regardless
# of digits/punctuation, so alphanumeric codes like "6E10" and "IBA1"
# are preserved the same as pure-letter acronyms like "DLPFC"/"APOE").
# Deliberately NOT used on donor IDs anywhere — those don't reliably
# "look like" acronyms (e.g. "H21.33.040" is mostly digits with one
# leading letter), so donor names are simply left out of every call site
# instead of relying on this heuristic to detect them.
smart_lowercase <- function(text) {
  if (is.null(text) || length(text) == 0) return(text)
  vapply(text, function(one) {
    if (is.na(one)) return(one)
    words <- strsplit(one, " ", fixed = TRUE)[[1]]
    words <- vapply(words, function(w) {
      letters_only <- gsub("[^A-Za-z]", "", w)
      has_upper <- grepl("[A-Z]", letters_only)
      has_lower <- grepl("[a-z]", letters_only)
      if (has_upper && !has_lower) w else tolower(w)
    }, character(1))
    paste(words, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
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

# fetches+parses annotation files CONCURRENTLY (curl multi-handle, capped
# at max_concurrent) instead of one at a time, since latency per file (not
# parsing) is the bottleneck. Results land in .annotation_cache as each
# finishes, so later parse_halo_annotations_cached() calls are cache hits.
# progress_callback(done, total), if given, fires after every file.
# file_specs is list(url=, ref_width=); duplicates are fetched only once.
fetch_annotations_concurrently <- function(file_specs, progress_callback = NULL, max_concurrent = 10) {
  keyed <- lapply(file_specs, function(s) {
    s$cache_key <- paste(s$url, s$ref_width, sep = "::")
    s
  })
  keyed <- keyed[!duplicated(vapply(keyed, function(s) s$cache_key, character(1)))]
  
  to_fetch <- Filter(function(s) !exists(s$cache_key, envir = .annotation_cache, inherits = FALSE), keyed)
  
  total <- length(keyed)
  done_count <- total - length(to_fetch)
  if (!is.null(progress_callback) && done_count > 0) progress_callback(done_count, total)
  
  if (length(to_fetch) > 0) {
    pool <- curl::new_pool(total_con = max_concurrent)
    
    lapply(to_fetch, function(spec) {
      # explicit, generous timeout (annotation_fetch_timeout_sec, global.r) —
      # curl_fetch_multi()'s default handle apparently times out around 10s,
      # too short for this endpoint under load.
      h <- curl::new_handle(timeout = annotation_fetch_timeout_sec, connecttimeout = 30)
      curl::curl_fetch_multi(
        spec$url,
        done = function(resp) {
          polys <- tryCatch(parse_halo_annotations(resp$content, spec$ref_width), error = function(e) {
            warning("could not parse ", spec$url, ": ", e$message)
            list()
          })
          assign(spec$cache_key, polys, envir = .annotation_cache)
          done_count <<- done_count + 1
          if (!is.null(progress_callback)) progress_callback(done_count, total)
        },
        fail = function(err) {
          warning("could not fetch ", spec$url, ": ", err)
          assign(spec$cache_key, list(), envir = .annotation_cache)
          done_count <<- done_count + 1
          if (!is.null(progress_callback)) progress_callback(done_count, total)
        },
        pool = pool,
        handle = h
      )
    })
    
    curl::multi_run(pool = pool)
  }
  
  invisible(NULL)
}

# donor metadata — fully spec-driven by metadata_fields (global.r). Each
# field declares its own csv_column; load_specimen_metadata_csv() only
# reads those declared columns, so any other column is silently ignored.

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

# 5-year age-at-death bucket labels (e.g. "65-69") for a vector of ages,
# spanning exactly that vector's own range — used for the QNP Graphs
# page's derived "age_bucket" categorical field (global.r). Returns both
# the per-donor bucket assignment and the full ordered set of levels,
# since bucket labels don't sort correctly as plain strings once any
# reach 3 digits (e.g. "100-104" < "95-99" alphabetically).
compute_age_buckets <- function(ages) {
  rng <- range(ages, na.rm = TRUE)
  lower <- floor(rng[1] / 5) * 5
  upper <- ceiling((rng[2] + 1) / 5) * 5
  breaks <- seq(lower, upper, by = 5)
  levels <- paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)
  list(
    values = as.character(cut(ages, breaks = breaks, labels = levels, right = FALSE, include.lowest = TRUE)),
    levels = levels
  )
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
  out <- data.frame(donor = normalize_donor_id(df[[donor_id_column]]), stringsAsFactors = FALSE)
  
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

# QNP — values keyed by donor + region + subregion, from a separate csv
# (qnp_fields/qnp_metadata_csv_path, global.r). Unlike
# load_specimen_metadata_csv(), a missing/unreadable file here doesn't
# stop the app — it just means no QNP filters are available yet.

empty_qnp_metadata <- function() {
  df <- data.frame(donor = character(0), region_grouping = character(0), region = character(0), subregion = character(0),
                   stringsAsFactors = FALSE)
  for (f in qnp_fields) df[[f$id]] <- numeric(0)
  df
}

load_qnp_metadata_csv <- function(path) {
  if (!file.exists(path) && !grepl("^https?://", path)) {
    warning("QNP metadata csv not found at '", path, "' — QNP filters will be unavailable until it exists.")
    return(empty_qnp_metadata())
  }
  
  df <- tryCatch(read.csv(path, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) {
    warning("could not read QNP metadata csv: ", e$message)
    NULL
  })
  if (is.null(df)) return(empty_qnp_metadata())
  
  required <- c(qnp_donor_column, qnp_region_column, qnp_subregion_column)
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    warning("QNP metadata csv is missing required column(s): ", paste(missing_required, collapse = ", "))
    return(empty_qnp_metadata())
  }
  
  out <- data.frame(
    donor     = normalize_donor_id(df[[qnp_donor_column]]),
    region    = df[[qnp_region_column]],
    subregion = df[[qnp_subregion_column]],
    stringsAsFactors = FALSE
  )
  
  # region_grouping is optional — falls back to region itself (a 1:1
  # grouping) when the configured column isn't actually in the csv, so a
  # wrong/unconfirmed qnp_region_grouping_column degrades gracefully
  # instead of breaking anything.
  out$region_grouping <- if (!is.null(qnp_region_grouping_column) && qnp_region_grouping_column %in% names(df)) {
    df[[qnp_region_grouping_column]]
  } else {
    out$region
  }
  
  for (f in qnp_fields) {
    col <- f$csv_column
    if (is.null(col) || !(col %in% names(df))) {
      warning("QNP metadata csv is missing column '", col, "' for field '", f$id, "' — that filter will be unavailable")
      out[[f$id]] <- NA_real_
      next
    }
    out[[f$id]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  out
}

# "Filter Donors" page — donor_metadata (one row per donor) and
# qnp_metadata (one row per donor+region+subregion) share the donor key
# but differ in cardinality. QNP browsing has two modes (region-then-
# subregion, or stain-across-every-region) via a radio button; only
# percent-type fields get sliders.
#
# Whether a slider is "active" is TRACKED, not inferred from comparing its
# value against the data range — histoslider can snap handles to bin
# edges, so an untouched slider's value isn't guaranteed to equal the
# range, which caused a real bug (silently excluding whichever donor held
# a field's extreme value). iddonors_filter_active() instead records each
# input's first observed value as its baseline and reports "active" only
# once the current value differs from it.

# creates the per-session store of slider baselines. Kept in a plain
# environment (not reactiveValues) deliberately: it's memoization, and
# writing to it must NOT invalidate the reactive that's reading it.
new_iddonors_baseline_store <- function() new.env(parent = emptyenv())

# TRUE once `val` differs from the first value ever seen for this input id.
# The very first call for an id records the baseline and returns FALSE.
iddonors_filter_active <- function(store, id, val) {
  if (is.null(val)) return(FALSE)
  if (!exists(id, envir = store, inherits = FALSE)) {
    assign(id, val, envir = store)
    return(FALSE)
  }
  !isTRUE(all.equal(val, get(id, envir = store, inherits = FALSE)))
}

# unique stain groups among the given fields (default: the already
# percent-only qnp_fields, for the sidebar's Stain-mode controls), in the
# order they're declared in global.r — not alphabetical.
get_qnp_stain_groups <- function(fields = qnp_fields) {
  groups <- vapply(fields, function(f) f$stain_group, character(1))
  groups[!duplicated(groups)]
}

# a field's label for display: "Percent positive area" -> "% positive area".
qnp_field_display_label <- function(label) {
  label <- sub("^Percent\\b", "%", label)
  sub("\\bpercent\\b", "%", label)
}

# TRUE when a region has more than one subregion — the only case where a
# "Global (average)" choice is meaningful. With a single subregion the
# average would just BE that subregion's value.
qnp_region_has_multiple_subregions <- function(region) {
  subs <- qnp_by_region[[region]]
  !is.null(subs) && length(subs) > 1
}

# region_grouping -> region lookups, mirroring the region -> subregion
# pattern above exactly (including the same "only show a selector when
# there's more than one option" collapsing rule).
get_qnp_groupings <- function() sort(unique(qnp_metadata$region_grouping))

get_qnp_regions_in_grouping <- function(grouping) {
  sort(unique(qnp_metadata$region[qnp_metadata$region_grouping == grouping]))
}

qnp_grouping_has_multiple_regions <- function(grouping) {
  length(get_qnp_regions_in_grouping(grouping)) > 1
}

# which subregion key to use when we want "the region as a whole": the
# Global average when there are several subregions, otherwise the single
# subregion's own values (an average of one thing is that thing).
# Builders, filters and reset ALL call this, so their widget ids can never
# drift apart — that drift was a real, separately-diagnosed bug.
qnp_region_level_key <- function(region) {
  if (qnp_region_has_multiple_subregions(region)) qnp_global_sentinel else names(qnp_by_region[[region]])[[1]]
}

# returns a data.frame(donor, value) for one field at one region +
# (subregion or "Global"). "Global" is a genuine per-donor MEAN across
# every subregion they have in that region — never a pooling of raw rows,
# which would double-count a donor once per subregion.
identify_qnp_field_values <- function(region, subregion_choice, field_id) {
  subregion_subsets <- qnp_by_region[[region]]
  if (is.null(subregion_subsets)) return(data.frame(donor = character(0), value = numeric(0)))
  
  if (identical(subregion_choice, qnp_global_sentinel)) {
    combined <- do.call(rbind, subregion_subsets)
    agg <- stats::aggregate(
      combined[[field_id]], by = list(donor = combined$donor),
      FUN = function(x) { m <- mean(x, na.rm = TRUE); if (is.nan(m)) NA_real_ else m }
    )
    names(agg) <- c("donor", "value")
    agg
  } else {
    subset <- subregion_subsets[[subregion_choice]]
    if (is.null(subset)) return(data.frame(donor = character(0), value = numeric(0)))
    data.frame(donor = subset$donor, value = subset[[field_id]])
  }
}

# a widget id unique to (region, subregion-or-"Global", field).
identify_donor_qnp_widget_id <- function(region, subregion_choice, field_id) {
  paste0("iddonors_qnpr_", gsub("[^A-Za-z0-9]+", "_", paste(region, subregion_choice, field_id, sep = "_")))
}

# one field's label + histoslider, for one region+subregion-or-Global view.
build_qnp_field_slider <- function(f, region, subregion_choice) {
  vals_df <- identify_qnp_field_values(region, subregion_choice, f$id)
  vals <- stats::na.omit(vals_df$value)
  if (length(vals) == 0) return(NULL)
  
  # explicit breaks spanning EXACTLY this region+subregion's real min/max.
  # Without this, hist()'s automatic Sturges binning (histoslider's own
  # default when no breaks are given — confirmed against its actual
  # source) rounds bin edges outward past the true data range, so the
  # slider's overall draggable track ends up wider than the values it's
  # actually built from, even though the initial selection (start/end)
  # was already scoped correctly.
  rng <- range(vals)
  breaks <- if (rng[1] == rng[2]) c(rng[1] - 0.5, rng[1] + 0.5) else seq(rng[1], rng[2], length.out = 21)
  
  shiny::tagList(
    shiny::h6(qnp_field_display_label(f$label), style = "margin-bottom:2px;"),
    build_histoslider(identify_donor_qnp_widget_id(region, subregion_choice, f$id), vals, breaks = breaks)
  )
}

# a card whose HEADER is `header` and whose body is one slider per field in
# `fields`, all at the given region+subregion view. Returns NULL when none
# of those fields has any data there (so callers can drop empty cards).
build_qnp_card <- function(header, fields, region, subregion_choice) {
  sliders <- Filter(Negate(is.null), lapply(fields, function(f) {
    build_qnp_field_slider(f, region, subregion_choice)
  }))
  if (length(sliders) == 0) return(NULL)
  bslib::card(
    style = "margin-bottom:12px;",
    bslib::card_header(header),
    bslib::card_body(shiny::tagList(sliders))
  )
}

# lays cards out in a responsive multi-column grid (CSS columns), so the
# QNP accordion panel reads as several columns rather than one long strip.
qnp_cards_in_columns <- function(cards, min_col_width = "320px") {
  shiny::tags$div(
    style = sprintf("display:grid; grid-template-columns:repeat(auto-fit, minmax(%s, 1fr)); gap:12px;", min_col_width),
    cards
  )
}

# Region mode: one card per stain, at the chosen region + subregion/Global.
build_iddonors_qnp_region_sliders <- function(region, subregion_choice) {
  cards <- Filter(Negate(is.null), lapply(get_qnp_stain_groups(), function(stain) {
    fields <- Filter(function(f) identical(f$stain_group, stain), qnp_fields)
    build_qnp_card(stain, fields, region, subregion_choice)
  }))
  if (length(cards) == 0) return(shiny::helpText("No percent-field data for this selection."))
  qnp_cards_in_columns(cards)
}

# Stain mode: one card per REGION (a "subcard" for each), each showing the
# chosen stain's fields at that region's region-level value — no region
# selector, every region is shown at once.
build_iddonors_qnp_stain_sliders <- function(stain) {
  fields <- Filter(function(f) identical(f$stain_group, stain), qnp_fields)
  cards <- Filter(Negate(is.null), lapply(sort(names(qnp_by_region)), function(region) {
    build_qnp_card(prettify_region(region), fields, region, qnp_region_level_key(region))
  }))
  if (length(cards) == 0) return(shiny::helpText("No percent-field data for this stain."))
  qnp_cards_in_columns(cards)
}

# region mode's controls: a region selector, then (dependent on it) a
# subregion-or-Global selector.
build_iddonors_qnp_region_controls <- function() {
  shiny::tagList(
    shiny::selectInput("iddonors_qnp_grouping_sel", "Region group", choices = with_placeholder(get_qnp_groupings())),
    shiny::uiOutput("iddonors_qnp_region_ui"),
    shiny::uiOutput("iddonors_qnp_subregion_ui"),
    shiny::checkboxInput("iddonors_qnp_hide_no_data", "Hide donors without QNP data for this region", value = TRUE)
  )
}

# stain mode's controls: just a stain selector (every region then appears
# as its own card — see build_iddonors_qnp_stain_sliders()).
build_iddonors_qnp_stain_controls <- function() {
  shiny::selectInput("iddonors_qnp_stain_sel", "Stain", choices = with_placeholder(get_qnp_stain_groups()))
}

# applies one field's slider to the running donor set, for one
# region+subregion view. NA values always pass ("unknown" never excludes),
# and the slider only counts as a filter once it's actually been moved off
# its baseline (see iddonors_filter_active()).
apply_qnp_slider_filter <- function(donors, region, subregion_choice, f, input, store) {
  id <- identify_donor_qnp_widget_id(region, subregion_choice, f$id)
  val <- histoslider_range(input[[id]])
  if (!iddonors_filter_active(store, id, val)) return(donors)
  
  vals_df <- identify_qnp_field_values(region, subregion_choice, f$id)
  bad <- vals_df$donor[!is.na(vals_df$value) & (vals_df$value < val[1] | vals_df$value > val[2])]
  setdiff(donors, bad)
}

# region mode's filter: the one selected region+subregion, all stains.
# TRUE for donors who have AT LEAST ONE QNP row for the given region (any
# subregion) — powers the "hide donors without QNP data" checkbox.
donors_with_qnp_data_for_region <- function(region) {
  subs <- qnp_by_region[[region]]
  if (is.null(subs)) return(character(0))
  unique(do.call(rbind, subs)$donor)
}

filter_donors_identify_qnp_region_mode <- function(input, store) {
  region <- input$iddonors_qnp_region_sel
  subregion_choice <- input$iddonors_qnp_subregion_sel
  if (!is_selected(input$iddonors_qnp_grouping_sel) || !is_selected(region) || !is_selected(subregion_choice)) return(donor_choices)
  
  donors <- donor_choices
  for (f in qnp_fields) donors <- apply_qnp_slider_filter(donors, region, subregion_choice, f, input, store)
  
  # default TRUE (checkboxInput's own default, and also enforced here via
  # %||% in case the widget hasn't been built yet) — donors never measured
  # in this region at all are hidden unless explicitly unchecked.
  hide_no_data <- isTRUE(input$iddonors_qnp_hide_no_data) || is.null(input$iddonors_qnp_hide_no_data)
  if (hide_no_data) {
    donors <- intersect(donors, donors_with_qnp_data_for_region(region))
  }
  donors
}

# stain mode's filter: the chosen stain's fields across EVERY region (each
# at its region-level value), matching the per-region cards shown.
filter_donors_identify_qnp_stain_mode <- function(input, store) {
  stain <- input$iddonors_qnp_stain_sel
  if (!is_selected(stain)) return(donor_choices)
  
  fields <- Filter(function(f) identical(f$stain_group, stain), qnp_fields)
  donors <- donor_choices
  for (region in names(qnp_by_region)) {
    for (f in fields) {
      donors <- apply_qnp_slider_filter(donors, region, qnp_region_level_key(region), f, input, store)
    }
  }
  donors
}

# one widget for a single Demographic/Clinical field, from donor_metadata.
build_identify_donor_field_widget <- function(f) {
  if (f$type == "range") {
    breaks <- seq(floor(f$min), ceiling(f$max), by = 1)
    shiny::tagList(
      shiny::strong(f$label),
      build_histoslider(paste0("iddonors_", f$id, "_range"), donor_metadata[[f$id]], breaks = breaks),
      shiny::tags$hr()
    )
  } else {
    shiny::tagList(
      shiny::checkboxGroupInput(paste0("iddonors_", f$id, "_sel"), f$label, choices = stats::setNames(f$choices, smart_lowercase(f$choices)), selected = character(0)),
      shiny::plotOutput(paste0("iddonors_hist_", f$id), height = "170px"),
      shiny::tags$hr()
    )
  }
}

# registers the categorical bar-chart histograms (numeric fields need no
# registration — histoslider draws its own histogram inline).
register_identify_donors_histograms <- function(output) {
  register_metadata_histograms(output, "iddonors", donor_metadata)
}

# one accordion panel per metadata group, each holding that group's widgets.
build_identify_donors_metadata_accordion <- function() {
  panels <- lapply(names(metadata_display_groups), function(group_name) {
    bslib::accordion_panel(
      title = group_name,
      shiny::tagList(lapply(metadata_display_groups[[group_name]], function(fid) {
        f <- Find(function(x) identical(x$id, fid), metadata_fields)
        if (is.null(f)) return(NULL)
        build_identify_donor_field_widget(f)
      }))
    )
  })
  do.call(bslib::accordion, c(list(id = "iddonors_meta_accordion", open = FALSE), panels))
}

# the QNP accordion: one panel whose body is the mode radio + dynamic
# controls + the multi-column sliders. Always the filtering/slider view —
# global-vs-specific-subregion is chosen later, via the subregion
# selector's own "Global (average)" option (see
# register_identify_donors_qnp()'s iddonors_qnp_subregion_ui below).
build_identify_donors_qnp_accordion <- function() {
  body <- if (length(qnp_by_region) == 0) {
    shiny::helpText("No QNP data loaded yet.")
  } else {
    shiny::tagList(
      shiny::radioButtons(
        "iddonors_qnp_mode", "Browse QNP by:",
        choices = c("Region" = "region", "Stain" = "stain"), selected = "region", inline = TRUE
      ),
      shiny::uiOutput("iddonors_qnp_mode_ui"),
      shiny::uiOutput("iddonors_qnp_sliders_ui")
    )
  }
  bslib::accordion(bslib::accordion_panel(title = hdg_qnp, body), id = "iddonors_qnp_accordion", open = FALSE)
}

# sets up every QNP-mode-dependent renderUI — called once per session.
register_identify_donors_qnp <- function(output, input) {
  output$iddonors_qnp_mode_ui <- shiny::renderUI({
    if (identical(input$iddonors_qnp_mode, "stain")) build_iddonors_qnp_stain_controls() else build_iddonors_qnp_region_controls()
  })
  
  output$iddonors_qnp_region_ui <- shiny::renderUI({
    shiny::req(is_selected(input$iddonors_qnp_grouping_sel))
    grouping <- input$iddonors_qnp_grouping_sel
    regions <- get_qnp_regions_in_grouping(grouping)
    if (!qnp_grouping_has_multiple_regions(grouping)) {
      # nothing to disambiguate — still a REAL selectInput (hidden inside
      # our own div, not fought with Shiny's internals) so downstream code
      # that reads iddonors_qnp_region_sel works identically either way.
      return(shiny::tags$div(
        style = "display:none;",
        shiny::selectInput("iddonors_qnp_region_sel", NULL, choices = regions, selected = if (length(regions) == 1) regions[1] else NULL)
      ))
    }
    shiny::selectInput("iddonors_qnp_region_sel", "Region", choices = with_placeholder(regions))
  })
  
  output$iddonors_qnp_subregion_ui <- shiny::renderUI({
    shiny::req(is_selected(input$iddonors_qnp_region_sel))
    region <- input$iddonors_qnp_region_sel
    subregions <- sort(names(qnp_by_region[[region]]))
    # "Global (average)" only offered with >1 subregion — with just one,
    # the average IS that subregion's value, so it'd be a duplicate choice.
    choices <- if (qnp_region_has_multiple_subregions(region)) {
      c("Global (average)" = qnp_global_sentinel, subregions)
    } else {
      subregions
    }
    shiny::selectInput("iddonors_qnp_subregion_sel", "Subregion", choices = choices)
  })
  
  output$iddonors_qnp_sliders_ui <- shiny::renderUI({
    if (identical(input$iddonors_qnp_mode, "stain")) {
      shiny::req(is_selected(input$iddonors_qnp_stain_sel))
      build_iddonors_qnp_stain_sliders(input$iddonors_qnp_stain_sel)
    } else {
      shiny::req(is_selected(input$iddonors_qnp_grouping_sel), is_selected(input$iddonors_qnp_region_sel), is_selected(input$iddonors_qnp_subregion_sel))
      build_iddonors_qnp_region_sliders(input$iddonors_qnp_region_sel, input$iddonors_qnp_subregion_sel)
    }
  })
}

# reads every iddonors_* input and filters donor_choices down.
# Range fields (Demographic/Clinical and QNP alike) only filter once their
# slider has actually moved off its baseline; NA/unknown always passes.
filter_donors_identify_page <- function(input, store) {
  donors <- donor_choices
  
  for (f in metadata_fields) {
    if (f$type == "range") {
      id <- paste0("iddonors_", f$id, "_range")
      val <- histoslider_range(input[[id]])
      if (iddonors_filter_active(store, id, val)) {
        keep <- donor_metadata$donor[is.na(donor_metadata[[f$id]]) |
                                       (donor_metadata[[f$id]] >= val[1] & donor_metadata[[f$id]] <= val[2])]
        donors <- intersect(donors, keep)
      }
    } else {
      val <- input[[paste0("iddonors_", f$id, "_sel")]]
      if (!is.null(val) && length(val) > 0) {
        keep <- donor_metadata$donor[donor_metadata[[f$id]] %in% val]
        donors <- intersect(donors, keep)
      }
    }
  }
  
  qnp_keep <- if (identical(input$iddonors_qnp_mode, "stain")) {
    filter_donors_identify_qnp_stain_mode(input, store)
  } else {
    filter_donors_identify_qnp_region_mode(input, store)
  }
  intersect(donors, qnp_keep)
}

# resets every filter back to "no restriction", including the QNP
# region/stain choice itself — not just its sliders — so a prior Region
# vs Stain mode and its region/subregion or stain pick never silently
# survives a reset. Also CLEARS the baseline store, so freshly-built
# sliders record new baselines rather than reading as deliberate
# filtering.
reset_identify_donors_filters <- function(input, session, store) {
  rm(list = ls(envir = store, all.names = TRUE), envir = store)
  
  shiny::updateCheckboxInput(session, "iddonors_qnp_hide_no_data", value = TRUE)
  
  for (f in metadata_fields) {
    if (f$type == "range") {
      rng <- suppressWarnings(range(donor_metadata[[f$id]], na.rm = TRUE))
      if (all(is.finite(rng))) {
        histoslider::update_histoslider(paste0("iddonors_", f$id, "_range"), start = rng[1], end = rng[2], session = session)
      }
    } else {
      shiny::updateCheckboxGroupInput(session, paste0("iddonors_", f$id, "_sel"), selected = character(0))
    }
  }
  
  # unselect the QNP browse-by choice itself (mode + every cascading
  # selector), regardless of which is currently visible — a fresh
  # selection afterward builds new sliders at the full data range
  # automatically, so there's nothing left to reset sliders for here.
  shiny::updateRadioButtons(session, "iddonors_qnp_mode", selected = "region")
  shiny::updateSelectInput(session, "iddonors_qnp_grouping_sel", selected = "")
  shiny::updateSelectInput(session, "iddonors_qnp_region_sel", selected = "")
  shiny::updateSelectInput(session, "iddonors_qnp_subregion_sel", selected = "")
  shiny::updateSelectInput(session, "iddonors_qnp_stain_sel", selected = "")
}

# every metadata column for the matching donors, ready for display or
# download — all of donor_metadata's fields, donor id first.
identify_donors_table_data <- function(donor_ids, exclude_ids = character(0)) {
  field_ids <- setdiff(vapply(metadata_fields, function(f) f$id, character(1)), exclude_ids)
  cols <- c("donor", field_ids)
  cols <- intersect(cols, names(donor_metadata))
  df <- donor_metadata[donor_metadata$donor %in% donor_ids, cols, drop = FALSE]
  df <- df[order(df$donor), , drop = FALSE]
  for (nm in names(df)) if (is.numeric(df[[nm]])) df[[nm]] <- signif(df[[nm]], 4)
  # human-readable headers, matching each field's configured label
  labels <- c("Donor", vapply(metadata_fields, function(f) f$label, character(1)))
  names(labels) <- c("donor", vapply(metadata_fields, function(f) f$id, character(1)))
  names(df) <- unname(labels[names(df)])
  df
}

# QNP in wide form: one row per donor, one column per measure per region
# — pivoted from qnp_metadata's long form (a row per donor+region+
# subregion) so each donor fits a single row alongside demographic/
# clinical columns. Column names are "<Region> | <measure>", or
# "<Region> | <Subregion> | <measure>" only where a region has more than
# one subregion (averaging there would silently lose data). Uses
# qnp_fields_all (every measure, not just the on-page percent-only
# sliders) — download/popup only, never the page's own table.
build_qnp_wide_columns <- function(donor_ids) {
  out <- data.frame(donor = sort(donor_ids), stringsAsFactors = FALSE)
  if (nrow(qnp_metadata) == 0 || length(donor_ids) == 0) return(out)
  
  for (region in sort(names(qnp_by_region))) {
    multi <- qnp_region_has_multiple_subregions(region)
    for (subregion in sort(names(qnp_by_region[[region]]))) {
      subset <- qnp_by_region[[region]][[subregion]]
      if (is.null(subset) || nrow(subset) == 0) next
      
      prefix <- if (multi) {
        paste0(prettify_region(region), " | ", subregion, " | ")
      } else {
        paste0(prettify_region(region), " | ")
      }
      
      for (f in qnp_fields_all) {
        if (!(f$id %in% names(subset))) next
        vals <- subset[[f$id]]
        if (all(is.na(vals))) next  # nothing measured here — skip the column entirely
        # match on donor so rows line up regardless of subset ordering
        out[[paste0(prefix, f$label)]] <- signif(vals[match(out$donor, subset$donor)], 6)
      }
    }
  }
  out
}

# the full download payload: demographic + clinical columns (the same ones
# the on-page table shows) joined to every QNP measure in wide form.
identify_donors_export_data <- function(donor_ids) {
  base <- identify_donors_table_data(donor_ids)
  wide <- build_qnp_wide_columns(donor_ids)
  # identify_donors_table_data() renames "donor" -> "Donor" for display
  names(wide)[names(wide) == "donor"] <- "Donor"
  merge(base, wide, by = "Donor", all.x = TRUE, sort = TRUE)
}

# the "Sync zoom/pan across images" checkbox + info tooltip, identical on
# all three comparison pages apart from id and default.
sync_zoom_control <- function(id, default_checked) {
  checkbox_attrs <- list(type = "checkbox", id = id, style = "margin:0;")
  if (isTRUE(default_checked)) checkbox_attrs$checked <- NA  # NA renders the bare boolean HTML attribute; omitting it entirely leaves the box unchecked
  
  shiny::tagList(
    shiny::tags$div(
      style = "display:flex; align-items:center; gap:6px;",
      bslib::tooltip(shiny::tags$span(style = sprintf("color:%s;", icon_color), bsicons::bs_icon("info-circle-fill")), tt_sync_zoom, placement = "right"),
      do.call(shiny::tags$input, checkbox_attrs),
      shiny::tags$label(`for` = id, style = "margin:0; cursor:pointer; font-weight:bold;", "Sync zoom/pan across images")
    ),
    shiny::tags$div(style = "height:10px;")
  )
}

# one external top-bar link (sea-ad.org/brain-map.org/github) — identical
# styling apart from href and label.
top_bar_link <- function(href, label) {
  shiny::tags$a(
    href = href, target = "_blank",
    style = sprintf("color:%s; text-decoration:none; display:flex; align-items:center; gap:4px; font-size:0.9rem;", icon_color),
    label, bsicons::bs_icon("arrow-up-right")
  )
}

# shared style for the purple card-header look used across the donor
# popup's Demographic/Clinical/QNP cards (previously copy-pasted three
# times below, each with a hardcoded hex instead of referencing
# metadata_chart_color).
purple_card_header_style <- function() {
  sprintf("background:%s; color:#fff; font-weight:600;", metadata_chart_color)
}

# every metadata value for ONE donor — demographic, clinical, and each QNP
# row on record. This is the body of the click-a-donor-name popup; it is
# not rendered inline anywhere on the page.
render_donor_all_metadata <- function(donor_id) {
  row <- donor_metadata[donor_metadata$donor == donor_id, , drop = FALSE]
  if (nrow(row) == 0) return(shiny::p("No metadata found for this donor."))
  
  field_by_id <- stats::setNames(metadata_fields, vapply(metadata_fields, function(f) f$id, character(1)))
  
  group_cards <- lapply(names(metadata_display_groups), function(group_name) {
    bslib::card(
      style = "margin-bottom:10px;",
      bslib::card_header(group_name, style = purple_card_header_style()),
      bslib::card_body(
        gap = "0px",
        shiny::tagList(lapply(metadata_display_groups[[group_name]], function(fid) {
          f <- field_by_id[[fid]]
          if (is.null(f)) return(NULL)
          shiny::tags$div(
            shiny::tags$strong(paste0(f$label, ": ")),
            shiny::tags$span(as.character(row[[fid]]))
          )
        }))
      )
    )
  })
  
  donor_regions <- sort(unique(qnp_metadata$region[qnp_metadata$donor == donor_id]))
  qnp_section <- if (length(donor_regions) == 0) {
    bslib::card(
      style = "margin-bottom:10px;",
      bslib::card_header(hdg_qnp, style = purple_card_header_style()),
      bslib::card_body(shiny::p("No QNP data on record for this donor."))
    )
  } else {
    region_choices <- stats::setNames(donor_regions, vapply(donor_regions, prettify_region, character(1)))
    bslib::card(
      style = "margin-bottom:10px;",
      bslib::card_header(hdg_qnp, style = purple_card_header_style()),
      bslib::card_body(
        # one region per line (not inline) — click a region to see its
        # subregions below. starts with nothing checked (selected =
        # character(0)) rather than defaulting to the first region, so
        # no data shows until the user actually picks one.
        shiny::radioButtons("iddonors_popup_region_sel", NULL, choices = region_choices, selected = character(0), inline = FALSE),
        shiny::selectInput("iddonors_popup_stain_sel", "Stain", choices = with_placeholder(get_qnp_stain_groups(qnp_fields_all))),
        shiny::radioButtons(
          "iddonors_popup_qnp_view_mode", "Show QNP as:",
          choices = c("Global average (% only)" = "global", "All values by layer" = "layers"),
          selected = "layers", inline = TRUE
        ),
        shiny::uiOutput("iddonors_popup_qnp_ui")
      )
    )
  }
  
  shiny::tagList(group_cards, qnp_section)
}

# one region's QNP detail for the popup.
# view_mode "layers": every subregion (layer) as its own card, every
#   measure (qnp_fields_all).
# view_mode "global": ONE card — every percent-type field (qnp_fields,
#   already percent-only), averaged across this donor's subregions in
#   this region.
# stain, if given, narrows either field list down to just that one
# stain_group — NULL (the default) shows every stain's fields.
render_donor_qnp_region_detail <- function(donor_id, region, view_mode = "layers", stain = NULL) {
  rows <- qnp_metadata[qnp_metadata$donor == donor_id & qnp_metadata$region == region, , drop = FALSE]
  if (nrow(rows) == 0) return(shiny::p("No QNP data on record for this donor in this region."))
  
  # tight spacing between value lines. NOTE: bslib::card_body() is a
  # flex container with its own default gap between children — that gap
  # overrides any margin set on the children themselves, so it (not
  # line_style) is the actual lever for inter-line spacing here.
  line_style <- "margin:0; line-height:1.15;"
  
  narrow_to_stain <- function(fields) {
    if (is.null(stain)) return(fields)
    Filter(function(f) identical(f$stain_group, stain), fields)
  }
  
  if (identical(view_mode, "global")) {
    fields <- narrow_to_stain(qnp_fields)
    vals <- Filter(Negate(is.null), lapply(fields, function(f) {
      v <- mean(rows[[f$id]], na.rm = TRUE)
      if (is.na(v)) return(NULL)
      shiny::tags$div(style = line_style, shiny::tags$strong(paste0(qnp_field_display_label(f$label), ": ")), shiny::tags$span(signif(v, 4)))
    }))
    if (length(vals) == 0) return(shiny::p("No percent-field data for this donor/region/stain."))
    return(bslib::card(bslib::card_body(vals, gap = "0px")))
  }
  
  fields <- narrow_to_stain(qnp_fields_all)
  cards <- Filter(Negate(is.null), lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, , drop = FALSE]
    vals <- Filter(Negate(is.null), lapply(fields, function(f) {
      v <- r[[f$id]]
      if (is.null(v) || is.na(v)) return(NULL)
      shiny::tags$div(style = line_style, shiny::tags$strong(paste0(f$label, ": ")), shiny::tags$span(signif(v, 4)))
    }))
    if (length(vals) == 0) return(NULL)
    bslib::card(
      style = "margin-bottom:8px;",
      bslib::card_header(r$subregion),
      bslib::card_body(vals, gap = "0px")
    )
  }))
  if (length(cards) == 0) return(shiny::p("No QNP data for this stain in this region."))
  shiny::tagList(cards)
}

# a plain HTML table of the full metadata for every matching donor.
render_identify_donors_table <- function(donor_ids) {
  if (length(donor_ids) == 0) return(shiny::helpText("No donors match the current filters."))
  df <- identify_donors_table_data(donor_ids, exclude_ids = c("years_education", "thal_phase", "braak_stage"))
  
  header <- shiny::tags$tr(lapply(names(df), shiny::tags$th))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    shiny::tags$tr(lapply(names(df), function(nm) {
      # the Donor cell is a link: clicking it pushes that donor's id to the
      # server, which opens the all-metadata popup (see server.r). Done via
      # Shiny.setInputValue rather than per-row observers, since the row set
      # changes with every filter change.
      if (identical(nm, "Donor")) {
        donor_id <- as.character(df[i, nm])
        shiny::tags$td(
          shiny::tags$div(
            style = "display:flex; align-items:center; gap:6px; white-space:nowrap;",
            shiny::tags$a(
              href = "javascript:void(0)",
              onclick = sprintf("Shiny.setInputValue('iddonors_clicked_donor', '%s', {priority: 'event'})", donor_id),
              donor_id
            ),
            shiny::tags$span(
              style = sprintf("cursor:pointer; color:%s;", metadata_chart_color),
              title = tt_copy_donor_id,
              onclick = sprintf("copyTextRobust('%s', this)", donor_id),
              bsicons::bs_icon("copy")
            )
          )
        )
      } else {
        shiny::tags$td(as.character(df[i, nm]))
      }
    }))
  })
  
  shiny::tags$div(
    style = "overflow-x:auto;",
    shiny::tags$table(
      class = "table table-striped table-hover",
      shiny::tags$thead(header),
      shiny::tags$tbody(rows)
    )
  )
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
  
  result <- if (is.list(val) && !is.null(val$start) && !is.null(val$end)) {
    c(val$start, val$end)
  } else {
    suppressWarnings(as.numeric(unlist(val)))
  }
  
  # anything that doesn't cleanly resolve to exactly two real numbers (e.g.
  # an empty list() sent as the widget's initial value before it's ever been
  # touched) is treated as "no filter yet" rather than accidentally becoming
  # a val[1]/val[2] of NA, which would make every donor fail the comparison
  # and get excluded — this was the actual cause of donors going blank.
  if (length(result) != 2 || anyNA(result)) return(NULL)
  result
}

# reads the meta_* inputs for a given page prefix and filters donor_metadata
# down with dplyr. returns the vector of matching donor ids.
#
# type "range"  -> a [min,max] filter from the histoslider's dragged range.
# type "select" -> an unordered %in% filter from the checkbox group.
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

# wraps input_histoslider() with the shared metadata_chart_color. Color
# props confirmed against the react component source (samhogg/histoslider
# Histoslider.js): selectedColor/unselectedColor, not color (an earlier
# guess that silently did nothing) — selectedColor tints the dragged
# range, unselectedColor the rest.
build_histoslider <- function(id, values, breaks = NULL) {
  # breaks: histoslider's own default is rlang::missing_arg(), not NULL —
  # passing NULL makes its internal hist() fail, so omit the argument
  # entirely via do.call() when none is given.
  # start/end: explicitly set to the field's real data range so the
  # widget's initial selection is exactly what we tell it, not something
  # it independently infers.
  clean_vals <- stats::na.omit(values)
  rng <- if (length(clean_vals) > 0) range(clean_vals) else c(0, 1)
  
  args <- list(
    id, NULL, values,
    start = rng[1], end = rng[2],
    options = list(selectedColor = metadata_chart_color, unselectedColor = metadata_chart_color)
  )
  if (!is.null(breaks)) args$breaks <- breaks
  do.call(histoslider::input_histoslider, args)
}

# one bslib accordion_panel per metadata field:
#   "range"  -> a histoslider (histogram + range filter combined).
#   "select" -> a plain checkboxGroupInput (label immediately next to each
#               checkbox) plus a separate histogram registered server-side
#               via register_metadata_histograms().
# widget ids are prefixed per page.
#
# builds one accordion per page with two top-level panels, "demographic"
# and "clinical" (from metadata_display_groups, global.r), each holding a
# sub-accordion with one panel per field in that group. Used by the four
# regular pages' "Filter donors by metadata" — QNP filtering lives entirely
# on the separate Identify Donors page instead (build_identify_donors_metadata_accordion()).
build_metadata_accordion <- function(prefix, data) {
  field_by_id <- stats::setNames(metadata_fields, vapply(metadata_fields, function(f) f$id, character(1)))
  
  build_field_widget <- function(f) {
    if (f$type == "range") {
      # breaks at every integer so each bin has width 1 — gives a much finer
      # histogram than histoslider's default automatic binning.
      breaks <- seq(floor(f$min), ceiling(f$max), by = 1)
      build_histoslider(paste0(prefix, "_meta_", f$id, "_range"), data[[f$id]], breaks = breaks)
    } else {
      shiny::tagList(
        shiny::checkboxGroupInput(
          paste0(prefix, "_meta_", f$id, "_sel"), label = NULL,
          choices = stats::setNames(f$choices, smart_lowercase(f$choices)), selected = character(0)
        ),
        shiny::plotOutput(paste0(prefix, "_hist_", f$id), height = "170px")
      )
    }
  }
  
  # one sub-accordion panel per field (same as the original pre-QNP layout),
  # nested inside each group's own top-level panel.
  group_panels <- lapply(names(metadata_display_groups), function(group_name) {
    field_ids <- metadata_display_groups[[group_name]]
    field_panels <- lapply(field_ids, function(fid) {
      f <- field_by_id[[fid]]
      if (is.null(f)) return(NULL)
      bslib::accordion_panel(title = f$label, build_field_widget(f))
    })
    field_panels <- Filter(Negate(is.null), field_panels)
    
    sub_accordion <- do.call(bslib::accordion, c(
      list(id = paste0(prefix, "_meta_group_", tolower(gsub("[^A-Za-z0-9]+", "_", group_name))), open = FALSE),
      field_panels
    ))
    bslib::accordion_panel(title = group_name, sub_accordion)
  })
  
  do.call(bslib::accordion, c(list(id = paste0(prefix, "_metadata_accordion"), open = FALSE), group_panels))
}

# registers the renderPlot output for every categorical ("select") field's
# histogram under a page's prefix. call once per page (outside any
# observer) that includes a metadata accordion. shows the OVERALL
# distribution across all donors, not a live-filtered one. no y-axis (counts
# are labeled directly on top of each bar instead), bars ordered to match
# metadata_fields' declared choices, x-axis labels horizontal.
register_metadata_histograms <- function(output, prefix, data) {
  for (f in metadata_fields) {
    if (f$type != "select") next
    local({
      fld <- f
      output[[paste0(prefix, "_hist_", fld$id)]] <- shiny::renderPlot({
        # factor levels = fld$choices, so bar order always matches the
        # order declared in metadata_fields (global.r), not table()'s
        # default alphabetical ordering.
        counts <- table(factor(data[[fld$id]], levels = fld$choices))
        graphics::par(mar = c(4, 1, 2, 1))
        bp <- graphics::barplot(
          counts, col = metadata_chart_color, border = NA,
          yaxt = "n", xaxt = "n", ylim = c(0, max(counts) * 1.15)
        )
        graphics::text(x = bp, y = counts, labels = counts, pos = 3, cex = 1.1, xpd = TRUE)
        graphics::text(
          x = bp, y = graphics::par("usr")[3], labels = smart_lowercase(names(counts)),
          srt = 0, adj = c(0.5, 1.3), xpd = TRUE, cex = 1.1
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

# the clickable donor-info trigger — an icon that opens a modal with that
# donor's metadata + QNP crosswalk (see the shared
# observeEvent(input$donor_info_click, ...) in server.r). Shared across
# every page that shows one.
#
# Deliberately NOT bslib::popover()/tooltip() — those have a confirmed
# upstream limitation (rstudio/bslib#1019) where a trigger stops being
# interactive once its surrounding content was inserted dynamically (every
# context card/viewer grid here is renderUI content). A plain onclick +
# Shiny.setInputValue + modal sidesteps it, since it only depends on
# Shiny's own input binding, which does rescan the DOM after every render.
#
# region/stain may be NULL (e.g. Compare Regions, where region varies per
# image) — render_donor_metadata_qnp_block() treats NULL as "omit the QNP
# section", so passing NULL is how that section gets skipped.
#
# qnp_all_fields travels through as allFields: it tells the QNP block to
# show every field regardless of this stain's own crosswalk group (used
# by Compare Stains), while stain itself still reaches the popup so its
# header can name which stain the card belongs to.
donor_info_trigger <- function(donor_id, region = NULL, stain = NULL, mode = "combined", icon = "person-vcard", qnp_all_fields = FALSE) {
  shiny::tags$span(
    bsicons::bs_icon(icon),
    style = sprintf("color:%s; cursor:pointer; margin-left:6px;", icon_color),
    title = paste("Donor", donor_id),
    onclick = sprintf(
      "Shiny.setInputValue('donor_info_click', {donor: '%s', region: '%s', stain: '%s', mode: '%s', allFields: %s}, {priority: 'event'})",
      donor_id, region %||% "", stain %||% "", mode, if (isTRUE(qnp_all_fields)) "true" else "false"
    )
  )
}

render_viewer_grid_ui <- function(entries, label_field = c("stain", "donor", "region"), donor_info_style = c("none", "demo_only", "qnp_only", "split", "combined"), qnp_icon = "file-earmark-bar-graph", qnp_all_fields = FALSE) {
  label_field <- match.arg(label_field)
  donor_info_style <- match.arg(donor_info_style)
  if (length(entries) == 0) return(NULL)  # blank until something is actually loaded
  col_width <- if (length(entries) == 1) 12 else 6
  shiny::tagList(shiny::fluidRow(lapply(entries, function(e) {
    cid   <- paste0("osd-", safe_id(e$donor, e$stain, e$region))
    label <- if (label_field == "region") prettify_region(e$region) else e[[label_field]]
    
    triggers <- switch(donor_info_style,
                       "none"      = NULL,
                       "demo_only" = donor_info_trigger(e$donor, mode = "demo", icon = "person-vcard"),
                       "qnp_only"  = donor_info_trigger(e$donor, e$region, e$stain, mode = "qnp", icon = qnp_icon, qnp_all_fields = qnp_all_fields),
                       "combined"  = donor_info_trigger(e$donor, e$region, e$stain, mode = "combined", icon = "person-vcard"),
                       "split"     = shiny::tagList(
                         donor_info_trigger(e$donor, mode = "demo", icon = "person-vcard"),
                         donor_info_trigger(e$donor, e$region, e$stain, mode = "qnp", icon = qnp_icon, qnp_all_fields = qnp_all_fields)
                       )
    )
    heading <- if (is.null(triggers)) {
      shiny::h5(label)
    } else {
      shiny::tags$div(
        style = "display:flex; align-items:center; gap:6px;",
        shiny::h5(style = "margin:0;", label),
        triggers
      )
    }
    
    shiny::column(
      width = col_width,
      heading,
      shiny::tags$div(
        id = cid,
        style = "width:100%; height:450px; background:#000; border:1px solid #ccc; position:relative; margin-bottom:6px;"
      ),
      shiny::tags$hr()
    )
  })))
}

render_annotation_master_ui <- function(entries, id_prefix = "ann", varying_field = NULL) {
  if (length(entries) == 0) return(NULL)
  
  all_labels <- get_unique_annotation_labels(entries)
  if (length(all_labels) == 0) return(NULL)
  
  color_map <- build_annotation_color_map(all_labels)
  
  # id_prefix keeps ids unique across pages — all four pages' checklists
  # coexist in the DOM at once (navbarPage renders every tab up front), so
  # two pages both showing a "Layer1" label would otherwise collide.
  build_checkbox_row <- function(lab) {
    cb_id <- paste0(id_prefix, "_toggle_", gsub("[^A-Za-z0-9]+", "_", lab))
    # a plain "form-check" div — the same Bootstrap classes bslib's own
    # checkboxInput() renders under the hood — so this looks identical to
    # every other checkbox in the app, even though (unlike a real
    # checkboxInput) it's purely client-side: toggling annotation
    # visibility doesn't need a server round-trip at all.
    shiny::div(
      class = "form-check",
      style = "display:flex; align-items:center; gap:6px;",
      shiny::tags$input(
        class = "form-check-input", type = "checkbox", id = cb_id,
        style = "margin:0;",
        onclick = sprintf("toggleAnnotationLabel('%s', this.checked)", lab)
      ),
      shiny::tags$label(class = "form-check-label", `for` = cb_id, style = "margin:0;", smart_lowercase(lab)),
      shiny::tags$span(style = sprintf(
        "display:inline-block; width:18px; height:18px; border-radius:3px; background:%s; flex-shrink:0;",
        color_map[[lab]]
      ))
    )
  }
  
  # row-gap (spacing between WRAPPED lines) cut to roughly an eighth of the
  # original 40px; column-gap (spacing between items on the same line) is
  # unchanged — the request was specifically about line-to-line spacing.
  checkbox_group <- function(labels) {
    shiny::div(style = "display:flex; flex-wrap:wrap; column-gap:40px; row-gap:5px; margin-top:8px;", lapply(labels, build_checkbox_row))
  }
  
  # a label is "shared" only if EVERY entry that has any annotations at all
  # actually has that exact label — entries with none don't count against
  # it (nothing to compare there).
  entry_label_sets <- Filter(function(x) length(x) > 0, lapply(entries, function(e) {
    if (length(e$slot$annotation_files) == 0) return(character(0))
    vapply(e$slot$annotation_files, function(f) f$name, character(1))
  }))
  shared_labels <- if (length(entry_label_sets) > 0) Reduce(intersect, entry_label_sets) else character(0)
  specific_labels <- setdiff(all_labels, shared_labels)
  
  # flat list when there's nothing to distinguish — either every entry has
  # the exact same annotations, or the page has no varying dimension to
  # group the "specific" ones by (e.g. Home's single entry).
  if (length(specific_labels) == 0 || is.null(varying_field)) {
    return(shiny::tagList(shiny::h5(hdg_annotations), checkbox_group(all_labels)))
  }
  
  # group the SPECIFIC labels by WHICH SET OF ENTRIES actually has each one
  # — not by entry. Labels owned by the exact same set of entries collapse
  # into ONE heading (the entries' names, comma-joined) instead of each
  # entry getting its own separate, possibly-duplicate heading: if X, Y and
  # Z all have layers 1-4 but only Z also has layer 5, that's a heading
  # "X, Y, Z" for 1-4 and a separate heading "Z" for just 5 — not three
  # near-identical "X" / "Y" / "Z" sections repeating 1-4 each time.
  label_owners <- list()
  for (e in entries) {
    own_labels <- if (length(e$slot$annotation_files) > 0) vapply(e$slot$annotation_files, function(f) f$name, character(1)) else character(0)
    own_specific <- intersect(own_labels, specific_labels)
    if (length(own_specific) == 0) next
    value_name <- if (identical(varying_field, "region")) prettify_region(e$region) else e[[varying_field]]
    for (lab in own_specific) {
      label_owners[[lab]] <- union(label_owners[[lab]] %||% character(0), value_name)
    }
  }
  
  owner_key <- function(owners) paste(sort(owners), collapse = "\u0001")
  groups <- list()  # owner-set key -> list(owners = c(...), labels = c(...))
  for (lab in names(label_owners)) {
    key <- owner_key(label_owners[[lab]])
    if (is.null(groups[[key]])) groups[[key]] <- list(owners = label_owners[[lab]], labels = character(0))
    groups[[key]]$labels <- c(groups[[key]]$labels, lab)
  }
  
  shiny::tagList(
    shiny::h5(hdg_annotations),
    # "If there are no shared annotations, do not show that heading" — so
    # this section is skipped entirely rather than showing an empty one.
    if (length(shared_labels) > 0) shiny::tagList(shiny::h6(hdg_shared), checkbox_group(shared_labels)),
    shiny::tagList(lapply(groups, function(g) {
      shiny::tagList(
        shiny::h6(paste(sort(g$owners), collapse = ", "), style = "margin-top:14px;"),
        checkbox_group(g$labels)
      )
    }))
  )
}

# builds the json-ready payload for the 'loadImages' custom message.
# show_overlay (per-page checkbox) blanks overlayUrl entirely when off.
#
# EAGER annotation loading: every entry's annotation files are parsed here
# (via the cache) before the message is sent, so once an image appears,
# checking a box just toggles visibility with no fetch delay. Trades a
# longer wait at Load/Compare time for instant toggling afterward.
build_images_payload <- function(entries, overlay_opacity, show_overlay = TRUE, progress_callback = NULL) {
  color_map <- build_annotation_color_map(get_unique_annotation_labels(entries))
  
  # fetch every annotation file across all entries CONCURRENTLY in one
  # batch first (see fetch_annotations_concurrently()) — network latency
  # per file, not parsing, is what actually gates load time. Everything
  # below reads from the now-warm cache.
  all_specs <- list()
  for (e in entries) {
    for (f in e$slot$annotation_files) {
      all_specs[[length(all_specs) + 1]] <- list(url = f$url, ref_width = e$slot$svs_width)
    }
  }
  if (length(all_specs) > 0) {
    fetch_annotations_concurrently(all_specs, progress_callback = progress_callback)
  }
  
  lapply(entries, function(e) {
    ann_files <- e$slot$annotation_files
    if (length(ann_files) > 0) {
      ann_files <- ann_files[order(vapply(ann_files, function(f) f$name, character(1)))]
    }
    groups <- lapply(ann_files, function(f) {
      polys <- parse_halo_annotations_cached(f$url, e$slot$svs_width)  # cache hit — already fetched above
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

# compact list of ALL metadata_fields for one donor — used inside the info
# popover next to each donor heading (Compare Stains/Donors) and in the
# context card (Home/Compare Stains/Compare Regions).
render_donor_demo_clinical <- function(donor_id) {
  row <- donor_metadata[donor_metadata$donor == donor_id, , drop = FALSE]
  if (nrow(row) == 0) return(shiny::p("No metadata found for this donor."))
  shiny::tagList(lapply(metadata_fields, function(f) {
    shiny::tags$div(
      style = "margin-bottom:4px; white-space:nowrap;",
      shiny::tags$strong(class = "context-card-label", paste0(f$label, ": ")),
      shiny::tags$span(class = "context-card-value", smart_lowercase(as.character(row[[f$id]])))
    )
  }))
}

render_donor_metadata_list <- function(donor_id, image_region = NULL, image_stain = NULL) {
  shiny::tagList(
    render_donor_demo_clinical(donor_id),
    render_donor_metadata_qnp_block(donor_id, image_region, image_stain)
  )
}

# QNP values for one donor, scoped to the image region+stain currently
# being viewed on Compare Donors (crosswalked via qnp_region_crosswalk /
# qnp_stain_crosswalk, global.r) — every measure (qnp_fields_all, not just
# percent-type), across every layer/subregion in that region.
render_donor_metadata_qnp_block <- function(donor_id, image_region, image_stain = NULL, standalone = FALSE, all_fields = FALSE) {
  # divider only makes sense when this block follows something else (the
  # combined view, where it separates demo/clinical from QNP) — shown on
  # its own (Compare Stains'/Compare Regions' QNP-only trigger), there's
  # nothing above it to separate from.
  leading_hr <- if (standalone) NULL else shiny::tags$hr()
  
  # region unknown entirely (Compare Regions, where region varies and
  # there's no single value to scope QNP to) — omit cleanly. This is an
  # expected, intentional case, not an error, so no message either.
  if (is.null(image_region)) return(NULL)
  
  # header always names the region/stain combination this card is scoped
  # to — reused across every return branch below, including the
  # crosswalk-miss and no-data fallbacks, so it's always clear which
  # image a popup was opened from.
  header_detail <- if (!is.null(image_stain)) {
    sprintf("%s / %s", smart_lowercase(prettify_region(image_region)), smart_lowercase(image_stain))
  } else {
    smart_lowercase(prettify_region(image_region))
  }
  header <- shiny::tagList(shiny::strong(hdg_qnp), sprintf(" — %s", header_detail))
  
  qnp_regions <- qnp_region_crosswalk[[image_region]]
  if (is.null(qnp_regions)) {
    return(shiny::tagList(
      leading_hr,
      header,
      shiny::p(style = "font-size:0.85em; color:#888;",
               "No QNP crosswalk entry for this region — see qnp_region_crosswalk in global.R.")
    ))
  }
  
  # all_fields (Compare Stains) shows every stain group's fields for this
  # region rather than narrowing to one — a genuinely-given-but-unmapped
  # stain still surfaces as a real crosswalk gap when all_fields is off.
  fields <- if (all_fields || is.null(image_stain)) {
    qnp_fields_all
  } else {
    stain_groups <- qnp_stain_crosswalk[[image_stain]]
    if (is.null(stain_groups)) {
      return(shiny::tagList(
        leading_hr,
        header,
        shiny::p(style = "font-size:0.85em; color:#888;",
                 "No QNP crosswalk entry for this stain — see qnp_stain_crosswalk in global.R.")
      ))
    }
    Filter(function(f) f$stain_group %in% stain_groups, qnp_fields_all)
  }
  
  rows <- qnp_metadata[qnp_metadata$donor == donor_id & qnp_metadata$region %in% qnp_regions, , drop = FALSE]
  
  if (nrow(rows) == 0 || length(fields) == 0) {
    return(shiny::tagList(leading_hr, header, shiny::p("No QNP data on record for this donor/region.")))
  }
  
  # only label each row with its specific QNP region code when more than
  # one was pooled together (e.g. MEC-HIP covering both MEC and HIP) —
  # with just one, the region is already implied and needn't be repeated.
  show_region_label <- length(qnp_regions) > 1
  
  shiny::tagList(
    leading_hr,
    header,
    shiny::tagList(lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, , drop = FALSE]
      vals <- Filter(Negate(is.null), lapply(fields, function(f) {
        v <- r[[f$id]]
        if (is.null(v) || is.na(v)) return(NULL)
        shiny::tags$div(shiny::tags$strong(paste0(f$label, ": ")), signif(v, 4))
      }))
      if (length(vals) == 0) return(NULL)
      subregion_label <- if (show_region_label) paste0(r$region, " - ", r$subregion) else r$subregion
      shiny::tags$div(
        style = sprintf("margin:6px 0; padding:6px; background:%s; border-radius:4px;", accent_bg_color),
        shiny::tags$div(style = "margin-bottom:2px; font-weight:600;", subregion_label),
        vals
      )
    }))
  )
}

# =============================================================================
# QNP Graphs page — ggplot2 boxplots/scatterplots of donor_metadata's
# demographic/clinical fields against qnp_metadata's measures. Every QNP
# value plotted is a per-donor REGION-LEVEL value (identify_qnp_field_values()
# + qnp_region_level_key(), same "Global (average) across subregions, or the
# lone subregion's own value" logic Filter Donors already uses) — never a
# raw per-subregion row, which would double-count a donor once per layer.
#
# One X axis (a category, CPS, or a QNP measure) and up to
# qnp_graph_color_cap Y-axis QNP measures, reshaped to long format so every
# measure renders as one grouped/colored series: a categorical X (age_bucket
# included) gives a dodged boxplot (one box + jittered points per measure
# within each category), anything else gives a scatter (one colored series
# per measure). Hovering any point shows a tooltip; clicking one copies its
# donor id (both via plotly).
# =============================================================================

# id -> label choices for every QNP measure, using qnp_fields_all's own
# label verbatim (not stain-prefixed) — used by qnp_graph_field_choices_by_stain().
qnp_graph_field_choices <- function() {
  stats::setNames(vapply(qnp_fields_all, function(f) f$id, character(1)), vapply(qnp_fields_all, function(f) f$label, character(1)))
}

# QNP measures grouped into one optgroup per stain (rather than one flat
# ~30-item list) — both axis pickers use this, so a stain's own measures
# sit together and neither needs as much scrolling to find one. region,
# if given, narrows this down to only measures with at least one non-NA
# value there (identify_qnp_field_values(), same lookup the actual plot
# data uses) — so a single-region plot's y-axis picker never offers a
# measure that region has no data for at all. NULL (the default, and
# always used in "regions"/facet mode) shows every measure regardless.
qnp_graph_field_choices_by_stain <- function(region = NULL) {
  fields <- qnp_fields_all
  if (!is.null(region)) {
    fields <- Filter(function(f) {
      vals <- identify_qnp_field_values(region, qnp_region_level_key(region), f$id)$value
      any(!is.na(vals))
    }, fields)
  }
  groups <- get_qnp_stain_groups(fields)
  stats::setNames(lapply(groups, function(g) {
    group_fields <- Filter(function(f) identical(f$stain_group, g), fields)
    stats::setNames(vapply(group_fields, function(f) f$id, character(1)), vapply(group_fields, function(f) f$label, character(1)))
  }), groups)
}

# label -> id choices for the fields usable as the x-axis's categorical
# (boxplot) group: every metadata_fields select-type field in
# qnp_graph_categorical_fields, plus the derived age_bucket (not itself a
# metadata_fields entry, so added by hand) — in metadata_fields' own order,
# with age_bucket last.
qnp_graph_categorical_choices <- function() {
  fields <- Filter(function(f) f$id %in% qnp_graph_categorical_fields, metadata_fields)
  choices <- stats::setNames(vapply(fields, function(f) f$id, character(1)), vapply(fields, function(f) f$label, character(1)))
  c(choices, stats::setNames(qnp_graph_age_bucket_field, qnp_graph_age_bucket_label))
}

# the x-axis picker's full choice set: categorical fields (age_bucket
# included) and CPS grouped together, then one optgroup per stain for
# every QNP measure — keeps a selectInput spanning ~40 very different
# fields navigable without much scrolling.
qnp_graph_x_choices <- function() {
  c(
    list(
      "Demographic / clinical" = qnp_graph_categorical_choices(),
      "Clinical score"         = stats::setNames(qnp_graph_cps_field, "CPS")
    ),
    qnp_graph_field_choices_by_stain()
  )
}

# label for any x-axis field id — metadata_fields' own label, or the
# fixed label for the derived age_bucket field (not a metadata_fields entry).
qnp_graph_x_label <- function(field_id) {
  if (identical(field_id, qnp_graph_age_bucket_field)) return(qnp_graph_age_bucket_label)
  f <- Find(function(x) identical(x$id, field_id), metadata_fields)
  if (!is.null(f)) return(f$label)
  Find(function(x) identical(x$id, field_id), qnp_fields_all)$label
}

# factor levels for a categorical x field — metadata_fields' own declared
# choices, or age_bucket's own computed 5-year bucket order (global.r).
qnp_graph_categorical_levels <- function(field_id) {
  if (identical(field_id, qnp_graph_age_bucket_field)) return(qnp_graph_age_bucket_levels)
  Find(function(f) identical(f$id, field_id), metadata_fields)$choices
}

# a range control's c(min, max) for coord_cartesian() — NULL if the
# control doesn't exist yet (e.g. no y fields chosen, so nothing to
# compute bounds from). is_histoslider selects histoslider's own
# reported-value format (a list with $start/$end — see
# histoslider_range()) for the X-axis control, vs. a plain sliderInput's
# own c(min, max) vector for the Y-axis one.
qnp_graph_axis_range <- function(val, is_histoslider = FALSE) {
  if (is_histoslider) return(histoslider_range(val))
  if (is.null(val) || length(val) != 2) return(NULL)
  val
}

# slider bounds for the Y-axis custom-range control, computed from the
# actual plotted values: a default selection matching the data's own
# range, a little padding beyond that so the slider's own min/max aren't
# razor-tight against the default, and everything rounded to 1 decimal
# place (both to keep the displayed numbers readable and because a step
# with more decimal places than that reads as a rounding artifact, not a
# meaningful position).
qnp_graph_slider_bounds <- function(vals) {
  rng <- range(vals, na.rm = TRUE)
  span <- diff(rng)
  if (span == 0) span <- max(abs(rng[1]), 1)
  pad <- span * 0.1
  step <- max(round(span / 100, 1), 0.1)
  list(min = round(rng[1] - pad, 1), max = round(rng[2] + pad, 1), default = round(rng, 1), step = step)
}

# shared font styling for every QNP Graphs plot — axis tick labels in the
# app's own Light body font, axis TITLES in its Bold heading font,
# matching the rest of the app's two-font system (falls back to each
# theme's plain default if the fonts couldn't be registered — see
# qnp_graph_axis_font/qnp_graph_title_font, global.r).
qnp_graph_font_theme <- function() {
  ggplot2::theme(
    axis.text  = ggplot2::element_text(family = qnp_graph_axis_font),
    axis.title = ggplot2::element_text(family = qnp_graph_title_font, face = "bold")
  )
}

# facet strip styling shared by both plot builders — black background,
# white text, and — since facets wrap at 3 per row (ncol=3 below), so
# multi-region plots often span several rows — more vertical gap between
# rows than horizontal gap between columns.
qnp_graph_facet_theme <- function() {
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "black"),
    strip.text        = ggplot2::element_text(color = "white"),
    panel.spacing.x   = ggplot2::unit(0.5, "lines"),
    panel.spacing.y   = ggplot2::unit(0.5, "lines")
  )
}

# bottom-placed legend shared by both plot builders, with extra space
# above it so it doesn't crowd the x-axis title directly above it.
qnp_graph_legend_theme <- function() {
  ggplot2::theme(
    legend.position    = "bottom",
    legend.box.spacing = ggplot2::unit(20, "pt")
  )
}

# with_placeholder(), generalized for a GROUPED choices list (a named list
# of named vectors, rendered as <optgroup>s) — prepends a blank choice
# outside every group, same effect as with_placeholder() has on a flat one.
with_placeholder_grouped <- function(grouped_choices, label = "Select...") {
  c(stats::setNames(list(""), label), grouped_choices)
}

# region picker for "within one region" mode — a region-grouping cascade
# identical to build_iddonors_qnp_region_controls()'s, minus the
# subregion level (this page always plots a region's own region-level
# value, via qnp_region_level_key(), never a specific subregion).
build_qplot_region_picker <- function() {
  shiny::tagList(
    shiny::selectInput("qplot_grouping_sel", "Region group", choices = with_placeholder(get_qnp_groupings())),
    shiny::uiOutput("qplot_region_ui")
  )
}

# per-donor region-level value(s) for one or more QNP fields, left-joined
# with donor_metadata so its categorical/cps columns are available for
# plotting alongside. region = NULL (facet mode) stacks every region into
# one long data frame with its own `region` column; a single region name
# scopes it to just that region. Reuses identify_qnp_field_values() so a
# donor is never double-counted across subregions.
build_qnp_graph_data <- function(field_ids, region = NULL) {
  field_ids <- unique(field_ids)
  regions <- if (is.null(region)) names(qnp_by_region) else region
  
  per_region <- lapply(regions, function(r) {
    field_dfs <- lapply(field_ids, function(fid) {
      df <- identify_qnp_field_values(r, qnp_region_level_key(r), fid)
      names(df) <- c("donor", fid)
      df
    })
    merged <- Reduce(function(a, b) merge(a, b, by = "donor", all = TRUE), field_dfs)
    if (nrow(merged) == 0) return(NULL)
    merged$region <- r
    merged
  })
  df <- do.call(rbind, Filter(Negate(is.null), per_region))
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  merge(df, donor_metadata, by = "donor", all.x = TRUE)
}

# reshapes wide QNP graph data (one column per y measure) into long format
# — one row per donor(-region) per measure — so multiple measures plot as
# a single grouped/colored series. label_by is a named character vector
# (field id -> display label): it becomes the `measure` factor's levels
# (so the legend/facets show real labels, not ids) and feeds each row's
# hover tooltip (donor id + that specific measure's value).
build_qnp_graph_long <- function(data, y_fields, label_by) {
  pieces <- lapply(y_fields, function(fid) {
    out <- data[, setdiff(names(data), y_fields), drop = FALSE]
    out$measure <- label_by[[fid]]
    out$value <- data[[fid]]
    out$tooltip <- paste0(out$donor, "<br>", label_by[[fid]], ": ", signif(out$value, 4))
    out
  })
  df <- do.call(rbind, pieces)
  df$measure <- factor(df$measure, levels = unname(label_by[y_fields]))
  df
}

# up to n distinct colors (khroma's "muted" scheme — same palette used for
# annotation colors elsewhere) for the up-to-qnp_graph_color_cap measures
# plotted at once.
qnp_graph_color_palette <- function(n) {
  if (n == 0) return(character(0))
  as.character(khroma::colour("muted")(n))
}

# grouped boxplot: x = a categorical demographic/clinical field (or the
# derived age_bucket), one dodged box (+ jittered points, hoverable,
# clickable, and sized via point_size — the qplot_point_size slider) per
# y measure within each x category. x-axis factor levels come from
# qnp_graph_categorical_levels() — metadata_fields' own declared
# `choices` (e.g. cerad_score's Absent/Sparse/Moderate/Frequent), or
# age_bucket's own 5-year bucket order — not alphabetical, so ordinal
# fields read in their real order.
build_qnp_grouped_boxplot <- function(long_data, x_field, x_label, facet = FALSE, y_range = NULL, point_size = qnp_graph_point_size, point_alpha = qnp_graph_point_alpha) {
  x_choices <- qnp_graph_categorical_levels(x_field)
  long_data[[x_field]] <- factor(long_data[[x_field]], levels = x_choices)
  long_data <- long_data[!is.na(long_data[[x_field]]) & !is.na(long_data$value), , drop = FALSE]
  if (nrow(long_data) == 0) return(NULL)
  
  palette <- stats::setNames(qnp_graph_color_palette(nlevels(long_data$measure)), levels(long_data$measure))
  
  # suppressWarnings(): text/key are plotly-only aesthetics (for hover
  # tooltips and click-to-copy) that ggplot2 itself doesn't recognize —
  # the resulting "Ignoring unknown aesthetics" warning fires right here,
  # when this layer is added, not later at ggplotly()/render time, so
  # it has to be caught at the source. Expected and harmless.
  p <- suppressWarnings(
    ggplot2::ggplot(long_data, ggplot2::aes(x = .data[[x_field]], y = value, fill = measure)) +
      ggplot2::geom_boxplot(outlier.shape = NA, position = ggplot2::position_dodge(width = 0.8), alpha = qnp_graph_fill_alpha, width = 0.7) +
      ggplot2::geom_point(
        # fill (not just color) has to be mapped here too — position_jitterdodge()
        # groups points by the fill aesthetic, so without it every measure's
        # points jitter around the same center instead of aligning under
        # their own box.
        ggplot2::aes(color = measure, fill = measure, text = tooltip, key = donor),
        position = ggplot2::position_jitterdodge(jitter.width = 0.03, dodge.width = 0.8),
        alpha = point_alpha, size = point_size
      ) +
      ggplot2::scale_fill_manual(values = palette, guide = "none") +
      # legend comes from color (a point's own key glyph is already a
      # borderless circle), not fill (a box) — per request.
      ggplot2::scale_color_manual(values = palette, name = NULL) +
      ggplot2::labs(x = smart_lowercase(x_label), y = NULL) +
      ggplot2::theme_classic(base_size = qnp_graph_base_text_size) +
      qnp_graph_legend_theme() +
      qnp_graph_font_theme() +
      qnp_graph_facet_theme()
  )
  
  if (!is.null(y_range)) p <- p + ggplot2::coord_cartesian(ylim = y_range)
  if (facet) p <- p + ggplot2::facet_wrap(~region, ncol = 3, axes = "all_x")
  p
}

# scatter: x is numeric (CPS or a QNP measure), one colored, hoverable
# and clickable series per y measure, lightly jittered so exactly-
# overlapping points don't hide each other. x_is_cps expands the x-axis
# to always include 0 and 1 (CPS's nominal range), ticked every 0.5 — a
# coord_cartesian() union with the actual data's own range (not a hard
# clip), so a value that falls slightly outside 0-1 still shows rather
# than getting cut off. x_range/y_range, if given, override the axis
# range outright (CPS's own 0/1-inclusive behavior included).
build_qnp_multi_scatter <- function(long_data, x_field, x_label, x_is_cps = FALSE, facet = FALSE, x_range = NULL, y_range = NULL, point_size = qnp_graph_point_size, point_alpha = qnp_graph_point_alpha) {
  long_data <- long_data[!is.na(long_data[[x_field]]) & !is.na(long_data$value), , drop = FALSE]
  if (nrow(long_data) == 0) return(NULL)
  
  palette <- stats::setNames(qnp_graph_color_palette(nlevels(long_data$measure)), levels(long_data$measure))
  
  # a small jitter (1% of each axis's own range) so exactly-overlapping
  # points are still visible as separate marks, without meaningfully
  # distorting the real x/y relationship being plotted.
  x_span <- diff(range(long_data[[x_field]], na.rm = TRUE))
  y_span <- diff(range(long_data$value, na.rm = TRUE))
  
  # suppressWarnings(): text/key are plotly-only aesthetics (for hover
  # tooltips and click-to-copy) that ggplot2 itself doesn't recognize —
  # the resulting "Ignoring unknown aesthetics" warning fires right here,
  # when this layer is added, not later at ggplotly()/render time, so
  # it has to be caught at the source. Expected and harmless.
  p <- suppressWarnings(
    ggplot2::ggplot(long_data, ggplot2::aes(x = .data[[x_field]], y = value, color = measure, text = tooltip, key = donor)) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = x_span * 0.01, height = y_span * 0.01),
        alpha = point_alpha, size = point_size
      ) +
      ggplot2::scale_color_manual(values = palette, name = NULL) +
      ggplot2::labs(x = x_label, y = NULL) +
      ggplot2::theme_classic(base_size = qnp_graph_base_text_size) +
      qnp_graph_legend_theme() +
      qnp_graph_font_theme() +
      qnp_graph_facet_theme()
  )
  
  final_xlim <- if (!is.null(x_range)) {
    x_range
  } else if (x_is_cps) {
    range(c(0, 1, long_data[[x_field]]), na.rm = TRUE)
  } else {
    NULL
  }
  if (x_is_cps && !is.null(final_xlim)) {
    breaks <- seq(floor(final_xlim[1] / 0.5) * 0.5, ceiling(final_xlim[2] / 0.5) * 0.5, by = 0.5)
    p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  }
  if (!is.null(final_xlim) || !is.null(y_range)) {
    p <- p + ggplot2::coord_cartesian(xlim = final_xlim, ylim = y_range)
  }
  if (facet) p <- p + ggplot2::facet_wrap(~region, ncol = 3, axes = "all_x")
  p
}

# reads qplot_x_field/qplot_y_fields (+ the region/compare-mode inputs)
# and builds the plot data — NULL when a required selection isn't made
# yet. x may itself be a QNP measure (comparing one against others), in
# which case it has to be fetched from qnp_metadata just like the y
# fields are; CPS and age_bucket instead arrive for free via
# donor_metadata's own join.
build_qplot_data_from_inputs <- function(input) {
  region <- if (identical(input$qplot_compare_mode, "single")) {
    if (!is_selected(input$qplot_region_sel)) return(NULL)
    input$qplot_region_sel
  } else {
    NULL
  }
  
  x_field <- input$qplot_x_field
  y_fields <- input$qplot_y_fields
  if (!is_selected(x_field) || length(y_fields) == 0) return(NULL)
  
  x_is_qnp <- !(x_field %in% c(qnp_graph_categorical_fields, qnp_graph_cps_field))
  qnp_ids <- unique(c(if (x_is_qnp) x_field, y_fields))
  
  build_qnp_graph_data(qnp_ids, region = region)
}

# dispatches to the right plot builder based on whether qplot_x_field is
# one of the categorical fields, age_bucket included (-> grouped boxplot)
# or not (-> scatter, CPS or a QNP measure) — keeps server.r's own render
# a thin wrapper. Reads the optional custom axis ranges (qplot_x_range —
# a histoslider, note the is_histoslider flag; qplot_y_range — a plain
# range slider) too — NULL (not yet built, e.g. no y fields chosen) leaves
# that axis on its normal automatic scaling (qnp_graph_axis_range()).
build_qplot_from_inputs <- function(input, data) {
  x_field <- input$qplot_x_field
  y_fields <- input$qplot_y_fields
  if (!is_selected(x_field) || length(y_fields) == 0) return(NULL)
  
  qnp_label <- function(id) Find(function(f) identical(f$id, id), qnp_fields_all)$label
  y_labels <- stats::setNames(vapply(y_fields, qnp_label, character(1)), y_fields)
  long_data <- build_qnp_graph_long(data, y_fields, y_labels)
  facet <- identical(input$qplot_compare_mode, "regions")
  y_range <- qnp_graph_axis_range(input$qplot_y_range)
  point_size  <- if (is.null(input$qplot_point_size))  qnp_graph_point_size  else input$qplot_point_size
  point_alpha <- if (is.null(input$qplot_point_alpha)) qnp_graph_point_alpha else input$qplot_point_alpha
  
  if (x_field %in% qnp_graph_categorical_fields) {
    build_qnp_grouped_boxplot(long_data, x_field, qnp_graph_x_label(x_field), facet = facet, y_range = y_range, point_size = point_size, point_alpha = point_alpha)
  } else {
    x_is_cps <- identical(x_field, qnp_graph_cps_field)
    x_label <- if (x_is_cps) "CPS" else qnp_label(x_field)
    x_range <- qnp_graph_axis_range(input$qplot_x_range, is_histoslider = TRUE)
    build_qnp_multi_scatter(long_data, x_field, x_label, x_is_cps = x_is_cps, facet = facet, x_range = x_range, y_range = y_range, point_size = point_size, point_alpha = point_alpha)
  }
}