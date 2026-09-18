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

# fetches+parses many annotation files CONCURRENTLY (all requests in flight
# at once via curl's multi-handle interface, capped at `max_concurrent`),
# instead of one at a time — since the actual bottleneck here is network
# latency per file, not parsing, this is the main lever for making a large
# comparison load faster. results are written straight into
# .annotation_cache as each one finishes, so parse_halo_annotations_cached()
# calls made afterward are all instant cache hits.
#
# `progress_callback(done, total)`, if given, is called after EVERY file
# completes (success or failure) — not just once at the end — so callers
# can show live, incrementing progress instead of an indefinite spinner
# that gives no sense of whether anything is actually happening.
#
# `file_specs` is a list of list(url=, ref_width=) — duplicates (same url +
# ref_width appearing across multiple entries) are only fetched once.
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

# ---------------------------------------------------------------------------
# QNP (quantitative neuropathology) — values keyed by donor + region +
# subregion, loaded from a separate csv (see qnp_fields/qnp_metadata_csv_path
# in global.r). Unlike load_specimen_metadata_csv(), a missing/unreadable
# file here doesn't stop the app — it just means no QNP filters are
# available yet, which is the expected state before that file exists.
# ---------------------------------------------------------------------------

empty_qnp_metadata <- function() {
  df <- data.frame(donor = character(0), region = character(0), subregion = character(0),
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

# ---------------------------------------------------------------------------
# "Filter Donors" page.
#
# Two files, one shared key: donor_metadata (one row per donor: demographic
# + clinical) and qnp_metadata (one row per donor+region+subregion: QNP).
# Both are "donor-associated metadata" — the only structural difference is
# that a donor has exactly one demographic/clinical row but potentially
# several QNP rows (one per region+subregion they were measured in).
#
# QNP browsing is split into two modes via a radio button:
#   Region mode: pick a region, then a subregion (or "Global" = that
#     donor's mean across the region's subregions, offered only when the
#     region HAS more than one subregion) — then one card per stain.
#   Stain mode: pick a stain — then one card per REGION, each showing that
#     stain's fields at that region's Global (or single-subregion) value.
#     No region selector: every region is shown at once as its own card.
# Only percent-type fields get sliders (qnp_fields is filtered to just
# those in global.r).
#
# WHETHER A SLIDER IS "ACTIVE" IS TRACKED, NOT INFERRED.
# Earlier versions tried to detect "the user hasn't touched this yet" by
# comparing the widget's reported value against the data's exact range.
# That is fundamentally unreliable: histoslider's React component can snap
# handles to histogram bin edges, so an untouched slider's reported value
# is NOT guaranteed to equal the data range — which is what kept silently
# excluding the one donor holding a field's extreme value (the persistent
# "83 of 84 on page load" bug), no matter how the tolerance was tuned.
# Instead, iddonors_filter_active() records each input's FIRST observed
# value as its baseline and reports the filter as active only once the
# current value differs from it. No tolerances, no assumptions about the
# widget's internals.
# ---------------------------------------------------------------------------

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

# unique stain groups among the (already percent-only) qnp_fields, in the
# order they're declared in global.r — not alphabetical.
get_qnp_stain_groups <- function() {
  groups <- vapply(qnp_fields, function(f) f$stain_group, character(1))
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

# which subregion key to use when we want "the region as a whole": the
# Global average when there are several subregions, otherwise the single
# subregion's own values (an average of one thing is that thing).
# Builders, filters and reset ALL call this, so their widget ids can never
# drift apart — that drift was a real, separately-diagnosed bug.
qnp_region_level_key <- function(region) {
  if (qnp_region_has_multiple_subregions(region)) "Global" else names(qnp_by_region[[region]])[[1]]
}

# returns a data.frame(donor, value) for one field at one region +
# (subregion or "Global"). "Global" is a genuine per-donor MEAN across
# every subregion they have in that region — never a pooling of raw rows,
# which would double-count a donor once per subregion.
identify_qnp_field_values <- function(region, subregion_choice, field_id) {
  subregion_subsets <- qnp_by_region[[region]]
  if (is.null(subregion_subsets)) return(data.frame(donor = character(0), value = numeric(0)))
  
  if (identical(subregion_choice, "Global")) {
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
  shiny::tagList(
    shiny::h6(qnp_field_display_label(f$label), style = "margin-bottom:2px;"),
    build_histoslider(identify_donor_qnp_widget_id(region, subregion_choice, f$id), vals)
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
    shiny::selectInput("iddonors_qnp_region_sel", "Region", choices = with_placeholder(sort(names(qnp_by_region)))),
    shiny::uiOutput("iddonors_qnp_subregion_ui")
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
filter_donors_identify_qnp_region_mode <- function(input, store) {
  region <- input$iddonors_qnp_region_sel
  subregion_choice <- input$iddonors_qnp_subregion_sel
  if (!is_selected(region) || !is_selected(subregion_choice)) return(donor_choices)
  
  donors <- donor_choices
  for (f in qnp_fields) donors <- apply_qnp_slider_filter(donors, region, subregion_choice, f, input, store)
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
      shiny::checkboxGroupInput(paste0("iddonors_", f$id, "_sel"), f$label, choices = f$choices, selected = character(0)),
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
# controls + the multi-column cards.
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
  bslib::accordion(bslib::accordion_panel(title = "QNP", body), id = "iddonors_qnp_accordion", open = FALSE)
}

# sets up every QNP-mode-dependent renderUI — called once per session.
register_identify_donors_qnp <- function(output, input) {
  output$iddonors_qnp_mode_ui <- shiny::renderUI({
    if (identical(input$iddonors_qnp_mode, "stain")) build_iddonors_qnp_stain_controls() else build_iddonors_qnp_region_controls()
  })
  
  output$iddonors_qnp_subregion_ui <- shiny::renderUI({
    shiny::req(is_selected(input$iddonors_qnp_region_sel))
    region <- input$iddonors_qnp_region_sel
    subregions <- sort(names(qnp_by_region[[region]]))
    # "Global (average)" only offered with >1 subregion — with just one,
    # the average IS that subregion's value, so it'd be a duplicate choice.
    choices <- if (qnp_region_has_multiple_subregions(region)) {
      c("Global (average)" = "Global", subregions)
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
      shiny::req(is_selected(input$iddonors_qnp_region_sel), is_selected(input$iddonors_qnp_subregion_sel))
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

# resets every filter back to "no restriction". Also CLEARS the baseline
# store, so the values pushed here become the new baselines rather than
# reading as deliberate user filtering.
reset_identify_donors_filters <- function(input, session, store) {
  rm(list = ls(envir = store, all.names = TRUE), envir = store)
  
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
  
  reset_one_qnp_field <- function(f, region, subregion_choice) {
    vals_df <- identify_qnp_field_values(region, subregion_choice, f$id)
    rng <- suppressWarnings(range(vals_df$value, na.rm = TRUE))
    if (all(is.finite(rng))) {
      histoslider::update_histoslider(
        identify_donor_qnp_widget_id(region, subregion_choice, f$id),
        start = rng[1], end = rng[2], session = session
      )
    }
  }
  
  if (identical(input$iddonors_qnp_mode, "stain")) {
    stain <- input$iddonors_qnp_stain_sel
    if (is_selected(stain)) {
      fields <- Filter(function(x) identical(x$stain_group, stain), qnp_fields)
      for (region in names(qnp_by_region)) {
        for (f in fields) reset_one_qnp_field(f, region, qnp_region_level_key(region))
      }
    }
  } else {
    region <- input$iddonors_qnp_region_sel
    subregion_choice <- input$iddonors_qnp_subregion_sel
    if (is_selected(region) && is_selected(subregion_choice)) {
      for (f in qnp_fields) reset_one_qnp_field(f, region, subregion_choice)
    }
  }
}

# every metadata column for the matching donors, ready for display or
# download — all of donor_metadata's fields, donor id first.
identify_donors_table_data <- function(donor_ids) {
  cols <- c("donor", vapply(metadata_fields, function(f) f$id, character(1)))
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

# ---------------------------------------------------------------------------
# QNP in WIDE form: one row per donor, one column per measure per region.
# qnp_metadata is long (a row per donor+region+subregion), so this pivots
# it out so each donor fits a single row alongside their demographic and
# clinical columns.
#
# Column names are "<Region> | <measure>" where a region has a single
# subregion, and "<Region> | <Subregion> | <measure>" where it has several
# — dropping the subregion in the multi-subregion case would force an
# average and silently lose data, so it's kept only where it's actually
# needed to disambiguate.
#
# Uses qnp_fields_all (EVERY measure), not the percent-only qnp_fields the
# on-page sliders use. DOWNLOAD/POPUP ONLY — never rendered into the
# page's table.
# ---------------------------------------------------------------------------
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

# every metadata value for ONE donor — demographic, clinical, and each QNP
# row on record. This is the body of the click-a-donor-name popup; it is
# not rendered inline anywhere on the page.
render_donor_all_metadata <- function(donor_id) {
  row <- donor_metadata[donor_metadata$donor == donor_id, , drop = FALSE]
  if (nrow(row) == 0) return(shiny::p("No metadata found for this donor."))
  
  field_by_id <- stats::setNames(metadata_fields, vapply(metadata_fields, function(f) f$id, character(1)))
  
  group_blocks <- lapply(names(metadata_display_groups), function(group_name) {
    shiny::tagList(
      shiny::strong(group_name),
      shiny::div(
        style = "display:flex; flex-wrap:wrap; gap:14px; margin:6px 0 14px 0;",
        lapply(metadata_display_groups[[group_name]], function(fid) {
          f <- field_by_id[[fid]]
          if (is.null(f)) return(NULL)
          shiny::tags$div(shiny::tags$strong(paste0(f$label, ": ")), as.character(row[[fid]]))
        })
      )
    )
  })
  
  qnp_rows <- qnp_metadata[qnp_metadata$donor == donor_id, , drop = FALSE]
  qnp_block <- if (nrow(qnp_rows) == 0) {
    shiny::tagList(shiny::strong("QNP"), shiny::p("No QNP data on record for this donor."))
  } else {
    shiny::tagList(
      shiny::strong("QNP"),
      shiny::tagList(lapply(seq_len(nrow(qnp_rows)), function(i) {
        r <- qnp_rows[i, , drop = FALSE]
        vals <- Filter(Negate(is.null), lapply(qnp_fields_all, function(f) {
          v <- r[[f$id]]
          if (is.null(v) || is.na(v)) return(NULL)
          shiny::tags$div(shiny::tags$strong(paste0(f$label, ": ")), signif(v, 4))
        }))
        if (length(vals) == 0) return(NULL)
        shiny::tags$div(
          style = "margin:8px 0; padding:8px; background:#f6f2fb; border-radius:4px;",
          shiny::tags$div(
            style = "margin-bottom:4px;",
            shiny::tags$strong(paste0(prettify_region(r$region), ": ", r$subregion))
          ),
          vals
        )
      }))
    )
  }
  
  shiny::tagList(group_blocks, shiny::tags$hr(), qnp_block)
}

# a plain HTML table of the full metadata for every matching donor.
render_identify_donors_table <- function(donor_ids) {
  if (length(donor_ids) == 0) return(shiny::helpText("No donors match the current filters."))
  df <- identify_donors_table_data(donor_ids)
  
  header <- shiny::tags$tr(lapply(names(df), shiny::tags$th))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    shiny::tags$tr(lapply(names(df), function(nm) {
      # the Donor cell is a link: clicking it pushes that donor's id to the
      # server, which opens the all-metadata popup (see server.r). Done via
      # Shiny.setInputValue rather than per-row observers, since the row set
      # changes with every filter change.
      if (identical(nm, "Donor")) {
        donor_id <- as.character(df[i, nm])
        shiny::tags$td(shiny::tags$a(
          href = "javascript:void(0)",
          onclick = sprintf("Shiny.setInputValue('iddonors_clicked_donor', '%s', {priority: 'event'})", donor_id),
          donor_id
        ))
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
# type "range"  -> a [min,max] filter on the raw numeric column.
# type "select" -> an unordered %in% filter from the checkbox group.
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

# wraps input_histoslider() with the shared metadata_chart_color (global.r).
# NOTE: histoslider's documented API doesn't expose a color option — the
# `options` argument here is unverified against the package's actual JS
# component and may not visibly change anything. If the bar color still
# doesn't match, the CSS override in ui.R's <style> block is the more
# reliable lever (target whatever class the rendered bars actually use —
# inspect one with your browser's dev tools to confirm the selector).
# confirmed against the actual react component source (samhogg/histoslider
# Histoslider.js) and the R wrapper's docs (input_histoslider.Rd, which
# explicitly documents `options` as a pass-through to that component's
# props): the real color props are `selectedColor`/`unselectedColor`, not
# `color` (an earlier guess that silently did nothing). selectedColor tints
# the bars within the dragged range, unselectedColor tints the rest — using
# our border/fill purples for a two-tone look consistent with the plain
# categorical histograms elsewhere.
build_histoslider <- function(id, values, breaks = NULL) {
  # histoslider's own default for `breaks` is rlang::missing_arg() (an
  # intentionally MISSING argument, not NULL) — passing a literal NULL
  # instead makes its internal hist() call fail with "Invalid breakpoints
  # ... NULL". so when no breaks were given, omit the argument entirely
  # via do.call() rather than passing breaks = NULL.
  #
  # start/end are ALSO left out of the R wrapper's documented signature by
  # default (both NULL) — meaning the widget infers its own initial
  # selection rather than us ever telling it what "untouched" should look
  # like. explicitly passing start/end = this field's own full data range
  # removes that ambiguity: the widget's initial value is now exactly what
  # we told it to be, not something it independently derived.
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
# builds one accordion per page with two top-level panels, "Demographic"
# and "Clinical" (from metadata_display_groups, global.r), each holding a
# sub-accordion with one panel per field in that group. Used by the four
# regular pages' "Filter donors by metadata" — QNP filtering lives entirely
# on the separate Identify Donors page instead (build_identify_donors_accordion()).
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
          choices = f$choices, selected = character(0)
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
          x = bp, y = graphics::par("usr")[3], labels = names(counts),
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

render_viewer_grid_ui <- function(entries, label_field = c("stain", "donor", "region"), show_donor_info = FALSE) {
  label_field <- match.arg(label_field)
  if (length(entries) == 0) return(NULL)  # blank until something is actually loaded
  col_width <- if (length(entries) == 1) 12 else 6
  shiny::tagList(shiny::fluidRow(lapply(entries, function(e) {
    cid   <- paste0("osd-", safe_id(e$donor, e$stain, e$region))
    label <- if (label_field == "region") prettify_region(e$region) else e[[label_field]]
    
    heading <- if (show_donor_info && label_field == "donor") {
      shiny::tags$div(
        style = "display:flex; align-items:center; gap:6px;",
        shiny::h5(style = "margin:0;", label),
        bslib::popover(
          shiny::tags$span(shiny::icon("circle-info"), style = "color:#888; cursor:pointer;"),
          title = paste("Donor", label),
          render_donor_metadata_list(e$donor)
        )
      )
    } else {
      shiny::h5(label)
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

render_annotation_master_ui <- function(entries, id_prefix = "ann") {
  if (length(entries) == 0) return(NULL)
  
  all_labels <- get_unique_annotation_labels(entries)
  if (length(all_labels) == 0) return(NULL)
  
  color_map <- build_annotation_color_map(all_labels)
  
  shiny::tagList(
    shiny::strong("Annotations:"),
    shiny::div(
      style = "display:flex; flex-wrap:wrap; gap:40px; margin-top:12px;",
      lapply(all_labels, function(lab) {
        # id_prefix keeps ids unique across pages — all four pages' checklists
        # coexist in the DOM at once (navbarPage renders every tab up front),
        # so two pages both showing a "Layer1" label would otherwise collide.
        cb_id <- paste0(id_prefix, "_toggle_", gsub("[^A-Za-z0-9]+", "_", lab))
        # a plain "form-check" div — the same Bootstrap classes bslib's own
        # checkboxInput() renders under the hood — so this looks identical
        # to every other checkbox in the app, even though (unlike a real
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
          shiny::tags$label(class = "form-check-label", `for` = cb_id, style = "margin:0;", lab),
          shiny::tags$span(style = sprintf(
            "display:inline-block; width:18px; height:18px; border-radius:3px; background:%s; flex-shrink:0;",
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
build_images_payload <- function(entries, overlay_opacity, show_overlay = TRUE, progress_callback = NULL) {
  color_map <- build_annotation_color_map(get_unique_annotation_labels(entries))
  
  # gather every annotation file across ALL entries and fetch them
  # CONCURRENTLY in one batch (see fetch_annotations_concurrently()) before
  # doing anything else — this is what actually speeds up a large
  # comparison's load time, since it's dominated by network latency per
  # file, not by parsing. everything below this point is then just reading
  # from the now-warm cache.
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
# popover next to each donor heading on the Compare Donors page. unlike
# render_donor_metadata_card(), this has no heading/grouping of its own,
# since the popover title already provides that context.
render_donor_metadata_list <- function(donor_id) {
  row <- donor_metadata[donor_metadata$donor == donor_id, , drop = FALSE]
  if (nrow(row) == 0) return(shiny::p("No metadata found for this donor."))
  
  shiny::tagList(lapply(metadata_fields, function(f) {
    shiny::tags$div(
      style = "margin-bottom:4px; white-space:nowrap;",
      shiny::tags$strong(paste0(f$label, ": ")), as.character(row[[f$id]])
    )
  }))
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