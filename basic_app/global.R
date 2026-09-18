library(dplyr)
library(bslib)
library(histoslider)
library(httr)
library(curl)
library(khroma)
library(shinyjs)

source("R/functions.R")

# ---------------------------------------------------------------------------
# donor metadata field spec — THE single source of truth for what metadata
# exists in this app. load_specimen_metadata_csv() only ever reads the
# columns declared here (via `csv_column`); any other column in the
# specimen CSV is ignored automatically. Adding/removing a metadata field is
# entirely an edit here — no other file needs to change.
#
#   - "range"   fields: a histoslider over the raw numeric column. do NOT
#     set min/max here — always derived from real data (derive_metadata_fields()),
#     never guessed.
#   - "select"  fields: unordered categorical values, filtered via checkboxes.
#     choices are OPTIONAL here — omit them to auto-derive from real data
#     (new values then show up with no code change); if hardcoded, that list
#     is authoritative and won't be touched by real data.
# ---------------------------------------------------------------------------
metadata_fields <- list(
  list(id = "age_at_death",   label = "Age at death",     type = "range",
       csv_column = "Age at death (years)"),
  list(id = "sex",             label = "Sex",              type = "select",
       choices = c("Female", "Male"), csv_column = "Sex"),
  list(id = "apoe_genotype",   label = "APOE genotype",    type = "select",
       choices = c("2/2", "2/3", "2/4", "3/3", "3/4", "4/4"), csv_column = "APOE genotype"),
  list(id = "cog_status",      label = "Cognitive status", type = "select",
       choices = c("Dementia", "No dementia"), csv_column = "Cognitive status"),
  list(id = "adnc",            label = "ADNC",             type = "select",
       choices = c("Not AD", "Low", "Intermediate", "High"), csv_column = "ADNC"),
  list(id = "thal_phase",      label = "Thal phase",       type = "select",
       choices = as.character(0:5), csv_column = "Thal phase"),
  list(id = "braak_stage",     label = "Braak stage",      type = "select",
       choices = c("0", "I", "II", "III", "IV", "V", "VI"), csv_column = "Braak stage"),
  list(id = "cerad_score",     label = "CERAD score",      type = "select",
       choices = c("Absent", "Sparse", "Moderate", "Frequent"), csv_column = "CERAD score"),
  list(id = "years_education", label = "Years of education", type = "range",
       csv_column = "Years of education (years)"),
  list(id = "cps",             label = "CPS", type = "range",
       csv_column = "Continuous Pseudo-progression Score")
)

# ---------------------------------------------------------------------------
# donor metadata — loaded from the real specimen csv (no dummy-data fallback:
# a bad path here should error loudly rather than silently show fake data).
# ---------------------------------------------------------------------------
specimen_metadata_csv_path <- "ins/SpecimenMetadata.csv"

donor_metadata  <- load_specimen_metadata_csv(specimen_metadata_csv_path)
metadata_fields <- derive_metadata_fields(metadata_fields, donor_metadata)

# ---------------------------------------------------------------------------
# manifest source — a single csv covering any number of donors. required
# columns on every row: file_type, stain_type, donor, region, s3_uri.
# annotation_name (or subregion) is only needed on annotation-xml rows.
# width/height are OPTIONAL columns on RAW_IMAGE rows — if present (e.g.
# after running precompute_manifest_dimensions.R separately), they're used
# directly; if absent/blank, the app falls back to reading them live from
# the .svs file's own header (see read_tiff_dimensions() in functions.r).
# any donor/region/stain appearing in this csv is picked up automatically,
# no code changes needed elsewhere.
# ---------------------------------------------------------------------------
# how long (seconds) to wait per annotation-file request before giving up —
# increase this if you see "could not fetch ... Timeout was reached"
# warnings at startup/load time; the S3 endpoint can be slow under load.
annotation_fetch_timeout_sec <- 60

manifest_csv_path <- "ins/260909_manifest_fill.csv"

csv_entries <- tryCatch(read_manifest_csv_entries(manifest_csv_path), error = function(e) {
  warning("could not read manifest csv: ", manifest_csv_path, " - ", e$message)
  list()
})

donor_manifest <- build_donor_manifest_from_entries(csv_entries)
donor_choices   <- names(donor_manifest)
all_stains      <- get_all_stains()

# ---------------------------------------------------------------------------
# groups metadata_fields into cards for display (currently used under the
# single image on the Home page — see render_donor_metadata_card()). edit
# this to add/remove fields or reorder/regroup them; it only references
# metadata_fields' ids, so it can't drift out of sync with the field specs
# above. any field id omitted here simply won't be shown in a card (it's
# still fully usable everywhere else — filters, accordions, etc).
# ---------------------------------------------------------------------------
metadata_display_groups <- list(
  "Demographic" = c("age_at_death", "sex", "apoe_genotype", "years_education"),
  "Clinical"    = c("cog_status", "adnc", "thal_phase", "braak_stage", "cerad_score", "cps")
)

# ---------------------------------------------------------------------------
# QNP (quantitative neuropathology) filters — structurally different from
# metadata_fields above: each value is keyed by donor + region + subregion
# (the same layer/subregion concept used for HALO annotations elsewhere),
# not just donor. Read from a SEPARATE csv (qnp_metadata_csv_path). Every
# field is numeric ("these are all numerical filters" per the request).
#
# `csv_column` values below are placeholders using your literal descriptions
# — replace them with the exact header text once you have the real QNP csv,
# since I don't have that file to confirm exact column naming/casing against.
#
# `stain_group` is only used for organizing the filter UI (one accordion
# panel per stain) — it doesn't need to match anything in the csv itself.
# ---------------------------------------------------------------------------
qnp_fields <- list(
  list(id = "avg_6e10_object_area",              label = "Average object area",
       stain_group = "6E10", csv_column = "average 6e10 positive object area"),
  list(id = "avg_6e10_object_median_diameter",    label = "Average object median diameter",
       stain_group = "6E10", csv_column = "average 6e10 positive object median diameter"),
  list(id = "n_6e10_objects_per_area",            label = "Number of objects per area",
       stain_group = "6E10", csv_column = "number of 6e10 positive objects per area"),
  list(id = "pct_6e10_dense_core_plaque_area",    label = "Percent dense core plaque area",
       stain_group = "6E10", csv_column = "percent 6e10 dense core plaque area"),
  list(id = "pct_6e10_diffuse_plaque_area",       label = "Percent diffuse plaque area",
       stain_group = "6E10", csv_column = "percent 6e10 diffuse plaque area"),
  list(id = "pct_6e10_fibrilar_plaque_area",      label = "Percent fibrilar plaque area",
       stain_group = "6E10", csv_column = "percent 6e10 fibrilar plaque area"),
  list(id = "pct_6e10_positive_area",             label = "Percent positive area",
       stain_group = "6E10", csv_column = "percent 6e10 positive area"),
  
  list(id = "avg_iba1_process_area_per_cell",     label = "Average process area per cell",
       stain_group = "Iba1", csv_column = "average Iba1 positive process area per cell"),
  list(id = "avg_iba1_process_length_per_cell",   label = "Average process length per cell",
       stain_group = "Iba1", csv_column = "average Iba1 positive process length per cell"),
  list(id = "n_iba1_cells_per_area",              label = "Number of cells per area",
       stain_group = "Iba1", csv_column = "number of Iba1 positive cells per area"),
  list(id = "n_iba1_activated_cells_per_area",    label = "Number of activated cells per area",
       stain_group = "Iba1", csv_column = "number of activated Iba1 positive cells per area"),
  list(id = "n_iba1_inactivated_cells_per_area",  label = "Number of inactivated cells per area",
       stain_group = "Iba1", csv_column = "number of inactivated Iba1 positive cells per area"),
  list(id = "pct_iba1_positive_area",             label = "Percent positive area",
       stain_group = "Iba1", csv_column = "percent Iba1 positive area"),
  
  list(id = "n_6e10_coloc_iba1_per_area",         label = "Number of 6E10 objects colocalized with Iba1 per area",
       stain_group = "6E10 x Iba1", csv_column = "number of 6e10 positive objects colocalized with Iba1 positive objects per area"),
  list(id = "pct_6e10_coloc_iba1",                label = "Percent of 6E10 objects colocalized with Iba1",
       stain_group = "6E10 x Iba1", csv_column = "percent of 6e10 positive objects colocalized with Iba1 positive objects"),
  
  list(id = "avg_hematoxylin_nucleus_area",       label = "Average nucleus area",
       stain_group = "Hematoxylin", csv_column = "average Hematoxylin positive nucleus area"),
  list(id = "avg_hematoxylin_nucleus_perimeter",  label = "Average nucleus perimeter",
       stain_group = "Hematoxylin", csv_column = "average Hematoxylin positive nucleus perimeter"),
  list(id = "avg_hematoxylin_nucleus_roundness",  label = "Average nucleus roundness",
       stain_group = "Hematoxylin", csv_column = "average Hematoxylin positive nucleus roundness"),
  list(id = "n_hematoxylin_nuclei_per_area",      label = "Number of nuclei per area",
       stain_group = "Hematoxylin", csv_column = "number of Hematoxylin positive nuclei per area"),
  
  list(id = "pct_gfap_positive_area",             label = "Percent positive area",
       stain_group = "GFAP", csv_column = "percent GFAP positive area"),
  
  list(id = "avg_asyn_cell_area",                 label = "Average cell area",
       stain_group = "aSyn", csv_column = "average aSyn positive cell area"),
  list(id = "n_asyn_cells_per_area",              label = "Number of cells per area",
       stain_group = "aSyn", csv_column = "number of aSyn positive cells per area"),
  list(id = "pct_asyn_positive_area",             label = "Percent positive area",
       stain_group = "aSyn", csv_column = "percent aSyn positive area"),
  
  list(id = "avg_at8_cell_area",                  label = "Average cell area",
       stain_group = "AT8", csv_column = "average AT8 positive cell area"),
  list(id = "n_at8_cells_per_area",               label = "Number of cells per area",
       stain_group = "AT8", csv_column = "number of AT8 positive cells per area"),
  list(id = "pct_at8_positive_area",              label = "Percent positive area",
       stain_group = "AT8", csv_column = "percent AT8 positive area"),
  
  list(id = "avg_ptdp43_cell_area",               label = "Average cell area",
       stain_group = "pTDP43", csv_column = "average pTDP43 positive cell area"),
  list(id = "n_ptdp43_cells_per_area",            label = "Number of cells per area",
       stain_group = "pTDP43", csv_column = "number of pTDP43 positive cells per area"),
  list(id = "pct_ptdp43_positive_area",           label = "Percent positive area",
       stain_group = "pTDP43", csv_column = "percent pTDP43 positive area"),
  
  list(id = "avg_neun_cell_area",                 label = "Average cell area",
       stain_group = "NeuN", csv_column = "average NeuN positive cell area"),
  list(id = "n_neun_cells_per_area",              label = "Number of cells per area",
       stain_group = "NeuN", csv_column = "number of NeuN positive cells per area"),
  list(id = "pct_neun_positive_area",             label = "Percent positive area",
       stain_group = "NeuN", csv_column = "percent NeuN positive area")
)

# donor/region/subregion column headers in the QNP csv (edit if different).
qnp_donor_column     <- "Donor ID"
qnp_region_column     <- "region"
qnp_subregion_column <- "analysis region"

qnp_metadata_csv_path <- "ins/QNPMetadata.csv"  # <- point this at your real QNP csv

qnp_metadata <- load_qnp_metadata_csv(qnp_metadata_csv_path)

# precomputed ONCE — every place that needs "the QNP rows for this region+
# subregion" (the Identify Donors page's filter and its lazy accordion
# builder, both of which run on every relevant reactive tick) reads this
# instead of re-deriving it from qnp_metadata via unique()/boolean-masking
# each time. qnp_by_region[[region]] is itself a named list keyed by
# subregion, so both levels are O(1) lookups instead of repeated scans.
qnp_by_region <- if (nrow(qnp_metadata) == 0) {
  list()
} else {
  split_by_region <- split(qnp_metadata, qnp_metadata$region)
  lapply(split_by_region, function(df_region) split(df_region, df_region$subregion))
}
# every qnp_fields entry is numeric ("these are all numerical filters" per
# the request) — retrofitting type = "range" here (rather than repeating it
# in every single list() entry above) lets shared code treat metadata_fields
# and qnp_fields identically wherever it just needs to know a field's type.
qnp_fields <- lapply(qnp_fields, function(f) { f$type <- "range"; f })

# the COMPLETE QNP field spec, kept before the percent-only narrowing
# below. Used for things that should show/export every measure — the CSV
# download and the per-donor popup — while the on-page sliders use the
# narrowed qnp_fields. (qnp_metadata itself was loaded above with all of
# these columns, so the underlying data is all there either way.)
qnp_fields_all <- qnp_fields

# only percent-type measures get sliders on the Filter Donors page —
# "average X area", "number of X per area" etc are dropped from the
# FILTERING spec, per the request ("only create a slider for values that
# are a percent"). every percent field's id was given a "pct_" prefix when
# qnp_fields was first defined above, so that's what this filters on.
qnp_fields <- Filter(function(f) grepl("^pct_", f$id), qnp_fields)

# shared theme — passed to navbarPage(theme = ...) in ui.R. defined here
# (rather than at the bottom) since metadata_chart_color, below, needs it.
app_theme <- bslib::bs_theme(bootswatch = "lux")

# shared color used for BOTH histoslider bars and the categorical histogram
# bars (register_metadata_histograms()), so all metadata charts look
# consistent — pulled directly from the Lux theme's actual "primary" purple
# rather than a hardcoded guess, so it always matches whatever bootswatch is
# set above even if that changes later.
# shared color used for BOTH histoslider bars and the categorical histogram
# bars (register_metadata_histograms()), so all metadata charts look
# consistent. NOTE: previously this was derived via bslib::bs_get_variables()
# to auto-match the theme's primary color, but that returned an unresolved
# Sass reference (rendered literally as black) rather than a compiled hex
# value — so it's hardcoded directly here instead.
metadata_chart_color <- "#7952b3"

# ---------------------------------------------------------------------------
# annotation colors are chosen automatically per load — see
# build_annotation_color_map() in functions.r. it picks a colorblind-friendly
# qualitative palette from khroma sized to how many distinct annotation
# labels are actually present (muted <10, sunset 10-11, nightfall 12-17),
# and assigns one color per label. nothing to configure here unless you want
# to swap the palette scheme itself.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# precompute the "Filter donors by metadata" accordion (Demographic /
# Clinical / QNP, with all its nested sub-accordions) ONCE per page prefix,
# right here at app startup — not inside a renderUI, not per click, not per
# user session. Everything it's built from (donor_metadata, qnp_metadata,
# metadata_fields, qnp_fields) is static once these CSVs are loaded above,
# so there's nothing to gain by rebuilding it later — every session's first
# click on "Filter donors by metadata" now just hands back this already-built
# object instantly, instead of re-walking every region/subregion/stain/field
# combination from scratch. This is the fix for the earlier "first click is
# slow" issue: that cost still exists, it's just paid once when the app
# process starts rather than once per click.
# ---------------------------------------------------------------------------
precomputed_metadata_accordion_ui <- lapply(
  list(home = "home", dstain = "dstain", sdonor = "sdonor", sregion = "sregion"),
  function(p) build_metadata_accordion(p, donor_metadata)
)