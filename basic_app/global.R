library(dplyr)
library(bslib)
library(histoslider)
library(httr)
library(curl)
library(khroma)
library(shinyjs)

source("R/functions.R")

# donor metadata field spec — the single source of truth for what
# metadata exists in the app. load_specimen_metadata_csv() only reads
# columns declared here via csv_column; anything else in the specimen CSV
# is ignored. Range fields' min/max are always derived from real data
# (derive_metadata_fields()); select fields' choices are auto-derived
# unless hardcoded here.
metadata_fields <- list(
  list(id = "age_at_death",   label = "age at death",     type = "range",
       csv_column = "Age at death (years)"),
  list(id = "sex",             label = "sex",              type = "select",
       choices = c("Female", "Male"), csv_column = "Sex"),
  list(id = "apoe_genotype",   label = "APOE genotype",    type = "select",
       choices = c("2/2", "2/3", "2/4", "3/3", "3/4", "4/4"), csv_column = "APOE genotype"),
  list(id = "cog_status",      label = "cognitive status", type = "select",
       choices = c("Dementia", "No dementia"), csv_column = "Cognitive status"),
  list(id = "adnc",            label = "ADNC",             type = "select",
       choices = c("Not AD", "Low", "Intermediate", "High"), csv_column = "ADNC"),
  list(id = "thal_phase",      label = "thal phase",       type = "select",
       choices = as.character(0:5), csv_column = "Thal phase"),
  list(id = "braak_stage",     label = "braak stage",      type = "select",
       choices = c("0", "I", "II", "III", "IV", "V", "VI"), csv_column = "Braak stage"),
  list(id = "cerad_score",     label = "CERAD score",      type = "select",
       choices = c("Absent", "Sparse", "Moderate", "Frequent"), csv_column = "CERAD score"),
  list(id = "years_education", label = "years of education", type = "range",
       csv_column = "Years of education (years)"),
  list(id = "cps",             label = "CPS", type = "range",
       csv_column = "Continuous Pseudo-progression Score")
)

# loaded from the real specimen csv — no dummy-data fallback, a bad path
# here should error loudly rather than silently show fake data.
specimen_metadata_csv_path <- "ins/SpecimenMetadata.csv"

donor_metadata  <- load_specimen_metadata_csv(specimen_metadata_csv_path)
metadata_fields <- derive_metadata_fields(metadata_fields, donor_metadata)

# manifest source — a single csv covering any number of donors. required
# columns per row: file_type, stain_type, donor, region, s3_uri.
# annotation_name (or subregion) is only needed on annotation-xml rows.
# width/height are optional on RAW_IMAGE rows; if absent, the app reads
# them live from the .svs file's own header (read_tiff_dimensions()).
#
# increase this if you see "could not fetch ... Timeout was reached" at
# startup/load time — the S3 endpoint can be slow under load.
annotation_fetch_timeout_sec <- 60

manifest_csv_path <- "ins/260909_manifest_fill.csv"

csv_entries <- tryCatch(read_manifest_csv_entries(manifest_csv_path), error = function(e) {
  warning("could not read manifest csv: ", manifest_csv_path, " - ", e$message)
  list()
})

donor_manifest <- build_donor_manifest_from_entries(csv_entries)
donor_choices   <- names(donor_manifest)
all_stains      <- get_all_stains()

# groups metadata_fields into cards for display. references field ids
# only, so it can't drift out of sync with the specs above.
metadata_display_groups <- list(
  "demographic" = c("age_at_death", "sex", "apoe_genotype", "years_education"),
  "clinical"    = c("cog_status", "adnc", "thal_phase", "braak_stage", "cerad_score", "cps")
)

# QNP (quantitative neuropathology) filters — keyed by donor + region +
# subregion (unlike metadata_fields, which is one row per donor). Read
# from a separate csv (qnp_metadata_csv_path below). Every field is
# numeric. stain_group only organizes the filter UI (one accordion panel
# per stain) — it doesn't need to match the csv itself.
#
# csv_column values below are PLACEHOLDER descriptions, not confirmed
# against a real QNP csv — replace with the exact header text once you
# have that file, or every field will warn "missing column" at startup.
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

# donor/region_grouping/region/subregion column headers in the QNP csv.
# region_grouping is optional — falls back to region itself if the column
# doesn't exist (see load_qnp_metadata_csv()).
qnp_donor_column           <- "Donor ID"
qnp_region_grouping_column <- "region"
qnp_region_column          <- "brain region"
qnp_subregion_column       <- "analysis region"

qnp_metadata_csv_path <- "ins/QNPMetadata.csv"  # <- point this at your real QNP csv

qnp_metadata <- load_qnp_metadata_csv(qnp_metadata_csv_path)

# precomputed ONCE — every place needing "the QNP rows for this
# region+subregion" (Identify Donors' filter + accordion builder, both on
# every relevant reactive tick) reads this instead of re-deriving it each
# time. qnp_by_region[[region]] is itself keyed by subregion, so both
# levels are O(1) lookups.
qnp_by_region <- if (nrow(qnp_metadata) == 0) {
  list()
} else {
  split_by_region <- split(qnp_metadata, qnp_metadata$region)
  lapply(split_by_region, function(df_region) split(df_region, df_region$subregion))
}
# every qnp_fields entry is numeric — retrofitting type = "range" here
# (rather than repeating it in every list() entry above) lets shared code
# treat metadata_fields and qnp_fields identically.
qnp_fields <- lapply(qnp_fields, function(f) { f$type <- "range"; f })

# the complete QNP field spec, kept before the percent-only narrowing
# below — used where every measure should show (CSV download, per-donor
# popup), while on-page sliders use the narrowed qnp_fields.
qnp_fields_all <- qnp_fields

# Crosswalks between the image manifest's region/stain naming and QNP's
# own naming (qnp_metadata$region, qnp_fields$stain_group) — used to find
# the right QNP rows for the region+stain currently being viewed in an
# image-viewer donor popup.
#
# THESE ARE BEST-EFFORT GUESSES, not confirmed against a real QNP csv.
# Run sort(unique(qnp_metadata$region)) once you have real data and
# correct any right-hand side that doesn't match. The stain crosswalk is
# grounded in inventory_manifest.R's stain_type naming, mapping each
# combined-image stain to every qnp_fields stain_group it covers.
qnp_region_crosswalk <- list(
  "Dorsolateral Prefrontal Cortex (DLPFC)"                           = "DFC",
  "Medial Entorhinal Cortex and Hippocampus (MEC-HIP)"               = c("MEC", "HIP"),
  "Middle Temporal Gyrus (MTG) and Superior Temporal Gyrus (STG)"    = c("MTG", "STG"),
  "Primary Visual Cortex - Extrastriate Occipital Cortex (V1C-ESOC)" = c("V1C", "ESOC")
)
# NOTE: qnp_metadata$region also has AnG, CaH, FI, and ITG, which have no
# corresponding manifest/image region at all — QNP genuinely has more
# region coverage than the image viewers do, and that's expected. Those
# four remain fully usable on the Filter Donors page (which reads
# qnp_metadata directly, never through this crosswalk); they simply never
# show up in an image-viewer donor-info popup, since there's no image for
# them to be attached to.

qnp_stain_crosswalk <- list(
  "Abeta (6E10) and IBA1"     = c("6E10", "Iba1", "6E10 x Iba1"),
  "pTau (AT8) and pTDP-43"    = c("AT8", "pTDP43"),
  "NeuN"                      = "NeuN",
  "a-Synuclein"                = "aSyn",
  "GFAP"                       = "GFAP",
  "H&E-LFB"                    = "Hematoxylin"
)

# only percent-type measures get sliders on Filter Donors — every
# percent field's id has a "pct_" prefix, so that's what this filters on.
qnp_fields <- Filter(function(f) grepl("^pct_", f$id), qnp_fields)

# ---------------------------------------------------------------------------
# colors — every color used across ui.r/server.r/functions.r, in one place.
# ---------------------------------------------------------------------------
# NOTE: metadata_chart_color was requested as "#646FF" (5 hex digits,
# invalid) — completed to "#6464FF" as a best guess; confirm/correct if wrong.
metadata_chart_color  <- "#6464FF"  # brand color; also the Bootstrap theme's "primary"
table_stripe_color    <- "#E8E9FF"  # muted tint of metadata_chart_color; Filter Donors table stripe
sidebar_bg_color      <- "#ffffff"
sidebar_divider_color <- "#DED9D1"
brand_primary_color   <- "#aaa39f"  # the "sea-ad" brand text
accent_bg_color       <- "#f6f2fb"  # QNP/constraint card background tint
action_button_color   <- "#dc9600"  # Reset image zoom, Download table, Copy donor list buttons
context_card_bg_color <- "#FCE9C2"  # muted tint of action_button_color; context card background
reset_button_color    <- "#000000"  # page-level Reset buttons
icon_color            <- "#000000"  # plain black icon/link text (overrides Bootstrap's default link blue)

# Deliberately not a bootswatch preset — Lux's all-caps navbar text fought
# the custom navbar look requested, so this is plain Bootstrap 5 with just
# the primary color overridden.
app_theme <- bslib::bs_theme(primary = metadata_chart_color)

# ---------------------------------------------------------------------------
# hardcoded UI strings — one place so wording can't drift between
# functions.r/server.r/ui.r. Naming convention:
#   tt_<name>  — hover tooltip text
#   lbl_<name> — a short name for a button, feature, or the app
#   hdg_<name> — a structural section heading, reused across pages
#   msg_<name> — a notification message
# ---------------------------------------------------------------------------
lbl_app_title <- "SEA-AD Neuropathology Viewer"

# Compare Donors page: max donors comparable at once — used for the radio
# label, selectize's maxItems cap, the random-sample size, and the
# metadata-mode truncation, so all four always agree with each other.
donor_compare_cap <- 10
donor_compare_min <- 2
tt_donor_compare_cap <- sprintf("Number of donors that can be selected is capped at %d.", donor_compare_cap)

lbl_scratchpad <- "Scratchpad"
lbl_reset_image_zoom <- "Reset image zoom"

hdg_annotations <- "annotations"
hdg_shared      <- "shared"
hdg_qnp         <- "QNP"

msg_filters_reset <- "Filters reset."
msg_page_reset    <- "Page reset."

tt_sync_zoom <- paste(
  "Zoom is relative to each image individually, so synced views move",
  "together, but different sections or regions may not line up anatomically."
)
tt_back_to_top   <- "Back to top"
tt_copy_donor_id <- "Copy donor id"

# Compare Donors: max donors comparable at once — shared by the radio
# label, selectize's maxItems cap, the random-sample size, and the
# metadata-mode truncation.
donor_compare_cap <- 10
donor_compare_min <- 2
tt_donor_compare_cap <- sprintf("Number of donors that can be selected is capped at %d.", donor_compare_cap)

# sentinel meaning "averaged across every subregion in this region", used
# throughout the QNP filtering/display code instead of a repeated literal.
qnp_global_sentinel <- "Global"

# Filter Donors tab's exact title — compared against input$main_nav in
# server.r, and used by ui.r's tabPanel() itself, so the two can't drift.
filter_donors_tab_name <- "Filter Donors"

# precomputed ONCE per page prefix at startup, not per click/session —
# everything it's built from is static once the CSVs above are loaded.
precomputed_metadata_accordion_ui <- lapply(
  list(home = "home", dstain = "dstain", sdonor = "sdonor", sregion = "sregion"),
  function(p) build_metadata_accordion(p, donor_metadata)
)