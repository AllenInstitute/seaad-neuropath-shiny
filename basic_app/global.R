library(dplyr)
library(bslib)
library(histoslider)
library(httr)
library(curl)
library(khroma)
library(shinyjs)
library(ggplot2)
library(plotly)
library(sysfonts)
library(showtext)

source("R/functions.R")

# donor metadata field spec — the single source of truth for what
# metadata exists in the app. load_specimen_metadata_csv() only reads
# columns declared here via csv_column; anything else in the specimen CSV
# is ignored. Range fields' min/max are always derived from real data
# (derive_metadata_fields()); select fields' choices are auto-derived
# unless hardcoded here.
metadata_fields <- list(
  list(id = "age_at_death", label = "age at death", type = "range",
       csv_column = "Age at death (years)"),
  list(id = "sex", label = "sex", type = "select",
       choices = c("Female", "Male"), csv_column = "Sex"),
  list(id = "apoe_genotype", label = "APOE genotype", type = "select",
       choices = c("2/2", "2/3", "2/4", "3/3", "3/4", "4/4"), csv_column = "APOE genotype"),
  list(id = "cog_status", label = "cognitive status", type = "select",
       choices = c("Dementia", "No dementia"), csv_column = "Cognitive status"),
  list(id = "adnc", label = "ADNC", type = "select",
       choices = c("Not AD", "Low", "Intermediate", "High"), csv_column = "ADNC"),
  list(id = "thal_phase", label = "thal phase", type = "select",
       choices = as.character(0:5), csv_column = "Thal phase"),
  list(id = "braak_stage", label = "braak stage", type = "select",
       choices = c("0", "I", "II", "III", "IV", "V", "VI"), csv_column = "Braak stage"),
  list(id = "cerad_score", label = "CERAD score", type = "select",
       choices = c("Absent", "Sparse", "Moderate", "Frequent"), csv_column = "CERAD score"),
  list(id = "years_education", label = "years of education", type = "range",
       csv_column = "Years of education (years)"),
  list(id = "cps", label = "CPS", type = "range",
       csv_column = "Continuous Pseudo-progression Score")
)

# loaded from the real specimen csv — no dummy-data fallback, a bad path
# here should error loudly rather than silently show fake data.
specimen_metadata_csv_path <- "ins/SpecimenMetadata.csv"

donor_metadata  <- load_specimen_metadata_csv(specimen_metadata_csv_path)
metadata_fields <- derive_metadata_fields(metadata_fields, donor_metadata)

# QNP Graphs' derived "age at death, 5-yr bins" categorical field — age
# itself is continuous, unsuited to a boxplot x-axis. Derived here (not
# read from a csv column), so it's tracked separately from metadata_fields
# rather than added as a fake entry there. See compute_age_buckets() in
# functions.r.
qnp_graph_age_bucket_field <- "age_bucket"
qnp_graph_age_bucket_label <- "age at death (5-yr bins)"
age_buckets <- compute_age_buckets(donor_metadata$age_at_death)
donor_metadata$age_bucket <- age_buckets$values
qnp_graph_age_bucket_levels <- age_buckets$levels

# register the app's own custom fonts with R's graphics device too — CSS
# @font-face (ui.r) only applies to HTML, never to plot images rendered
# by ggplot2/showtext. Uses the .ttf files under www/fonts/ (added
# alongside the .woff2 ones ui.r's CSS already uses) — the .woff2 files
# themselves errored here ("freetype: unknown file format") in an
# environment whose freetype build doesn't support woff2. Falls back to
# each theme's plain default font rather than failing the app if these
# can't be read either.
qnp_graph_axis_font  <- "sans-serif"
qnp_graph_title_font <- "sans-serif"
qnp_graph_axis_font_path  <- "www/fonts/AllenInstituteText-Light.ttf"
qnp_graph_title_font_path <- "www/fonts/AllenInstituteHeadline-Bold.ttf"
if (file.exists(qnp_graph_axis_font_path) && file.exists(qnp_graph_title_font_path)) {
  tryCatch({
    sysfonts::font_add(family = "AllenTextLight", regular = qnp_graph_axis_font_path)
    sysfonts::font_add(family = "AllenHeadlineBold", regular = qnp_graph_title_font_path)
    showtext::showtext_auto()
    qnp_graph_axis_font  <- "AllenTextLight"
    qnp_graph_title_font <- "AllenHeadlineBold"
  }, error = function(e) {
    warning("could not register custom fonts for ggplot2 plots (", e$message, ") — falling back to the default font.")
  })
} else {
  # names/paths the app checked don't match what's actually on disk —
  # showing exactly what was searched (and from where) so this is fixable
  # in one look, rather than a generic "not found" with no path to check.
  warning(
    "custom fonts for ggplot2 plots not found — checked for '", qnp_graph_axis_font_path, "' and '", qnp_graph_title_font_path,
    "' relative to the working directory '", getwd(), "' — falling back to the default font."
  )
}

# manifest source — a single csv covering any number of donors. required
# columns per row: file_type, stain_type, donor, region, s3_uri.
# annotation_name (or subregion) is only needed on annotation-xml rows.
# width/height are optional on RAW_IMAGE rows; if absent, the app reads
# them live from the .svs file's own header (read_tiff_dimensions()).
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

qnp_fields <- list(
  list(id = "avg_6e10_object_area",              label = "Avg 6E10+ object area",
       stain_group = "6E10", csv_column = "average 6e10 positive object area"),
  list(id = "avg_6e10_object_median_diameter",    label = "Avg 6E10+ object median diameter",
       stain_group = "6E10", csv_column = "average 6e10 positive object median diameter"),
  list(id = "n_6e10_objects_per_area",            label = "# of 6E10+ objects/area",
       stain_group = "6E10", csv_column = "number of 6e10 positive objects per area"),
  list(id = "pct_6e10_dense_core_plaque_area",    label = "% 6E10+ dense core plaque area",
       stain_group = "6E10", csv_column = "percent 6e10 dense core plaque area"),
  list(id = "pct_6e10_diffuse_plaque_area",       label = "% 6E10+ diffuse plaque area",
       stain_group = "6E10", csv_column = "percent 6e10 diffuse plaque area"),
  list(id = "pct_6e10_fibrilar_plaque_area",      label = "% 6E10+ fibrilar plaque area",
       stain_group = "6E10", csv_column = "percent 6e10 fibrilar plaque area"),
  list(id = "pct_6e10_positive_area",             label = "% 6E10+ positive area",
       stain_group = "6E10", csv_column = "percent 6e10 positive area"),
  
  list(id = "avg_iba1_process_area_per_cell",     label = "Avg IBA1+ process area/cell",
       stain_group = "Iba1", csv_column = "average Iba1 positive process area per cell"),
  list(id = "avg_iba1_process_length_per_cell",   label = "Avg IBA1+ process length/cell",
       stain_group = "Iba1", csv_column = "average Iba1 positive process length per cell"),
  list(id = "n_iba1_cells_per_area",              label = "# IBA1+ of cells/area",
       stain_group = "Iba1", csv_column = "number of Iba1 positive cells per area"),
  list(id = "n_iba1_activated_cells_per_area",    label = "# of IBA1+ activated cells/area",
       stain_group = "Iba1", csv_column = "number of activated Iba1 positive cells per area"),
  list(id = "n_iba1_inactivated_cells_per_area",  label = "# of IBA1+ inactivated cells/area",
       stain_group = "Iba1", csv_column = "number of inactivated Iba1 positive cells per area"),
  list(id = "pct_iba1_positive_area",             label = "% IBA1+ area",
       stain_group = "Iba1", csv_column = "percent Iba1 positive area"),
  
  list(id = "n_6e10_coloc_iba1_per_area",         label = "# 6E10+/IBA1+ colocalized objects/area",
       stain_group = "6E10 x Iba1", csv_column = "number of 6e10 positive objects colocalized with Iba1 positive objects per area"),
  list(id = "pct_6e10_coloc_iba1",                label = "% 6E10 objects colocalized with Iba1",
       stain_group = "6E10 x Iba1", csv_column = "percent of 6e10 positive objects colocalized with Iba1 positive objects"),
  
  list(id = "avg_hematoxylin_nucleus_area",       label = "Avg hematoxylin+ nucleus area",
       stain_group = "Hematoxylin", csv_column = "average Hematoxylin positive nucleus area"),
  list(id = "avg_hematoxylin_nucleus_perimeter",  label = "Avg hematoxylin+ nucleus perimeter",
       stain_group = "Hematoxylin", csv_column = "average Hematoxylin positive nucleus perimeter"),
  list(id = "avg_hematoxylin_nucleus_roundness",  label = "Avg hematoxylin+ nucleus roundness",
       stain_group = "Hematoxylin", csv_column = "average Hematoxylin positive nucleus roundness"),
  list(id = "n_hematoxylin_nuclei_per_area",      label = "# hematoxylin+ nuclei/area",
       stain_group = "Hematoxylin", csv_column = "number of Hematoxylin positive nuclei per area"),
  
  list(id = "pct_gfap_positive_area",             label = "% GFAP+ area",
       stain_group = "GFAP", csv_column = "percent GFAP positive area"),
  
  list(id = "avg_asyn_cell_area",                 label = "Avg a-syn+ cell area",
       stain_group = "aSyn", csv_column = "average aSyn positive cell area"),
  list(id = "n_asyn_cells_per_area",              label = "Number of a-syn+ cells/area",
       stain_group = "aSyn", csv_column = "number of aSyn positive cells per area"),
  list(id = "pct_asyn_positive_area",             label = "% positive area",
       stain_group = "aSyn", csv_column = "percent aSyn positive area"),
  
  list(id = "avg_at8_cell_area",                  label = "Avg AT8+ cell area",
       stain_group = "AT8", csv_column = "average AT8 positive cell area"),
  list(id = "n_at8_cells_per_area",               label = "# AT8+ cells/area",
       stain_group = "AT8", csv_column = "number of AT8 positive cells per area"),
  list(id = "pct_at8_positive_area",              label = "% AT8+ area",
       stain_group = "AT8", csv_column = "percent AT8 positive area"),
  
  list(id = "avg_ptdp43_cell_area",               label = "Avg pTDP-43+ cell area",
       stain_group = "pTDP43", csv_column = "average pTDP43 positive cell area"),
  list(id = "n_ptdp43_cells_per_area",            label = "# pTDP-43+ cells/area",
       stain_group = "pTDP43", csv_column = "number of pTDP43 positive cells per area"),
  list(id = "pct_ptdp43_positive_area",           label = "% pTDP-43+ area",
       stain_group = "pTDP43", csv_column = "percent pTDP43 positive area"),
  
  list(id = "avg_neun_cell_area",                 label = "Avg NeuN+ cell area",
       stain_group = "NeuN", csv_column = "average NeuN positive cell area"),
  list(id = "n_neun_cells_per_area",              label = "# NeuN+ cells/area",
       stain_group = "NeuN", csv_column = "number of NeuN positive cells per area"),
  list(id = "pct_neun_positive_area",             label = "% NeuN+ area",
       stain_group = "NeuN", csv_column = "percent NeuN positive area")
)

# donor/region_grouping/region/subregion column headers in the QNP csv.
# region_grouping is optional — falls back to region itself if the column
# doesn't exist (see load_qnp_metadata_csv()).
qnp_donor_column <- "Donor ID"
qnp_region_grouping_column <- "region"
qnp_region_column <- "brain region"
qnp_subregion_column <- "analysis region"

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
qnp_region_crosswalk <- list(
  "Dorsolateral Prefrontal Cortex (DLPFC)"                           = "DFC",
  "Medial Entorhinal Cortex and Hippocampus (MEC-HIP)"               = c("MEC", "HIP"),
  "Middle Temporal Gyrus (MTG) and Superior Temporal Gyrus (STG)"    = c("MTG", "STG"),
  "Primary Visual Cortex - Extrastriate Occipital Cortex (V1C-ESOC)" = c("V1C", "ESOC")
)

# qnp_metadata$region also has AnG, CaH, FI, and ITG, with no
# corresponding manifest/image region — expected, since QNP has more
# region coverage than the image viewers. Those four stay fully usable on
# Filter Donors (which reads qnp_metadata directly, not through this
# crosswalk); they just never show up in an image-viewer donor popup.

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
lbl_app_title         <- "SEA-AD Neuropathology Viewer"
lbl_brand_primary     <- "sea-ad"
lbl_brand_secondary   <- "neuropathology viewer"
lbl_scratchpad        <- "Scratchpad"
lbl_reset_image_zoom  <- "Reset image zoom"

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
filter_donors_tab_name <- "filter donors"

# same pattern for the QNP Graphs tab.
qnp_graph_tab_name <- "qnp graphs"

# QNP Graphs page: the demographic/clinical fields offered on the x-axis
# as a boxplot grouping category — every metadata_fields select-type field,
# plus the derived age_bucket (age itself, years_education, and cps are
# continuous, unsuited to a discrete grouping).
qnp_graph_categorical_fields <- c("sex", "apoe_genotype", "cog_status", "adnc", "thal_phase", "braak_stage", "cerad_score", qnp_graph_age_bucket_field)

# the one numeric clinical field offered as the "CPS vs QNP" scatter's x-axis.
qnp_graph_cps_field <- "cps"

# cap on how many QNP measures can be plotted on the Y axis at once — each
# gets its own color, and a scatter's legend/palette stop being readable
# well beyond a handful of series.
qnp_graph_color_cap <- 5

# shared sizing/opacity for both plot builders (build_qnp_grouped_boxplot(),
# build_qnp_multi_scatter()) — one place so the two can't drift apart if
# ever tuned separately.
qnp_graph_base_text_size <- 12
qnp_graph_point_size <- 2.5
qnp_graph_fill_alpha  <- 0.15  # boxplot fill
qnp_graph_point_alpha <- 0.55  # points, both plot types

# precomputed ONCE per page prefix at startup, not per click/session —
# everything it's built from is static once the CSVs above are loaded.
precomputed_metadata_accordion_ui <- lapply(
  list(home = "home", dstain = "dstain", sdonor = "sdonor", sregion = "sregion"),
  function(p) build_metadata_accordion(p, donor_metadata)
)