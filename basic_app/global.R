library(dplyr)
library(bslib)
library(histoslider)
library(httr)
library(shinycssloaders)
library(khroma)
library(shinyWidgets)

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
#   - "ordinal" fields: categorical values with a meaningful order — declare
#     `choices` IN ORDER (position 1..N is what gets filtered on via a
#     histoslider). choices must always be given explicitly; there's no way
#     to safely auto-derive a clinically-correct order from raw data.
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
  list(id = "adnc",            label = "ADNC",             type = "ordinal",
       choices = c("Not AD", "Low", "Intermediate", "High"), csv_column = "ADNC"),
  list(id = "thal_phase",      label = "Thal phase",       type = "ordinal",
       choices = as.character(0:5), csv_column = "Thal phase"),
  list(id = "braak_stage",     label = "Braak stage",      type = "ordinal",
       choices = c("0", "I", "II", "III", "IV", "V", "VI"), csv_column = "Braak stage"),
  list(id = "cerad_score",     label = "CERAD score",      type = "ordinal",
       choices = c("Absent", "Sparse", "Moderate", "Frequent"), csv_column = "CERAD score"),
  list(id = "years_education", label = "Years of education", type = "range",
       csv_column = "Years of education (years)"),
  list(id = "cps",             label = "Continuous Pseudo-progression Score (CPS)", type = "range",
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
  "Pathology"   = c("cog_status", "adnc", "thal_phase", "braak_stage", "cerad_score", "cps")
)

# shared color used for BOTH histoslider bars and the categorical histogram
# bars (register_metadata_histograms()), so all metadata charts look
# consistent. change this one value to restyle every metadata chart at once.
metadata_chart_color <- "black"

# ---------------------------------------------------------------------------
# annotation colors are chosen automatically per load — see
# build_annotation_color_map() in functions.r. it picks a colorblind-friendly
# qualitative palette from khroma sized to how many distinct annotation
# labels are actually present (muted <10, sunset 10-11, nightfall 12-17),
# and assigns one color per label. nothing to configure here unless you want
# to swap the palette scheme itself.
# ---------------------------------------------------------------------------

# shared theme — passed to navbarPage(theme = ...) in ui.R.
app_theme <- bslib::bs_theme(bootswatch = "lux")