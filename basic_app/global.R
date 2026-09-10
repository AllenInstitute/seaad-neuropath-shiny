library(dplyr)
library(bslib)
library(histoslider)
library(httr)
library(shinycssloaders)

source("R/functions.R")

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
manifest_csv_path <- "ins/260909_manifest_fill.csv"  # e.g. "ins/AllDonorsManifest.csv"

csv_entries <- tryCatch(read_manifest_csv_entries(manifest_csv_path), error = function(e) {
  warning("could not read manifest csv: ", manifest_csv_path, " - ", e$message)
  list()
})

donor_manifest <- build_donor_manifest_from_entries(csv_entries)
donor_choices   <- names(donor_manifest)
all_stains      <- get_all_stains()

# ---------------------------------------------------------------------------
# donor metadata — loaded from the real specimen csv (no dummy-data fallback:
# a bad path here should error loudly rather than silently show fake data).
# ---------------------------------------------------------------------------
specimen_metadata_csv_path <- "ins/SpecimenMetadata.csv"

donor_metadata <- load_specimen_metadata_csv(specimen_metadata_csv_path)

# each field needs id/label/type, set here and never auto-changed:
#   - "range" fields: do NOT set min/max here — they are always derived from
#     the real data below (see derive_metadata_fields()), never guessed.
#   - "select" fields: choices are OPTIONAL here. If you hardcode `choices`,
#     that list is authoritative and won't be touched by real data (so a
#     brand-new category value in future data would need to be added here
#     manually). Omit `choices` entirely to have them auto-derived from
#     donor_metadata instead (new values then show up with no code change).
metadata_fields <- list(
  list(id = "age_at_death",    label = "Age at death",      type = "range"),
  list(id = "sex",              label = "Sex",               type = "select", choices = c("Female", "Male")),
  list(id = "apoe_genotype",    label = "APOE genotype",     type = "select",
       choices = c("2/2", "2/3", "2/4", "3/3", "3/4", "4/4")),
  list(id = "cog_status",       label = "Cognitive status",  type = "select",
       choices = c("Dementia", "No dementia")),
  list(id = "adnc",             label = "ADNC",               type = "select",
       choices = c("Not AD", "Low", "Intermediate", "High")),
  list(id = "thal_phase",       label = "Thal phase",         type = "select"),  # choices auto-derived
  list(id = "braak_stage",      label = "Braak stage",        type = "select"),  # choices auto-derived
  list(id = "cerad_score",      label = "CERAD score",        type = "select"),  # choices auto-derived
  list(id = "years_education", label = "Years of education", type = "range"),
  list(id = "cps",              label = "Continuous Pseudo-progression Score (CPS)", type = "range")
)

metadata_fields <- derive_metadata_fields(metadata_fields, donor_metadata)

# ---------------------------------------------------------------------------
# annotation colors — set a PALETTE, not a per-label dictionary. each
# annotation label gets a color from this palette automatically (assigned
# consistently via a hash of the label, so the same label always gets the
# same color) — no need to know every label in advance or maintain a
# per-label mapping by hand. set to character(0) to disable overrides
# entirely and keep each file's original HALO-authored color instead.
#
# any R color vector works here — a hand-picked list, or a real colormap:
#   grDevices::hcl.colors(n, palette = "Dark 3")
#   RColorBrewer::brewer.pal(n, "Set2")
#   viridisLite::viridis(n)
# ---------------------------------------------------------------------------
annotation_color_palette <- grDevices::hcl.colors(8, palette = "Dark 3")

# shared theme — passed to navbarPage(theme = ...) in ui.R.
app_theme <- bslib::bs_theme(bootswatch = "lux")