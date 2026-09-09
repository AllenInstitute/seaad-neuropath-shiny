library(jsonlite)
library(xml2)
library(dplyr)
library(bslib)
library(histoslider)

source("R/functions.R")

# ---------------------------------------------------------------------------
# List of JSON manifest sources — one entry per donor+region. Each can be a
# local file path (e.g. from a downloaded/mounted folder) or an https URL.
# Fill this in with your real manifest files across all donors/regions.
# ---------------------------------------------------------------------------
manifest_json_paths <- c(
  "https://sea-ad-quantitative-neuropathology.s3.amazonaws.com/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004.json",
  "https://sea-ad-quantitative-neuropathology.s3.amazonaws.com/middle-temporal-gyrus-and-superior-temporal-gyrus/H20.33.001/H20.33.001.json",
  "https://sea-ad-quantitative-neuropathology.s3.amazonaws.com/middle-temporal-gyrus-and-superior-temporal-gyrus/H20.33.002/H20.33.002.json"
  # "path/to/H19.33.004_middle-temporal-gyrus-and-superior-temporal-gyrus.json",
  # "https://.../another_donor_another_region.json",
)

donor_manifest <- build_donor_manifest(manifest_json_paths)
DONOR_CHOICES  <- names(donor_manifest)
ALL_STAINS     <- get_all_stains()

# ---------------------------------------------------------------------------
# Donor metadata. If a real specimen CSV is available, point this at it and
# METADATA_FIELDS' bounds/choices will be derived from the actual data
# (min/max for range fields, distinct values for select fields) rather than
# the placeholder values hardcoded in functions.R. Leave blank to fall back
# to dummy data.
# ---------------------------------------------------------------------------
specimen_metadata_csv_path <- "ins/SpecimenMetadata.csv"  # e.g. "path/to/specimen_metadata.csv"

donor_metadata <- if (nzchar(specimen_metadata_csv_path)) {
  load_specimen_metadata_csv(specimen_metadata_csv_path)
} else {
  generate_dummy_metadata(DONOR_CHOICES)
}

METADATA_FIELDS <- derive_metadata_fields(METADATA_FIELDS, donor_metadata)

# Shared theme object — passed to navbarPage(theme = ...) in ui.R. Using
# bslib for theming (rather than shinythemes) keeps it consistent with the
# bslib::accordion() components used for the metadata filters.
APP_THEME <- bslib::bs_theme(bootswatch = "lux")