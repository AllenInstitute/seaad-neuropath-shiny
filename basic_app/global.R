library(jsonlite)
library(xml2)

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
# Donor metadata — DUMMY DATA for now. Replace generate_dummy_metadata() with
# a real query (database, CSV, API, etc.) once that source is available.
# ---------------------------------------------------------------------------
donor_metadata <- generate_dummy_metadata(DONOR_CHOICES)

# Attach metadata onto each donor's manifest entry too, so it travels with
# the rest of that donor's data if needed elsewhere.
for (d in DONOR_CHOICES) {
  donor_manifest[[d]]$metadata <- as.list(donor_metadata[donor_metadata$donor == d, ])
}

# Fallback bounds for the age filter slider when there's no metadata yet.
AGE_RANGE_DEFAULT <- if (nrow(donor_metadata) > 0) range(donor_metadata$age) else c(50, 100)