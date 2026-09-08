# --- Donor file manifest ---
# Hardcoded from the JSON manifest for donor H19.33.004. Converts s3:// URIs
# to public https:// URLs (adjust the bucket-to-domain mapping below if this
# bucket isn't served the same way as the sea-ad-quantitative-neuropathology one).

s3_to_https <- function(s3_uri) {
  parts <- sub("^s3://", "", s3_uri)
  bucket <- sub("/.*$", "", parts)
  key <- sub("^[^/]+/", "", parts)
  paste0("https://", bucket, ".s3.amazonaws.com/", key)
}

# One entry per stain: primary (base) DZI, analysis DZI (proper pre-built .dzi,
# same resolution as annotation.svg so it overlays with NO scale correction
# needed), and the raw .svs width/height (the coordinate space whole-slide
# .annotations vertex files are recorded in).
donor_manifest <- list(
  "a-Syn" = list(
    primary_dzi  = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-a-Syn-primary/H19.33.004-A6-ASYN-primary.dzi"),
    analysis_dzi = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-a-Syn-analysis/H19.33.004-A6-a-Syn_analysis.dzi"),
    raw_svs_width = 51792, raw_svs_height = 41405
  ),
  "AT" = list(
    primary_dzi  = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-AT-primary/H19.33.004-A6-AT-primary.dzi"),
    analysis_dzi = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-AT-analysis/H19.33.004-A6-AT_analysis.dzi"),
    raw_svs_width = 55775, raw_svs_height = 45245
  ),
  "GFAP" = list(
    primary_dzi  = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-GFAP-primary/H19.33.004-A6-GFAP-primary.dzi"),
    analysis_dzi = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-GFAP-analysis/H19.33.004-A6-GFAP_analysis.dzi"),
    raw_svs_width = 51792, raw_svs_height = 41485
  ),
  "I6" = list(
    primary_dzi  = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-I6-primary/H19.33.004-A6-I6-primary.dzi"),
    analysis_dzi = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-I6-analysis/H19.33.004-A6-I6_analysis.dzi"),
    raw_svs_width = 51792, raw_svs_height = 42131
  ),
  "NeuN" = list(
    primary_dzi  = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A06-NeuN-primary/H19.33.004-A06-NEUN-primary.dzi"),
    analysis_dzi = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A06-NeuN-analysis/H19.33.004-A06-NeuN_analysis.dzi"),
    raw_svs_width = 51792, raw_svs_height = 42046
  ),
  "LFB" = list(
    primary_dzi  = s3_to_https("s3://staging-sea-ad-neuropath/middle-temporal-gyrus-and-superior-temporal-gyrus/H19.33.004/H19.33.004-A6-LFB-primary/H19.33.004-A6-LFB-primary.dzi"),
    analysis_dzi = NULL,  # no analysis/annotation files exist for this stain
    raw_svs_width = 53783, raw_svs_height = 41201
  )
)

STAIN_CHOICES <- names(donor_manifest)