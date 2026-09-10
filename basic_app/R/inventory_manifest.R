# library(data.table)
# library(stringr)
# library(dplyr)
# library(tidyr)
# 
# setwd("/Users/irika.sinha/Documents/GitHub/seaad-neuropath-shiny/basic_app")
# full_manifest <- fread("ins/inventory.csv")
# 
# manifest_filt <- full_manifest %>% filter(#suffix %in% c("",".dzi",".annotations",".svs"),
#                                           str_detect(path, "simple_viewer", negate = T),
#                                           str_detect(path, "temp/", negate = T),
#                                           str_detect(path, "middle-temporal-gyrus/", negate = T),
#                                           str_detect(name, "primary.dzi") | str_detect(name, "analysis.dzi") | str_detect(name, "\\.annotations") | str_detect(name, "\\.svs"),
#                                           !(name %in% c(seq(0,15),"downsampled"))) %>%
#   arrange(parent_prefix) %>%
#   mutate(donor = str_split_i(path,"/",2),
#          roi = str_split_i(path,"/",1),
#          stain = str_split_i(str_split_i(path,"/",3),"-",-1),
#          stain_type = case_when(stain == "AT" ~ "AT8",
#                            stain == "NEUN" ~ "NeuN",
#                            stain == "ASYN" ~ "a-Synuclein",
#                            stain == "GFAP" ~ "GFAP",
#                            stain == "I6" ~ "Abeta (6E10) and IBA1",
#                            stain == "LFB" ~ "H&E-LFB",
#                            .default = ""),
#          region = case_when(roi == "dorsolateral-prefrontal-cortex" ~ "Dorsolateral Prefrontal Cortex (DLPFC)",
#                             roi == "hippocampus-medial-entorhinal-cortex" ~ "Medial Entorhinal Cortex and Hippocampus (MEC-HIP)",
#                             roi == "middle-temporal-gyrus-and-superior-temporal-gyrus" ~ "Middle Temporal Gyrus (MTG) and Superior Temporal Gyrus (STG)",
#                             roi == "primary-visual-cortex-extrastriate-occipital-cortex" ~ "Primary Visual Cortex - Extrastriate Occipital Cortex (V1C-ESOC)",
#                             .default = roi),
#          annotation_type = case_when(str_detect(name, "\\.annotations") ~ str_split_i(name,"_",2),
#                                      .default= ""),
#          full_path = str_c("s3://sea-ad-quantitative-neuropathology",path,sep="/"),
#          annotation_meta = case_when(annotation_type != "" ~ str_c(annotation_type, full_path, sep ="::"),
#                                      .default="")) %>%
#   # dplyr::select(!c(compound_suffix, size_bytes, last_modified, storage_class, etag,record_type, bucket)) %>%
#   dplyr::select(region,donor,stain_type,annotation_type,parent_prefix,name,suffix,full_path, annotation_meta) %>%
#   filter(stain_type != "", str_detect(name, fixed(donor)))
# 
# # test <- manifest_filt %>% filter(donor == "H21.33.028", region == "Medial Entorhinal Cortex and Hippocampus (MEC-HIP)")
# final_df <- manifest_filt %>% group_by(donor, region, stain_type) %>% summarise(RAW_IMAGE_DEEPZOOM = full_path[str_detect(name, "primary.dzi")],
#                                                               RAW_IMAGE = full_path[str_detect(name, ".svs")],
#                                                               HALO_ANALYSIS_IMAGE_DEEPZOOM = ifelse(length(str_subset(name, "analysis.dzi")) > 0,
#                                                                                                     full_path[str_detect(name, "analysis.dzi")],""),
#                                                               SUBREGION_ANNOTATIONS_XML = str_c(str_subset((annotation_meta),"."), collapse=",")) %>%
#   tidyr::separate_longer_delim(SUBREGION_ANNOTATIONS_XML, ",") %>%
#   tidyr::pivot_longer(cols = c(RAW_IMAGE_DEEPZOOM, RAW_IMAGE, HALO_ANALYSIS_IMAGE_DEEPZOOM, SUBREGION_ANNOTATIONS_XML),
#                       names_to="file_type",
#                       values_to = "s3_uri") %>% unique() %>%
#   tidyr::separate_wider_delim(cols = s3_uri, delim = "::", names = c("annotation_name", "s3_uri"),
#                               too_few = "align_end") %>%
#   dplyr::select(c(file_type,stain_type,donor,region,s3_uri,annotation_name))
# # write.csv(final_df,"outs/260909_manifest.csv")
# 
# # =============================================================================
# # precompute manifest dimensions.R
# # reuses the exact same TIFF-header-reading + s3-to-https logic the app
# # itself uses (read_tiff_dimensions, http_range_bytes, s3_to_https)
# # =============================================================================
# 
# manifest <- final_df
# 
# if (!("width" %in% names(manifest)))  manifest$width  <- NA_real_
# if (!("height" %in% names(manifest))) manifest$height <- NA_real_
# 
# is_raw_image <- manifest$file_type == "RAW_IMAGE"
# 
# ## Call svs image and determine dimensions
# for (i in which(is_raw_image)) {
#   url <- s3_to_https(manifest$s3_uri[i])
#   cat(sprintf("[%d/%d] %s ... ", i, nrow(manifest), url))
#   
#   dims <- tryCatch(read_tiff_dimensions(url), error = function(e) {
#     cat("FAILED:", conditionMessage(e), "\n")
#     list(width = NA_real_, height = NA_real_)
#   })
#   
#   if (!is.na(dims$width)) {
#     manifest$width[i]  <- dims$width
#     manifest$height[i] <- dims$height
#     cat(sprintf("%s x %s\n", dims$width, dims$height))
#   }
# }
# 
# # failed <- is_raw_image & is.na(manifest$width)
# # if (any(failed)) {
# #   cat("\nWARNING:", sum(failed), "RAW_IMAGE row(s) could not be read. Their\n")
# #   cat("width/height will stay blank, and the app will fall back to trying\n")
# #   cat("to read them live at startup for these specific rows:\n")
# #   print(manifest$s3_uri[failed])
# # }
# 
# # write.csv(manifest, "outs/260909_manifest_fill.csv", row.names = FALSE)
# 
