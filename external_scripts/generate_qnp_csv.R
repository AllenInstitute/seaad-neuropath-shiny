# library(readxl)
# library(dplyr)
# library(purrr)
# 
# filename <- "/Users/irika.sinha/Downloads/sea-ad-quantitative-neuropathology-063026.xlsx"
# sheet_names <- readxl::excel_sheets(filename)[-c(1,2,3,4)]
# 
# df_colnames <- colnames(readxl::read_xlsx(filename,sheet=2))
# df <- data.frame(matrix(nrow = 0, ncol = length(df_colnames)))
# colnames(df) <- df_colnames
# 
# combined_df <- setNames(sheet_names, sheet_names) %>% 
#   map_df(~ read_excel(filename, sheet = .x), .id = "region") %>%
#   mutate(`analysis region` = ifelse(region %in% c("MTG", "ITG", "MTG"), paste(region, `analysis region`,sep = ": " ), `analysis region`),
#          region = region %>% 
#            recode_values(
#                 "AnG" ~ "Angular Gyrus (AnG)",
#                 "CaH" ~ "Caudate Nucleus (CaH, CaB, CaT)",
#                 "DFC" ~ "Dorsolateral Prefrontal Cortex (DLPFC)" ,
#                 "FI" ~ "Frontoinsular Cortex (FI)",
#                 "Hip-MEC" ~ "Medial Entorhinal Cortex and Hippocampus (MEC-HIP)",
#                 "ITG" ~ "Temporal Gyrus (STG, MTG, ITG)",
#                 "MTG" ~ "Temporal Gyrus (STG, MTG, ITG)",
#                 "STG" ~ "Temporal Gyrus (STG, MTG, ITG)",
#                 "VIC-ESOC" ~ "Primary Visual Cortex - Extrastriate Occipital Cortex (V1C-ESOC)"))
# # write.csv(combined_df, "/Users/irika.sinha/Documents/GitHub/seaad-neuropath-shiny/basic_app/ins/QNPMetadata.csv", row.names = F)
#          