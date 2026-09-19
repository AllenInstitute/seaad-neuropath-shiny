# =============================================================================
# R/aboutpage.R
#
# The About tab's content lives entirely here, separate from the rest of the
# app, specifically so it can be edited (contact info, usage notes,
# acknowledgements, etc.) without touching ui.R/server.R/global.R/functions.R.
#
# Shiny automatically sources every .R file in an R/ subdirectory alongside
# the app before global.R/ui.R/server.R run, so these two functions are
# already available by the time ui.R and server.R need them — no explicit
# source() call required.
# =============================================================================
library(faq)

# Returns the About tabPanel. Called once from ui.R, inside navbarPage(...).
about_tab_ui <- function() {
  tabPanel("About",
           fluidPage(
             
             h4("Usage"),
             p("Pick a donor, region, and stain on the Home tab to view a single slide, ",
               "or use one of the Compare tabs to view several images side by side. ",
               "Annotation checkboxes only fetch their underlying file the first time they're switched on."),
             
             h4("Relevant Publications"),
             p("See: ", a("https://brain-map.org/consortia/sea-ad/our-science", href="https://brain-map.org/consortia/sea-ad/our-science", target = "_blank")),
             p(strong("Quantitative neuropathology (QNP) values from:"), br(), 
               "Multiregional single-cell profiling reveals shared andspecialized cellular vulnerability in Alzheimer’s disease", 
               a("(Travaglini,Gabitto, Ding, et al., bioRxiv, 2026)", href = "https://doi.org/10.64898/2026.07.01.734821", target = "_blank"), br(),
               "The Caudate Nucleus Exhibits Distinct Pathology and Cell Type-Specific Responses Across Alzheimer’s Disease",
               a("(Kana et al., bioRxiv, 2026)", href = "https://www.biorxiv.org/content/10.64898/2026.01.10.694705v2", target = "_blank")),
             
             h4("Acknowledgements"),
             p("App developed by Irika R. Sinha. Claude Sonnet 5 and OpenAI GPT-5.6 Sol were used to develop and troubleshoot code."),
             p("More information on the SEA-AD project can be found on the ",
               tags$a(href = "https://sea-ad.org/", "SEA-AD homepage.", target = "_blank")),
             
             h4("Specimen metadata"),
             downloadButton("about_download_metadata", "Download specimen metadata CSV", style = sprintf("background-color:%s; border-color:%s; color:#fff;", action_button_color, action_button_color)),
             br(),
             tags$img(
               src = "SEA-AD_Alternate-Renewal2.png",
               style = "max-height:100px; margin-bottom:16px; border-radius:6px;"
             ),
             tags$img(
               src = "AI_medium_maroon_black.svg",
               style = "max-height:100px; margin-bottom:16px; border-radius:6px;margin-left:20px;"
             ),
             # br(),
             # h4("Frequently Asked Questions"),
             # faq::faq(data = df, elementId = "faq", height = "50%",faqtitle = ""),
           )
  )
}

# Registers the About tab's server-side outputs. Called once from server.R.
about_tab_server <- function(input, output, session) {
  
  output$about_download_metadata <- downloadHandler(
    filename = function() basename(specimen_metadata_csv_path),
    content  = function(file) file.copy(specimen_metadata_csv_path, file)
  )
}