function(input, output, session) {
  
  # small styled card summarizing a comparison page's two FIXED (non-varying)
  # fields. built from the loaded entries (not the live dropdown values) so it
  # only appears once Load has actually been clicked — see req(length(...)>0).
  constraint_card <- function(...) {
    pairs <- list(...)
    div(
      style = "padding:10px 16px; margin-bottom:12px; background:#f6f2fb; border-left:4px solid #7952b3; border-radius:4px;",
      tagList(lapply(names(pairs), function(k) {
        tags$span(style = "margin-right:24px;", tags$strong(paste0(k, ": ")), pairs[[k]])
      }))
    )
  }
  
  # each page's "currently loaded" state lives in a reactiveVal (not a plain
  # eventReactive) so the nav-change observer at the bottom of this file can
  # explicitly clear it back to empty when the user switches tabs.
  home_entries_rv    <- reactiveVal(list())
  dstain_entries_rv  <- reactiveVal(list())
  sdonor_entries_rv  <- reactiveVal(list())
  sregion_entries_rv <- reactiveVal(list())
  
  # ===========================================================================
  # home — single donor/region/stain
  # ===========================================================================
  
  observeEvent(input$home_donor, {
    req(input$home_donor, nzchar(input$home_donor))
    updateSelectInput(session, "home_region", choices = with_placeholder(get_regions_for_donor(input$home_donor)))
  })
  
  observeEvent(list(input$home_donor, input$home_region), {
    req(input$home_donor, input$home_region, nzchar(input$home_donor), nzchar(input$home_region))
    updateSelectInput(session, "home_stain",
                      choices = with_placeholder(get_stain_choices_for_donor_region(input$home_donor, input$home_region)))
  }, ignoreInit = TRUE)
  
  output$home_annotation_ui <- renderUI({ render_annotation_master_ui(home_entries_rv()) })
  output$home_viewer_grid   <- renderUI({ render_viewer_grid_ui(home_entries_rv(), label_field = "stain") })
  
  observeEvent(input$home_load_btn, {
    req(input$home_donor, input$home_region, input$home_stain,
        nzchar(input$home_donor), nzchar(input$home_region), nzchar(input$home_stain))
    slot <- donor_manifest[[input$home_donor]][[input$home_region]][[input$home_stain]]
    shiny::validate(shiny::need(!is.null(slot), "No image found for this donor/region/stain combination."))
    
    entries <- list(list(donor = input$home_donor, region = input$home_region, stain = input$home_stain, slot = slot))
    home_entries_rv(entries)
    
    images <- build_images_payload(entries, input$home_overlay_opacity, input$home_show_overlay)
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images)), once = TRUE)
  })
  
  # ===========================================================================
  # compare stains — constraints: donor + region. varies: stain.
  # ===========================================================================
  
  observeEvent(input$dstain_donor, {
    req(input$dstain_donor, nzchar(input$dstain_donor))
    updateSelectInput(session, "dstain_region", choices = with_placeholder(get_regions_for_donor(input$dstain_donor)))
  })
  
  observeEvent(list(input$dstain_donor, input$dstain_region), {
    req(input$dstain_donor, input$dstain_region, nzchar(input$dstain_donor), nzchar(input$dstain_region))
    updateSelectInput(session, "dstain_stains",
                      choices = get_stain_choices_for_donor_region(input$dstain_donor, input$dstain_region))
  }, ignoreInit = TRUE)
  
  # card is built from the loaded entries, so it appears/disappears together with the grid
  output$dstain_context_card <- renderUI({
    entries <- dstain_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card("Donor" = e1$donor, "Region" = prettify_region(e1$region))
  })
  
  output$dstain_annotation_ui <- renderUI({ render_annotation_master_ui(dstain_entries_rv()) })
  output$dstain_viewer_grid   <- renderUI({ render_viewer_grid_ui(dstain_entries_rv(), label_field = "stain") })
  
  observeEvent(input$dstain_load_btn, {
    req(input$dstain_donor, input$dstain_region, length(input$dstain_stains) > 0,
        nzchar(input$dstain_donor), nzchar(input$dstain_region))
    
    entries <- lapply(input$dstain_stains, function(stain) {
      slot <- donor_manifest[[input$dstain_donor]][[input$dstain_region]][[stain]]
      if (is.null(slot)) return(NULL)
      list(donor = input$dstain_donor, region = input$dstain_region, stain = stain, slot = slot)
    })
    entries <- Filter(Negate(is.null), entries)
    shiny::validate(shiny::need(length(entries) > 0, "No valid images found for this donor + region + stain selection."))
    
    dstain_entries_rv(entries)
    images <- build_images_payload(entries, input$dstain_overlay_opacity, input$dstain_show_overlay)
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images)), once = TRUE)
  })
  
  # ===========================================================================
  # compare donors — constraints: stain + region. varies: donor.
  # ===========================================================================
  
  register_metadata_histograms(output, "sdonor", donor_metadata)
  
  observeEvent(input$sdonor_stain, {
    req(input$sdonor_stain, nzchar(input$sdonor_stain))
    updateSelectInput(session, "sdonor_region", choices = with_placeholder(get_regions_for_stain(input$sdonor_stain)))
  })
  
  # keep the manual donor list free of dead-end choices: only donors that
  # actually have an image for the currently chosen stain+region.
  observeEvent(list(input$sdonor_stain, input$sdonor_region), {
    req(input$sdonor_stain, input$sdonor_region, nzchar(input$sdonor_stain), nzchar(input$sdonor_region))
    valid_donors <- get_donors_with_stain_region(input$sdonor_stain, input$sdonor_region)
    updateSelectInput(session, "sdonor_donors_manual", choices = valid_donors, selected = character(0))
  }, ignoreInit = TRUE)
  
  output$sdonor_context_card <- renderUI({
    entries <- sdonor_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card("Stain" = e1$stain, "Region" = prettify_region(e1$region))
  })
  
  output$sdonor_annotation_ui <- renderUI({ render_annotation_master_ui(sdonor_entries_rv()) })
  output$sdonor_viewer_grid   <- renderUI({ render_viewer_grid_ui(sdonor_entries_rv(), label_field = "donor") })
  
  observeEvent(input$sdonor_load_btn, {
    req(input$sdonor_stain, input$sdonor_region, nzchar(input$sdonor_stain), nzchar(input$sdonor_region))
    
    donors <- switch(input$sdonor_subset_mode,
                     "all"      = donor_choices,
                     "manual"   = input$sdonor_donors_manual,
                     "metadata" = filter_donors_by_metadata(donor_metadata, input, "sdonor")
    )
    
    entries <- lapply(donors, function(donor) {
      slot <- donor_manifest[[donor]][[input$sdonor_region]][[input$sdonor_stain]]
      if (is.null(slot)) return(NULL)  # skip donors without a valid image for this stain/region
      list(donor = donor, region = input$sdonor_region, stain = input$sdonor_stain, slot = slot)
    })
    entries <- Filter(Negate(is.null), entries)
    shiny::validate(shiny::need(length(entries) > 0, "No donors have a valid image for this stain + region selection."))
    
    sdonor_entries_rv(entries)
    images <- build_images_payload(entries, input$sdonor_overlay_opacity, input$sdonor_show_overlay)
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images)), once = TRUE)
  })
  
  # ===========================================================================
  # compare regions — constraints: donor + stain. varies: region (user-picked
  # from whichever regions that donor+stain combination actually has).
  # ===========================================================================
  
  observeEvent(input$sregion_donor, {
    req(input$sregion_donor, nzchar(input$sregion_donor))
    updateSelectInput(session, "sregion_stain",
                      choices = with_placeholder(get_stain_choices_for_donor(input$sregion_donor)))
  })
  
  observeEvent(list(input$sregion_donor, input$sregion_stain), {
    req(input$sregion_donor, input$sregion_stain, nzchar(input$sregion_donor), nzchar(input$sregion_stain))
    updateSelectInput(session, "sregion_regions",
                      choices = get_regions_with_stain_for_donor(input$sregion_donor, input$sregion_stain))
  }, ignoreInit = TRUE)
  
  output$sregion_context_card <- renderUI({
    entries <- sregion_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card("Donor" = e1$donor, "Stain" = e1$stain)
  })
  
  output$sregion_annotation_ui <- renderUI({ render_annotation_master_ui(sregion_entries_rv()) })
  output$sregion_viewer_grid   <- renderUI({ render_viewer_grid_ui(sregion_entries_rv(), label_field = "region") })
  
  observeEvent(input$sregion_load_btn, {
    req(input$sregion_donor, input$sregion_stain, length(input$sregion_regions) > 0,
        nzchar(input$sregion_donor), nzchar(input$sregion_stain))
    
    entries <- lapply(input$sregion_regions, function(region) {
      slot <- donor_manifest[[input$sregion_donor]][[region]][[input$sregion_stain]]
      if (is.null(slot)) return(NULL)
      list(donor = input$sregion_donor, region = region, stain = input$sregion_stain, slot = slot)
    })
    entries <- Filter(Negate(is.null), entries)
    shiny::validate(shiny::need(length(entries) > 0, "No valid images found for the selected regions."))
    
    sregion_entries_rv(entries)
    images <- build_images_payload(entries, input$sregion_overlay_opacity, input$sregion_show_overlay)
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images)), once = TRUE)
  })
  
  # ===========================================================================
  # about page — brief summary only, no full table (per request)
  # ===========================================================================
  
  output$about_metadata_summary <- renderText({
    sprintf("%d donors, %d metadata fields.", nrow(donor_metadata), ncol(donor_metadata) - 1)
  })
  
  output$about_download_metadata <- downloadHandler(
    filename = function() basename(specimen_metadata_csv_path),
    content  = function(file) file.copy(specimen_metadata_csv_path, file)
  )
  
  # ===========================================================================
  # shared: lazy annotation fetch — keyed by containerId, works regardless of
  # which page's images requested it. applies any configured color overrides.
  # ===========================================================================
  
  observeEvent(input$request_annotations, {
    reqs <- input$request_annotations$requests
    req(length(reqs) > 0)
    
    shinycssloaders::showPageSpinner(type = 5)
    on.exit(shinycssloaders::hidePageSpinner(), add = TRUE)
    
    results <- lapply(reqs, function(r) {
      label <- r$label
      polys <- parse_halo_annotations_cached(r$url, r$refWidth)
      polys <- apply_annotation_color_override(label, polys)
      list(containerId = r$containerId, groupIndex = r$groupIndex, polygons = polys)
    })
    
    session$sendCustomMessage("annotationsParsed", list(results = results))
  })
  
  # ===========================================================================
  # switching tabs clears every page back to blank: loaded images, dropdown
  # selections, and subset/overlay settings all reset. this is deliberately
  # global (not "only reset the page being left") so every page is guaranteed
  # blank the moment you land on it, regardless of which tab you came from.
  # ===========================================================================
  
  observeEvent(input$main_nav, {
    home_entries_rv(list())
    updateSelectInput(session, "home_donor", selected = "")
    updateSelectInput(session, "home_region", choices = with_placeholder(character(0)))
    updateSelectInput(session, "home_stain", choices = with_placeholder(character(0)))
    updateCheckboxInput(session, "home_show_overlay", value = FALSE)
    updateSliderInput(session, "home_overlay_opacity", value = 0)
    
    dstain_entries_rv(list())
    updateSelectInput(session, "dstain_donor", selected = "")
    updateSelectInput(session, "dstain_region", choices = with_placeholder(character(0)))
    updateSelectInput(session, "dstain_stains", choices = character(0), selected = character(0))
    updateCheckboxInput(session, "dstain_show_overlay", value = FALSE)
    updateSliderInput(session, "dstain_overlay_opacity", value = 0)
    
    sdonor_entries_rv(list())
    updateSelectInput(session, "sdonor_stain", selected = "")
    updateSelectInput(session, "sdonor_region", choices = with_placeholder(character(0)))
    updateRadioButtons(session, "sdonor_subset_mode", selected = "all")
    updateSelectInput(session, "sdonor_donors_manual", choices = donor_choices, selected = character(0))
    updateCheckboxInput(session, "sdonor_show_overlay", value = FALSE)
    updateSliderInput(session, "sdonor_overlay_opacity", value = 0)
    
    sregion_entries_rv(list())
    updateSelectInput(session, "sregion_donor", selected = "")
    updateSelectInput(session, "sregion_stain", choices = with_placeholder(character(0)))
    updateSelectInput(session, "sregion_regions", choices = character(0), selected = character(0))
    updateCheckboxInput(session, "sregion_show_overlay", value = FALSE)
    updateSliderInput(session, "sregion_overlay_opacity", value = 0)
  }, ignoreInit = TRUE)
}