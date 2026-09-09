function(input, output, session) {
  
  # ===========================================================================
  # HOME — single donor/region/stain, mask overlay explicitly toggleable
  # ===========================================================================
  
  observeEvent(input$home_donor, {
    updateSelectInput(session, "home_region", choices = get_regions_for_donor(input$home_donor))
  }, ignoreNULL = TRUE)
  
  observeEvent(list(input$home_donor, input$home_region), {
    req(input$home_donor, input$home_region)
    updateSelectInput(session, "home_stain",
                      choices = get_stain_choices_for_donor_region(input$home_donor, input$home_region))
  }, ignoreInit = TRUE)
  
  home_entries <- eventReactive(input$home_load_btn, {
    req(input$home_donor, input$home_region, input$home_stain)
    slot <- donor_manifest[[input$home_donor]][[input$home_region]][[input$home_stain]]
    req(slot)
    list(list(donor = input$home_donor, region = input$home_region, stain = input$home_stain, slot = slot))
  })
  
  output$home_annotation_ui <- renderUI({ render_annotation_master_ui(home_entries()) })
  output$home_viewer_grid   <- renderUI({ render_viewer_grid_ui(home_entries()) })
  
  observeEvent(input$home_load_btn, {
    entries <- home_entries()
    req(length(entries) > 0)
    
    images <- build_images_payload(entries, input$home_overlay_opacity)
    if (!isTRUE(input$home_show_mask)) images[[1]]$overlayUrl <- ""
    
    session$onFlushed(function() {
      session$sendCustomMessage("loadImages", list(images = images))
    }, once = TRUE)
  })
  
  # ===========================================================================
  # COMPARE STAINS FOR ONE DONOR (scoped to one region)
  # ===========================================================================
  
  observeEvent(input$dstain_donor, {
    updateSelectInput(session, "dstain_region", choices = get_regions_for_donor(input$dstain_donor))
  }, ignoreNULL = TRUE)
  
  observeEvent(list(input$dstain_donor, input$dstain_region), {
    req(input$dstain_donor, input$dstain_region)
    updateSelectInput(session, "dstain_stains",
                      choices = get_stain_choices_for_donor_region(input$dstain_donor, input$dstain_region))
  }, ignoreInit = TRUE)
  
  dstain_entries <- eventReactive(input$dstain_load_btn, {
    req(input$dstain_donor, input$dstain_region, length(input$dstain_stains) > 0)
    entries <- lapply(input$dstain_stains, function(stain) {
      slot <- donor_manifest[[input$dstain_donor]][[input$dstain_region]][[stain]]
      if (is.null(slot)) return(NULL)
      list(donor = input$dstain_donor, region = input$dstain_region, stain = stain, slot = slot)
    })
    Filter(Negate(is.null), entries)
  })
  
  output$dstain_annotation_ui <- renderUI({ render_annotation_master_ui(dstain_entries()) })
  output$dstain_viewer_grid   <- renderUI({ render_viewer_grid_ui(dstain_entries()) })
  
  observeEvent(input$dstain_load_btn, {
    entries <- dstain_entries()
    req(length(entries) > 0)
    images <- build_images_payload(entries, input$dstain_overlay_opacity)
    session$onFlushed(function() {
      session$sendCustomMessage("loadImages", list(images = images))
    }, once = TRUE)
  })
  
  # ===========================================================================
  # COMPARE ONE STAIN ACROSS DONORS
  # ===========================================================================
  
  register_metadata_histograms(output, "sdonor", donor_metadata)
  
  sdonor_entries <- eventReactive(input$sdonor_load_btn, {
    req(input$sdonor_stain)
    
    donors <- switch(input$sdonor_subset_mode,
                     "all"      = DONOR_CHOICES,
                     "manual"   = input$sdonor_donors_manual,
                     "metadata" = filter_donors_by_metadata(donor_metadata, input, "sdonor")
    )
    
    entries <- lapply(donors, function(donor) {
      slot <- get_stain_slot(donor, input$sdonor_stain)
      if (is.null(slot)) return(NULL)
      list(donor = donor, region = NULL, stain = input$sdonor_stain, slot = slot)
    })
    Filter(Negate(is.null), entries)
  })
  
  output$sdonor_annotation_ui <- renderUI({ render_annotation_master_ui(sdonor_entries()) })
  output$sdonor_viewer_grid   <- renderUI({ render_viewer_grid_ui(sdonor_entries()) })
  
  observeEvent(input$sdonor_load_btn, {
    entries <- sdonor_entries()
    req(length(entries) > 0)
    images <- build_images_payload(entries, input$sdonor_overlay_opacity)
    session$onFlushed(function() {
      session$sendCustomMessage("loadImages", list(images = images))
    }, once = TRUE)
  })
  
  # ===========================================================================
  # COMPARE ONE STAIN ACROSS REGIONS
  # ===========================================================================
  
  register_metadata_histograms(output, "sregion", donor_metadata)
  
  sregion_entries <- eventReactive(input$sregion_load_btn, {
    req(input$sregion_stain)
    
    donors <- switch(input$sregion_subset_mode,
                     "all"      = DONOR_CHOICES,
                     "manual"   = input$sregion_donors_manual,
                     "metadata" = filter_donors_by_metadata(donor_metadata, input, "sregion")
    )
    
    entries <- list()
    for (donor in donors) {
      for (region in get_regions_for_donor(donor)) {
        slot <- donor_manifest[[donor]][[region]][[input$sregion_stain]]
        if (!is.null(slot)) {
          entries[[length(entries) + 1]] <- list(donor = donor, region = region,
                                                 stain = input$sregion_stain, slot = slot)
        }
      }
    }
    entries
  })
  
  output$sregion_annotation_ui <- renderUI({ render_annotation_master_ui(sregion_entries()) })
  output$sregion_viewer_grid   <- renderUI({ render_viewer_grid_ui(sregion_entries()) })
  
  observeEvent(input$sregion_load_btn, {
    entries <- sregion_entries()
    req(length(entries) > 0)
    images <- build_images_payload(entries, input$sregion_overlay_opacity)
    session$onFlushed(function() {
      session$sendCustomMessage("loadImages", list(images = images))
    }, once = TRUE)
  })
  
  # ===========================================================================
  # SHARED: lazy annotation fetch — keyed by containerId, so it works
  # regardless of which page's images requested it.
  # ===========================================================================
  
  observeEvent(input$request_annotations, {
    reqs <- input$request_annotations$requests
    req(length(reqs) > 0)
    
    results <- lapply(reqs, function(r) {
      list(
        containerId = r$containerId,
        groupIndex  = r$groupIndex,
        polygons    = parse_halo_annotations_cached(r$url, r$refWidth)
      )
    })
    
    session$sendCustomMessage("annotationsParsed", list(results = results))
  })
}