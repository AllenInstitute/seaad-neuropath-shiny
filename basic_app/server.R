function(input, output, session) {
  
  output$metadata_table <- renderTable({ donor_metadata })
  
  # --- Donor picked (mode = "donor") -> populate that donor's stain choices ---
  observeEvent(input$cmp_donor, {
    stains <- get_stain_choices_for_donor(input$cmp_donor)
    updateSelectInput(session, "cmp_stains", choices = stains)
  }, ignoreNULL = TRUE)
  
  # --- Assemble the set of {donor, stain, slot} entries for the current selection ---
  image_entries <- eventReactive(input$load_btn, {
    
    if (input$mode == "donor") {
      req(input$cmp_donor, length(input$cmp_stains) > 0)
      entries <- lapply(input$cmp_stains, function(stain) {
        slot <- get_stain_slot(input$cmp_donor, stain)
        if (is.null(slot)) return(NULL)
        list(donor = input$cmp_donor, stain = stain, slot = slot)
      })
      
    } else {
      req(input$cmp_stain)
      
      donors <- switch(input$donor_subset_mode,
                       "all"      = DONOR_CHOICES,
                       "manual"   = input$cmp_donors_manual,
                       "metadata" = filter_donors_by_metadata(
                         donor_metadata,
                         age_range = input$meta_age_range,
                         cerad_min = input$meta_cerad_min,
                         thal_min  = input$meta_thal_min,
                         braak_min = input$meta_braak_min
                       )
      )
      
      entries <- lapply(donors, function(donor) {
        slot <- get_stain_slot(donor, input$cmp_stain)
        if (is.null(slot)) return(NULL)
        list(donor = donor, stain = input$cmp_stain, slot = slot)
      })
    }
    
    Filter(Negate(is.null), entries)
  })
  
  # --- Render one viewer div + annotation checkbox per selected image ---
  output$viewer_grid <- renderUI({
    entries <- image_entries()
    if (length(entries) == 0) {
      return(helpText("No images match this selection. Adjust filters and click Load / Compare."))
    }
    
    tagList(fluidRow(lapply(entries, function(e) {
      cid <- paste0("osd-", safe_id(e$donor, e$stain))
      column(
        width = 6,
        h5(paste(e$donor, "\u2014", e$stain)),
        tags$div(
          id = cid,
          style = "width:100%; height:400px; background:#000; border:1px solid #ccc; position:relative; margin-bottom:6px;"
        ),
        tags$label(
          tags$input(
            type = "checkbox", checked = "checked",
            onclick = sprintf("toggleAnnotationsFor('%s', this.checked)", cid)
          ),
          " Show annotations"
        ),
        tags$hr()
      )
    })))
  })
  
  # --- Once the grid above has actually rendered, load each viewer's image ---
  observeEvent(input$load_btn, {
    entries <- image_entries()
    req(length(entries) > 0)
    
    images <- lapply(entries, function(e) {
      polys <- if (length(e$slot$annotation_files) > 0 && !is.na(e$slot$svs_width)) {
        parse_multiple_annotations(e$slot$annotation_files, e$slot$svs_width)
      } else {
        list()
      }
      
      list(
        id             = safe_id(e$donor, e$stain),
        dziUrl         = e$slot$primary_dzi,
        overlayUrl     = e$slot$annotation_dzi %||% "",
        overlayOpacity = input$overlay_opacity,
        polygons       = polys
      )
    })
    
    # Defer sending until after the reactive flush (which includes the UI
    # update above) so the target <div>s already exist in the DOM.
    session$onFlushed(function() {
      session$sendCustomMessage("loadImages", list(images = images))
    }, once = TRUE)
  })
}