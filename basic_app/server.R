function(input, output, session) {
  
  output$metadata_table <- renderTable({ donor_metadata })
  
  # --- mode = "donor": donor -> region -> stains, each level scoped exactly ---
  observeEvent(input$cmp_donor, {
    updateSelectInput(session, "cmp_region", choices = get_regions_for_donor(input$cmp_donor))
  }, ignoreNULL = TRUE)
  
  observeEvent(list(input$cmp_donor, input$cmp_region), {
    req(input$cmp_donor, input$cmp_region)
    updateSelectInput(session, "cmp_stains",
                      choices = get_stain_choices_for_donor_region(input$cmp_donor, input$cmp_region))
  }, ignoreInit = TRUE)
  
  # --- Assemble the set of {donor, stain, slot} entries for the current selection ---
  image_entries <- eventReactive(input$load_btn, {
    
    if (input$mode == "donor") {
      req(input$cmp_donor, input$cmp_region, length(input$cmp_stains) > 0)
      entries <- lapply(input$cmp_stains, function(stain) {
        slot <- donor_manifest[[input$cmp_donor]][[input$cmp_region]][[stain]]
        if (is.null(slot)) return(NULL)
        list(donor = input$cmp_donor, region = input$cmp_region, stain = stain, slot = slot)
      })
      
    } else if (input$mode == "stain") {
      req(input$cmp_stain)
      
      donors <- switch(input$donor_subset_mode,
                       "all"      = DONOR_CHOICES,
                       "manual"   = input$cmp_donors_manual,
                       "metadata" = filter_donors_by_metadata(donor_metadata, input)
      )
      
      entries <- lapply(donors, function(donor) {
        slot <- get_stain_slot(donor, input$cmp_stain)
        if (is.null(slot)) return(NULL)
        list(donor = donor, region = NULL, stain = input$cmp_stain, slot = slot)
      })
      
    } else { # mode == "region": every region available for each matching donor+stain
      req(input$cmp_stain_region)
      
      donors <- switch(input$donor_subset_mode_region,
                       "all"      = DONOR_CHOICES,
                       "manual"   = input$cmp_donors_manual_region,
                       "metadata" = filter_donors_by_metadata(donor_metadata, input)
      )
      
      entries <- list()
      for (donor in donors) {
        for (region in get_regions_for_donor(donor)) {
          slot <- donor_manifest[[donor]][[region]][[input$cmp_stain_region]]
          if (!is.null(slot)) {
            entries[[length(entries) + 1]] <- list(donor = donor, region = region,
                                                   stain = input$cmp_stain_region, slot = slot)
          }
        }
      }
    }
    
    Filter(Negate(is.null), entries)
  })
  
  # --- One master, alphabetically-sorted, off-by-default checkbox list -------
  # covering every unique annotation label across ALL currently loaded images.
  output$annotation_master_ui <- renderUI({
    entries <- image_entries()
    if (length(entries) == 0) return(NULL)
    
    all_labels <- character(0)
    for (e in entries) {
      if (length(e$slot$annotation_files) > 0) {
        all_labels <- c(all_labels, vapply(e$slot$annotation_files, annotation_label_from_url, character(1)))
      }
    }
    all_labels <- sort(unique(all_labels))
    
    if (length(all_labels) == 0) {
      return(helpText("No annotation files available for the current selection."))
    }
    
    tagList(
      strong("Annotations (applies to every loaded image with a matching file):"),
      div(
        style = "display:flex; flex-wrap:wrap; gap:14px; margin-top:6px;",
        lapply(all_labels, function(lab) {
          tags$label(
            tags$input(type = "checkbox", onclick = sprintf("toggleAnnotationLabel('%s', this.checked)", lab)),
            paste0(" ", lab)
          )
        })
      )
    )
  })
  
  # --- One viewer div per selected image (no per-image annotation controls
  # anymore — the master checklist above handles all of them). ---
  output$viewer_grid <- renderUI({
    entries <- image_entries()
    if (length(entries) == 0) {
      return(helpText("No images match this selection. Adjust filters and click Load / Compare."))
    }
    
    tagList(fluidRow(lapply(entries, function(e) {
      cid <- paste0("osd-", safe_id(e$donor, e$stain, e$region))
      label <- if (!is.null(e$region)) {
        paste(e$donor, "\u2014", e$region, "\u2014", e$stain)
      } else {
        paste(e$donor, "\u2014", e$stain)
      }
      column(
        width = 6,
        h5(label),
        tags$div(
          id = cid,
          style = "width:100%; height:400px; background:#000; border:1px solid #ccc; position:relative; margin-bottom:6px;"
        ),
        tags$hr()
      )
    })))
  })
  
  # --- Load each image's base DZI + overlay DZI. Annotation XML is NOT
  # parsed here — only label/url/refWidth are sent, kept lazy for speed. ---
  observeEvent(input$load_btn, {
    entries <- image_entries()
    req(length(entries) > 0)
    
    images <- lapply(entries, function(e) {
      ann_files <- e$slot$annotation_files
      if (length(ann_files) > 0) {
        ann_files <- ann_files[order(vapply(ann_files, annotation_label_from_url, character(1)))]
      }
      
      groups <- lapply(ann_files, function(url) {
        list(label = annotation_label_from_url(url), url = url, refWidth = e$slot$svs_width)
      })
      
      list(
        id               = safe_id(e$donor, e$stain, e$region),
        dziUrl           = e$slot$primary_dzi,
        overlayUrl       = e$slot$annotation_dzi %||% "",
        overlayOpacity   = input$overlay_opacity,
        annotationGroups = groups
      )
    })
    
    # Defer sending until after the reactive flush (which includes the UI
    # updates above) so the target <div>s already exist in the DOM.
    session$onFlushed(function() {
      session$sendCustomMessage("loadImages", list(images = images))
    }, once = TRUE)
  })
  
  # --- Lazy annotation fetch: the client only asks for files it doesn't
  # already have cached, batched into one request across all viewers. ---
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