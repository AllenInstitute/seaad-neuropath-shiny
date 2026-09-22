function(input, output, session) {
  
  # scroll-to-top arrow: left side on Filter Donors (per request), right
  # side on every other page.
  output$scroll_top_arrow_ui <- renderUI({
    side <- if (identical(input$main_nav, filter_donors_tab_name)) "left:18px;" else "right:18px;"
    tags$a(
      href = "javascript:void(0)",
      onclick = "window.scrollTo({top: 0, behavior: 'smooth'})",
      title = tt_back_to_top,
      style = paste(
        "position:fixed;", side, "bottom:24px; z-index:1050;",
        "width:42px; height:42px; border-radius:50%;",
        sprintf("background:%s; color:#fff; text-decoration:none;", metadata_chart_color),
        "display:flex; align-items:center; justify-content:center;",
        "font-size:20px; line-height:1; box-shadow:0 2px 6px rgba(0,0,0,0.3);"
      ),
      HTML("&uarr;")
    )
  })
  
  # scratchpad's only server-side logic — Clear just blanks the textarea.
  # Its value otherwise persists across tab switches for free (see the
  # comment in ui.r): nothing here resets it on tab change, deliberately.
  observeEvent(input$scratchpad_reset_btn, {
    updateTextAreaInput(session, "scratchpad_notes", value = "")
  })
  
  # Reset image zoom — purely a client-side action (resetting OpenSeadragon
  # viewports), so all four pages' buttons just trigger the same JS message;
  # see the 'resetZoom' handler in ui.r, which resets every currently
  # tracked viewer regardless of which page's button was clicked. Harmless
  # for viewers on a page that isn't currently visible.
  observeEvent(input$home_reset_zoom_btn,    { session$sendCustomMessage("resetZoom", list()) })
  observeEvent(input$dstain_reset_zoom_btn,  { session$sendCustomMessage("resetZoom", list()) })
  observeEvent(input$sdonor_reset_zoom_btn,  { session$sendCustomMessage("resetZoom", list()) })
  observeEvent(input$sregion_reset_zoom_btn, { session$sendCustomMessage("resetZoom", list()) })
  
  # small styled card summarizing a comparison page's two FIXED (non-varying)
  # fields. built from the loaded entries (not the live dropdown values) so it
  # only appears once Load has actually been clicked — see req(length(...)>0).
  constraint_card <- function(...) {
    pairs <- list(...)
    div(
      style = sprintf(
        "padding:10px 16px; margin-bottom:12px; background:%s; border-left:4px solid %s; border-radius:4px;",
        context_card_bg_color, action_button_color
      ),
      tagList(lapply(names(pairs), function(k) {
        tags$span(style = "margin-right:24px;", tags$strong(paste0(k, ": ")), pairs[[k]])
      }))
    )
  }
  
  # the "Reset image zoom" button, identical across all four pages apart
  # from its id.
  reset_zoom_btn_ui <- function(id) {
    actionButton(id, lbl_reset_image_zoom,
                 style = sprintf("background-color:%s; border-color:%s; color:#fff; font-size:16px;", action_button_color, action_button_color))
  }
  
  # shared Load/Compare body: fetches annotations (with a live progress
  # bar) and builds the images payload, then sends it to the browser.
  # syncCheckboxId names the page's sync-zoom checkbox, checked LIVE by
  # the JS on every pan/zoom event (not baked in at load time), so
  # unchecking it after images are loaded takes effect immediately.
  load_images_with_progress <- function(entries, overlay_opacity, show_overlay, sync_checkbox_id = NULL) {
    images <- shiny::withProgress(message = "Loading images...", value = 0, {
      build_images_payload(entries, overlay_opacity, show_overlay,
                           progress_callback = function(done, total) {
                             shiny::setProgress(value = done / total, detail = sprintf("Fetching annotations: %d of %d", done, total))
                           })
    })
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images, syncCheckboxId = sync_checkbox_id)), once = TRUE)
  }
  
  # each page's "currently loaded" state lives in a reactiveVal (not a plain
  # eventReactive) so the nav-change observer at the bottom of this file can
  # explicitly clear it back to empty when the user switches tabs.
  home_entries_rv    <- reactiveVal(list())
  dstain_entries_rv  <- reactiveVal(list())
  sdonor_entries_rv  <- reactiveVal(list())
  sregion_entries_rv <- reactiveVal(list())
  
  
  # ===========================================================================
  # shinyjs: keep each page's Load/Compare button disabled until its required
  # selections are actually made, so it's simply not possible to click Load
  # with an incomplete selection instead of failing after the fact.
  # ===========================================================================
  
  observe({
    valid <- is_selected(input$home_donor) && is_selected(input$home_region) && is_selected(input$home_stain)
    shinyjs::toggleState("home_load_btn", condition = valid)
  })
  
  observe({
    valid <- is_selected(input$dstain_donor) && is_selected(input$dstain_region) &&
      length(input$dstain_stains) > 0
    shinyjs::toggleState("dstain_load_btn", condition = valid)
  })
  
  observe({
    valid <- is_selected(input$sdonor_stain) && is_selected(input$sdonor_region)
    if (identical(input$sdonor_subset_mode, "manual")) {
      valid <- valid && length(input$sdonor_donors_manual) > 0
    }
    shinyjs::toggleState("sdonor_load_btn", condition = valid)
  })
  
  observe({
    valid <- is_selected(input$sregion_donor) && is_selected(input$sregion_stain) &&
      length(input$sregion_regions) > 0
    shinyjs::toggleState("sregion_load_btn", condition = valid)
  })
  
  # ===========================================================================
  # home — single donor/region/stain
  # ===========================================================================
  
  register_metadata_histograms(output, "home", donor_metadata)
  
  # built ONLY once the checkbox is actually turned on — this accordion
  # (especially its QNP branch) can be large, and conditionalPanel alone
  # doesn't defer rendering, it just CSS-hides already-shipped HTML. Building
  # it eagerly for all four pages at startup was overwhelming the browser.
  output$home_metadata_accordion_ui <- renderUI({
    req(isTRUE(input$home_filter_donors))
    precomputed_metadata_accordion_ui[["home"]]
  })
  
  # narrows the Donor dropdown to only donors matching the metadata filters
  # when the toggle is on; reads input$home_filter_donors AND (indirectly,
  # inside filter_donors_by_metadata) every home_meta_* input, so this
  # re-runs automatically whenever any of them change.
  observe({
    filtered <- if (isTRUE(input$home_filter_donors)) {
      intersect(donor_choices, filter_donors_by_metadata(donor_metadata, input, "home"))
    } else {
      donor_choices
    }
    updateSelectInput(session, "home_donor", label = dropdown_label("Donor", filtered), choices = with_placeholder(filtered))
  })
  
  observeEvent(input$home_donor, {
    req(input$home_donor, nzchar(input$home_donor))
    home_regions <- get_regions_for_donor(input$home_donor)
    updateSelectInput(session, "home_region", label = dropdown_label("Region", home_regions), choices = with_placeholder(home_regions))
  })
  
  observeEvent(list(input$home_donor, input$home_region), {
    req(input$home_donor, input$home_region, nzchar(input$home_donor), nzchar(input$home_region))
    home_stains <- get_stain_choices_for_donor_region(input$home_donor, input$home_region)
    updateSelectInput(session, "home_stain", label = dropdown_label("Stain", home_stains), choices = with_placeholder(home_stains))
  }, ignoreInit = TRUE)
  
  output$home_annotation_ui <- renderUI({ render_annotation_master_ui(home_entries_rv(), id_prefix = "home") })
  output$home_viewer_grid   <- renderUI({ render_viewer_grid_ui(home_entries_rv(), label_field = "stain") })
  
  output$home_context_card <- renderUI({
    entries <- home_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card(
      "donor" = shiny::tagList(e1$donor, donor_info_trigger(e1$donor, e1$region, e1$stain)),
      "region" = smart_lowercase(prettify_region(e1$region)),
      "stain" = smart_lowercase(e1$stain)
    )
  })
  
  output$home_reset_zoom_btn_ui <- renderUI({
    req(length(home_entries_rv()) > 0)
    reset_zoom_btn_ui("home_reset_zoom_btn")
  })
  
  observeEvent(input$home_load_btn, {
    req(input$home_donor, input$home_region, input$home_stain,
        nzchar(input$home_donor), nzchar(input$home_region), nzchar(input$home_stain))
    slot <- donor_manifest[[input$home_donor]][[input$home_region]][[input$home_stain]]
    shiny::validate(shiny::need(!is.null(slot), "No image found for this donor/region/stain combination."))
    
    entries <- list(list(donor = input$home_donor, region = input$home_region, stain = input$home_stain, slot = slot))
    home_entries_rv(entries)
    load_images_with_progress(entries, input$home_overlay_opacity, input$home_show_overlay)
  })
  
  # ===========================================================================
  # compare stains — constraints: donor + region. varies: stain.
  # ===========================================================================
  
  register_metadata_histograms(output, "dstain", donor_metadata)
  
  output$dstain_metadata_accordion_ui <- renderUI({
    req(isTRUE(input$dstain_filter_donors))
    precomputed_metadata_accordion_ui[["dstain"]]
  })
  
  observe({
    filtered <- if (isTRUE(input$dstain_filter_donors)) {
      intersect(donor_choices, filter_donors_by_metadata(donor_metadata, input, "dstain"))
    } else {
      donor_choices
    }
    updateSelectInput(session, "dstain_donor", label = dropdown_label("Donor", filtered), choices = with_placeholder(filtered))
  })
  
  observeEvent(input$dstain_donor, {
    req(input$dstain_donor, nzchar(input$dstain_donor))
    dstain_regions <- get_regions_for_donor(input$dstain_donor)
    updateSelectInput(session, "dstain_region", label = dropdown_label("Region", dstain_regions), choices = with_placeholder(dstain_regions))
  })
  
  observeEvent(list(input$dstain_donor, input$dstain_region), {
    req(input$dstain_donor, input$dstain_region, nzchar(input$dstain_donor), nzchar(input$dstain_region))
    dstain_stain_choices <- get_stain_choices_for_donor_region(input$dstain_donor, input$dstain_region)
    updateSelectInput(session, "dstain_stains", label = dropdown_label("Stains to compare", dstain_stain_choices), choices = dstain_stain_choices)
  }, ignoreInit = TRUE)
  
  # card is built from the loaded entries, so it appears/disappears together with the grid
  output$dstain_context_card <- renderUI({
    entries <- dstain_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card("donor" = shiny::tagList(e1$donor, donor_info_trigger(e1$donor, mode = "demo", icon = "person-vcard")), "region" = smart_lowercase(prettify_region(e1$region)))
  })
  
  output$dstain_reset_zoom_btn_ui <- renderUI({
    req(length(dstain_entries_rv()) > 0)
    reset_zoom_btn_ui("dstain_reset_zoom_btn")
  })
  
  output$dstain_annotation_ui <- renderUI({ render_annotation_master_ui(dstain_entries_rv(), id_prefix = "dstain", varying_field = "stain") })
  output$dstain_viewer_grid   <- renderUI({ render_viewer_grid_ui(dstain_entries_rv(), label_field = "stain", donor_info_style = "qnp_only", qnp_icon = "file-earmark-bar-graph", qnp_all_fields = TRUE) })
  
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
    
    entries <- sort_entries_by(entries, "stain")
    dstain_entries_rv(entries)
    load_images_with_progress(entries, input$dstain_overlay_opacity, input$dstain_show_overlay, "dstain_sync_zoom")
  })
  
  # ===========================================================================
  # compare donors — constraints: stain + region. varies: donor.
  # ===========================================================================
  
  register_metadata_histograms(output, "sdonor", donor_metadata)
  
  output$sdonor_metadata_accordion_ui <- renderUI({
    req(identical(input$sdonor_subset_mode, "metadata"))
    precomputed_metadata_accordion_ui[["sdonor"]]
  })
  
  observeEvent(input$sdonor_stain, {
    req(input$sdonor_stain, nzchar(input$sdonor_stain))
    sdonor_regions <- get_regions_for_stain(input$sdonor_stain)
    updateSelectInput(session, "sdonor_region", label = dropdown_label("Region", sdonor_regions), choices = with_placeholder(sdonor_regions))
  })
  
  # keep the manual donor list free of dead-end choices: only donors that
  # actually have an image for the currently chosen stain+region.
  observeEvent(list(input$sdonor_stain, input$sdonor_region), {
    req(input$sdonor_stain, input$sdonor_region, nzchar(input$sdonor_stain), nzchar(input$sdonor_region))
    valid_donors <- get_donors_with_stain_region(input$sdonor_stain, input$sdonor_region)
    updateSelectInput(session, "sdonor_donors_manual", choices = valid_donors, selected = character(0))
  }, ignoreInit = TRUE)
  
  # the manual picker's label lives as separate static text (its own
  # selectizeInput label is NULL — see ui.r), so the count has to update
  # this output instead of updateSelectInput()'s own label argument.
  output$sdonor_manual_label_ui <- renderUI({
    valid_donors <- if (is_selected(input$sdonor_stain) && is_selected(input$sdonor_region)) {
      get_donors_with_stain_region(input$sdonor_stain, input$sdonor_region)
    } else {
      character(0)
    }
    tags$strong(dropdown_label("Donors to compare", valid_donors))
  })
  
  output$sdonor_manual_count_ui <- renderUI({
    tags$div(sprintf("%d/%d selected", length(input$sdonor_donors_manual), donor_compare_cap), style = "margin-top:4px; font-size:0.9em; color:#666;")
  })
  
  output$sdonor_context_card <- renderUI({
    entries <- sdonor_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card("stain" = smart_lowercase(e1$stain), "region" = smart_lowercase(prettify_region(e1$region)))
  })
  
  output$sdonor_reset_zoom_btn_ui <- renderUI({
    req(length(sdonor_entries_rv()) > 0)
    reset_zoom_btn_ui("sdonor_reset_zoom_btn")
  })
  
  output$sdonor_annotation_ui <- renderUI({ render_annotation_master_ui(sdonor_entries_rv(), id_prefix = "sdonor", varying_field = "donor") })
  output$sdonor_viewer_grid   <- renderUI({ render_viewer_grid_ui(sdonor_entries_rv(), label_field = "donor", donor_info_style = "combined") })
  
  observeEvent(input$sdonor_load_btn, {
    req(input$sdonor_stain, input$sdonor_region, nzchar(input$sdonor_stain), nzchar(input$sdonor_region))
    
    donors <- switch(input$sdonor_subset_mode,
                     "random" = {
                       # sample from donors that actually HAVE a valid image for this
                       # stain+region — sampling from every donor blindly could pick
                       # ones with no matching slot, silently ending up with fewer
                       # than requested once those get filtered out below.
                       valid_for_selection <- get_donors_with_stain_region(input$sdonor_stain, input$sdonor_region)
                       n_wanted <- input$sdonor_random_n
                       if (is.null(n_wanted) || is.na(n_wanted)) n_wanted <- donor_compare_min
                       n_wanted <- max(donor_compare_min, min(donor_compare_cap, round(n_wanted)))
                       sample(valid_for_selection, size = min(n_wanted, length(valid_for_selection)))
                     },
                     "manual"   = input$sdonor_donors_manual,
                     "metadata" = {
                       matched <- filter_donors_by_metadata(donor_metadata, input, "sdonor")
                       if (length(matched) > donor_compare_cap) {
                         showNotification(
                           sprintf("%d donors matched — showing a random %d (capped).", length(matched), donor_compare_cap),
                           type = "warning"
                         )
                         matched <- sample(matched, donor_compare_cap)
                       }
                       matched
                     }
    )
    
    entries <- lapply(donors, function(donor) {
      slot <- donor_manifest[[donor]][[input$sdonor_region]][[input$sdonor_stain]]
      if (is.null(slot)) return(NULL)  # skip donors without a valid image for this stain/region
      list(donor = donor, region = input$sdonor_region, stain = input$sdonor_stain, slot = slot)
    })
    entries <- Filter(Negate(is.null), entries)
    shiny::validate(shiny::need(length(entries) > 0, "No donors have a valid image for this stain + region selection."))
    
    entries <- sort_entries_by(entries, "donor")
    sdonor_entries_rv(entries)
    load_images_with_progress(entries, input$sdonor_overlay_opacity, input$sdonor_show_overlay, "sdonor_sync_zoom")
  })
  
  # ===========================================================================
  # compare regions — constraints: donor + stain. varies: region (user-picked
  # from whichever regions that donor+stain combination actually has).
  # ===========================================================================
  
  register_metadata_histograms(output, "sregion", donor_metadata)
  
  output$sregion_metadata_accordion_ui <- renderUI({
    req(isTRUE(input$sregion_filter_donors))
    precomputed_metadata_accordion_ui[["sregion"]]
  })
  observe({
    filtered <- if (isTRUE(input$sregion_filter_donors)) {
      intersect(donor_choices, filter_donors_by_metadata(donor_metadata, input, "sregion"))
    } else {
      donor_choices
    }
    updateSelectInput(session, "sregion_donor", label = dropdown_label("Donor", filtered), choices = with_placeholder(filtered))
  })
  
  observeEvent(input$sregion_donor, {
    req(input$sregion_donor, nzchar(input$sregion_donor))
    sregion_stains <- get_stain_choices_for_donor(input$sregion_donor)
    updateSelectInput(session, "sregion_stain", label = dropdown_label("Stain", sregion_stains), choices = with_placeholder(sregion_stains))
  })
  
  observeEvent(list(input$sregion_donor, input$sregion_stain), {
    req(input$sregion_donor, input$sregion_stain, nzchar(input$sregion_donor), nzchar(input$sregion_stain))
    sregion_region_choices <- get_regions_with_stain_for_donor(input$sregion_donor, input$sregion_stain)
    updateSelectInput(session, "sregion_regions", label = dropdown_label("Regions to compare", sregion_region_choices), choices = sregion_region_choices)
  }, ignoreInit = TRUE)
  
  output$sregion_context_card <- renderUI({
    entries <- sregion_entries_rv()
    req(length(entries) > 0)
    e1 <- entries[[1]]
    constraint_card("donor" = shiny::tagList(e1$donor, donor_info_trigger(e1$donor, NULL, e1$stain)), "stain" = smart_lowercase(e1$stain))
  })
  
  output$sregion_reset_zoom_btn_ui <- renderUI({
    req(length(sregion_entries_rv()) > 0)
    reset_zoom_btn_ui("sregion_reset_zoom_btn")
  })
  
  output$sregion_annotation_ui <- renderUI({ render_annotation_master_ui(sregion_entries_rv(), id_prefix = "sregion", varying_field = "region") })
  output$sregion_viewer_grid   <- renderUI({ render_viewer_grid_ui(sregion_entries_rv(), label_field = "region", donor_info_style = "qnp_only", qnp_icon = "file-earmark-bar-graph") })
  
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
    
    entries <- sort_entries_by(entries, "region")
    sregion_entries_rv(entries)
    load_images_with_progress(entries, input$sregion_overlay_opacity, input$sregion_show_overlay, "sregion_sync_zoom")
  })
  
  # ===========================================================================
  # filter donors (qnp + metadata) — filters on top, table below. Range
  # sliders only count as active once moved off their first observed value,
  # tracked in this per-session baseline store (see the big comment above
  # iddonors_filter_active() in functions.r for why inferring it from the
  # data range never worked reliably).
  # ===========================================================================
  
  iddonors_baselines <- new_iddonors_baseline_store()
  
  register_identify_donors_histograms(output)
  register_identify_donors_qnp(output, input)
  
  observeEvent(input$iddonors_reset_btn, {
    reset_identify_donors_filters(input, session, iddonors_baselines)
    bslib::accordion_panel_close("iddonors_meta_accordion", TRUE, session = session)
    bslib::accordion_panel_close("iddonors_qnp_accordion", TRUE, session = session)
    showNotification(msg_filters_reset, type = "message")
  })
  
  output$identify_donors_metadata_accordion_ui <- renderUI({
    req(identical(input$main_nav, filter_donors_tab_name))
    build_identify_donors_metadata_accordion()
  })
  
  output$identify_donors_qnp_accordion_ui <- renderUI({
    req(identical(input$main_nav, filter_donors_tab_name))
    build_identify_donors_qnp_accordion()
  })
  
  identify_matching_donors <- reactive({
    req(identical(input$main_nav, filter_donors_tab_name))
    filter_donors_identify_page(input, iddonors_baselines)
  })
  
  output$identify_donors_count_text <- renderText({
    sprintf("%d of %d donors match.", length(identify_matching_donors()), length(donor_choices))
  })
  
  # a plain count can go quiet at 0 without the user noticing WHY (which
  # slider did it) — this makes the "you've filtered everyone out" state
  # impossible to miss, without needing the filters themselves to become
  # reactive to each other (see the design note in the chat reply for why
  # that's a much bigger, riskier change than this warning).
  output$identify_donors_zero_warning_ui <- renderUI({
    req(length(identify_matching_donors()) == 0)
    div(class = "alert alert-warning", style = "margin-top:8px;",
        "No donors match the current combination of filters. Try widening a slider or clearing a checkbox.")
  })
  
  output$identify_donors_table_ui <- renderUI({
    render_identify_donors_table(identify_matching_donors())
  })
  
  output$iddonors_download_btn <- downloadHandler(
    filename = function() sprintf("filtered_donors_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      # export includes every QNP measure in wide form (one column per
      # region/subregion/measure) — deliberately richer than the on-page
      # table, which stays demographic + clinical only.
      utils::write.csv(identify_donors_export_data(identify_matching_donors()), file, row.names = FALSE)
    }
  )
  
  # shared handler for every page's donor-info icon (donor_info_trigger(),
  # functions.r) — one modal for Home/Compare Stains/Compare Regions'
  # context cards and Compare Donors' per-image icons. region/stain arrive
  # as empty strings when donor_info_trigger() was given NULL for either
  # (Compare Regions, which has no single fixed region).
  observeEvent(input$donor_info_click, {
    info <- input$donor_info_click
    req(is_selected(info$donor))
    region <- if (!is.null(info$region) && nzchar(info$region)) info$region else NULL
    stain  <- if (!is.null(info$stain)  && nzchar(info$stain))  info$stain  else NULL
    mode <- info$mode %||% "combined"
    all_fields <- isTRUE(info$allFields)
    
    content <- switch(mode,
                      "demo" = render_donor_demo_clinical(info$donor),
                      "qnp"  = render_donor_metadata_qnp_block(info$donor, region, stain, standalone = TRUE, all_fields = all_fields),
                      render_donor_metadata_list(info$donor, image_region = region, image_stain = stain)
    )
    title_suffix <- switch(mode, "demo" = " — demographic & clinical", "qnp" = " — QNP", "")
    
    showModal(modalDialog(
      title = paste0("Donor ", info$donor, title_suffix),
      content,
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close")
    ))
  })
  
  # tracks which donor's popup is currently open — needed now that the
  # QNP section inside it is region-dependent and reactive (a radio
  # button INSIDE the modal), not a one-shot static render.
  iddonors_popup_donor <- reactiveVal(NULL)
  
  # clicking a donor's name in the table opens a popup with their
  # Demographic/Clinical metadata plus a region-by-region QNP browser.
  observeEvent(input$iddonors_clicked_donor, {
    donor_id <- input$iddonors_clicked_donor
    req(is_selected(donor_id))
    iddonors_popup_donor(donor_id)
    showModal(modalDialog(
      title = donor_id,
      render_donor_all_metadata(donor_id),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close")
    ))
  })
  
  # the region-dependent half of the popup — rebuilt whenever the region,
  # stain, or view-mode control inside the (currently open) modal changes.
  # nothing displays until both region and stain are explicitly picked —
  # neither one defaults to a value, so there's no "default view".
  output$iddonors_popup_qnp_ui <- renderUI({
    donor_id <- iddonors_popup_donor()
    req(is_selected(donor_id))
    if (!is_selected(input$iddonors_popup_region_sel) || !is_selected(input$iddonors_popup_stain_sel)) {
      return(shiny::helpText("Select a region and stain above to see QNP data."))
    }
    view_mode <- input$iddonors_popup_qnp_view_mode %||% "layers"
    render_donor_qnp_region_detail(donor_id, input$iddonors_popup_region_sel, view_mode, stain = input$iddonors_popup_stain_sel)
  })
  
  # kept only as a hidden text source for the page's Copy button.
  # the copy text is baked directly into the button's data attribute at
  # render time — simpler than reading a hidden DOM element's text at
  # click time, and removes the possibility of that element not yet being
  # populated when clicked.
  output$iddonors_copy_list_btn_ui <- renderUI({
    list_text <- paste(sort(identify_matching_donors()), collapse = ", ")
    tags$button(
      bsicons::bs_icon("copy"), "Copy donor list", class = "btn",
      style = sprintf("background-color:transparent; border:none; color:%s;", metadata_chart_color),
      `data-copy-text` = list_text,
      onclick = "copyTextRobust(this.getAttribute('data-copy-text'), this)"
    )
  })
  
  # ===========================================================================
  # qnp graphs — boxplots/scatterplots of demographic+clinical fields
  # against QNP measures. See build_qplot_* / build_qnp_graph_data() in
  # functions.r for the actual data prep and ggplot2 building; this section
  # is just the reactive wiring around them.
  # ===========================================================================
  
  output$qplot_region_picker_ui <- renderUI({ build_qplot_region_picker() })
  
  output$qplot_region_ui <- renderUI({
    req(is_selected(input$qplot_grouping_sel))
    regions <- get_qnp_regions_in_grouping(input$qplot_grouping_sel)
    if (!qnp_grouping_has_multiple_regions(input$qplot_grouping_sel)) {
      # nothing to disambiguate — still a real selectInput (hidden), so
      # downstream code reading qplot_region_sel works identically either way.
      return(tags$div(
        style = "display:none;",
        selectInput("qplot_region_sel", NULL, choices = regions, selected = if (length(regions) == 1) regions[1] else NULL)
      ))
    }
    selectInput("qplot_region_sel", "Region", choices = with_placeholder(regions))
  })
  
  # whether the x/y axis pickers should be usable at all — always true in
  # "regions" (facet) mode, but in "single" mode not until a region is
  # actually chosen (picking axes before that would let the y-axis
  # picker's own region-filtered choices — see qplot_y_fields_ui below —
  # change out from under a selection already made).
  qplot_region_ready <- reactive({
    if (!identical(input$qplot_compare_mode, "single")) return(TRUE)
    is_selected(input$qplot_grouping_sel) && is_selected(input$qplot_region_sel)
  })
  
  observe({
    shinyjs::toggleState("qplot_x_field", condition = qplot_region_ready())
  })
  
  # changing the region (in single mode) can invalidate a prior x/y
  # choice outright (the y-axis picker's own choices are narrowed to
  # that region — see below), so both reset rather than silently
  # carrying over a selection that may no longer make sense.
  observeEvent(input$qplot_region_sel, {
    updateSelectInput(session, "qplot_x_field", selected = "")
    updateSelectizeInput(session, "qplot_y_fields", selected = character(0))
  }, ignoreInit = TRUE)
  
  qplot_data <- reactive({
    req(identical(input$main_nav, qnp_graph_tab_name))
    build_qplot_data_from_inputs(input)
  })
  
  qplot_object <- reactive({
    data <- qplot_data()
    req(!is.null(data), nrow(data) > 0)
    build_qplot_from_inputs(input, data)
  })
  
  # taller for a faceted multi-row plot (3 per row — see facet_wrap(ncol=3)
  # in functions.r) rather than a fixed height that would otherwise
  # squeeze every row into the same box regardless of how many there are.
  # Single-region mode (one panel) just gets a plain default.
  qplot_plot_height <- reactive({
    if (!identical(input$qplot_compare_mode, "regions")) return(550)
    data <- qplot_data()
    n_regions <- if (!is.null(data)) length(unique(data$region)) else 1
    n_rows <- ceiling(max(n_regions, 1) / 3)
    max(550, 300 * n_rows + 150)
  })
  
  output$qplot_output_wrapper <- renderUI({
    plotly::plotlyOutput("qplot_output", height = paste0(qplot_plot_height(), "px"))
  })
  
  # Y-axis measure picker — region, if given (single-region mode with a
  # region actually chosen), narrows the choices to measures that
  # actually have data there, so it never offers a dead-end pick. Rebuilt
  # whenever the region changes; any no-longer-valid selection is simply
  # dropped (matching how other cascading selectors on this page behave).
  # Disabled (shinyjs::disabled(), baked into the widget itself rather
  # than toggled after the fact) until qplot_region_ready() — selectize
  # respects a disabled underlying <select> at initialization, so this
  # doesn't need a separate toggleState() observer racing the rebuild.
  output$qplot_y_fields_ui <- renderUI({
    region <- if (identical(input$qplot_compare_mode, "single") && is_selected(input$qplot_region_sel)) input$qplot_region_sel else NULL
    widget <- selectizeInput(
      "qplot_y_fields", sprintf("Y axis (up to %d QNP measures)", qnp_graph_color_cap),
      choices = qnp_graph_field_choices_by_stain(region), multiple = TRUE,
      options = list(maxItems = qnp_graph_color_cap, plugins = list("remove_button"))
    )
    if (qplot_region_ready()) widget else shinyjs::disabled(widget)
  })
  
  # custom-range controls — bounds/default computed from whatever's
  # actually plotted right now, so they start matching the data instead
  # of an arbitrary fixed range. Rebuilt (or removed, via req()) whenever
  # the relevant selection changes. X is a histoslider (build_histoslider(),
  # functions.r) — the same widget and metadata_chart_color styling the
  # comparison pages' own metadata filters use; Y stays a plain range
  # slider (rounded to 1 decimal place — qnp_graph_slider_bounds()).
  output$qplot_x_range_ui <- renderUI({
    x_field <- input$qplot_x_field
    req(is_selected(x_field), !(x_field %in% qnp_graph_categorical_fields))
    data <- qplot_data()
    req(!is.null(data), nrow(data) > 0, x_field %in% names(data))
    vals <- data[[x_field]]
    vals <- vals[!is.na(vals)]
    req(length(vals) > 0)
    # CPS's range always includes 0 and 1 (its nominal bounds), same as
    # before — build_histoslider() derives its draggable start/end
    # straight from these values, so folding 0/1 in here is what lets
    # the handles reach them even if the actual data doesn't.
    if (identical(x_field, qnp_graph_cps_field)) vals <- c(vals, 0, 1)
    tagList(tags$label("X range"), build_histoslider("qplot_x_range", vals))
  })
  
  output$qplot_y_range_ui <- renderUI({
    y_fields <- input$qplot_y_fields
    req(length(y_fields) > 0)
    data <- qplot_data()
    req(!is.null(data), nrow(data) > 0)
    present_fields <- intersect(y_fields, names(data))
    req(length(present_fields) > 0)
    vals <- unlist(data[, present_fields, drop = FALSE])
    vals <- vals[!is.na(vals)]
    req(length(vals) > 0)
    b <- qnp_graph_slider_bounds(vals)
    sliderInput("qplot_y_range", "Y range", min = b$min, max = b$max, value = b$default, step = b$step)
  })
  
  # source names this plot for the click observer below (event_data()
  # needs it to target this specific widget); event_register() (right
  # after ggplotly(), before any further modification) is what makes
  # plotly actually emit plotly_click events for it. boxmode="group"
  # fixes a real plotly limitation: ggplotly() doesn't translate a
  # dodged geom_boxplot's own position correctly on its own, so without
  # this every measure's boxes stack on top of each other even though
  # their jittered points DO dodge correctly. The legend's own y position
  # is pushed further down (plotly's native layout, not just the ggplot2
  # theme's legend.box.spacing) since ggplotly() doesn't always preserve
  # that theme spacing faithfully. Fonts are set the same way, for the
  # same reason: ggplotly() doesn't reliably carry over the ggplot2
  # theme's axis.text/axis.title font family, so plotly's own layout
  # fonts are set explicitly instead — this is also what makes an
  # exported PNG (plotly's own toolbar, not a separate download button)
  # match the on-screen fonts, since both are rendered by the browser
  # from these same layout settings, not by R's graphics device. The
  # browser resolves "AllenTextLight"/"AllenHeadlineBold" via the same
  # @font-face rules ui.r's CSS already declares for the rest of the app.
  # config()'s toImageButtonOptions raises that export's own resolution
  # (2x scale over a 1600x1000 base) above plotly's own, fairly low-res
  # default. (The "Ignoring unknown aesthetics: text/key" warning from
  # plotly's own text/key aesthetics is suppressed where the plot is
  # actually built — build_qnp_grouped_boxplot()/build_qnp_multi_scatter(),
  # functions.r — not here, since it fires at construction time, before
  # this block ever sees the plot.)
  output$qplot_output <- plotly::renderPlotly({
    data <- qplot_data()
    shiny::validate(shiny::need(!is.null(data), "Select the required fields above to see a plot."))
    shiny::validate(shiny::need(nrow(data) > 0, "No QNP data available for this selection."))
    p <- qplot_object()
    shiny::validate(shiny::need(!is.null(p), "Select the required fields above to see a plot."))
    pl <- plotly::ggplotly(p, tooltip = "text", source = "qplot_output")
    pl <- plotly::event_register(pl, "plotly_click")
    pl <- plotly::layout(
      pl,
      boxmode = "group",
      legend = list(y = -0.35, yanchor = "top", font = list(family = qnp_graph_axis_font)),
      font   = list(family = qnp_graph_axis_font),
      xaxis  = list(tickfont = list(family = qnp_graph_axis_font), title = list(font = list(family = qnp_graph_title_font))),
      yaxis  = list(tickfont = list(family = qnp_graph_axis_font), title = list(font = list(family = qnp_graph_title_font)))
    )
    plotly::config(pl, toImageButtonOptions = list(format = "png", filename = "qnp_graph", width = 1600, height = 1000, scale = 2))
  })
  
  # clicking a plotted point copies its donor id. The point's `key` aes
  # (mapped from donor in build_qnp_grouped_boxplot()/build_qnp_multi_scatter())
  # arrives here as customdata via plotly's click event; clicking a
  # boxplot's box shape itself (no key mapped there) has no key and is a
  # no-op. suppressWarnings(): event_data() warns "source not registered"
  # any time output$qplot_output isn't currently a real, rendered plotly
  # widget (e.g. before x/y are chosen, when it shows a validation
  # message instead) — an expected, harmless state, not a bug, since
  # event_register() (above) does register this source once a plot
  # actually renders.
  observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = "qplot_output")), {
    d <- suppressWarnings(plotly::event_data("plotly_click", source = "qplot_output"))
    req(!is.null(d$key), nzchar(d$key))
    session$sendCustomMessage("copyToClipboard", list(text = d$key))
    showNotification(sprintf("Copied donor id: %s", d$key), type = "message")
  })
  
  observeEvent(input$qplot_reset_btn, {
    updateRadioButtons(session, "qplot_compare_mode", selected = "single")
    updateSelectInput(session, "qplot_grouping_sel", selected = "")
    updateSelectInput(session, "qplot_region_sel", selected = "")
    updateSelectInput(session, "qplot_x_field", selected = "")
    updateSelectizeInput(session, "qplot_y_fields", selected = character(0))
    updateSliderInput(session, "qplot_point_size", value = qnp_graph_point_size)
    updateSliderInput(session, "qplot_point_alpha", value = qnp_graph_point_alpha)
    showNotification(msg_page_reset, type = "message")
  })
  
  # ===========================================================================
  # about page — defined in R/aboutpage.R, edited independently of this file.
  # ===========================================================================
  
  about_tab_server(input, output, session)
  
  # ===========================================================================
  # switching tabs clears every page back to blank: loaded images, dropdown
  # selections, and subset/overlay settings all reset. this is deliberately
  # global (not "only reset the page being left") so every page is guaranteed
  # blank the moment you land on it, regardless of which tab you came from.
  # ===========================================================================
  
  # each page's reset, factored out so the per-page Reset buttons and the
  # tab-change observer below share ONE implementation rather than two
  # copies that could drift apart.
  reset_home_page <- function() {
    home_entries_rv(list())
    updateCheckboxInput(session, "home_filter_donors", value = FALSE)
    updateSelectInput(session, "home_donor", selected = "")
    updateSelectInput(session, "home_region", label = dropdown_label("Region", character(0)), choices = with_placeholder(character(0)))
    updateSelectInput(session, "home_stain", label = dropdown_label("Stain", character(0)), choices = with_placeholder(character(0)))
    updateCheckboxInput(session, "home_show_overlay", value = FALSE)
    updateSliderInput(session, "home_overlay_opacity", value = 0)
  }
  
  reset_dstain_page <- function() {
    dstain_entries_rv(list())
    updateCheckboxInput(session, "dstain_filter_donors", value = FALSE)
    updateSelectInput(session, "dstain_donor", selected = "")
    updateSelectInput(session, "dstain_region", label = dropdown_label("Region", character(0)), choices = with_placeholder(character(0)))
    updateSelectInput(session, "dstain_stains", label = dropdown_label("Stains to compare", character(0)), choices = character(0), selected = character(0))
    updateCheckboxInput(session, "dstain_show_overlay", value = FALSE)
    updateSliderInput(session, "dstain_overlay_opacity", value = 0)
    updateCheckboxInput(session, "dstain_sync_zoom", value = TRUE)
  }
  
  reset_sdonor_page <- function() {
    sdonor_entries_rv(list())
    updateSelectInput(session, "sdonor_stain", selected = "")
    updateSelectInput(session, "sdonor_region", label = dropdown_label("Region", character(0)), choices = with_placeholder(character(0)))
    updateRadioButtons(session, "sdonor_subset_mode", selected = "manual")
    updateSelectInput(session, "sdonor_donors_manual", choices = donor_choices, selected = character(0))
    updateSliderInput(session, "sdonor_random_n", value = donor_compare_min)
    updateCheckboxInput(session, "sdonor_show_overlay", value = FALSE)
    updateSliderInput(session, "sdonor_overlay_opacity", value = 0)
    updateCheckboxInput(session, "sdonor_sync_zoom", value = TRUE)
  }
  
  reset_sregion_page <- function() {
    sregion_entries_rv(list())
    updateCheckboxInput(session, "sregion_filter_donors", value = FALSE)
    updateSelectInput(session, "sregion_donor", selected = "")
    updateSelectInput(session, "sregion_stain", label = dropdown_label("Stain", character(0)), choices = with_placeholder(character(0)))
    updateSelectInput(session, "sregion_regions", label = dropdown_label("Regions to compare", character(0)), choices = character(0), selected = character(0))
    updateCheckboxInput(session, "sregion_show_overlay", value = FALSE)
    updateSliderInput(session, "sregion_overlay_opacity", value = 0)
    updateCheckboxInput(session, "sregion_sync_zoom", value = FALSE)
  }
  
  observeEvent(input$home_reset_btn,    { reset_home_page();    showNotification(msg_page_reset, type = "message") })
  observeEvent(input$dstain_reset_btn,  { reset_dstain_page();  showNotification(msg_page_reset, type = "message") })
  observeEvent(input$sdonor_reset_btn,  { reset_sdonor_page();  showNotification(msg_page_reset, type = "message") })
  observeEvent(input$sregion_reset_btn, { reset_sregion_page(); showNotification(msg_page_reset, type = "message") })
  
  observeEvent(input$main_nav, {
    reset_home_page()
    reset_dstain_page()
    reset_sdonor_page()
    reset_sregion_page()
  }, ignoreInit = TRUE)
}