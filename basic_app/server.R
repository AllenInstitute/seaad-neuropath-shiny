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
    actionButton("home_reset_zoom_btn", lbl_reset_image_zoom,
                 style = sprintf("background-color:%s; border-color:%s; color:#fff; font-size:16px;", action_button_color, action_button_color))
  })
  
  observeEvent(input$home_load_btn, {
    req(input$home_donor, input$home_region, input$home_stain,
        nzchar(input$home_donor), nzchar(input$home_region), nzchar(input$home_stain))
    slot <- donor_manifest[[input$home_donor]][[input$home_region]][[input$home_stain]]
    shiny::validate(shiny::need(!is.null(slot), "No image found for this donor/region/stain combination."))
    
    entries <- list(list(donor = input$home_donor, region = input$home_region, stain = input$home_stain, slot = slot))
    home_entries_rv(entries)
    
    # annotation files for this entry are fetched CONCURRENTLY inside
    # build_images_payload() (see its comment), with live progress reported
    # here via a determinate progress bar — so it's clear this is actually
    # working and roughly how much is left, rather than an indefinite spinner.
    images <- shiny::withProgress(message = "Loading image...", value = 0, {
      build_images_payload(entries, input$home_overlay_opacity, input$home_show_overlay,
                           progress_callback = function(done, total) {
                             shiny::setProgress(value = done / total, detail = sprintf("Fetching annotations: %d of %d", done, total))
                           })
    })
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images)), once = TRUE)
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
    actionButton("dstain_reset_zoom_btn", lbl_reset_image_zoom,
                 style = sprintf("background-color:%s; border-color:%s; color:#fff; font-size:16px;", action_button_color, action_button_color))
  })
  
  output$dstain_annotation_ui <- renderUI({ render_annotation_master_ui(dstain_entries_rv(), id_prefix = "dstain", varying_field = "stain") })
  output$dstain_viewer_grid   <- renderUI({ render_viewer_grid_ui(dstain_entries_rv(), label_field = "stain", donor_info_style = "qnp_only", qnp_icon = "file-earmark-bar-graph") })
  
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
    
    images <- shiny::withProgress(message = "Loading images...", value = 0, {
      build_images_payload(entries, input$dstain_overlay_opacity, input$dstain_show_overlay,
                           progress_callback = function(done, total) {
                             shiny::setProgress(value = done / total, detail = sprintf("Fetching annotations: %d of %d", done, total))
                           })
    })
    # syncCheckboxId names the checkbox for the JS to check LIVE on every
    # zoom/pan event, rather than a fixed value baked in at load time —
    # that's what lets unchecking it after images are already loaded take
    # effect immediately, with no reload needed.
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images, syncCheckboxId = "dstain_sync_zoom")), once = TRUE)
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
    actionButton("sdonor_reset_zoom_btn", lbl_reset_image_zoom,
                 style = sprintf("background-color:%s; border-color:%s; color:#fff; font-size:16px;", action_button_color, action_button_color))
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
    
    images <- shiny::withProgress(message = "Loading images...", value = 0, {
      build_images_payload(entries, input$sdonor_overlay_opacity, input$sdonor_show_overlay,
                           progress_callback = function(done, total) {
                             shiny::setProgress(value = done / total, detail = sprintf("Fetching annotations: %d of %d", done, total))
                           })
    })
    # syncCheckboxId names the checkbox for the JS to check LIVE on every
    # zoom/pan event, rather than a fixed value baked in at load time —
    # that's what lets unchecking it after images are already loaded take
    # effect immediately, with no reload needed.
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images, syncCheckboxId = "sdonor_sync_zoom")), once = TRUE)
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
    actionButton("sregion_reset_zoom_btn", lbl_reset_image_zoom,
                 style = sprintf("background-color:%s; border-color:%s; color:#fff; font-size:16px;", action_button_color, action_button_color))
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
    
    images <- shiny::withProgress(message = "Loading images...", value = 0, {
      build_images_payload(entries, input$sregion_overlay_opacity, input$sregion_show_overlay,
                           progress_callback = function(done, total) {
                             shiny::setProgress(value = done / total, detail = sprintf("Fetching annotations: %d of %d", done, total))
                           })
    })
    # syncCheckboxId names the checkbox for the JS to check LIVE on every
    # zoom/pan event, rather than a fixed value baked in at load time —
    # that's what lets unchecking it after images are already loaded take
    # effect immediately, with no reload needed.
    session$onFlushed(function() session$sendCustomMessage("loadImages", list(images = images, syncCheckboxId = "sregion_sync_zoom")), once = TRUE)
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
  # functions.r) — one modal mechanism for Home's context card, Compare
  # Stains'/Compare Regions' context card, and Compare Donors' per-image
  # icons, all driven by the SAME click event rather than four separate
  # handlers. region/stain arrive as empty strings (not real values) when
  # donor_info_trigger() was given NULL for either, which happens on
  # Compare Regions specifically (no single fixed region there).
  observeEvent(input$donor_info_click, {
    info <- input$donor_info_click
    req(is_selected(info$donor))
    region <- if (!is.null(info$region) && nzchar(info$region)) info$region else NULL
    stain  <- if (!is.null(info$stain)  && nzchar(info$stain))  info$stain  else NULL
    mode <- info$mode %||% "combined"
    
    content <- switch(mode,
                      "demo" = render_donor_demo_clinical(info$donor),
                      "qnp"  = render_donor_metadata_qnp_block(info$donor, region, stain, standalone = TRUE),
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
  
  # the region-dependent half of the popup — rebuilt whenever the radio
  # inside the (currently open) modal changes.
  output$iddonors_popup_qnp_ui <- renderUI({
    donor_id <- iddonors_popup_donor()
    req(is_selected(donor_id), is_selected(input$iddonors_popup_region_sel))
    view_mode <- input$iddonors_popup_qnp_view_mode %||% "layers"
    render_donor_qnp_region_detail(donor_id, input$iddonors_popup_region_sel, view_mode)
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