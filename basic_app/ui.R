library(shiny)
library(bslib)
library(shinyjs)

tagList(
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML(sprintf("
      @font-face {
        font-family: 'AllenHeadline';
        src: url('fonts/AllenInstituteHeadline-Regular.woff2') format('woff2');
      }
      @font-face {
        font-family: 'AllenHeadlineBold';
        src: url('fonts/AllenInstituteHeadline-Bold.woff2') format('woff2');
      }
      @font-face {
        font-family: 'AllenTextLight';
        src: url('fonts/AllenInstituteText-Light.woff2') format('woff2');
      }
      @font-face {
        font-family: 'AllenTextRegular';
        src: url('fonts/AllenInstituteText-Regular.woff2') format('woff2');
      }

      /* #20 body text (light weight, per request) */
      body, p, span, div, .form-control, .form-select {
        font-family: 'AllenTextLight', sans-serif;
      }

      /* accordion labels, filter names (the base label part of a
         dropdown's 'Name (n)'), context card labels, and any plain text
         opted into the Regular weight (.text-regular) — more specific
         than the .navbar/headers rule below, so .accordion-button here
         correctly overrides that one */
      .accordion-button, .filter-name-regular, .context-card-label, .text-regular {
        font-family: 'AllenTextRegular', sans-serif !important;
      }
      /* the n count suffix on a dropdown label, and context card
         values, use the light weight */
      .filter-count-light, .context-card-value {
        font-family: 'AllenTextLight', sans-serif;
      }

      /* #19 top bar and headers */
      .navbar, h1, h2, h3, h4, h5, h6, .btn, .card-header {
        font-family: 'AllenHeadline', sans-serif;
      }

      /* #21 title (the navbar brand specifically) — more specific than
         the .navbar rule above so it correctly overrides it */
      .navbar-brand {
        font-family: 'AllenHeadlineBold', sans-serif !important;
      }

      /* explicit navbar height, so the scratchpad and top-bar links below
         (position:fixed, outside the navbar's own layout) can match it
         exactly and land on the same line as the brand and page names */
      .navbar { min-height: 60px; }

      /* #5 navbar background white, page names in the brand color */
      .navbar { background-color: #fff !important; }
      .navbar .nav-link {
        color: %s !important;
        font-size: 0.85rem;
        font-weight: normal !important;   /* #1 unbold */
        text-transform: lowercase !important;   /* #8 */
      }
      .navbar .navbar-brand { text-transform: none !important; }

      /* #1 fix: Shiny's navbarPage() renders the brand (.navbar-header)
         and the page-name links (.navbar-nav) as FLOATED SIBLINGS, not
         flex children — align-items has no effect on floated elements,
         which is why that didn't work before. This removes the floats
         and forces flexbox on the actual containers instead. Covers both
         the classic markup (.navbar-header) and the newer one
         (.navbar-collapse) since which one bslib renders isn't fully
         certain without live testing. */
      .navbar > .container-fluid {
        display: flex !important;
        align-items: center !important;
        flex-wrap: wrap;
      }
      .navbar-header, .navbar-collapse, .navbar-nav {
        float: none !important;
        display: flex !important;
        align-items: center !important;
        margin: 0 !important;
      }
      .navbar-nav > li {
        float: none !important;
      }

      /* space between the brand and the page-name links, and a
         guaranteed minimum gap after them before the top-bar links */
      .navbar-nav {
        margin-left: 32px !important;
        margin-right: 40px !important;
      }

      /* #4 headers should never render all-caps (accordion panel titles,
         card headers, sub-headers in the QNP filters, the Filter Donors
         page header, etc.) */
      h1, h2, h3, h4, h5, h6, .accordion-button, .card-header {
        text-transform: none !important;
      }

      /* #5 buttons in lowercase */
      .btn {
        text-transform: lowercase !important;
      }

      /* #8 buffer below the page's last element (Reset image zoom) */
      body {
        padding-bottom: 40px;
      }

      /* #2 Load/Compare button text white */
      .btn-primary {
        color: #fff !important;
      }

      /* explicit active-tab underline width — every other divider line
         below is matched to this same width per request */
      .navbar-nav .nav-link.active {
        border-bottom: 2px solid %s !important;
      }

      /* sidebar: white background, no border around the box itself */
      .well, .col-sm-4 > .well {
        background-color: %s !important;
        border: none !important;
      }
      .well hr {
        border-top: 2px solid %s;
        margin: 14px 0;
      }
      /* vertical line between the sidebar and main panel — on the
         containing column, not the box, so it's a standalone divider
         rather than looking attached to the filter box */
      .col-sm-4 {
        border-right: 2px solid %s;
      }

      /* muted brand-color table stripe instead of Bootstrap's default
         gray, Filter Donors table only. Bootstrap's .table-striped sets
         --bs-table-accent-bg (applied via an inset box-shadow, not
         background-color) from --bs-table-striped-bg on odd rows — so
         the variable has to be overridden here rather than fighting
         that box-shadow with background-color directly. */
      #identify_donors_table_ui .table-striped {
        --bs-table-bg: #fff;
        --bs-table-striped-bg: %s;
      }
    ", metadata_chart_color, sidebar_divider_color, sidebar_bg_color, sidebar_divider_color, sidebar_divider_color, table_stripe_color)))
  ),
  
  # always-visible scroll-to-top arrow. Sits outside navbarPage entirely
  # and is position:fixed, so it stays on the side of the viewport no
  # matter how far down the page is scrolled — server-rendered so its
  # side (left on Filter Donors, right elsewhere) can depend on the
  # active tab; see output$scroll_top_arrow_ui in server.r.
  uiOutput("scroll_top_arrow_ui"),
  
  # three top-bar links, black text with an arrow-up-right icon each.
  # Static-flow styled (not position:fixed) and given an id so the script
  # right below can move this div into the navbar's own container —
  # making it a genuine part of the navbar's layout rather than a
  # floating overlay like the scratchpad.
  tags$div(
    id = "top_bar_links",
    style = "display:flex; align-items:center; gap:24px; margin-left:auto; margin-right:220px;",
    top_bar_link("https://sea-ad.org", "sea-ad.org"),
    top_bar_link("https://brain-map.org", "brain-map.org"),
    top_bar_link("https://github.com/AllenInstitute/seaad-neuropath-shiny", "github")
  ),
  tags$script(HTML("
    $(document).ready(function() {
      var links = document.getElementById('top_bar_links');
      var container = document.querySelector('.navbar .container-fluid');
      if (links && container) { container.appendChild(links); }
    });
  ")),
  
  # floating scratchpad — session-only (survives switching tabs, not a
  # page refresh). It's a small toggle button that expands a panel
  # holding a plain textAreaInput. Because that input is static (never
  # inside a renderUI, and deliberately left out of the tab-change reset
  # observer in server.r), Shiny keeps its value automatically for the
  # whole session — no reactiveVal or other plumbing needed for that part.
  tags$div(
    style = "position:fixed; top:0; right:60px; z-index:1060; height:60px; display:flex; align-items:center;",
    tags$div(
      style = paste(
        "position:absolute; top:50%; right:44px; transform:translateY(-50%); white-space:nowrap;",
        "display:flex; align-items:center; gap:4px;",
        sprintf("font-size:13px; font-weight:600; color:%s;", metadata_chart_color)
      ),
      lbl_scratchpad, HTML("&rarr;")
    ),
    tags$button(
      type = "button", title = lbl_scratchpad,
      onclick = "document.getElementById('scratchpad_panel').classList.toggle('scratchpad-hidden')",
      style = paste(
        "width:34px; height:34px; border-radius:50%; border:none;",
        sprintf("background:%s; color:#fff; font-size:16px; cursor:pointer;", metadata_chart_color)
      ),
      bsicons::bs_icon("stickies")
    ),
    tags$div(
      id = "scratchpad_panel",
      class = "scratchpad-hidden",
      style = paste(
        "position:absolute; top:60px; right:0; width:280px;",
        "background:#fff; border:1px solid #ddd; border-radius:8px;",
        "box-shadow:0 2px 10px rgba(0,0,0,0.25); padding:10px;"
      ),
      strong(lbl_scratchpad),
      textAreaInput("scratchpad_notes", NULL, rows = 8, width = "100%", resize = "vertical"),
      actionButton("scratchpad_reset_btn", "Clear", class = "btn-sm btn-secondary")
    )
  ),
  tags$style(HTML(".scratchpad-hidden { display: none; }")),
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/openseadragon.min.js"),
    tags$script(HTML("
      // navigator.clipboard needs a SECURE CONTEXT (https, or localhost) —
      // on a plain http:// deployment it doesn't exist at all, and calling
      // it does nothing with no visible error. Sandboxed webviews (e.g.
      // RStudio's built-in Viewer pane) can ALSO fail even when the API
      // exists, because they often can't reach the system clipboard at
      // all — if that's what's happening here, opening the app in a real
      // browser tab (not the Viewer pane) should resolve it immediately.
      //
      // el is the CLICKED element (pass `this` from onclick) — flashes it
      // to 'Copied!' briefly so clicking always gives visible confirmation
      // either way, rather than a silent success-or-failure.
      function copyTextRobust(text, el) {
        function flash() {
          if (!el) return;
          var orig = el.getAttribute('data-orig') || el.innerHTML;
          el.setAttribute('data-orig', orig);
          el.innerHTML = 'Copied!';
          setTimeout(function () { el.innerHTML = orig; }, 1200);
        }
        function fallback() {
          var ta = document.createElement('textarea');
          ta.value = text;
          ta.style.position = 'fixed';
          ta.style.left = '-9999px';
          document.body.appendChild(ta);
          ta.focus();
          ta.select();
          var ok = false;
          try { ok = document.execCommand('copy'); } catch (e) {}
          document.body.removeChild(ta);
          // execCommand's return value was never being checked before —
          // meaning 'Copied!' could show even when copying silently
          // failed (e.g. a sandboxed iframe, like RStudio's Viewer pane,
          // blocking clipboard access entirely). If BOTH mechanisms fail,
          // this guarantees the text is still visible to copy by hand.
          if (ok) { flash(); } else { window.prompt('Copy failed — copy manually:', text); }
        }
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(text).then(flash).catch(fallback);
        } else {
          fallback();
        }
      }
    ")),
    tags$style(HTML("
      /* wider popovers so donor-metadata content (render_donor_metadata_list)
         doesn't get cut off by Bootstrap's fairly narrow default max-width */
      .popover { max-width: 340px; }

      /* disabled Load/Compare buttons: a plain light gray instead of
         Bootstrap's default washed-out-primary look */
      .btn:disabled, .btn.disabled {
        background-color: #e0e0e0 !important;
        border-color: #d0d0d0 !important;
        color: #999999 !important;
        opacity: 1 !important;
      }
    "))
  ),
  
  navbarPage(
    title = tags$div(
      style = "display:flex; align-items:center; gap:8px;",
      tags$img(src = "AI_lens_black.png", style = "height:1.3em;"),
      tags$span(style = sprintf("color:%s;", brand_primary_color), lbl_brand_primary),
      tags$span(style = sprintf("color:%s;", icon_color), " / "),
      tags$span(style = sprintf("color:%s;", icon_color), lbl_brand_secondary)
    ),
    windowTitle = lbl_app_title,
    theme = app_theme,
    id = "main_nav",  # lets server.R detect tab switches and reset every page
    
    # =========================================================================
    # home — single donor/region/stain, not a comparison.
    # =========================================================================
    tabPanel("Home",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("home_donor", dropdown_label("Donor", donor_choices), choices = with_placeholder(donor_choices)),
                 checkboxInput("home_filter_donors", "Filter donors by metadata", value = FALSE),
                 conditionalPanel(
                   condition = "input.home_filter_donors",
                   uiOutput("home_metadata_accordion_ui")
                 ),
                 tags$hr(),
                 selectInput("home_region", "Region", choices = with_placeholder(character(0))),
                 tags$hr(),
                 selectInput("home_stain", "Stain", choices = with_placeholder(character(0))),
                 tags$hr(),
                 # unchecked by default -> the opacity slider below stays hidden until turned on
                 checkboxInput("home_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.home_show_overlay",
                   sliderInput("home_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("home_load_btn", "Load", class = "btn-primary"),
                 actionButton("home_reset_btn", "Reset", style = sprintf("margin-left:8px; background-color:%s; border-color:%s; color:#fff;", reset_button_color, reset_button_color))
               ),
               mainPanel(
                 width = 8,
                 uiOutput("home_context_card"),
                 tags$div(style = "height:10px;"),
                 uiOutput("home_annotation_ui"),
                 tags$div(style = "height:20px;"),
                 uiOutput("home_viewer_grid"),
                 tags$div(style = "height:10px;"),
                 tags$div(style = "margin-right:60px;", uiOutput("home_reset_zoom_btn_ui"))
               )
             )
    ),
    
    navbarMenu("Comparisons",
               # =========================================================================
               # compare stains — constraints: single donor, single region. varies: stain.
               # =========================================================================
               tabPanel("Compare Stains",
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            selectInput("dstain_donor", dropdown_label("Donor", donor_choices), choices = with_placeholder(donor_choices)),
                            checkboxInput("dstain_filter_donors", "Filter donors by metadata", value = FALSE),
                            conditionalPanel(
                              condition = "input.dstain_filter_donors",
                              uiOutput("dstain_metadata_accordion_ui")
                            ),
                            tags$hr(),
                            selectInput("dstain_region", "Region", choices = with_placeholder(character(0))),
                            tags$hr(),
                            selectInput("dstain_stains", "Stains to compare", choices = character(0), multiple = TRUE),
                            tags$hr(),
                            checkboxInput("dstain_show_overlay", "Show mask/analysis overlay", value = FALSE),
                            conditionalPanel(
                              condition = "input.dstain_show_overlay",
                              sliderInput("dstain_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                            ),
                            actionButton("dstain_load_btn", "Load / Compare", class = "btn-primary"),
                            actionButton("dstain_reset_btn", "Reset", style = sprintf("margin-left:8px; background-color:%s; border-color:%s; color:#fff;", reset_button_color, reset_button_color))
                          ),
                          mainPanel(
                            width = 8,
                            uiOutput("dstain_context_card"),   # only appears once Load has been clicked
                            tags$div(style = "height:10px;"),
                            sync_zoom_control("dstain_sync_zoom", default_checked = TRUE),
                            uiOutput("dstain_annotation_ui"),
                            tags$div(style = "height:18px;"),
                            uiOutput("dstain_viewer_grid"),
                            tags$div(style = "height:10px;"),
                            uiOutput("dstain_reset_zoom_btn_ui")
                          )
                        )
               ),
               
               # =========================================================================
               # compare donors — constraints: single stain, single region. varies: donor.
               # =========================================================================
               tabPanel("Compare Donors",
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            selectInput("sdonor_stain", dropdown_label("Stain", all_stains), choices = with_placeholder(all_stains)),
                            tags$hr(),
                            selectInput("sdonor_region", "Region", choices = with_placeholder(character(0))),
                            tags$hr(),
                            radioButtons(
                              "sdonor_subset_mode", "Donors",
                              choices = stats::setNames(
                                c("manual", "random", "metadata"),
                                c("Select specific donors", "Select donors at random", "Filter by metadata")
                              ),
                              selected = "manual"
                            ),
                            conditionalPanel(
                              condition = "input.sdonor_subset_mode == 'manual'",
                              # choices here are narrowed server-side to only donors that actually
                              # have this stain+region combination once both are picked.
                              # maxItems enforces the cap natively (selectize simply won't
                              # accept an 11th pick) — selectizeInput() (rather than
                              # selectInput()) is what exposes that option.
                              tags$div(
                                style = "display:flex; align-items:center; gap:6px;",
                                uiOutput("sdonor_manual_label_ui", inline = TRUE),
                                bslib::tooltip(tags$span(style = "color:#000;", bsicons::bs_icon("info-circle-fill")), tt_donor_compare_cap, placement = "right")
                              ),
                              selectizeInput(
                                "sdonor_donors_manual", label = NULL,
                                choices = donor_choices, multiple = TRUE,
                                options = list(maxItems = donor_compare_cap)
                              ),
                              uiOutput("sdonor_manual_count_ui")
                            ),
                            conditionalPanel(
                              condition = "input.sdonor_subset_mode == 'random'",
                              sliderInput("sdonor_random_n", "Number of donors", min = donor_compare_min, max = donor_compare_cap, value = donor_compare_min, step = 1)
                            ),
                            conditionalPanel(
                              condition = "input.sdonor_subset_mode == 'metadata'",
                              uiOutput("sdonor_metadata_accordion_ui")
                            ),
                            tags$hr(),
                            checkboxInput("sdonor_show_overlay", "Show mask/analysis overlay", value = FALSE),
                            conditionalPanel(
                              condition = "input.sdonor_show_overlay",
                              sliderInput("sdonor_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                            ),
                            actionButton("sdonor_load_btn", "Load / Compare", class = "btn-primary"),
                            actionButton("sdonor_reset_btn", "Reset", style = sprintf("margin-left:8px; background-color:%s; border-color:%s; color:#fff;", reset_button_color, reset_button_color))
                          ),
                          mainPanel(
                            width = 8,
                            uiOutput("sdonor_context_card"),
                            tags$div(style = "height:10px;"),
                            sync_zoom_control("sdonor_sync_zoom", default_checked = TRUE),
                            uiOutput("sdonor_annotation_ui"),
                            tags$div(style = "height:18px;"),
                            uiOutput("sdonor_viewer_grid"),
                            tags$div(style = "height:10px;"),
                            uiOutput("sdonor_reset_zoom_btn_ui")
                          )
                        )
               ),
               
               # =========================================================================
               # compare regions — constraints: single donor, single stain.
               # varies: region (every region that donor+stain combination has).
               # =========================================================================
               tabPanel("Compare Regions",
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            selectInput("sregion_donor", dropdown_label("Donor", donor_choices), choices = with_placeholder(donor_choices)),
                            checkboxInput("sregion_filter_donors", "Filter donors by metadata", value = FALSE),
                            conditionalPanel(
                              condition = "input.sregion_filter_donors",
                              uiOutput("sregion_metadata_accordion_ui")
                            ),
                            tags$hr(),
                            selectInput("sregion_stain", "Stain", choices = with_placeholder(character(0))),
                            tags$hr(),
                            selectInput("sregion_regions", "Regions to compare", choices = character(0), multiple = TRUE),
                            tags$hr(),
                            checkboxInput("sregion_show_overlay", "Show mask/analysis overlay", value = FALSE),
                            conditionalPanel(
                              condition = "input.sregion_show_overlay",
                              sliderInput("sregion_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                            ),
                            actionButton("sregion_load_btn", "Load / Compare", class = "btn-primary"),
                            actionButton("sregion_reset_btn", "Reset", style = sprintf("margin-left:8px; background-color:%s; border-color:%s; color:#fff;", reset_button_color, reset_button_color))
                          ),
                          mainPanel(
                            width = 8,
                            uiOutput("sregion_context_card"),
                            tags$div(style = "height:10px;"),
                            sync_zoom_control("sregion_sync_zoom", default_checked = FALSE),
                            uiOutput("sregion_annotation_ui"),
                            tags$div(style = "height:18px;"),
                            uiOutput("sregion_viewer_grid"),  # shows a validation error here if nothing matches
                            tags$div(style = "height:10px;"),
                            uiOutput("sregion_reset_zoom_btn_ui")
                          )
                        )
               )
    ),
    
    navbarMenu("QNP",
               # =========================================================================
               # identify donors — dedicated page combining demographic, clinical, AND
               # QNP filters (the only page where QNP shows up now) to find a set of
               # donors of interest, which can then be pulled into the Compare Donors
               # page (see its "Use donor set" button) or copied out directly.
               # =========================================================================
               tabPanel(filter_donors_tab_name,
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            actionButton("iddonors_reset_btn", "Reset filters", style = sprintf("margin-bottom:12px; background-color:%s; border-color:%s; color:#fff;", reset_button_color, reset_button_color)),
                            uiOutput("identify_donors_metadata_accordion_ui"),
                            uiOutput("identify_donors_qnp_accordion_ui")
                          ),
                          mainPanel(
                            width = 8,
                            h4("Matching donors"),
                            textOutput("identify_donors_count_text"),
                            uiOutput("identify_donors_zero_warning_ui"),
                            div(
                              style = "margin:12px 0; display:flex; gap:8px;",
                              downloadButton("iddonors_download_btn", "Download table (.csv)", style = sprintf("background-color:%s; border-color:%s; color:#fff;", action_button_color, action_button_color)),
                              uiOutput("iddonors_copy_list_btn_ui", inline = TRUE)
                            ),
                            helpText("Click a donor's name for all metadata, including QNP values."),
                            uiOutput("identify_donors_table_ui")
                          )
                        )
               )
    ),
    
    # =========================================================================
    # about — defined in R/aboutpage.R, edited independently of this file.
    # =========================================================================
    about_tab_ui()
  ),
  
  tags$script(HTML("
    // containerId -> { osd: OpenSeadragon instance, annotationGroups: [...], hiddenGroups: {} }
    var viewers = {};

    // guards against infinite recursion: programmatically moving viewer B's
    // viewport to match viewer A also fires B's own 'animation' handler,
    // which would otherwise try to sync everyone AGAIN (including back to
    // A). While this flag is true, syncViewportToGroup() is a no-op.
    var isSyncingViewers = false;

    // mirrors one viewer's zoom + pan center to every OTHER viewer in
    // groupContainerIds. Syncs both, not just zoom — zoom alone wouldn't
    // show the corresponding region across images if each was panned
    // somewhere different first.
    function syncViewportToGroup(sourceContainerId, groupContainerIds) {
      if (isSyncingViewers) return;
      var source = viewers[sourceContainerId];
      if (!source || !source.osd) return;
      var vp = source.osd.viewport;
      var zoom = vp.getZoom();
      var center = vp.getCenter();

      isSyncingViewers = true;
      groupContainerIds.forEach(function(id) {
        if (id === sourceContainerId) return;
        var v = viewers[id];
        if (!v || !v.osd) return;
        v.osd.viewport.zoomTo(zoom, null, true);
        v.osd.viewport.panTo(center, true);
      });
      isSyncingViewers = false;
    }

    // creates (once) the transparent svg layer used to draw annotation polygons on top of a viewer
    function ensureSvgOverlay(containerId) {
      var container = document.getElementById(containerId);
      var svgId = containerId + '-annotation-svg';
      var svg = document.getElementById(svgId);
      if (!svg) {
        svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.setAttribute('id', svgId);
        svg.style.position = 'absolute';
        svg.style.top = '0';
        svg.style.left = '0';
        svg.style.width = '100%';
        svg.style.height = '100%';
        svg.style.pointerEvents = 'none';
        container.style.position = 'relative';
        container.appendChild(svg);
      }
      return svg;
    }

    // rebuilds one viewer's polygon shapes at its current pan/zoom position.
    // called on every pan/zoom/resize since a polygon's screen position depends on the current view.
    function redrawAnnotations(containerId) {
      var v = viewers[containerId];
      if (!v || !v.osd) return;
      var svg = ensureSvgOverlay(containerId);
      while (svg.firstChild) svg.removeChild(svg.firstChild);

      (v.annotationGroups || []).forEach(function(group, gi) {
        var g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        g.setAttribute('id', containerId + '-group-' + gi);
        if (v.hiddenGroups[gi]) g.style.display = 'none';

        (group.polygons || []).forEach(function(region) {
          var pts = region.points.split(' ').map(function(p) {
            var xy = p.split(',');
            var vp = new OpenSeadragon.Point(parseFloat(xy[0]), parseFloat(xy[1]));
            var px = v.osd.viewport.viewportToViewerElementCoordinates(vp);
            return px.x + ',' + px.y;
          }).join(' ');
          var poly = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
          poly.setAttribute('points', pts);
          poly.setAttribute('stroke', region.color);
          poly.setAttribute('fill', region.color);
          poly.setAttribute('fill-opacity', '0.15');
          poly.setAttribute('stroke-width', '2');
          g.appendChild(poly);
        });

        svg.appendChild(g);
      });
    }

    // one master checkbox per unique annotation label toggles that label across
    // every currently-loaded image with a matching file. All annotation
    // polygons were already parsed server-side and sent along in the
    // 'loadImages' message, so this is a pure visibility toggle — no fetch,
    // no round-trip to the server.
    function toggleAnnotationLabel(label, visible) {
      Object.keys(viewers).forEach(function(containerId) {
        var v = viewers[containerId];
        (v.annotationGroups || []).forEach(function(group, gi) {
          if (group.label !== label) return;
          v.hiddenGroups[gi] = !visible;
          var gEl = document.getElementById(containerId + '-group-' + gi);
          if (gEl) gEl.style.display = visible ? 'block' : 'none';
        });
      });
    }

    // resets every currently tracked viewer back to its initial view.
    // Triggered by any page's Reset image zoom button — harmless for
    // viewers on a page that isn't the one currently visible.
    Shiny.addCustomMessageHandler('resetZoom', function(message) {
      Object.keys(viewers).forEach(function(id) {
        var v = viewers[id];
        if (v && v.osd) v.osd.viewport.goHome();
      });
    });

    // (re)creates an OpenSeadragon viewer per image spec sent from the server.
    Shiny.addCustomMessageHandler('loadImages', function(message) {
      // all container ids in THIS batch (i.e. this page's grid) — the sync
      // group a viewer propagates to. syncCheckboxId names the page's
      // toggle checkbox, checked LIVE on every animation event below
      // (rather than once here) — otherwise unchecking it after images
      // are already loaded wouldn't take effect until reloading.
      var groupContainerIds = (message.images || []).map(function(imgSpec) { return 'osd-' + imgSpec.id; });
      var syncCheckboxId = message.syncCheckboxId;

      (message.images || []).forEach(function(imgSpec) {
        var containerId = 'osd-' + imgSpec.id;
        var container = document.getElementById(containerId);
        if (!container) return;

        if (viewers[containerId] && viewers[containerId].osd) {
          viewers[containerId].osd.destroy();
        }

        var hiddenGroups = {};
        (imgSpec.annotationGroups || []).forEach(function(g, gi) { hiddenGroups[gi] = true; }); // off by default

        viewers[containerId] = {
          osd: null,
          annotationGroups: imgSpec.annotationGroups || [],
          hiddenGroups: hiddenGroups
        };

        var osd = OpenSeadragon({
          id: containerId,
          prefixUrl: 'https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/images/',
          tileSources: imgSpec.dziUrl,
          showNavigator: false
        });
        viewers[containerId].osd = osd;

        osd.addHandler('animation', function() {
          redrawAnnotations(containerId);
          // no checkbox id (e.g. Home, which has no toggle at all) is
          // treated as sync-enabled — moot there anyway, since a single-
          // image batch has no other viewer to propagate to.
          var checkboxEl = syncCheckboxId ? document.getElementById(syncCheckboxId) : null;
          var syncEnabled = checkboxEl ? checkboxEl.checked : true;
          if (syncEnabled) syncViewportToGroup(containerId, groupContainerIds);
        });
        osd.addHandler('open', function() { redrawAnnotations(containerId); });
        osd.addHandler('resize', function() { redrawAnnotations(containerId); });

        // overlay is added only after the base image opens, so its tile
        // requests don't compete with the base image's initial ones.
        if (imgSpec.overlayUrl) {
          osd.addOnceHandler('open', function() {
            osd.addTiledImage({
              tileSource: imgSpec.overlayUrl,
              opacity: imgSpec.overlayOpacity != null ? imgSpec.overlayOpacity : 0
            });
          });
        }
      });
    });
  "))
)