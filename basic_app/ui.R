library(shiny)
library(bslib)
library(shinyjs)

tagList(
  
  useShinyjs(),
  
  # always-visible scroll-to-top arrow. Sits outside navbarPage entirely
  # and is position:fixed, so it stays on the side of the viewport no
  # matter how far down the page is scrolled or which tab is active.
  tags$a(
    href = "javascript:void(0)",
    onclick = "window.scrollTo({top: 0, behavior: 'smooth'})",
    title = "Back to top",
    style = paste(
      "position:fixed; right:18px; bottom:24px; z-index:1050;",
      "width:42px; height:42px; border-radius:50%;",
      "background:#7952b3; color:#fff; text-decoration:none;",
      "display:flex; align-items:center; justify-content:center;",
      "font-size:20px; line-height:1; box-shadow:0 2px 6px rgba(0,0,0,0.3);"
    ),
    HTML("&uarr;")
  ),
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/openseadragon.min.js"),
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
    title = "SEA-AD Viewer",
    theme = app_theme,
    id = "main_nav",  # lets server.R detect tab switches and reset every page
    
    # =========================================================================
    # home — single donor/region/stain, not a comparison.
    # =========================================================================
    tabPanel("Home",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("home_donor", "Donor", choices = with_placeholder(donor_choices)),
                 checkboxInput("home_filter_donors", "Filter donors by metadata", value = FALSE),
                 conditionalPanel(
                   condition = "input.home_filter_donors",
                   uiOutput("home_metadata_accordion_ui")
                 ),
                 selectInput("home_region", "Region", choices = with_placeholder(character(0))),
                 selectInput("home_stain", "Stain", choices = with_placeholder(character(0))),
                 tags$hr(),
                 # unchecked by default -> the opacity slider below stays hidden until turned on
                 checkboxInput("home_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.home_show_overlay",
                   sliderInput("home_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("home_load_btn", "Load", class = "btn-primary"),
                 actionButton("home_reset_btn", "Reset", class = "btn-secondary", style = "margin-left:8px;")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("home_annotation_ui"),
                 tags$div(style = "height:20px;"),
                 uiOutput("home_viewer_grid"),
                 uiOutput("home_donor_metadata")
               )
             )
    ),
    
    # =========================================================================
    # compare stains — constraints: single donor, single region. varies: stain.
    # =========================================================================
    tabPanel("Compare Stains",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("dstain_donor", "Donor", choices = with_placeholder(donor_choices)),
                 checkboxInput("dstain_filter_donors", "Filter donors by metadata", value = FALSE),
                 conditionalPanel(
                   condition = "input.dstain_filter_donors",
                   uiOutput("dstain_metadata_accordion_ui")
                 ),
                 selectInput("dstain_region", "Region", choices = with_placeholder(character(0))),
                 selectInput("dstain_stains", "Stains to compare", choices = character(0), multiple = TRUE),
                 tags$hr(),
                 checkboxInput("dstain_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.dstain_show_overlay",
                   sliderInput("dstain_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("dstain_load_btn", "Load / Compare", class = "btn-primary"),
                 actionButton("dstain_reset_btn", "Reset", class = "btn-secondary", style = "margin-left:8px;")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("dstain_context_card"),   # only appears once Load has been clicked
                 tags$div(style = "height:18px;"),  # space between the card and the annotation checkboxes
                 uiOutput("dstain_annotation_ui"),
                 tags$div(style = "height:20px;"),
                 uiOutput("dstain_viewer_grid")
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
                 selectInput("sdonor_stain", "Stain", choices = with_placeholder(all_stains)),
                 selectInput("sdonor_region", "Region", choices = with_placeholder(character(0))),
                 radioButtons(
                   "sdonor_subset_mode", "Donors",
                   choices = c("All donors" = "all", "Select specific donors" = "manual", "Filter by metadata" = "metadata")
                 ),
                 conditionalPanel(
                   condition = "input.sdonor_subset_mode == 'manual'",
                   # choices here are narrowed server-side to only donors that actually
                   # have this stain+region combination once both are picked.
                   selectInput("sdonor_donors_manual", "Donors to compare", choices = donor_choices, multiple = TRUE)
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
                 actionButton("sdonor_reset_btn", "Reset", class = "btn-secondary", style = "margin-left:8px;")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("sdonor_context_card"),
                 tags$div(style = "height:18px;"),
                 uiOutput("sdonor_annotation_ui"),
                 tags$div(style = "height:20px;"),
                 uiOutput("sdonor_viewer_grid")
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
                 selectInput("sregion_donor", "Donor", choices = with_placeholder(donor_choices)),
                 checkboxInput("sregion_filter_donors", "Filter donors by metadata", value = FALSE),
                 conditionalPanel(
                   condition = "input.sregion_filter_donors",
                   uiOutput("sregion_metadata_accordion_ui")
                 ),
                 selectInput("sregion_stain", "Stain", choices = with_placeholder(character(0))),
                 selectInput("sregion_regions", "Regions to compare", choices = character(0), multiple = TRUE),
                 tags$hr(),
                 checkboxInput("sregion_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.sregion_show_overlay",
                   sliderInput("sregion_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("sregion_load_btn", "Load / Compare", class = "btn-primary"),
                 actionButton("sregion_reset_btn", "Reset", class = "btn-secondary", style = "margin-left:8px;")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("sregion_context_card"),
                 tags$div(style = "height:18px;"),
                 uiOutput("sregion_annotation_ui"),
                 tags$div(style = "height:20px;"),
                 uiOutput("sregion_viewer_grid")  # shows a validation error here if nothing matches
               )
             )
    ),
    
    # =========================================================================
    # identify donors — dedicated page combining demographic, clinical, AND
    # QNP filters (the only page where QNP shows up now) to find a set of
    # donors of interest, which can then be pulled into the Compare Donors
    # page (see its "Use donor set" button) or copied out directly.
    # =========================================================================
    tabPanel("Filter Donors",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 actionButton("iddonors_reset_btn", "Reset filters", class = "btn-secondary", style = "margin-bottom:12px;"),
                 uiOutput("identify_donors_metadata_accordion_ui"),
                 uiOutput("identify_donors_qnp_accordion_ui")
               ),
               mainPanel(
                 width = 8,
                 h4("Matching donors"),
                 textOutput("identify_donors_count_text"),
                 div(
                   style = "margin:12px 0; display:flex; gap:8px;",
                   downloadButton("iddonors_download_btn", "Download table (.csv)", class = "btn-secondary"),
                   tags$button(
                     "Copy donor list",
                     class = "btn btn-secondary",
                     onclick = "navigator.clipboard.writeText(document.getElementById('identify_donors_list_pre').innerText)"
                   )
                 ),
                 helpText("Click a donor's name for all of their metadata, including every QNP measure."),
                 uiOutput("identify_donors_table_ui"),
                 # hidden plain list, kept only so the Copy button has plain text to grab
                 tags$div(style = "display:none;", tags$pre(id = "identify_donors_list_pre", textOutput("identify_donors_list_text")))
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

    // (re)creates an OpenSeadragon viewer per image spec sent from the server.
    Shiny.addCustomMessageHandler('loadImages', function(message) {
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

        osd.addHandler('animation', function() { redrawAnnotations(containerId); });
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