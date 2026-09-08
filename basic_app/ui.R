library(shiny)

fluidPage(
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/openseadragon.min.js")
  ),
  
  titlePanel("SEA-AD Neuropathology Slide Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      tabsetPanel(
        id = "sidebar_tabs",
        
        # ---------------------------------------------------------------
        tabPanel("Selection",
                 br(),
                 radioButtons(
                   "mode", "Comparison mode",
                   choices = c(
                     "Compare stains for one donor"  = "donor",
                     "Compare one stain across donors" = "stain"
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.mode == 'donor'",
                   selectInput("cmp_donor", "Donor", choices = DONOR_CHOICES),
                   selectInput("cmp_stains", "Stains to compare", choices = character(0), multiple = TRUE)
                 ),
                 
                 conditionalPanel(
                   condition = "input.mode == 'stain'",
                   selectInput("cmp_stain", "Stain", choices = ALL_STAINS),
                   radioButtons(
                     "donor_subset_mode", "Donors",
                     choices = c(
                       "All donors"               = "all",
                       "Select specific donors"   = "manual",
                       "Filter by metadata"       = "metadata"
                     )
                   ),
                   conditionalPanel(
                     condition = "input.donor_subset_mode == 'manual'",
                     selectInput("cmp_donors_manual", "Donors to compare", choices = DONOR_CHOICES, multiple = TRUE)
                   ),
                   conditionalPanel(
                     condition = "input.donor_subset_mode == 'metadata'",
                     helpText("Set thresholds in the Metadata tab, then Load below.")
                   )
                 ),
                 
                 tags$hr(),
                 sliderInput("overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0.5, step = 0.05),
                 actionButton("load_btn", "Load / Compare", class = "btn-primary")
        ),
        
        # ---------------------------------------------------------------
        tabPanel("Metadata",
                 br(),
                 helpText("Used only when Selection mode is 'Compare one stain across donors'",
                          "with donor filter set to 'Filter by metadata'."),
                 sliderInput("meta_age_range", "Age range",
                             min = floor(AGE_RANGE_DEFAULT[1]), max = ceiling(AGE_RANGE_DEFAULT[2]),
                             value = AGE_RANGE_DEFAULT),
                 numericInput("meta_cerad_min", "Min CERAD score (0-3)", value = 0, min = 0, max = 3),
                 numericInput("meta_thal_min", "Min Thal phase (0-5)", value = 0, min = 0, max = 5),
                 numericInput("meta_braak_min", "Min Braak stage (0-6)", value = 0, min = 0, max = 6),
                 tags$hr(),
                 strong("All donor metadata"),
                 tableOutput("metadata_table")
        )
      )
    ),
    
    mainPanel(
      width = 8,
      uiOutput("viewer_grid")
    )
  ),
  
  tags$script(HTML("
    var viewers = {}; // containerId -> { osd, annotationData }

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

    function redrawAnnotations(containerId) {
      var v = viewers[containerId];
      if (!v || !v.osd) return;
      var svg = ensureSvgOverlay(containerId);
      while (svg.firstChild) svg.removeChild(svg.firstChild);
      (v.annotationData || []).forEach(function(region) {
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
        svg.appendChild(poly);
      });
    }

    // Purely client-side visibility toggle — no server round-trip needed,
    // since all annotation polygon data was already sent when the image loaded.
    function toggleAnnotationsFor(containerId, visible) {
      var svg = document.getElementById(containerId + '-annotation-svg');
      if (svg) svg.style.display = visible ? 'block' : 'none';
    }

    Shiny.addCustomMessageHandler('loadImages', function(message) {
      (message.images || []).forEach(function(imgSpec) {
        var containerId = 'osd-' + imgSpec.id;
        var container = document.getElementById(containerId);
        if (!container) return;

        if (viewers[containerId] && viewers[containerId].osd) {
          viewers[containerId].osd.destroy();
        }
        viewers[containerId] = { osd: null, annotationData: imgSpec.polygons || [] };

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

        if (imgSpec.overlayUrl) {
          osd.addOnceHandler('open', function() {
            osd.addTiledImage({
              tileSource: imgSpec.overlayUrl,
              opacity: imgSpec.overlayOpacity != null ? imgSpec.overlayOpacity : 0.5
            });
          });
        }
      });
    });
  "))
)