library(shiny)

fluidPage(
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/openseadragon.min.js")
  ),
  
  titlePanel("S3 Deep Zoom Slide Viewer — SEA-AD Neuropathology"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("stain_select", "Stain", choices = STAIN_CHOICES),
      helpText("Picking a stain auto-fills the URLs and reference width below from the donor manifest."),
      
      tags$hr(),
      
      textInput("dzi_url", "Base image DZI URL (RAW_IMAGE_DEEPZOOM)", value = "", width = "100%"),
      textInput("overlay_url", "Overlay DZI URL (HALO_ANALYSIS_IMAGE_DEEPZOOM, optional)", value = "", width = "100%"),
      conditionalPanel(
        condition = "input.overlay_url != ''",
        sliderInput("overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0.5, step = 0.05)
      ),
      
      textInput("annotations_url", "Whole-slide .annotations URL (optional)", value = "", width = "100%"),
      
      actionButton("load_btn", "Load", class = "btn-primary"),
      br(), br(),
      checkboxInput("show_annotations", "Show annotations", value = TRUE),
      
      tags$hr(),
      strong("Annotation alignment"),
      helpText("Vector annotations are recorded in the coordinate space of the FULL-RESOLUTION",
               "slide (the .svs), not the (often downsampled) DZI you're viewing. This field is",
               "auto-filled with the raw .svs width for the selected stain — leave it as-is unless",
               "the overlay still looks wrong, then use the offset/scale fields to fine-tune."),
      numericInput("annotation_ref_width", "Annotation coordinate width (px)", value = NA),
      numericInput("offset_x", "X offset (px)", value = 0, step = 100),
      numericInput("offset_y", "Y offset (px)", value = 0, step = 100),
      numericInput("scale_factor", "Scale correction", value = 1, step = 0.001),
      actionButton("recalc_btn", "Recalculate Annotations"),
      
      helpText("Note: all URLs need to be publicly reachable with CORS enabled.")
    ),
    
    mainPanel(
      tags$div(
        id = "openseadragon1",
        style = "width:100%; height:700px; background:#000; border:1px solid #ccc; position:relative;"
      )
    )
  ),
  
  tags$script(HTML("
    var osdViewer = null;
    var overlayLayer = null;
    var annotationData = [];

    function ensureSvgOverlay() {
      var container = document.getElementById('openseadragon1');
      var svg = document.getElementById('annotation-svg');
      if (!svg) {
        svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.setAttribute('id', 'annotation-svg');
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

    function redrawAnnotations() {
      if (!osdViewer) return;
      var svg = ensureSvgOverlay();
      while (svg.firstChild) svg.removeChild(svg.firstChild);
      if (!annotationData || annotationData.length === 0) return;

      annotationData.forEach(function(region) {
        var pts = region.points.split(' ').map(function(p) {
          var xy = p.split(',');
          var vp = new OpenSeadragon.Point(parseFloat(xy[0]), parseFloat(xy[1]));
          var px = osdViewer.viewport.viewportToViewerElementCoordinates(vp);
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

    Shiny.addCustomMessageHandler('loadDZI', function(message) {
      if (osdViewer) { osdViewer.destroy(); osdViewer = null; }
      overlayLayer = null;
      annotationData = [];

      osdViewer = OpenSeadragon({
        id: 'openseadragon1',
        prefixUrl: 'https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/images/',
        tileSources: message.url,
        showNavigator: true
      });

      osdViewer.addHandler('animation', redrawAnnotations);
      osdViewer.addHandler('open', redrawAnnotations);
      osdViewer.addHandler('resize', redrawAnnotations);
    });

    Shiny.addCustomMessageHandler('drawAnnotations', function(message) {
      annotationData = message.polygons || [];
      redrawAnnotations();
    });

    Shiny.addCustomMessageHandler('toggleAnnotations', function(message) {
      var svg = document.getElementById('annotation-svg');
      if (svg) svg.style.display = message.visible ? 'block' : 'none';
    });

    // Generic DZI overlay (works for masks, HALO_ANALYSIS_IMAGE_DEEPZOOM, etc.
    // — anything that's already a proper .dzi, no conversion needed).
    Shiny.addCustomMessageHandler('loadOverlay', function(message) {
      if (!osdViewer) return;
      if (overlayLayer) {
        osdViewer.world.removeItem(overlayLayer);
        overlayLayer = null;
      }
      osdViewer.addTiledImage({
        tileSource: message.url,
        opacity: message.opacity,
        success: function(event) { overlayLayer = event.item; }
      });
    });

    Shiny.addCustomMessageHandler('setOverlayOpacity', function(message) {
      if (overlayLayer) { overlayLayer.setOpacity(message.opacity); }
    });
  "))
)