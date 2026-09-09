library(shiny)
library(bslib)

tagList(
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/openseadragon.min.js")
  ),
  
  navbarPage(
    title = "SEA-AD Neuropathology Viewer",
    theme = APP_THEME,
    
    # =========================================================================
    # HOME — single donor/region/stain, not a comparison. Mask overlay is
    # explicitly toggleable rather than always shown.
    # =========================================================================
    tabPanel("Home",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("home_donor", "Donor", choices = DONOR_CHOICES),
                 selectInput("home_region", "Region", choices = character(0)),
                 selectInput("home_stain", "Stain", choices = character(0)),
                 tags$hr(),
                 checkboxInput("home_show_mask", "Show mask/analysis overlay", value = TRUE),
                 conditionalPanel(
                   condition = "input.home_show_mask",
                   sliderInput("home_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0.5, step = 0.05)
                 ),
                 actionButton("home_load_btn", "Load", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("home_annotation_ui"),
                 tags$hr(),
                 uiOutput("home_viewer_grid")
               )
             )
    ),
    
    # =========================================================================
    # COMPARE STAINS FOR ONE DONOR (scoped to one region of that donor)
    # =========================================================================
    tabPanel("Compare Stains (Donor)",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("dstain_donor", "Donor", choices = DONOR_CHOICES),
                 selectInput("dstain_region", "Region", choices = character(0)),
                 selectInput("dstain_stains", "Stains to compare", choices = character(0), multiple = TRUE),
                 tags$hr(),
                 sliderInput("dstain_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0.5, step = 0.05),
                 actionButton("dstain_load_btn", "Load / Compare", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("dstain_annotation_ui"),
                 tags$hr(),
                 uiOutput("dstain_viewer_grid")
               )
             )
    ),
    
    # =========================================================================
    # COMPARE ONE STAIN ACROSS DONORS
    # =========================================================================
    tabPanel("Compare Donors (Stain)",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("sdonor_stain", "Stain", choices = ALL_STAINS),
                 radioButtons(
                   "sdonor_subset_mode", "Donors",
                   choices = c("All donors" = "all", "Select specific donors" = "manual", "Filter by metadata" = "metadata")
                 ),
                 conditionalPanel(
                   condition = "input.sdonor_subset_mode == 'manual'",
                   selectInput("sdonor_donors_manual", "Donors to compare", choices = DONOR_CHOICES, multiple = TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.sdonor_subset_mode == 'metadata'",
                   build_metadata_accordion("sdonor", donor_metadata)
                 ),
                 tags$hr(),
                 sliderInput("sdonor_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0.5, step = 0.05),
                 actionButton("sdonor_load_btn", "Load / Compare", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("sdonor_annotation_ui"),
                 tags$hr(),
                 uiOutput("sdonor_viewer_grid")
               )
             )
    ),
    
    # =========================================================================
    # COMPARE ONE STAIN ACROSS REGIONS
    # =========================================================================
    tabPanel("Compare Regions (Stain)",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("sregion_stain", "Stain", choices = ALL_STAINS),
                 helpText("Shows every region available for each matching donor+stain combination."),
                 radioButtons(
                   "sregion_subset_mode", "Donors",
                   choices = c("All donors" = "all", "Select specific donors" = "manual", "Filter by metadata" = "metadata")
                 ),
                 conditionalPanel(
                   condition = "input.sregion_subset_mode == 'manual'",
                   selectInput("sregion_donors_manual", "Donors to compare", choices = DONOR_CHOICES, multiple = TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.sregion_subset_mode == 'metadata'",
                   build_metadata_accordion("sregion", donor_metadata)
                 ),
                 tags$hr(),
                 sliderInput("sregion_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0.5, step = 0.05),
                 actionButton("sregion_load_btn", "Load / Compare", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("sregion_annotation_ui"),
                 tags$hr(),
                 uiOutput("sregion_viewer_grid")
               )
             )
    )
  ),
  
  tags$script(HTML("
    var viewers = {}; // containerId -> { osd, annotationGroups: [{label,url,refWidth,polygons}], hiddenGroups: {} }

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

    // Rebuilds SVG shapes for whichever annotation groups have actually been
    // fetched (group.polygons !== null) — unfetched groups are simply skipped
    // until the user turns them on for the first time.
    function redrawAnnotations(containerId) {
      var v = viewers[containerId];
      if (!v || !v.osd) return;
      var svg = ensureSvgOverlay(containerId);
      while (svg.firstChild) svg.removeChild(svg.firstChild);

      (v.annotationGroups || []).forEach(function(group, gi) {
        if (!group.polygons) return;
        var g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        g.setAttribute('id', containerId + '-group-' + gi);
        if (v.hiddenGroups[gi]) g.style.display = 'none';

        group.polygons.forEach(function(region) {
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

    // One master checkbox per unique annotation label toggles that label
    // across EVERY currently loaded image (on the SAME page) that has a
    // matching file. Any group not yet fetched gets requested from the
    // server in one batch.
    function toggleAnnotationLabel(label, visible) {
      var pending = [];

      Object.keys(viewers).forEach(function(containerId) {
        var v = viewers[containerId];
        (v.annotationGroups || []).forEach(function(group, gi) {
          if (group.label !== label) return;
          v.hiddenGroups[gi] = !visible;

          if (visible && !group.polygons) {
            pending.push({
              containerId: containerId, groupIndex: gi,
              url: group.url, refWidth: group.refWidth
            });
          } else {
            var gEl = document.getElementById(containerId + '-group-' + gi);
            if (gEl) gEl.style.display = visible ? 'block' : 'none';
          }
        });
      });

      if (pending.length > 0) {
        Shiny.setInputValue('request_annotations', { requests: pending }, { priority: 'event' });
      }
    }

    Shiny.addCustomMessageHandler('annotationsParsed', function(message) {
      var touched = {};
      (message.results || []).forEach(function(r) {
        var v = viewers[r.containerId];
        if (!v || !v.annotationGroups[r.groupIndex]) return;
        v.annotationGroups[r.groupIndex].polygons = r.polygons;
        touched[r.containerId] = true;
      });
      Object.keys(touched).forEach(function(cid) { redrawAnnotations(cid); });
    });

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
          annotationGroups: imgSpec.annotationGroups || [], // {label, url, refWidth, polygons: null}
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