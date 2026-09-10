library(shiny)
library(bslib)
library(shinycssloaders)

tagList(
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/openseadragon@4/build/openseadragon/openseadragon.min.js")
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
                 selectInput("home_region", "Region", choices = with_placeholder(character(0))),
                 selectInput("home_stain", "Stain", choices = with_placeholder(character(0))),
                 tags$hr(),
                 # unchecked by default -> the opacity slider below stays hidden until turned on
                 checkboxInput("home_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.home_show_overlay",
                   sliderInput("home_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("home_load_btn", "Load", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 withSpinner(uiOutput("home_annotation_ui"), type = 5),
                 withSpinner(uiOutput("home_viewer_grid"), type = 5)
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
                 selectInput("dstain_region", "Region", choices = with_placeholder(character(0))),
                 selectInput("dstain_stains", "Stains to compare", choices = character(0), multiple = TRUE),
                 tags$hr(),
                 checkboxInput("dstain_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.dstain_show_overlay",
                   sliderInput("dstain_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("dstain_load_btn", "Load / Compare", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("dstain_context_card"),   # only appears once Load has been clicked
                 tags$div(style = "height:18px;"),  # space between the card and the annotation checkboxes
                 withSpinner(uiOutput("dstain_annotation_ui"), type = 5),
                 withSpinner(uiOutput("dstain_viewer_grid"), type = 5)
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
                   build_metadata_accordion("sdonor", donor_metadata)
                 ),
                 tags$hr(),
                 checkboxInput("sdonor_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.sdonor_show_overlay",
                   sliderInput("sdonor_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("sdonor_load_btn", "Load / Compare", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("sdonor_context_card"),
                 tags$div(style = "height:18px;"),
                 withSpinner(uiOutput("sdonor_annotation_ui"), type = 5),
                 withSpinner(uiOutput("sdonor_viewer_grid"), type = 5)
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
                 selectInput("sregion_stain", "Stain", choices = with_placeholder(character(0))),
                 selectInput("sregion_regions", "Regions to compare", choices = character(0), multiple = TRUE),
                 tags$hr(),
                 checkboxInput("sregion_show_overlay", "Show mask/analysis overlay", value = FALSE),
                 conditionalPanel(
                   condition = "input.sregion_show_overlay",
                   sliderInput("sregion_overlay_opacity", "Overlay opacity", min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 actionButton("sregion_load_btn", "Load / Compare", class = "btn-primary")
               ),
               mainPanel(
                 width = 8,
                 uiOutput("sregion_context_card"),
                 tags$div(style = "height:18px;"),
                 withSpinner(uiOutput("sregion_annotation_ui"), type = 5),
                 withSpinner(uiOutput("sregion_viewer_grid"), type = 5)  # shows a validation error here if nothing matches
               )
             )
    ),
    
    # =========================================================================
    # about — freely editable. no full metadata table (summary only).
    # =========================================================================
    tabPanel("About",
             fluidPage(
               h3("About this viewer"),
               
               h4("Usage"),
               p("Pick a donor, region, and stain on the Home tab to view a single slide, ",
                 "or use one of the Compare tabs to view several images side by side. ",
                 "Annotation checkboxes only fetch their underlying file the first time ",
                 "they're switched on."),
               
               h4("Contact"),
               p("EDIT ME: name / email / team distribution list for questions about this viewer."),
               
               h4("Acknowledgements"),
               p("EDIT ME: funding sources, consortia, data-generating labs, etc."),
               
               h4("Specimen metadata"),
               p("Loaded from: ", tags$code(specimen_metadata_csv_path)),
               textOutput("about_metadata_summary"),
               br(),
               downloadButton("about_download_metadata", "Download specimen metadata CSV")
             )
    )
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
        if (!group.polygons) return; // not fetched yet — nothing to draw
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

    // one master checkbox per unique annotation label toggles that label across
    // every currently-loaded image with a matching file; unfetched groups get
    // batched into one request to the server (lazy loading).
    function toggleAnnotationLabel(label, visible) {
      var pending = [];

      Object.keys(viewers).forEach(function(containerId) {
        var v = viewers[containerId];
        (v.annotationGroups || []).forEach(function(group, gi) {
          if (group.label !== label) return;
          v.hiddenGroups[gi] = !visible;

          if (visible && !group.polygons) {
            pending.push({ containerId: containerId, groupIndex: gi, url: group.url, refWidth: group.refWidth, label: label });
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

    // server has finished parsing a batch of annotation files — store the polygons and redraw.
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