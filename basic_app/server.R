library(shiny)
library(xml2)

# HALO/ImageScope-style LineColor is a decimal-packed BGR integer.
# Convert it to a standard #RRGGBB hex string for use in SVG.
bgr_dec_to_hex <- function(dec) {
  dec <- suppressWarnings(as.integer(dec))
  if (is.na(dec)) return("#FF0000")
  r <- bitwAnd(dec, 255)
  g <- bitwAnd(bitwShiftR(dec, 8), 255)
  b <- bitwAnd(bitwShiftR(dec, 16), 255)
  sprintf("#%02X%02X%02X", r, g, b)
}

# Parse a HALO .annotations XML file into a list of polygons, each with
# normalized "x,y x,y ..." viewport-coordinate point strings ready to hand
# straight to the JS side as an SVG <polygon points="..."> attribute.
#
# `ref_width` is the width of whatever coordinate space the annotation's raw
# X/Y vertices are recorded in — this is usually the FULL-RESOLUTION .svs
# width, NOT the (possibly downsampled) DZI you're displaying. Getting this
# value wrong is what causes an offset/scale mismatch in the overlay.
parse_halo_annotations <- function(ann_url, ref_width, skip_hidden = TRUE,
                                   offset_x = 0, offset_y = 0, scale_factor = 1) {
  doc <- read_xml(ann_url)
  xml_ns_strip(doc)
  
  annotation_nodes <- xml_find_all(doc, "//Annotation")
  if (length(annotation_nodes) == 0) {
    stop("No <Annotation> nodes found — the file's XML schema may differ from what was assumed.")
  }
  
  polygons <- list()
  
  for (ann in annotation_nodes) {
    ann_name  <- xml_attr(ann, "Name")
    visible   <- xml_attr(ann, "Visible")
    color_hex <- bgr_dec_to_hex(xml_attr(ann, "LineColor"))
    
    if (skip_hidden && !is.na(visible) && identical(tolower(visible), "false")) next
    
    regions <- xml_find_all(ann, ".//Region")
    for (reg in regions) {
      verts <- xml_find_all(reg, ".//V")  # HALO uses <V X=".." Y="..">, not <Vertex>
      if (length(verts) < 3) next         # need at least a triangle to draw a polygon
      
      raw_x <- as.numeric(xml_attr(verts, "X"))
      raw_y <- as.numeric(xml_attr(verts, "Y"))
      
      adj_x <- raw_x * scale_factor + offset_x
      adj_y <- raw_y * scale_factor + offset_y
      
      xs <- adj_x / ref_width
      ys <- adj_y / ref_width  # yes, width — OSD viewport convention
      
      points_str <- paste(sprintf("%f,%f", xs, ys), collapse = " ")
      
      polygons[[length(polygons) + 1]] <- list(
        name   = ann_name,
        color  = color_hex,
        points = points_str
      )
    }
  }
  
  polygons
}


function(input, output, session) {
  
  # --- Auto-fill URLs and reference width when a stain is picked ---
  observeEvent(input$stain_select, {
    m <- donor_manifest[[input$stain_select]]
    req(m)
    
    updateTextInput(session, "dzi_url", value = m$primary_dzi)
    updateTextInput(session, "overlay_url", value = if (is.null(m$analysis_dzi)) "" else m$analysis_dzi)
    updateNumericInput(session, "annotation_ref_width", value = m$raw_svs_width)
  }, ignoreNULL = TRUE)
  
  
  observeEvent(input$load_btn, {
    req(input$dzi_url)
    
    session$sendCustomMessage("loadDZI", list(url = input$dzi_url))
    
    if (nzchar(input$overlay_url)) {
      session$sendCustomMessage(
        "loadOverlay",
        list(url = input$overlay_url, opacity = input$overlay_opacity)
      )
    }
    
    if (nzchar(input$annotations_url)) {
      req(input$annotation_ref_width)
      
      polys <- tryCatch(
        parse_halo_annotations(
          input$annotations_url, input$annotation_ref_width,
          offset_x = input$offset_x, offset_y = input$offset_y,
          scale_factor = input$scale_factor
        ),
        error = function(e) {
          showNotification(paste("Could not parse annotations file:", e$message), type = "warning")
          list()
        }
      )
      session$sendCustomMessage("drawAnnotations", list(polygons = polys))
    }
  })
  
  # Re-parse and redraw the annotation overlay only — doesn't touch the
  # image or overlay DZI, so this is fast for iteratively calibrating.
  observeEvent(input$recalc_btn, {
    req(input$annotations_url, nzchar(input$annotations_url), input$annotation_ref_width)
    
    polys <- tryCatch(
      parse_halo_annotations(
        input$annotations_url, input$annotation_ref_width,
        offset_x = input$offset_x, offset_y = input$offset_y,
        scale_factor = input$scale_factor
      ),
      error = function(e) {
        showNotification(paste("Could not parse annotations file:", e$message), type = "warning")
        list()
      }
    )
    session$sendCustomMessage("drawAnnotations", list(polygons = polys))
  })
  
  observeEvent(input$show_annotations, {
    session$sendCustomMessage("toggleAnnotations", list(visible = input$show_annotations))
  })
  
  observeEvent(input$overlay_opacity, {
    req(nzchar(input$overlay_url))
    session$sendCustomMessage("setOverlayOpacity", list(opacity = input$overlay_opacity))
  }, ignoreInit = TRUE)
}