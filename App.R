# app.R
# MSI Clustering + Interactive Annotation (Leaflet-based)
# ────────────────────────────────────────────────────────────────

library(shiny)
library(ggplot2)
library(png)
library(grid)
library(RColorBrewer)
library(mongolite)
library(uuid)
library(jsonlite)
library(BiocParallel)
library(Cardinal)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)

source("processing_clustering.R")
source("Helper_functions.R")

options(shiny.maxRequestSize = 5000 * 1024^2)
options(shiny.launch.browser = TRUE)

# Parallel backend
bp <- workers = parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# Mongo connection
msi_con <- mongo(
  collection = "msi_data",
  db = "msi_project",
  url = "mongodb://localhost"
)

# ────────────────────────────────────────────────────────────────
# UI
# ────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "MSI Clustering & Prediction",
  tabPanel("Welcome",
           h3("Welcome to the MSI Clustering App"),
           p("Upload imzML + ibd files, perform clustering, and compare to histology.")
  ),
  tabPanel("Clustering",
           sidebarLayout(
             sidebarPanel(
               fileInput("msi_files", "Upload imzML + ibd files",
                         multiple = TRUE, accept = c(".imzML", ".ibd")),
               fileInput("histology", "Optional: Upload histology image",
                         accept = c(".png", ".jpg", ".jpeg", ".tif")),
               numericInput("clusters", "Number of clusters:", value = 3, min = 2, max = 30),
               selectInput("method", "Clustering method:", choices = c("K-means", "Hierarchical")),
               actionButton("run", "Run Clustering"),
               selectInput("orientation", "Orientation adjustment:",
                           choices = c("Default", "Swap axes", "Flip X", "Flip Y", "Flip Both")),
               textInput("class_label", "Assign Class:", value = "Class1"),
               actionButton("assign_class", "Assign to Selection"),
               actionButton("assign_all", "Assign ALL unassigned"),
               tags$hr(),
               actionButton("commit_db", "Commit to MongoDB"),
               width = 2
             ),
             mainPanel(
               leafletOutput("msi_leaflet", height = "700px"),
               width = 10
             )
           )
  ),
  tabPanel("Prediction",
           h3("Prediction page"),
           p("This is where tissue classification or other predictions could be implemented.")
  )
)

# ────────────────────────────────────────────────────────────────
# SERVER
# ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive storage
  uploaded_paths <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  clustered_data <- reactiveVal(NULL)
  annotated_data <- reactiveVal(NULL)
  drawn_polygon <- reactiveVal(NULL)
  
  # Color palette
  my_palette <- c(
    "red", "blue", "green", "orange", "purple", "brown", "pink", "cyan", "magenta", "yellow",
    "darkred", "darkblue", "darkgreen", "darkorange", "darkviolet", "gold", "gray20", "gray50",
    "deepskyblue", "springgreen", "navy", "maroon", "olive", "turquoise", "orchid", "salmon",
    "khaki", "steelblue", "seagreen", "tan"
  )
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  
  # Upload handling
  observeEvent(input$msi_files, {
    req(input$msi_files)
    imzml_idx <- grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)
    ibd_idx <- grepl("\\.ibd$", input$msi_files$name, ignore.case = TRUE)
    
    uploaded_paths(list(
      imzml = input$msi_files$datapath[imzml_idx],
      ibd = input$msi_files$datapath[ibd_idx],
      imzml_name = input$msi_files$name[imzml_idx],
      ibd_name = input$msi_files$name[ibd_idx]
    ))
    processed_data(NULL)
    clustered_data(NULL)
  })
  
  # Processing (only once per upload)
  observeEvent(input$run, {
    paths <- uploaded_paths(); req(paths)
    
    if (is.null(processed_data())) {
      withProgress(message = "Processing MSI files…", value = 0, {
        df <- process_msi_files(
          imzml_path = paths$imzml,
          ibd_path = paths$ibd,
          imzml_name = paths$imzml_name,
          ref_mz_path = "ref_mz.csv"
        )
        processed_data(df)
      })
    }
  })
  
  # Clustering
  observeEvent(input$run, {
    df <- processed_data(); req(df)
    withProgress(message = "Running clustering…", value = 0, {
      if (input$method == "K-means") {
        df <- run_kmeans(df, k = input$clusters)
      } else {
        df <- run_hclust(df, k = input$clusters)
      }
      df$row_id <- seq_len(nrow(df))
      clustered_data(df)
      annotated_data(df)
    })
  })
  
  # ─────────────────────────────
  # LEAFLET DISPLAY + SELECTION
  # ─────────────────────────────
  output$msi_leaflet <- renderLeaflet({
    df <- clustered_data(); req(df)
    cols <- brewer.pal(max(df$cluster), "Set3")
    
    leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>%
      addTiles() %>%
      addRasterImage(
        matrix(df$cluster, nrow = length(unique(df$y)), ncol = length(unique(df$x))),
        colors = cols, opacity = 0.8
      ) %>%
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        circleMarkerOptions = FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = "black"))
      ) %>%
      addLayersControl(overlayGroups = "draw", options = layersControlOptions(collapsed = FALSE))
  })
  
  # Capture drawn polygons
  observeEvent(input$msi_leaflet_draw_new_feature, {
    feature <- input$msi_leaflet_draw_new_feature
    coords <- feature$geometry$coordinates[[1]]
    poly <- st_polygon(list(do.call(rbind, coords))) %>% st_sfc(crs = 4326)
    drawn_polygon(poly)
  })
  
  # Assign class to polygon selection
  observeEvent(input$assign_class, {
    df <- annotated_data(); req(df)
    poly <- drawn_polygon(); req(poly)
    
    # Create sf points
    pts <- st_as_sf(df, coords = c("x", "y"), crs = 4326)
    inside <- lengths(st_within(pts, poly)) > 0
    
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class[inside] <- input$class_label
    annotated_data(df)
    
    cols <- class_colors(); lab <- input$class_label
    if (!(lab %in% names(cols))) {
      i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
      next_color_i(if (i == length(my_palette)) 1 else i + 1)
    }
    
    showNotification(
      sprintf("Assigned '%s' to %d pixels.", input$class_label, sum(inside)),
      type = "message", duration = 3
    )
  })
  
  # Assign all unassigned
  observeEvent(input$assign_all, {
    df <- annotated_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    n_unassigned <- sum(is.na(df$Class))
    if (n_unassigned > 0) {
      df$Class[is.na(df$Class)] <- input$class_label
      annotated_data(df)
      cols <- class_colors(); lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, n_unassigned),
                       type = "message", duration = 3)
    } else {
      showNotification("No unassigned pixels left.", type = "warning", duration = 3)
    }
  })
  
  # Commit to MongoDB
  observeEvent(input$commit_db, {
    df <- annotated_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class <- as.character(df$Class)
    
    paths <- uploaded_paths()
    assignment_id <- as.character(UUIDgenerate())
    committed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
    
    df$assignment_id <- assignment_id
    df$committed_at <- committed_at
    df$method <- input$method
    df$k <- as.integer(input$clusters)
    df$orientation <- input$orientation
    df$imzml_file <- if (!is.null(paths)) paths$imzml_name else NA_character_
    df$ibd_file <- if (!is.null(paths)) paths$ibd_name else NA_character_
    df$histology_file <- if (!is.null(input$histology)) basename(input$histology$name) else NA_character_
    
    tryCatch({
      safe_df <- normalize_for_mongo(df)
      msi_con$insert(safe_df)
      showNotification(sprintf("Committed %d rows (assignment_id=%s).",
                               nrow(df), assignment_id),
                       type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste0("MongoDB commit failed: ", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })
}

shinyApp(ui = ui, server = server)
