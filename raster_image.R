# App.R

library(shiny)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
library(plotly)
library(RColorBrewer)
library(mongolite)
library(uuid)
library(jsonlite)
library(BiocParallel)

source("processing_clustering.R")
source("Helper_functions.R")

options(shiny.maxRequestSize = 5000*1024^2)
options(shiny.launch.browser = TRUE)


bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)


# Mongo connection
msi_con <- mongo(
  collection = "msi_data",
  db = "msi_project",
  url = "mongodb://localhost"
)



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
               uiOutput("cluster_layout"), width = 10
             )
           )
  ),
  tabPanel("Prediction",
           h3("Prediction page"),
           p("This is where tissue classification or other predictions could be implemented.")
  )
)

server <- function(input, output, session) {
  
  uploaded_paths <- reactiveVal(NULL)
  
  observeEvent(input$msi_files, {
    req(input$msi_files)
    imzml_idx <- grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)
    ibd_idx   <- grepl("\\.ibd$",   input$msi_files$name, ignore.case = TRUE)
    uploaded_paths(list(
      imzml = input$msi_files$datapath[imzml_idx],
      ibd   = input$msi_files$datapath[ibd_idx],
      imzml_name = input$msi_files$name[imzml_idx],
      ibd_name   = input$msi_files$name[ibd_idx]
    ))
  })
  
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    paths <- uploaded_paths(); req(paths)
    if (is.null(processed_data())) {
      withProgress(message = "Running preprocessing…", value = 0, {
        incProgress(0.05, detail = "Preparing inputs")
        df <- tryCatch({
          process_msi_files(
            imzml_path = paths$imzml,
            ibd_path   = paths$ibd,
            imzml_name = paths$imzml_name,
            ref_mz_path = "ref_mz.csv"
          )
        }, error = function(e) {
          showNotification(paste("Processing failed:", conditionMessage(e)),
                           type = "error", duration = 10)
          return(NULL)
        })
        incProgress(1)
        processed_data(df)
      })
    }
  })
  
  observeEvent(input$msi_files, { processed_data(NULL) })
  
  clustered_data <- eventReactive(input$run, {
    df <- processed_data(); req(df)
    withProgress(message = "Clustering…", value = 0, {
      out <- tryCatch({
        if (input$method == "K-means") run_kmeans(df, k = input$clusters)
        else run_hclust(df, k = input$clusters)
      }, error = function(e) {
        showNotification(paste("Clustering failed:", conditionMessage(e)),
                         type = "error", duration = 10)
        return(NULL)
      })
      out
    })
  })
  
  # --- Raster color palette and annotation state ---
  my_palette <- c(
    "purple","orange","steelblue","seagreen","red","blue","green","brown","pink","cyan","magenta","yellow",
    "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
    "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
    "khaki","tan"
  )
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  annotated_data <- reactiveVal(NULL)
  observeEvent(input$run, { annotated_data(NULL); class_colors(c("Unassigned" = "grey80")); next_color_i(1) })
  
  # --- Function: Raster rendering helper ---
  make_raster <- function(df, fill_var, colors) {
    df$x <- df$x - min(df$x) + 1
    df$y <- df$y - min(df$y) + 1
    width <- max(df$x); height <- max(df$y)
    mat <- matrix(NA_character_, nrow = height, ncol = width)
    mat[cbind(df$y, df$x)] <- as.character(df[[fill_var]])
    mat[is.na(mat)] <- "Unassigned"
    col_img <- matrix(colors[mat], nrow = height, ncol = width)
    as.raster(col_img)
  }
  
  # --- Assign all unassigned pixels ---
  observeEvent(input$assign_all, {
    df <- annotated_data() %||% clustered_data(); req(df)
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
  
  # --- Assign selected region via brush ---
  observeEvent(input$assign_class, {
    brush <- input$cluster_brush; req(brush)
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    sel <- df[df$x >= brush$xmin & df$x <= brush$xmax &
                df$y >= brush$ymin & df$y <= brush$ymax, , drop = FALSE]
    if (nrow(sel) > 0) {
      df$Class[df$x %in% sel$x & df$y %in% sel$y] <- input$class_label
      annotated_data(df)
      cols <- class_colors(); lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, nrow(sel)),
                       type = "message", duration = 3)
    } else {
      showNotification("No pixels selected.", type = "warning", duration = 3)
    }
  })
  
  # --- Cluster raster plot ---
  output$cluster_plot <- renderPlot({
    df <- clustered_data(); req(df)
    cols <- setNames(brewer.pal(max(df$cluster), "Set3")[seq_len(max(df$cluster))],
                     as.character(1:max(df$cluster)))
    raster_img <- make_raster(df, "cluster", cols)
    plot(1, type = "n", xlim = c(1, ncol(raster_img)), ylim = c(1, nrow(raster_img)),
         xlab = "x", ylab = "y", main = "MSI Clustering Result")
    rasterImage(raster_img, 1, 1, ncol(raster_img), nrow(raster_img))
  })
  
  # --- Class raster plot ---
  output$class_plot <- renderPlot({
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
    cols <- class_colors(); present <- unique(df$Class_plot)
    cols_used <- cols[present]; names(cols_used) <- present
    raster_img <- make_raster(df, "Class_plot", cols_used)
    plot(1, type = "n", xlim = c(1, ncol(raster_img)), ylim = c(1, nrow(raster_img)),
         xlab = "x", ylab = "y", main = "User Annotation Result")
    rasterImage(raster_img, 1, 1, ncol(raster_img), nrow(raster_img))
  })
  
  # --- Zoom reset support ---
  observeEvent(input$reset_zoom, {
    output$cluster_plot <- renderPlot({
      df <- clustered_data(); req(df)
      cols <- setNames(brewer.pal(max(df$cluster), "Set3")[seq_len(max(df$cluster))],
                       as.character(1:max(df$cluster)))
      raster_img <- make_raster(df, "cluster", cols)
      plot(1, type = "n", xlim = c(1, ncol(raster_img)), ylim = c(1, nrow(raster_img)))
      rasterImage(raster_img, 1, 1, ncol(raster_img), nrow(raster_img))
    })
  })
  
  # --- Layout switching ---
  output$cluster_layout <- renderUI({
    req(clustered_data())
    fluidRow(
      column(6,
             plotOutput("cluster_plot", height = "600px",
                        brush = brushOpts(id = "cluster_brush", resetOnNew = TRUE)),
             actionButton("reset_zoom", "Reset Zoom")
      ),
      column(6, plotOutput("class_plot", height = "600px"))
    )
  })
  
  # --- MongoDB commit ---
  observeEvent(input$commit_db, {
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class <- as.character(df$Class)
    paths <- uploaded_paths()
    assignment_id <- as.character(UUIDgenerate())
    committed_at  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
    df$row_id         <- seq_len(nrow(df))
    df$assignment_id  <- assignment_id
    df$committed_at   <- committed_at
    df$method         <- input$method
    df$k              <- as.integer(input$clusters)
    df$orientation    <- input$orientation
    df$imzml_file     <- if (!is.null(paths)) paths$imzml_name else NA_character_
    df$ibd_file       <- if (!is.null(paths)) paths$ibd_name   else NA_character_
    df$histology_file <- if (!is.null(input$histology)) basename(input$histology$name) else NA_character_
    tryCatch({
      safe_df <- normalize_for_mongo(df)
      msi_con$insert(safe_df)
      showNotification(sprintf("Success: committed %d rows (assignment_id=%s).",
                               nrow(df), assignment_id),
                       type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste0("MongoDB commit failed: ", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })
}


shinyApp(ui = ui, server = server)