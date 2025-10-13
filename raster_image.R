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
  
  # --- State and color palette ---
  my_palette <- c(
    "red","blue","green","orange","purple","brown","pink","cyan","magenta","yellow",
    "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
    "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
    "khaki","steelblue","seagreen","tan"
  )
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  annotated_data <- reactiveVal(NULL)
  observeEvent(input$run, { annotated_data(NULL); class_colors(c("Unassigned" = "grey80")); next_color_i(1) })
  
  # --- Function: create raster -> base64 PNG ---
  make_raster_image <- function(df, fill_var, colors) {
    df$x <- df$x - min(df$x) + 1
    df$y <- df$y - min(df$y) + 1
    width <- max(df$x); height <- max(df$y)
    mat <- matrix(NA_character_, nrow = height, ncol = width)
    mat[cbind(df$y, df$x)] <- as.character(df[[fill_var]])
    mat[is.na(mat)] <- "Unassigned"
    col_img <- matrix(colors[mat], nrow = height, ncol = width)
    raster_img <- as.raster(col_img)
    tmp <- tempfile(fileext = ".png")
    png::writePNG(raster_img, tmp)
    base64enc::dataURI(file = tmp, mime = "image/png")
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
  
  # --- Assign selected via lasso/box selection ---
  observeEvent(input$assign_class, {
    sel <- event_data("plotly_selected", source = "cluster"); req(sel)
    if (!"key" %in% names(sel)) return()
    idx <- sort(unique(as.integer(sel$key)))
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class[idx] <- input$class_label
    annotated_data(df)
    cols <- class_colors(); lab <- input$class_label
    if (!(lab %in% names(cols))) {
      i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
      next_color_i(if (i == length(my_palette)) 1 else i + 1)
    }
    showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, length(idx)),
                     type = "message", duration = 3)
  })
  
  # --- Cluster plot (raster background + invisible points for selection) ---
  output$cluster_plot <- renderPlotly({
    df <- clustered_data(); req(df)
    df$row_id <- seq_len(nrow(df))
    cols <- setNames(brewer.pal(max(df$cluster), "Set3")[seq_len(max(df$cluster))],
                     as.character(1:max(df$cluster)))
    img_uri <- make_raster_image(df, "cluster", cols)
    
    fig <- plot_ly(source = "cluster") %>%
      layout(
        images = list(
          list(
            source = img_uri,
            xref = "x", yref = "y",
            x = 0, y = max(df$y),
            sizex = max(df$x), sizey = max(df$y),
            sizing = "stretch", layer = "below"
          )
        ),
        title = "MSI Clustering Result",
        xaxis = list(range = c(0, max(df$x)), title = "x"),
        yaxis = list(range = c(0, max(df$y)), title = "y", scaleanchor = "x", scaleratio = 1)
      ) %>%
      add_markers(
        data = df, x = ~x, y = ~y, key = ~row_id,
        opacity = 0.01, hoverinfo = "skip", showlegend = FALSE,
        marker = list(size = 4, symbol = "square", color = "transparent")
      ) %>%
      config(displaylogo = FALSE)
    event_register(fig, "plotly_selected")
  })
  
  # --- Class plot (raster only, reactive to annotation) ---
  output$class_plot <- renderPlotly({
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
    cols <- class_colors(); present <- unique(df$Class_plot)
    cols_used <- cols[present]; names(cols_used) <- present
    img_uri <- make_raster_image(df, "Class_plot", cols_used)
    
    plot_ly() %>%
      layout(
        images = list(
          list(
            source = img_uri,
            xref = "x", yref = "y",
            x = 0, y = max(df$y),
            sizex = max(df$x), sizey = max(df$y),
            sizing = "stretch", layer = "below"
          )
        ),
        title = "User Annotation Result",
        xaxis = list(range = c(0, max(df$x)), title = "x"),
        yaxis = list(range = c(0, max(df$y)), title = "y", scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("select2d","lasso2d","hoverClosestCartesian",
                                        "hoverCompareCartesian","toggleSpikelines","toImage"))
  })
  
  # --- Layout UI (cluster + class) ---
  output$cluster_layout <- renderUI({
    req(clustered_data())
    fluidRow(
      column(6, plotlyOutput("cluster_plot", height = "600px")),
      column(6, plotlyOutput("class_plot", height = "600px"))
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