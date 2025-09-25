library(shiny)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
library(plotly)
library(RColorBrewer)
library(mongolite)

# Source your MSI processing and clustering functions
source("Shiny_Clustering.R")

options(shiny.maxRequestSize = 500*1024^2)

# --- MongoDB connection ---
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
                         multiple = TRUE,
                         accept = c(".imzML", ".ibd")),
               
               fileInput("histology", "Optional: Upload histology image",
                         accept = c(".png", ".jpg", ".jpeg", ".tif")),
               
               numericInput("clusters", "Number of clusters:", 
                            value = 3, min = 2, max = 30),
               
               selectInput("method", "Clustering method:",
                           choices = c("K-means", "Hierarchical")),
               
               actionButton("run", "Run Clustering"),
               
               selectInput("orientation", "Orientation adjustment:",
                           choices = c("Default", "Swap axes", "Flip X", "Flip Y", "Flip Both")),
               
               textInput("class_label", "Assign Class:", value = "Class1"),
               actionButton("assign_class", "Assign to Selection"),
               actionButton("assign_all", "Assign ALL unassigned"),
               
               width = 2
             ),
             
             mainPanel(
               uiOutput("cluster_layout"),
               width = 10
             )
           )
  ),
  
  tabPanel("Prediction",
           h3("Prediction page"),
           p("This is where tissue classification or other predictions could be implemented.")
  )
)

server <- function(input, output, session) {
  
  # uploaded file paths
  uploaded_paths <- reactiveVal(NULL)
  
  observeEvent(input$msi_files, {
    req(input$msi_files)
    imzml_path <- input$msi_files$datapath[grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)]
    ibd_path   <- input$msi_files$datapath[grepl("\\.ibd$", input$msi_files$name, ignore.case = TRUE)]
    
    validate(
      need(length(imzml_path) == 1, "Please upload one .imzML file"),
      need(length(ibd_path) == 1, "Please upload one .ibd file")
    )
    
    uploaded_paths(list(imzml = imzml_path, ibd = ibd_path))
  })
  
  # clustering + feature extraction
  clustered_data <- eventReactive(input$run, {
    paths <- uploaded_paths()
    req(paths)
    
    message("Preprocessing + clustering now running...")
    df <- process_msi_files(paths$imzml, paths$ibd, ref_mz_path = "ref_mz.csv")
    
    if (input$method == "K-means") {
      df <- run_kmeans(df, k = input$clusters)
    } else {
      df <- run_hclust(df, k = input$clusters)
    }
    
    # --- Add dataset_id + run_id ---
    dataset_id <- paste0("dataset_", as.integer(Sys.Date()))
    df$dataset_id <- dataset_id
    df$run_id <- df$runNames
    df$runNames <- NULL
    
    # Ensure Class column exists
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    
    # Reorder columns
    mz_cols <- grep("^mz_", colnames(df), value = TRUE)
    df <- df[, c("dataset_id", "run_id", "x", "y", mz_cols, "cluster", "Class")]
    
    df
  })
  
  # histology zoom ranges
  histology_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$histology_brush, {
    histology_ranges$x <- c(input$histology_brush$xmin, input$histology_brush$xmax)
    histology_ranges$y <- c(input$histology_brush$ymin, input$histology_brush$ymax)
  })
  
  observeEvent(input$reset_zoom, {
    histology_ranges$x <- NULL
    histology_ranges$y <- NULL
  })
  
  # annotated data (reactive copy)
  annotated_data <- reactiveVal(NULL)
  
  observeEvent(input$run, { 
    annotated_data(NULL)
    class_colors(c("Unassigned" = "grey80"))
    next_color_i(1)
  })
  
  # --- Assign ALL unassigned ---
  observeEvent(input$assign_all, {
    df <- annotated_data() %||% clustered_data()
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    
    n_unassigned <- sum(is.na(df$Class))
    if (n_unassigned > 0) {
      df$Class[is.na(df$Class)] <- input$class_label
      annotated_data(df)
      
      # assign new color if needed
      cols <- class_colors()
      lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i()
        cols[lab] <- my_palette[i]
        class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      
      # --- Save full annotated dataset to MongoDB ---
      msi_con$insert(df)
      
      showNotification(
        sprintf("Assigned '%s' to %d previously unassigned pixels. Data saved to MongoDB.",
                input$class_label, n_unassigned),
        type = "message", duration = 3
      )
    } else {
      showNotification("No unassigned pixels left.", type = "warning", duration = 3)
    }
  })
  
  # --- Assign selected pixels ---
  observeEvent(input$assign_class, {
    sel <- event_data("plotly_selected", source = "cluster")
    req(sel)
    req("key" %in% names(sel))
    
    idx <- sort(unique(as.integer(sel$key)))
    
    df <- annotated_data() %||% clustered_data()
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class[idx] <- input$class_label
    annotated_data(df)
    
    # assign new color if needed
    cols <- class_colors()
    lab <- input$class_label
    if (!(lab %in% names(cols))) {
      i <- next_color_i()
      cols[lab] <- my_palette[i]
      class_colors(cols)
      next_color_i(if (i == length(my_palette)) 1 else i + 1)
    }
    
    # --- Save full annotated dataset to MongoDB ---
    msi_con$insert(df)
    
    showNotification(
      sprintf("Assigned '%s' to %d pixels. Data saved to MongoDB.",
              input$class_label, length(idx)),
      type = "message", duration = 3
    )
  })
  
  # --- color palette ---
  my_palette <- c(
    "steelblue","seagreen","darkorange","darkviolet","gold","tan", "red","blue","green","orange","purple","brown","pink","cyan","magenta","yellow",
    "darkred","darkblue","darkgreen", "gray20","gray50",
    "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
    "khaki"
  )
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  
  # --- layout of cluster + class plots ---
  output$cluster_layout <- renderUI({
    req(clustered_data())
    if (is.null(input$histology)) {
      fluidRow(
        column(6, plotlyOutput("cluster_plot", height = "600px")),
        column(6, plotlyOutput("class_plot", height = "600px"))
      )
    } else {
      fluidRow(
        column(
          6,
          plotOutput("histology_plot", height = "600px",
                     brush = brushOpts(id = "histology_brush", resetOnNew = TRUE)),
          actionButton("reset_zoom", "Reset Zoom")
        ),
        column(
          6,
          plotlyOutput("cluster_plot", height = "600px"),
          plotlyOutput("class_plot", height = "600px")
        )
      )
    }
  })
  
  # --- histology plot ---
  output$histology_plot <- renderPlot({
    req(input$histology)
    img <- png::readPNG(input$histology$datapath)
    dims <- dim(img)
    
    x_range <- histology_ranges$x %||% c(0, dims[2])
    y_range <- histology_ranges$y %||% c(0, dims[1])
    
    ggplot() +
      annotation_raster(img, xmin = 0, xmax = dims[2], ymin = 0, ymax = dims[1]) +
      coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE) +
      theme_void()
  })
  
  # --- clustering plot ---
  output$cluster_plot <- renderPlotly({
    df <- clustered_data()
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") {
      tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") {
      df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") {
      df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") {
      df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot
    }
    df$row_id <- seq_len(nrow(df))
    
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = factor(cluster))) +
      geom_tile(width = 1, height = 1) +
      coord_equal() +
      theme_minimal() +
      guides(fill = "none") +
      labs(title = "MSI Clustering Result")
    
    fig <- ggplotly(g, tooltip = NULL, dynamicTicks = FALSE, source = "cluster")
    fig <- event_register(fig, "plotly_selected")
    
    fig %>%
      add_markers(
        data = df, x = ~x_plot, y = ~y_plot,
        key = ~row_id, opacity = 0.01, hoverinfo = "skip",
        marker = list(symbol = "square", size = 8),
        showlegend = FALSE, inherit = FALSE
      )
  })
  
  # --- class plot ---
  output$class_plot <- renderPlotly({
    df <- annotated_data() %||% clustered_data()
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
    
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") {
      tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") {
      df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") {
      df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") {
      df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot
    }
    
    present <- unique(df$Class_plot)
    cols <- class_colors()
    cols_used <- cols[present]; names(cols_used) <- present
    
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = Class_plot)) +
      geom_tile(width = 1, height = 1) +
      scale_fill_manual(values = cols_used, drop = FALSE) +
      coord_equal() +
      theme_minimal() +
      labs(fill = "Class", title = "User Annotation Result")
    
    ggplotly(g, tooltip = "fill")
  })
}

shinyApp(ui = ui, server = server)
