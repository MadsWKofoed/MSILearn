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


bp <- MulticoreParam(workers = parallel::detectCores() - 1)
register(bp)

setCardinalParallel(workers = snowWorkers())


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
  
  sel_raw <- reactive(event_data("plotly_selected", source = "cluster"))
  sel <- sel_raw %>% debounce(150)
  
  uploaded_paths <- reactiveVal(NULL)
  
  observeEvent(input$msi_files, {
    req(input$msi_files)
    # Match by extension on the *original names*; keep both paths
    imzml_idx <- grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)
    ibd_idx   <- grepl("\\.ibd$",   input$msi_files$name, ignore.case = TRUE)
    

    
    uploaded_paths(list(
      imzml = input$msi_files$datapath[imzml_idx],
      ibd   = input$msi_files$datapath[ibd_idx],
      imzml_name = input$msi_files$name[imzml_idx],
      ibd_name   = input$msi_files$name[ibd_idx]
    ))
  })
  
  # Preprocessing and saving dataframe in cache
  processed_data <- reactiveVal(NULL)
  
  
  observeEvent(input$run, {
    paths <- uploaded_paths()
    req(paths)
    
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
          showNotification(
            paste("Processing failed:", conditionMessage(e)),
            type = "error", duration = 10
          )
          return(NULL)
        })
        incProgress(1)
        processed_data(df)
      })
    }
  })
  
  
  # Reset the processed data when new files are used
  observeEvent(input$msi_files, {
    processed_data(NULL)  # reset cache
  })
  
  
  # Clustering on the processed data
  clustered_data <- eventReactive(input$run, {
    df <- processed_data()
    req(df)
    
    withProgress(message = "Clustering…", value = 0, {
      out <- tryCatch({
        if (input$method == "K-means") {
          run_kmeans(df, k = input$clusters)
        } else {
          run_hclust(df, k = input$clusters)
        }
      }, error = function(e) {
        showNotification(
          paste("Clustering failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
        return(NULL)
      })
      out
    })
  })
  
  
  
  # Histology zoom ranges
  histology_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$histology_brush, {
    histology_ranges$x <- c(input$histology_brush$xmin, input$histology_brush$xmax)
    histology_ranges$y <- c(input$histology_brush$ymin, input$histology_brush$ymax)
  })
  observeEvent(input$reset_zoom, {
    histology_ranges$x <- NULL; histology_ranges$y <- NULL
  })
  
  # Class assignment storage & palette
  annotated_data <- reactiveVal(NULL)
  observeEvent(input$run, { annotated_data(NULL); class_colors(c("Unassigned" = "grey80")); next_color_i(1) })
  
  my_palette <- c(
    "red","blue","green","orange","purple","brown","pink","cyan","magenta","yellow",
    "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
    "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
    "khaki","steelblue","seagreen","tan"
  )
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  
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
  
  observeEvent(input$assign_class, {
    s <- sel(); req(s); req("key" %in% names(s))
    idx <- sort(unique(as.integer(s$key)))
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
  
  observeEvent(sel(), {
    idx <- as.integer(sel()$key)
    plotlyProxy("cluster_plot", session) %>%
      plotlyProxyInvoke("restyle", list(selectedpoints = list(idx)), list(1))
  })
  
  
  plot_ranges <- reactive({
    df <- clustered_data(); req(df)
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") { tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") { df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") { df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") { df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot }
    list(x = range(df$x_plot, na.rm = TRUE), y = range(df$y_plot, na.rm = TRUE))
  })
  
  output$cluster_layout <- renderUI({
    req(clustered_data())
    if (is.null(input$histology)) {
      fluidRow(
        column(6, plotlyOutput("cluster_plot", height = "600px")),
        column(6, plotlyOutput("class_plot", height = "600px"))
      )
    } else {
      fluidRow(
        column(6, plotOutput("histology_plot", height = "600px",
                             brush = brushOpts(id = "histology_brush", resetOnNew = TRUE)),
               actionButton("reset_zoom", "Reset Zoom")),
        column(6, plotlyOutput("cluster_plot", height = "600px"),
               plotlyOutput("class_plot", height = "600px"))
      )
    }
  })
  
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
  
  output$cluster_plot <- renderPlotly({
    df <- clustered_data(); req(df)
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") { tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") { df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") { df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") { df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot }
    df$row_id <- seq_len(nrow(df))
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = factor(cluster))) +
      geom_tile(width = 1, height = 1) + coord_equal() + theme_minimal() +
      guides(fill = "none") + labs(title = "MSI Clustering Result")
    fig <- ggplotly(g, tooltip = NULL, dynamicTicks = FALSE, source = "cluster")
    fig <- event_register(fig, "plotly_selected")
    fig %>%
      add_markers(
        data = df, x = ~x_plot, y = ~y_plot,
        key = ~row_id, opacity = 0, hoverinfo = "skip",
        marker = list(symbol = "square", size = 6),
        showlegend = FALSE, inherit = FALSE, type = "scattergl"
      ) %>%
      layout(
        dragmode = NULL,
        xaxis = list(range = plot_ranges()$x),
        yaxis = list(range = plot_ranges()$y, scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("hoverClosestCartesian","hoverCompareCartesian",
                                        "toggleSpikelines","toImage"))
  })
  
  output$class_plot <- renderPlotly({
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") { tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") { df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") { df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") { df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot }
    present <- unique(df$Class_plot)
    cols <- class_colors(); cols_used <- cols[present]; names(cols_used) <- present
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = Class_plot)) +
      geom_tile(width = 1, height = 1) +
      scale_fill_manual(values = cols_used, drop = FALSE) +
      coord_equal() + theme_minimal() +
      labs(fill = "Class", title = "User Annotation Result")
    ggplotly(g, tooltip = "fill") %>%
      layout(
        dragmode = "zoom",
        xaxis = list(range = plot_ranges()$x),
        yaxis = list(range = plot_ranges()$y, scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("select2d","lasso2d","hoverClosestCartesian",
                                        "hoverCompareCartesian","toggleSpikelines","toImage"))
  })
  
  # Commit to MongoDB (unchanged logic)
  observeEvent(input$commit_db, {
    df <- annotated_data() %||% clustered_data()
    req(df)
    
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
      ins_count <- nrow(df)
      showNotification(sprintf("Success: committed %d rows (assignment_id=%s).", 
                               ins_count, assignment_id),
                       type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste0("MongoDB commit failed: ", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })
  
  
}

shinyApp(ui = ui, server = server)
