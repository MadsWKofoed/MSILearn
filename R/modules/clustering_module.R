# R/modules/clustering_module.R

clustering_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Clustering",
           sidebarLayout(
             sidebarPanel(
               selectInput(ns("sample_select"), "Select sample:", choices = "Loading..."),
               uiOutput(ns("snr_ui")),
               uiOutput(ns("tol_ui")),
               uiOutput(ns("ref_ui")),
               actionButton(ns("load_dataset"), "Load selected dataset"),
               tags$hr(),
               
               numericInput(ns("clusters"), "Number of clusters:", value = 3, min = 2, max = 30),
               selectInput(ns("method"), "Clustering method:", choices = c("K-means", "Hierarchical")),
               actionButton(ns("run_clustering"), "Run Clustering"),
               tags$hr(),
               
               selectInput(ns("orientation"), "Orientation adjustment:",
                           choices = c("Default", "Swap axes", "Flip X", "Flip Y", "Flip Both")),
               textInput(ns("class_label"), "Assign Class:", value = "Class1"),
               actionButton(ns("assign_class"), "Assign to Selection"),
               actionButton(ns("assign_all"), "Assign ALL unassigned"),
               tags$hr(),
               actionButton(ns("commit_db"), "Commit to MongoDB"),
               width = 2
             ),
             mainPanel(
               uiOutput(ns("cluster_layout")),
               textOutput(ns("status_text")),
               width = 10
             )
           )
  )
}


clustering_module_server <- function(id, msi_con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                       db = "MSI_database", 
                       url = "mongodb://localhost")
    mongo_data <- mongo(collection = "msi_data", 
                       db = "msi_project", 
                       url = "mongodb://localhost")
    
    processed_data <- reactiveVal(NULL)
    clustered_data <- reactiveVal(NULL)
    annotated_data <- reactiveVal(NULL)
    
    # --- Load all unique samples ---
    observe({
      artifacts <- mongo_meta$find(
        query = '{"stage_type": "binned_dataframe"}',
        fields = '{"_id": 0, "sample_name": 1}'
      )
      
      if (nrow(artifacts) == 0) {
        updateSelectInput(session, "sample_select", choices = "No samples found")
      } else {
        samples <- unique(artifacts$sample_name)
        updateSelectInput(session, "sample_select", choices = samples)
      }
    })
    
    # --- Update SNR dropdown when sample is selected ---
    output$snr_ui <- renderUI({
      req(input$sample_select)
      
      artifacts <- query_artifacts(
        sample_name = input$sample_select,
        stage_type = "binned_dataframe"
      )
      
      if (nrow(artifacts) == 0) return(NULL)
      
      snr_values <- unique(artifacts$snr)
      snr_values <- snr_values[!is.na(snr_values)]
      snr_values <- sort(as.numeric(snr_values))
      
      selectInput(ns("snr_select"), "Select SNR:", choices = snr_values)
    })
    
    # --- Update tolerance dropdown when SNR is selected ---
    output$tol_ui <- renderUI({
      req(input$sample_select, input$snr_select)
      
      artifacts <- query_artifacts(
        sample_name = input$sample_select,
        stage_type = "binned_dataframe",
        snr = as.numeric(input$snr_select)
      )
      
      if (nrow(artifacts) == 0) return(NULL)
      
      tol_values <- unique(artifacts$tolerance)
      tol_values <- tol_values[!is.na(tol_values)]
      tol_values <- sort(as.numeric(tol_values))
      
      selectInput(ns("tol_select"), "Select tolerance:", choices = tol_values)
    })
    
    # --- Update reference dropdown when tolerance is selected ---
    output$ref_ui <- renderUI({
      req(input$sample_select, input$snr_select, input$tol_select)
      
      artifacts <- query_artifacts(
        sample_name = input$sample_select,
        stage_type = "binned_dataframe",
        snr = as.numeric(input$snr_select),
        tolerance = as.numeric(input$tol_select)
      )
      
      if (nrow(artifacts) == 0) return(NULL)
      
      ref_names <- unique(artifacts$reference_name)
      ref_names <- ref_names[!is.na(ref_names)]
      
      selectInput(ns("ref_select"), "Select reference:", choices = ref_names)
    })
    
    # --- Load selected dataset ---
    observeEvent(input$load_dataset, {
      req(input$sample_select, input$snr_select, input$tol_select, input$ref_select)
      
      # Disable button during loading
      shinyjs::disable("load_dataset")
      on.exit(shinyjs::enable("load_dataset"))
      
      # Create progress
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Loading dataset...", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      tryCatch({
        # Query exact match
        artifacts <- query_artifacts(
          sample_name = input$sample_select,
          stage_type = "binned_dataframe",
          snr = as.numeric(input$snr_select),
          tolerance = as.numeric(input$tol_select),
          reference_name = input$ref_select
        )
        
        if (nrow(artifacts) == 0) {
          showNotification("No matching dataset found.", type = "error")
          return()
        }
        
        progress$set(value = 30, message = "Loading from database...")
        
        # Load the first match
        gridfs_id <- artifacts$gridfs_id[1]
        df <- load_artifact_by_id(gridfs_id)
        
        progress$set(value = 90, message = "Processing data...")
        
        # Store dataset info for reference
        dataset_info <- paste0(
          "Sample: ", input$sample_select, "\n",
          "SNR: ", input$snr_select, " | ",
          "Tolerance: ", input$tol_select, " | ",
          "Reference: ", input$ref_select, "\n",
          "Dimensions: ", nrow(df), " pixels × ", sum(grepl("^mz_", names(df))), " features"
        )
        
        processed_data(df)
        clustered_data(NULL)
        annotated_data(NULL)
        
        progress$set(value = 100, message = "Complete!")
        
        output$status_text <- renderText(dataset_info)
        
        showNotification(
          paste0("Dataset loaded: ", nrow(df), " pixels × ", 
                sum(grepl("^mz_", names(df))), " features"),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading dataset:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
    
    # --- Run clustering ---
    observeEvent(input$run_clustering, {
      df <- processed_data()
      req(df)
      
      # Disable button during clustering
      shinyjs::disable("run_clustering")
      on.exit(shinyjs::enable("run_clustering"))
      
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Running clustering...", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      tryCatch({
        progress$set(value = 30, 
                    message = paste0("Running ", input$method, " with k=", input$clusters, "..."))
        
        clustered <- if (input$method == "K-means") {
          run_kmeans(df, input$clusters)
        } else {
          run_hclust(df, input$clusters)
        }
        
        progress$set(value = 90, message = "Finalizing...")
        
        clustered_data(clustered)
        annotated_data(NULL)
        
        progress$set(value = 100, message = "Complete!")
        
        output$status_text <- renderText(
          paste0("Clustering complete: ", input$method, " with ", input$clusters, " clusters")
        )
        
        showNotification(
          paste0("Clustering complete: ", input$clusters, " clusters identified"),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Clustering error:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
    
    # --- State and colors ---
    my_palette <- c(
      "red","blue","green","orange","purple","brown","pink","cyan","magenta","yellow",
      "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
      "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
      "khaki","steelblue","seagreen","tan"
    )
    class_colors <- reactiveVal(c("Unassigned" = "grey80"))
    next_color_i <- reactiveVal(1)
    
    # Reset colors when new clustering is run
    observeEvent(input$run_clustering, {
      annotated_data(NULL)
      class_colors(c("Unassigned" = "grey80"))
      next_color_i(1)
    })
    
    # --- Raster helper ---
    make_raster_png <- function(df, fill_var, colors) {
      df$x <- df$x - min(df$x) + 1
      df$y <- df$y - min(df$y) + 1
      width <- max(df$x)
      height <- max(df$y)
      mat <- matrix(NA_character_, nrow = height, ncol = width)
      mat[cbind(df$y, df$x)] <- as.character(df[[fill_var]])
      mat[is.na(mat)] <- "Unassigned"
      col_img <- matrix(colors[mat], nrow = height, ncol = width)
      
      rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
      rgb_array <- array(NA_real_, dim = c(height, width, 4))
      rgb_array[,,1] <- matrix(rgb_vals["red", ], nrow = height, ncol = width)
      rgb_array[,,2] <- matrix(rgb_vals["green", ], nrow = height, ncol = width)
      rgb_array[,,3] <- matrix(rgb_vals["blue", ], nrow = height, ncol = width)
      rgb_array[,,4] <- matrix(rgb_vals["alpha", ], nrow = height, ncol = width)
      
      tmp <- tempfile(fileext = ".png")
      png::writePNG(rgb_array, target = tmp)
      base64enc::dataURI(file = tmp, mime = "image/png")
    }
    
    # --- Selection storage ---
    sel_shape <- reactiveVal(NULL)
    
    # --- Capture drawn shapes ---
# --- Capture drawn shapes (auto-detect completion) ---
observeEvent(event_data("plotly_relayout", source = "cluster"), {
  ev <- event_data("plotly_relayout", source = "cluster")
  req(ev)
  
  # Check if shapes array was updated (shape completed)
  if ("shapes" %in% names(ev)) {
    shapes_list <- ev$shapes
    if (length(shapes_list) > 0) {
      latest_shape <- shapes_list[[length(shapes_list)]]
      
      # Polygon/lasso
      if (!is.null(latest_shape$path)) {
        path <- latest_shape$path
        coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
        xy <- do.call(rbind, strsplit(coords, ","))
        x <- as.numeric(xy[,1])
        y <- as.numeric(xy[,2])
        
        sel_shape(list(type = "polygon", x = x, y = y))
        
        showNotification(
          paste0("✓ Polygon captured (", length(x), " points). Click 'Assign to Selection'."),
          type = "message", 
          duration = 3
        )
      }
      
      # Rectangle
      else if (!is.null(latest_shape$x0)) {
        sel_shape(list(
          type = "rect",
          x0 = latest_shape$x0,
          x1 = latest_shape$x1,
          y0 = latest_shape$y0,
          y1 = latest_shape$y1
        ))
        
        showNotification(
          "✓ Rectangle captured. Click 'Assign to Selection'.",
          type = "message", 
          duration = 3
        )
      }
    }
  }
})
    
    # --- Assign to selection ---
    observeEvent(input$assign_class, {
      shape <- sel_shape()
      if (is.null(shape)) {
        showNotification("No selection drawn. Use Lasso or Rectangle tool.", 
                        type = "warning", duration = 4)
        return()
      }
      
      df <- annotated_data() %||% clustered_data()
      req(df)
      if (!"Class" %in% names(df)) df$Class <- NA_character_
      
      y_max <- max(df$y)
      
      if (identical(shape$type, "polygon")) {
        poly_x <- shape$x
        poly_y <- y_max - shape$y
        inside <- sp::point.in.polygon(df$x, df$y, poly_x, poly_y) > 0
      } else if (identical(shape$type, "rect")) {
        x0 <- shape$x0
        x1 <- shape$x1
        yy0 <- y_max - shape$y0
        yy1 <- y_max - shape$y1
        y0 <- min(yy0, yy1)
        y1 <- max(yy0, yy1)
        inside <- df$x >= x0 & df$x <= x1 & df$y >= y0 & df$y <= y1
      } else {
        showNotification("Unsupported shape type.", type = "error", duration = 4)
        return()
      }
      
      n_sel <- sum(inside)
      if (n_sel == 0) {
        showNotification("No pixels in selection.", type = "warning", duration = 4)
        return()
      }
      
      df$Class[inside] <- input$class_label
      annotated_data(df)
      
      # Update colors
      cols <- class_colors()
      lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i()
        cols[lab] <- my_palette[i]
        class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      
      # Clear shapes
      try({
        plotlyProxy("cluster_plot", session) %>%
          plotlyProxyInvoke("relayout", list(shapes = list()))
      }, silent = TRUE)
      sel_shape(NULL)
      
      showNotification(
        sprintf("Assigned '%s' to %d pixels.", input$class_label, n_sel),
        type = "message", duration = 3
      )
    })
    
    # --- Assign all unassigned ---
    observeEvent(input$assign_all, {
      df <- annotated_data() %||% clustered_data()
      req(df)
      if (!"Class" %in% names(df)) df$Class <- NA_character_
      n_unassigned <- sum(is.na(df$Class))
      
      if (n_unassigned > 0) {
        df$Class[is.na(df$Class)] <- input$class_label
        annotated_data(df)
        
        cols <- class_colors()
        lab <- input$class_label
        if (!(lab %in% names(cols))) {
          i <- next_color_i()
          cols[lab] <- my_palette[i]
          class_colors(cols)
          next_color_i(if (i == length(my_palette)) 1 else i + 1)
        }
        
        showNotification(
          sprintf("Assigned '%s' to %d pixels.", input$class_label, n_unassigned),
          type = "message", duration = 3
        )
      } else {
        showNotification("No unassigned pixels.", type = "warning", duration = 3)
      }
    })
    
    # --- Cluster plot ---
    output$cluster_plot <- renderPlotly({
      df <- clustered_data()
      req(df)
      
      cols <- setNames(
        brewer.pal(max(df$cluster), "Set3")[seq_len(max(df$cluster))],
        as.character(1:max(df$cluster))
      )
      img_uri <- make_raster_png(df, "cluster", cols)
      
      plot_ly(source = "cluster") %>%
        layout(
          images = list(list(
            source = img_uri,
            xref = "x", yref = "y",
            x = 0, y = max(df$y),
            sizex = max(df$x), sizey = max(df$y),
            sizing = "stretch", layer = "below"
          )),
          dragmode = "drawclosedpath",
          newshape = list(line = list(color = "black", width = 1),
                         fillcolor = "rgba(0,0,0,0.05)"),
          title = "MSI Clustering Result",
          xaxis = list(range = c(0, max(df$x)), title = "x"),
          yaxis = list(range = c(0, max(df$y)), title = "y",
                      scaleanchor = "x", scaleratio = 1)
        ) %>%
        event_register("plotly_relayout") %>%  # Add this line
        config(
          displaylogo = FALSE,
          modeBarButtonsToAdd = list("drawclosedpath", "drawrect", "eraseshape"),
          modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", 
                                    "toggleSpikelines", "toImage")
        )
    })
    
    # --- Class plot ---
    output$class_plot <- renderPlotly({
      df <- annotated_data() %||% clustered_data()
      req(df)
      if (!"Class" %in% names(df)) df$Class <- NA_character_
      df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
      
      cols <- class_colors()
      present <- unique(df$Class_plot)
      cols_used <- cols[present]
      names(cols_used) <- present
      
      img_uri <- make_raster_png(df, "Class_plot", cols_used)
      
      plot_ly() %>%
        layout(
          images = list(list(
            source = img_uri,
            xref = "x", yref = "y",
            x = 0, y = max(df$y),
            sizex = max(df$x), sizey = max(df$y),
            sizing = "stretch", layer = "below"
          )),
          title = "User Annotation Result",
          xaxis = list(range = c(0, max(df$x)), title = "x"),
          yaxis = list(range = c(0, max(df$y)), title = "y",
                      scaleanchor = "x", scaleratio = 1)
        ) %>%
        config(displaylogo = FALSE)
    })
    
    # --- Layout ---
    output$cluster_layout <- renderUI({
      req(clustered_data())
      fluidRow(
        column(6, plotlyOutput(ns("cluster_plot"), height = "600px")),
        column(6, plotlyOutput(ns("class_plot"), height = "600px"))
      )
    })
    
    # --- Commit to MongoDB ---
    observeEvent(input$commit_db, {
      df <- annotated_data() %||% clustered_data()
      req(df)
      
      if (!"Class" %in% names(df)) df$Class <- NA_character_
      df$Class <- as.character(df$Class)
      
      assignment_id <- as.character(UUIDgenerate())
      committed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
      
      df$row_id <- seq_len(nrow(df))
      df$assignment_id <- assignment_id
      df$committed_at <- committed_at
      df$method <- input$method
      df$k <- as.integer(input$clusters)
      df$orientation <- input$orientation
      df$sample_name <- input$sample_select
      df$snr_used <- as.numeric(input$snr_select)
      df$tolerance_used <- as.numeric(input$tol_select)
      df$reference_used <- input$ref_select
      
      tryCatch({
        safe_df <- normalize_for_mongo(df)
        msi_con$insert(safe_df)
        
        showNotification(
          sprintf("Success: committed %d rows (assignment_id=%s).", nrow(df), assignment_id),
          type = "message", duration = 6
        )
      }, error = function(e) {
        showNotification(
          paste0("MongoDB commit failed: ", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })
  })
}