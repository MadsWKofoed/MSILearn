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
    mongo_cluster_meta <- mongo(collection = "clustering_metadata",
                               db = "MSI_database",
                               url = "mongodb://localhost")
    mongo_data <- mongo(collection = "msi_data", 
                       db = "msi_project", 
                       url = "mongodb://localhost")
    
    processed_data <- reactiveVal(NULL)
    clustered_data <- reactiveVal(NULL)
    annotated_data <- reactiveVal(NULL)
    original_clustered <- reactiveVal(NULL)
    
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
        
        # Store original before any transformations
        original_clustered(clustered)
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
    
    # --- Apply orientation adjustment ---
    observe({
      req(input$orientation)
      
      # Get base data
      base_df <- original_clustered()
      req(base_df)
      
      if (input$orientation == "Default") {
        isolate({
          clustered_data(base_df)
          annotated_data(NULL)
          class_colors(c())
          next_color_i(1)
        })
        return()
      }
      
      # Apply transformation to base data
      df_adjusted <- base_df
      
      if (input$orientation == "Swap axes") {
        temp <- df_adjusted$x
        df_adjusted$x <- df_adjusted$y
        df_adjusted$y <- temp
      } else if (input$orientation == "Flip X") {
        df_adjusted$x <- max(base_df$x) - df_adjusted$x + min(base_df$x)
      } else if (input$orientation == "Flip Y") {
        df_adjusted$y <- max(base_df$y) - df_adjusted$y + min(base_df$y)
      } else if (input$orientation == "Flip Both") {
        df_adjusted$x <- max(base_df$x) - df_adjusted$x + min(base_df$x)
        df_adjusted$y <- max(base_df$y) - df_adjusted$y + min(base_df$y)
      }
      
      # Use isolate to prevent triggering observer when updating
      isolate({
        clustered_data(df_adjusted)
        annotated_data(NULL)
        class_colors(c())
        next_color_i(1)
      })
    }) %>% bindEvent(input$orientation)


    # --- State and colors ---
    my_palette <- c(
      "red","blue","orange", "lightgreen","mediumpurple","brown","pink","cyan","magenta","yellow",
      "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
      "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
      "khaki","steelblue","seagreen","tan"
    )
    class_colors <- reactiveVal(c())
    next_color_i <- reactiveVal(1)
    
    # Reset colors when new clustering is run
    observeEvent(input$run_clustering, {
      annotated_data(NULL)
      class_colors(c())
      next_color_i(1)
    })
    
# --- Raster helper for both cluster and class plots (transparent background) ---
make_raster_png <- function(df, fill_var, colors) {
  df$x <- df$x - min(df$x) + 1
  df$y <- df$y - min(df$y) + 1
  width <- max(df$x)
  height <- max(df$y)
  
  # Create EMPTY matrix (NA = transparent)
  mat <- matrix(NA_character_, nrow = height, ncol = width)
  
  # Flip y-coordinates: (height - y + 1) converts bottom-up to top-down
  mat[cbind(height - df$y + 1, df$x)] <- as.character(df[[fill_var]])
  
  col_img <- matrix(colors[mat], nrow = height, ncol = width)
  
  rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
  rgb_array <- array(NA_real_, dim = c(height, width, 4))
  rgb_array[,,1] <- matrix(rgb_vals["red", ], nrow = height, ncol = width)
  rgb_array[,,2] <- matrix(rgb_vals["green", ], nrow = height, ncol = width)
  rgb_array[,,3] <- matrix(rgb_vals["blue", ], nrow = height, ncol = width)
  rgb_array[,,4] <- matrix(rgb_vals["alpha", ], nrow = height, ncol = width)
  
  # Make NA pixels transparent
  na_pixels <- is.na(mat)
  rgb_array[,,4][na_pixels] <- 0
  
  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgb_array, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}


    
    # --- Selection storage ---
    sel_shape <- reactiveVal(NULL)
    
    
    # --- Capture drawn shapes (auto-detect completion) ---
    observeEvent(event_data("plotly_relayout", source = "cluster"), {
      ev <- event_data("plotly_relayout", source = "cluster")
      req(ev)
      
      # Method 1: Direct path key (works after editing)
      if (any(grepl("shapes\\[\\d+\\]\\.path$", names(ev)))) {
        path_key <- grep("shapes\\[\\d+\\]\\.path$", names(ev), value = TRUE)[1]
        path <- ev[[path_key]]
        
        if (!is.null(path) && nchar(path) > 0) {
          coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
          xy <- do.call(rbind, strsplit(coords, ","))
          x <- as.numeric(xy[,1])
          y <- as.numeric(xy[,2])
          
          sel_shape(list(type = "polygon", x = x, y = y))
          
          showNotification(
            paste0("✓ Polygon captured (", length(x), " points)"),
            type = "message", 
            duration = 2
          )
        }
        return()
      }
      
      # Method 2: shapes array (works on first draw)
      if ("shapes" %in% names(ev)) {
        shapes_data <- ev$shapes
        
        # Check if it's a data.frame (from first event)
        if (is.data.frame(shapes_data) && nrow(shapes_data) > 0) {
          last_row <- shapes_data[nrow(shapes_data), ]
          
          if (!is.null(last_row$path) && nchar(last_row$path) > 0) {
            path <- last_row$path
            coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
            xy <- do.call(rbind, strsplit(coords, ","))
            x <- as.numeric(xy[,1])
            y <- as.numeric(xy[,2])
            
            sel_shape(list(type = "polygon", x = x, y = y))
            
            showNotification(
              paste0("✓ Polygon captured (", length(x), " points)"),
              type = "message", 
              duration = 2
            )
          }
        }
        return()
      }
    })
    
# --- Assign to selection ---
observeEvent(input$assign_class, {
  shape <- sel_shape()
  if (is.null(shape)) {
    showNotification("No selection drawn.", type = "warning", duration = 4)
    return()
  }
  
  df <- annotated_data() %||% clustered_data()
  req(df)
  
  # Initialize Class column if missing
  if (!"Class" %in% names(df)) {
    df$Class <- "Unassigned"
  } else {
    df$Class[is.na(df$Class)] <- "Unassigned"
  }
  df$Class <- as.character(df$Class)
  
  # Only polygon selection supported
  if (!identical(shape$type, "polygon")) {
    showNotification("Only polygon selection is supported.", type = "error", duration = 4)
    return()
  }
  
  # NO flip needed - plotly coordinates match our data coordinates
  poly_x <- shape$x
  poly_y <- shape$y
  inside <- sp::point.in.polygon(df$x, df$y, poly_x, poly_y) > 0
  
  n_sel <- sum(inside)
  if (n_sel == 0) {
    showNotification("No pixels in selection.", type = "warning", duration = 4)
    return()
  }
  
  lab <- input$class_label
  df$Class[inside] <- lab
  annotated_data(df)
  
  # Update colors - NEVER add Unassigned to palette
  if (lab != "Unassigned") {
    cols <- class_colors()
    
    if (!(lab %in% names(cols))) {
      i <- next_color_i()
      cols[lab] <- my_palette[i]
      next_color_i(if (i >= length(my_palette)) 1 else i + 1)
    }
    
    class_colors(cols)
  }
  
  # Clear shapes
  try({
    plotlyProxy("cluster_plot", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = list()))
  }, silent = TRUE)
  sel_shape(NULL)
  
  showNotification(
    sprintf("Assigned '%s' to %d pixels.", lab, n_sel),
    type = "message", duration = 3
  )
})

# --- Assign all unassigned ---
observeEvent(input$assign_all, {
  df <- annotated_data() %||% clustered_data()
  req(df)
  
  if (!"Class" %in% names(df)) {
    df$Class <- "Unassigned"
  } else {
    df$Class[is.na(df$Class)] <- "Unassigned"
  }
  df$Class <- as.character(df$Class)
  
  n_unassigned <- sum(df$Class == "Unassigned")
  
  if (n_unassigned > 0) {
    lab <- input$class_label
    df$Class[df$Class == "Unassigned"] <- lab
    annotated_data(df)
    
    # Update colors - NEVER add Unassigned to palette
    if (lab != "Unassigned") {
      cols <- class_colors()
      
      if (!(lab %in% names(cols))) {
        i <- next_color_i()
        cols[lab] <- my_palette[i]
        next_color_i(if (i >= length(my_palette)) 1 else i + 1)
      }
    
      class_colors(cols)
    }
    
    showNotification(
      sprintf("Assigned '%s' to %d pixels.", lab, n_unassigned),
      type = "message", duration = 3
    )
  } else {
    showNotification("No unassigned pixels.", type = "warning", duration = 3)
  }
})
    
    
# --- Cluster plot (use cluster version) ---
output$cluster_plot <- renderPlotly({
  df <- clustered_data()
  req(df)
  
  cols <- setNames(
    brewer.pal(max(df$cluster), "Set3")[seq_len(max(df$cluster))],
    as.character(1:max(df$cluster))
  )
  img_uri <- make_raster_png(df, "cluster", cols)
  
  # Determine axis ranges based on orientation
  base_df <- original_clustered()
  req(base_df)
  
  # Default ranges (increasing)
  x_range <- c(min(df$x), max(df$x))
  y_range <- c(min(df$y), max(df$y))
  
  # Reverse ranges for flipped axes
  orientation <- input$orientation %||% "Default"
  
  if (orientation == "Flip X" || orientation == "Flip Both") {
    x_range <- rev(x_range)
  }
  if (orientation == "Flip Y" || orientation == "Flip Both") {
    y_range <- rev(y_range)
  }
  
  p <- plot_ly(source = "cluster") %>%
    add_trace(x = NULL, y = NULL, type = "scatter", mode = "markers") %>%
    layout(
      images = list(list(
        source = img_uri,
        xref = "x", yref = "y",
        x = min(df$x), y = max(df$y),
        sizex = max(df$x) - min(df$x), 
        sizey = max(df$y) - min(df$y),
        sizing = "stretch", layer = "below"
      )),
      dragmode = "drawclosedpath",
      newshape = list(line = list(color = "black", width = 1),
                    fillcolor = "rgba(0,0,0,0.05)"),
      title = "MSI Clustering Result",
      xaxis = list(range = x_range, title = "x"),
      yaxis = list(range = y_range, title = "y",
                  scaleanchor = "x", scaleratio = 1),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToAdd = list("drawclosedpath", "eraseshape"),
      modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", 
                                "toggleSpikelines", "toImage", "select2d", "lasso2d")
    )
  
  # Add legend traces
  for (i in seq_len(max(df$cluster))) {
    p <- p %>%
      add_trace(
        x = c(min(df$x) - 1000),
        y = c(min(df$y) - 1000),
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = cols[as.character(i)]),
        name = paste("Cluster", i),
        showlegend = TRUE,
        hoverinfo = "skip"
      )
  }
  
  p
})

# --- Class plot (interactive with raster) ---
output$class_plot <- renderPlotly({
  df <- annotated_data() %||% clustered_data()
  req(df)
  
  # Initialize Class column if missing
  if (!"Class" %in% names(df)) {
    df$Class <- "Unassigned"
  } else {
    df$Class[is.na(df$Class)] <- "Unassigned"
  }
  df$Class <- as.character(df$Class)
  
  # Get colors
  cols_used <- class_colors()
  cols_used <- cols_used[names(cols_used) != "Unassigned"]
  
  present_classes <- unique(df$Class)
  present_classes <- c(
    if ("Unassigned" %in% present_classes) "Unassigned",
    sort(setdiff(present_classes, "Unassigned"))
  )
  
  plotly_light_blue <- "#B8BFFC"
  all_colors <- c("Unassigned" = plotly_light_blue)
  for (cls in present_classes) {
    if (cls != "Unassigned") {
      all_colors[cls] <- cols_used[[cls]]
    }
  }
  
  img_uri <- make_raster_png(df, "Class", all_colors)
  
  # Determine axis ranges based on orientation
  base_df <- original_clustered()
  req(base_df)
  
  # Default ranges (increasing)
  x_range <- c(min(df$x), max(df$x))
  y_range <- c(min(df$y), max(df$y))
  
  # Reverse ranges for flipped axes
  orientation <- input$orientation %||% "Default"
  
  if (orientation == "Flip X" || orientation == "Flip Both") {
    x_range <- rev(x_range)
  }
  if (orientation == "Flip Y" || orientation == "Flip Both") {
    y_range <- rev(y_range)
  }
  
  p <- plot_ly() %>%
    layout(
      images = list(list(
        source = img_uri,
        xref = "x", yref = "y",
        x = min(df$x), y = max(df$y),
        sizex = max(df$x) - min(df$x), 
        sizey = max(df$y) - min(df$y),
        sizing = "stretch", layer = "below"
      )),
      title = "Class Assignment",
      xaxis = list(range = x_range, title = "x"),
      yaxis = list(range = y_range, title = "y",
                  scaleanchor = "x", scaleratio = 1),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", 
                                "toggleSpikelines", "toImage", "select2d", "lasso2d")
    )
  
  # Add legend traces
  for (i in seq_along(present_classes)) {
    cls <- present_classes[i]
    col <- all_colors[[cls]]
    
    p <- p %>%
      add_trace(
        x = c(min(df$x) - 1000),
        y = c(min(df$y) - 1000),
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,   
          color = col
        ),
        name = cls,
        showlegend = TRUE,
        hoverinfo = "skip"
      )
  }
  
  p
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
      
      # Disable button during commit
      shinyjs::disable("commit_db")
      on.exit(shinyjs::enable("commit_db"))
      
      # Create progress
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Preparing data for commit...", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      tryCatch({
        # Ensure Class column exists
        if (!"Class" %in% names(df)) df$Class <- "Unassigned"
        df$Class <- as.character(df$Class)
        df$Class[is.na(df$Class)] <- "Unassigned"
        
        progress$set(value = 20, message = "Generating metadata...")
        
        # Generate assignment ID
        assignment_id <- as.character(UUIDgenerate())
        
        # Count class assignments
        class_counts <- table(df$Class)
        n_unassigned <- as.integer(class_counts["Unassigned"] %||% 0)
        n_assigned <- sum(class_counts[names(class_counts) != "Unassigned"])
        unique_classes <- sort(setdiff(unique(df$Class), "Unassigned"))
        
        progress$set(value = 40, message = "Saving clustering data to GridFS...")
        
        # Save data to GridFS
        temp_path <- tempfile(pattern = "annotated_clustering_", fileext = ".rds")
        saveRDS(df, temp_path)
        
        grid <- gridfs(db = "MSI_database", prefix = "fs", url = "mongodb://localhost")
        gridfs_result <- grid$upload(temp_path, name = paste0(assignment_id, "_annotated_clustering.rds"))
        
        # Extract just the ID as string (consistent with processing pipeline)
        gridfs_id <- as.character(gridfs_result$id)
        
        progress$set(value = 70, message = "Creating metadata record...")
        
        # Create metadata document for clustering collection
        metadata_doc <- list(
          assignment_id = assignment_id,
          gridfs_id = gridfs_id,  # Now this will be a clean string
          sample_name = input$sample_select,
          created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
          
          # Processing parameters used
          snr = as.numeric(input$snr_select),
          tolerance = as.numeric(input$tol_select),
          reference_name = input$ref_select,
          
          # Clustering parameters
          clustering_method = input$method,
          num_clusters = as.integer(input$clusters),
          orientation = input$orientation,
          
          # Data dimensions
          num_pixels = as.integer(nrow(df)),
          num_features = as.integer(sum(grepl("^mz_", names(df)))),
          
          # Assignment statistics
          num_assigned = as.integer(n_assigned),
          num_unassigned = as.integer(n_unassigned),
          unique_classes = if (length(unique_classes) > 0) paste(unique_classes, collapse = ", ") else "",
          num_unique_classes = as.integer(length(unique_classes))
        )
        
        progress$set(value = 90, message = "Inserting metadata...")
        
        # Insert into clustering metadata collection
        mongo_cluster_meta$insert(metadata_doc)
        
        progress$set(value = 100, message = "Complete!")
        
        showNotification(
          sprintf("✅ Clustering committed successfully\nAssignment ID: %s\n%d pixels: %d assigned, %d unassigned",
                  assignment_id, nrow(df), n_assigned, n_unassigned),
          type = "message",
          duration = 10
        )
        
      }, error = function(e) {
        showNotification(
          paste0("❌ MongoDB commit failed: ", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
      })
    })
  })
}