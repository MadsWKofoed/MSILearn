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
    
# --- Raster helper for CLUSTER plot (transparent background) ---
make_cluster_raster_png <- function(df, fill_var, colors) {
  df$x <- df$x - min(df$x) + 1
  df$y <- df$y - min(df$y) + 1
  width <- max(df$x)
  height <- max(df$y)
  
  # Create EMPTY matrix (NA = transparent)
  mat <- matrix(NA_character_, nrow = height, ncol = width)
  mat[cbind(df$y, df$x)] <- as.character(df[[fill_var]])
  
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
      
      # Method 3: Rectangle (direct coordinates)
      if (any(grepl("shapes\\[\\d+\\]\\.(x0|x1|y0|y1)$", names(ev)))) {
        rect_keys <- grep("shapes\\[\\d+\\]\\.(x0|x1|y0|y1)$", names(ev), value = TRUE)
        
        x0_key <- grep("x0$", rect_keys, value = TRUE)
        x1_key <- grep("x1$", rect_keys, value = TRUE)
        y0_key <- grep("y0$", rect_keys, value = TRUE)
        y1_key <- grep("y1$", rect_keys, value = TRUE)
        
        if (length(x0_key) > 0 && length(x1_key) > 0 && 
            length(y0_key) > 0 && length(y1_key) > 0) {
          
          x0 <- ev[[x0_key]]
          x1 <- ev[[x1_key]]
          y0 <- ev[[y0_key]]
          y1 <- ev[[y1_key]]
          
          if (!is.null(x0) && !is.null(x1) && !is.null(y0) && !is.null(y1)) {
            sel_shape(list(
              type = "rect",
              x0 = min(x0, x1), x1 = max(x0, x1),
              y0 = min(y0, y1), y1 = max(y0, y1)
            ))
            
            showNotification("✓ Rectangle captured", type = "message", duration = 2)
          }
        }
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
    
    # Ensure Unassigned stays grey
    cols["Unassigned"] <- "grey80"
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
      
      # Ensure Unassigned stays grey
      cols["Unassigned"] <- "grey80"
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
  img_uri <- make_cluster_raster_png(df, "cluster", cols)  # <-- Use cluster version
  
  p <- plot_ly(source = "cluster") %>%
    add_trace(x = NULL, y = NULL, type = "scatter", mode = "markers") %>%
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
                  scaleanchor = "x", scaleratio = 1),
      showlegend = FALSE
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToAdd = list("drawclosedpath", "drawrect", "eraseshape"),
      modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", 
                                "toggleSpikelines", "toImage")
    )
  
  p
})


# Add a new helper function specifically for CLASS plot (solid background)
make_class_raster_png <- function(df, fill_var, colors) {
  df$x <- df$x - min(df$x) + 1
  df$y <- df$y - min(df$y) + 1
  width <- max(df$x)
  height <- max(df$y)
  
  # Create matrix filled with "Unassigned" (not NA)
  mat <- matrix("Unassigned", nrow = height, ncol = width)
  mat[cbind(df$y, df$x)] <- as.character(df[[fill_var]])
  
  col_img <- matrix(colors[mat], nrow = height, ncol = width)
  
  rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
  rgb_array <- array(NA_real_, dim = c(height, width, 4))
  rgb_array[,,1] <- matrix(rgb_vals["red", ], nrow = height, ncol = width)
  rgb_array[,,2] <- matrix(rgb_vals["green", ], nrow = height, ncol = width)
  rgb_array[,,3] <- matrix(rgb_vals["blue", ], nrow = height, ncol = width)
  rgb_array[,,4] <- 1  # All pixels fully opaque
  
  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgb_array, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}    
    
# --- Updated raster helper that INCLUDES legend ---
make_class_plot_with_legend <- function(df, fill_var, colors) {
  # Ensure Class column exists
  if (!fill_var %in% names(df)) {
    df[[fill_var]] <- "Unassigned"
  }
  df[[fill_var]] <- factor(df[[fill_var]], levels = names(colors))
  
  # Create ggplot
  p <- ggplot(df, aes(x = x, y = y, fill = .data[[fill_var]])) +
    geom_tile() +
    scale_fill_manual(values = colors, drop = FALSE) +
    coord_equal() +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm")
    ) +
    labs(fill = "Classes")
  
  # Save to temp file
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, p, width = 10, height = 8, dpi = 150, bg = "transparent")
  
  # Return as data URI
  base64enc::dataURI(file = tmp, mime = "image/png")
}

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
  if (!("Unassigned" %in% names(cols_used))) {
    cols_used["Unassigned"] <- "grey80"
  }
  
  # Use cluster raster function (transparent background)
  img_uri <- make_cluster_raster_png(df, "Class", cols_used)
  
  # Get unique classes present in data for legend
  present_classes <- sort(unique(df$Class))
  
  # Build legend trace for each class
  legend_traces <- lapply(present_classes, function(cls) {
    plot_ly(
      x = 0, y = 0, 
      type = "scatter", 
      mode = "markers",
      marker = list(size = 10, color = cols_used[[cls]]),
      name = cls,
      showlegend = TRUE,
      hoverinfo = "skip"
    )
  })
  
  # Combine with image
  p <- plot_ly() %>%
    add_trace(x = NULL, y = NULL, type = "scatter", mode = "markers", showlegend = FALSE) %>%
    layout(
      images = list(list(
        source = img_uri,
        xref = "x", yref = "y",
        x = 0, y = max(df$y),
        sizex = max(df$x), sizey = max(df$y),
        sizing = "stretch", layer = "below"
      )),
      title = "Class Assignment",
      xaxis = list(range = c(0, max(df$x)), title = "x"),
      yaxis = list(range = c(0, max(df$y)), title = "y",
                  scaleanchor = "x", scaleratio = 1),
      showlegend = TRUE
    )
  
  # Add legend traces
  for (trace in legend_traces) {
    p <- add_trace(p, data = trace$x$data[[1]], inherit = FALSE)
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