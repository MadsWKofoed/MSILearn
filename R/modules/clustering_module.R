# R/modules/clustering_module.R

clustering_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Clustering",
           sidebarLayout(
             sidebarPanel(
               selectInput(ns("run_select"), "Select dataset (run):", choices = "Loading..."),
               uiOutput(ns("snr_ui")),
               uiOutput(ns("tol_ui")),
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
    
    mongo_meta <- mongo(collection = "processing_runs", db = "MSI_database", url = "mongodb://localhost")
    mongo_data <- mongo(collection = "msi_data", db = "msi_project", url = "mongodb://localhost")
    
    processed_data <- reactiveVal(NULL)
    clustered_data <- reactiveVal(NULL)
    annotated_data <- reactiveVal(NULL)
    
    # --- Load all runs ---
    observe({
      runs <- mongo_meta$find(fields = '{"_id":0, "run_id":1, "sample_name":1}')
      if (nrow(runs) == 0) {
        updateSelectInput(session, "run_select", choices = "No runs found")
      } else {
        choices <- setNames(runs$run_id, paste(runs$sample_name, "(", runs$run_id, ")"))
        updateSelectInput(session, "run_select", choices = choices)
      }
    })
    
    # --- Update SNR dropdown when run is selected ---
    output$snr_ui <- renderUI({
      req(input$run_select)
      run_doc <- mongo_meta$find(paste0('{"run_id": "', input$run_select, '"}'))
      stages_obj <- run_doc$stages[[1]]
      stage_names <- list_binned_stages(input$run_select)
      
      # Extract all SNR values from stage names
      snr_values <- unique(sub(".*_SNR([0-9.]+)_.*", "\\1", grep("binned_dataframe", stage_names, value = TRUE)))
      snr_values <- snr_values[snr_values != ".*"]  # remove bad matches
      snr_values <- sort(unique(as.numeric(snr_values)))
      
      selectInput(ns("snr_select"), "Select SNR:", choices = snr_values)
    })
    
    # --- Update tolerance dropdown when SNR is selected ---
    output$tol_ui <- renderUI({
      req(input$run_select, input$snr_select)
      run_doc <- mongo_meta$find(paste0('{"run_id": "', input$run_select, '"}'))
      stages_obj <- run_doc$stages[[1]]
      stage_names <- list_binned_stages(input$run_select)
      
      pattern <- paste0("binned_dataframe_SNR", input$snr_select, "_tol")
      tol_values <- unique(sub(".*_tol([0-9.]+)_.*", "\\1", grep(pattern, stage_names, value = TRUE)))
      tol_values <- tol_values[tol_values != ".*"]
      tol_values <- sort(unique(as.numeric(tol_values)))
      
      selectInput(ns("tol_select"), "Select tolerance:", choices = tol_values)
    })
    
    # --- Load selected dataset ---
    observeEvent(input$load_dataset, {
      req(input$run_select, input$snr_select, input$tol_select)
      run_id <- input$run_select
      
      # Find matching stage name
      run_doc <- mongo_meta$find(paste0('{"run_id": "', run_id, '"}'))
      stages_obj <- run_doc$stages[[1]]
      stage_names <- list_binned_stages(input$run_select)
      match_pattern <- paste0("binned_dataframe_SNR", input$snr_select, "_tol", input$tol_select)
      match_stage <- grep(match_pattern, stage_names, value = TRUE)
      
      if (length(match_stage) == 0) {
        showNotification("No matching dataset found for selected parameters.", type = "error")
        return()
      }
      
      stage_to_load <- match_stage[1]
      message("Loading stage: ", stage_to_load)
      df <- load_stage_from_mongo(run_id, stage_to_load)
      
      processed_data(df)
      output$status_text <- renderText(paste("Loaded dataset:", stage_to_load))
      showNotification(paste("Loaded dataset:", stage_to_load), type = "message")
    })
    
    # --- Clustering (same as before) ---
    observeEvent(input$run_clustering, {
      df <- processed_data(); req(df)
      withProgress(message = "Clustering...", value = 0, {
        clustered <- if (input$method == "K-means")
          run_kmeans(df, input$clusters)
        else
          run_hclust(df, input$clusters)
        clustered_data(clustered)
        annotated_data(NULL)
        incProgress(1)
      })
      output$status_text <- renderText("Clustering complete.")
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
    
    observeEvent(input$run, { annotated_data(NULL); class_colors(c("Unassigned" = "grey80")); next_color_i(1) })
    
    # --- Raster helper: return base64 PNG for plotly background ---
    make_raster_png <- function(df, fill_var, colors) {
      df$x <- df$x - min(df$x) + 1
      df$y <- df$y - min(df$y) + 1
      width <- max(df$x); height <- max(df$y)
      mat <- matrix(NA_character_, nrow = height, ncol = width)
      mat[cbind(df$y, df$x)] <- as.character(df[[fill_var]])
      mat[is.na(mat)] <- "Unassigned"
      col_img <- matrix(colors[mat], nrow = height, ncol = width)
      
      rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
      rgb_array <- array(NA_real_, dim = c(height, width, 4))
      rgb_array[,,1] <- matrix(rgb_vals["red", ],    nrow = height, ncol = width)
      rgb_array[,,2] <- matrix(rgb_vals["green",  ], nrow = height, ncol = width)
      rgb_array[,,3] <- matrix(rgb_vals["blue",   ], nrow = height, ncol = width)
      rgb_array[,,4] <- matrix(rgb_vals["alpha",  ], nrow = height, ncol = width)
      
      tmp <- tempfile(fileext = ".png")
      png::writePNG(rgb_array, target = tmp)
      base64enc::dataURI(file = tmp, mime = "image/png")
    }
    
    # --- Drawn selection storage (polygon or rect) ---
    sel_shape <- reactiveVal(NULL)  # list(type="polygon"/"rect", x=..., y=..., x0/x1/y0/y1)
    
    # Capture shapes drawn on the plot (lasso/rect), via plotly_relayout
    # --- Capture shapes automatically when drawn ---
    observeEvent(event_data("plotly_relayout", source = "cluster"), {
      ev <- event_data("plotly_relayout", source = "cluster")
      req(ev)
      
      # Look for a new shape definition in relayout data
      # Works for both lasso (path) and rectangle (x0/x1/y0/y1)
      if (any(grepl("^shapes\\[[0-9]+\\]\\.path$", names(ev)))) {
        path_key <- grep("^shapes\\[[0-9]+\\]\\.path$", names(ev), value = TRUE)[1]
        path <- ev[[path_key]]
        coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
        xy <- do.call(rbind, strsplit(coords, ","))
        x <- as.numeric(xy[,1]); y <- as.numeric(xy[,2])
        sel_shape(list(type = "polygon", x = x, y = y))
        return()
      }
      
      if (any(grepl("^shapes\\[[0-9]+\\]\\.(x0|x1|y0|y1)$", names(ev)))) {
        rect_data <- ev[grep("^shapes\\[[0-9]+\\]\\.(x0|x1|y0|y1)$", names(ev))]
        x0 <- as.numeric(rect_data[grep("x0$", names(rect_data))])
        x1 <- as.numeric(rect_data[grep("x1$", names(rect_data))])
        y0 <- as.numeric(rect_data[grep("y0$", names(rect_data))])
        y1 <- as.numeric(rect_data[grep("y1$", names(rect_data))])
        sel_shape(list(
          type = "rect",
          x0 = min(x0, x1), x1 = max(x0, x1),
          y0 = min(y0, y1), y1 = max(y0, y1)
        ))
        return()
      }
    })
    
    
    # --- Assign ALL unassigned ---
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
    
    # --- Cluster Plot (raster + drawing tools) ---
    output$cluster_plot <- renderPlotly({
      df <- clustered_data(); req(df)
      cols <- setNames(brewer.pal(max(df$cluster), "Set3")[seq_len(max(df$cluster))],
                       as.character(1:max(df$cluster)))
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
          # default tool: freehand polygon (lasso). You can switch to rectangle in the toolbar.
          dragmode = "drawclosedpath",
          newshape = list(line = list(color = "black", width = 1),
                          fillcolor = "rgba(0,0,0,0.05)"),
          title = "MSI Clustering Result",
          xaxis = list(range = c(0, max(df$x)), title = "x"),
          yaxis = list(range = c(0, max(df$y)), title = "y",
                       scaleanchor = "x", scaleratio = 1)
        ) %>%
        config(
          displaylogo = FALSE,
          modeBarButtonsToAdd = list("drawclosedpath","drawrect","eraseshape"),
          modeBarButtonsToRemove = c("hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines","toImage")
        )
    })
    
    # --- Class Plot (raster only) ---
    output$class_plot <- renderPlotly({
      df <- annotated_data() %||% clustered_data(); req(df)
      if (!"Class" %in% names(df)) df$Class <- NA_character_
      df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
      cols <- class_colors(); present <- unique(df$Class_plot)
      cols_used <- cols[present]; names(cols_used) <- present
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
    
    # --- Assign using the LAST drawn shape (lasso polygon OR rectangle) ---
    observeEvent(input$assign_class, {
      shape <- sel_shape()
      if (is.null(shape)) {
        showNotification("No selection drawn. Use the Lasso or Rectangle tool on the left plot.", type = "warning", duration = 4)
        return()
      }
      
      df <- annotated_data() %||% clustered_data(); req(df)
      if (!"Class" %in% names(df)) df$Class <- NA_character_
      
      y_max <- max(df$y)
      
      if (identical(shape$type, "polygon")) {
        # flip Y to match image coordinate system (plotly y up, image y down)
        poly_x <- shape$x
        poly_y <- y_max - shape$y
        inside <- sp::point.in.polygon(df$x, df$y, poly_x, poly_y) > 0
      } else if (identical(shape$type, "rect")) {
        x0 <- shape$x0; x1 <- shape$x1
        # flip y0/y1
        yy0 <- y_max - shape$y0; yy1 <- y_max - shape$y1
        y0 <- min(yy0, yy1); y1 <- max(yy0, yy1)
        inside <- df$x >= x0 & df$x <= x1 & df$y >= y0 & df$y <= y1
      } else {
        showNotification("Unsupported shape type.", type = "error", duration = 4)
        return()
      }
      
      n_sel <- sum(inside)
      if (n_sel == 0) {
        showNotification("The drawn region did not overlap any pixels.", type = "warning", duration = 4)
        return()
      }
      
      df$Class[inside] <- input$class_label
      annotated_data(df)
      
      # ensure color exists
      cols <- class_colors(); lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      
      # Clear shapes from the plot after assignment (tidy UX)
      try({
        plotlyProxy("cluster_plot", session) %>%
          plotlyProxyInvoke("relayout", list(shapes = list()))
      }, silent = TRUE)
      sel_shape(NULL)
      
      showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, n_sel),
                       type = "message", duration = 3)
    })
    
    # --- Layout identical to original ---
    output$cluster_layout <- renderUI({
      req(clustered_data())
      fluidRow(
        column(6, plotlyOutput(ns("cluster_plot"), height = "600px")),
        column(6, plotlyOutput(ns("class_plot"), height = "600px"))
      )
    })
    
    # --- Commit to MongoDB ---
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
  })
}
