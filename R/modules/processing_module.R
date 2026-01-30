# R/modules/processing_module.R
processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Processing",
           fluidRow(
             # Kolonne 1: Sidebar panel (2/12 bred)
             column(2,
               h4("Data Source"),
               radioButtons(
                 ns("data_source"),
                 "Select data source:",
                 choices = c("Upload new files", "Use existing dataset"),
                 selected = "Upload new files"
               ),
               
               # Upload section
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'Upload new files'", ns("data_source")),
                 fileInput(ns("msi_files"), "Upload imzML + ibd files", 
                          multiple = TRUE, accept = c(".imzML", ".ibd")),
                 textInput(ns("sample_name_upload"), "Sample name (optional):",
                          placeholder = "Leave empty to use filename")
               ),
               
               # Existing dataset section
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'Use existing dataset'", ns("data_source")),
                 selectInput(ns("existing_sample"), "Select sample:", 
                            choices = "Loading..."),
                 textOutput(ns("existing_info"))
               ),
               
               hr(),
               h4("Processing Parameters"),
               
               numericInput(ns("resolution"), "Resolution (ppm):", 
                           value = 10, min = 1, max = 100, step = 1),
               
               numericInput(ns("snr"), "Signal to Noise Ratio (SNR):", 
                           value = 3, min = 1.5, max = 30, step = 0.1),
               
               numericInput(ns("tolerance"), "Binning tolerance:", 
                           value = 0.5, min = 0.1, max = 3, step = 0.1),
               
               # Reference selection
               radioButtons(
                 ns("ref_source"),
                 "Reference list source:",
                 choices = c("From database", "Upload your own"),
                 selected = "From database"
               ),
               
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'Upload your own'", ns("ref_source")),
                 fileInput(ns("ref_csv"), "Upload m/z reference list (.csv)",
                          multiple = FALSE, accept = ".csv")
               ),
               
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'From database'", ns("ref_source")),
                 selectInput(ns("ref_csv_mongo"), "Select reference list:", 
                            choices = "Loading...")
               ),
               
               hr(),
               actionButton(ns("run_processing"), "Run Processing", 
                           class = "btn-primary"),
               br(), br(),
               actionButton(ns("clear_cache"), "Clear local cache", 
                           class = "btn-warning")
             ),
             
             # Kolonne 2: Status og Log (4/12 bred)
             column(4,
               h3("Processing Pipeline Status"),
               uiOutput(ns("pipeline_status")),
               hr(),
               h4("Processing Log"),
               verbatimTextOutput(ns("processing_log")),
               hr(),
               h4("Cache Status"),
               verbatimTextOutput(ns("cache_status"))
             ),
             
             # Kolonne 3: Plots (6/12 bred)
             column(6,
               # Øverste plot panel - Image plots
               wellPanel(
                 h4("MSI Images - Top 3 m/z (by variance)"),
                 tabsetPanel(
                   id = ns("image_tabs"),
                   tabPanel("Raw",
                            plotOutput(ns("top3_raw_plot"), height = "400px")
                   ),
                   tabPanel("Normalized",
                            plotOutput(ns("top3_norm_plot"), height = "400px")
                   )
                 )
               ),
               
               # Nederste plot panel - Distance plots
               wellPanel(
                 h4("Spatial vs Intensity Distance"),
                 tabsetPanel(
                   id = ns("distance_tabs"),
                   tabPanel("Binned",
                            plotOutput(ns("distance_binned_plot"), height = "400px")
                   ),
                   tabPanel("Scatter",
                            plotOutput(ns("distance_scatter_plot"), height = "400px")
                   )
                 )
               )
             )
           )
  )
}

processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MongoDB connections
    mongo_ref <- mongo(collection = "mz_references",
                       db = "msi_project", url = "mongodb://localhost:27018")
    
    mongo_meta <- mongo(collection = "processing_artifacts_metadata",
                       db = "MSI_database", url = "mongodb://localhost:27018")
    
    # Reactive values for state
    processing_log <- reactiveVal("")
    current_cache_dir <- reactiveVal(NULL)
    current_sample_name <- reactiveVal(NULL)
    plot_top3_raw <- reactiveVal(NULL)
    plot_top3_norm <- reactiveVal(NULL)
    plot_distance_binned <- reactiveVal(NULL)
    plot_distance_scatter <- reactiveVal(NULL)
    
    # Clean cache helper
    cleanup_cache_dir <- function(cache_dir = NULL) {
      dir_to_clean <- cache_dir %||% current_cache_dir()
      
      if (is.null(dir_to_clean) || !dir.exists(dir_to_clean)) {
        return(invisible(NULL))
      }
      
      tryCatch({
        files <- list.files(dir_to_clean, full.names = TRUE, recursive = TRUE)
        total_size <- sum(file.size(files)) / 1024^2
        unlink(dir_to_clean, recursive = TRUE)
        
        add_log(sprintf("✓ Cache cleaned: %.2f MB freed", total_size))
        current_cache_dir(NULL)
        
        invisible(total_size)
      }, error = function(e) {
        add_log(sprintf("⚠ Cache cleanup warning: %s", e$message))
        invisible(NULL)
      })
    }

    # Clean Cardinal temp files
    cleanup_cardinal_temp <- function() {
      tryCatch({
        # Find Cardinal temp filer
        temp_files <- list.files(
          tempdir(), 
          pattern = "(imzml_|Cardinal|matter_array)",
          full.names = TRUE, 
          recursive = TRUE
        )
        
        if (length(temp_files) > 0) {
          sizes <- file.size(temp_files)
          total_mb <- sum(sizes, na.rm = TRUE) / 1024^2
          
          unlink(temp_files, recursive = TRUE)
          add_log(sprintf("✓ System temp cleaned: %.2f MB", total_mb))
        }
        
        gc()  # Force garbage collection
        invisible(NULL)
      }, error = function(e) {
        invisible(NULL)
      })
    }
    
    # Helper: Add to log
    add_log <- function(msg) {
      timestamp <- format(Sys.time(), "[%H:%M:%S]")
      new_log <- paste0(processing_log(), timestamp, " ", msg, "\n")
      processing_log(new_log)
    }
    
    # Load reference options from database
    ref_docs <- reactive({
      mongo_ref$find(fields = '{"_id": 0, "reference_name": 1}')
    })
    
    observe({
      refs <- unique(ref_docs()$reference_name)
      if (length(refs) == 0) refs <- "No references found"
      updateSelectInput(session, "ref_csv_mongo", choices = refs)
    })
    
    # Load existing raw files from database - MAKE IT REACTIVE
    available_samples <- reactive({
      # Trigger re-run when data_source changes
      input$data_source
      
      artifacts <- mongo_meta$find(
        query = '{"stage_type": "raw_files"}',
        fields = '{"_id": 0, "sample_name": 1}'
      )
      
      if (nrow(artifacts) == 0) {
        "No samples in database"
      } else {
        unique(artifacts$sample_name)
      }
    })
    
    observe({
      samples <- available_samples()
      updateSelectInput(session, "existing_sample", choices = samples)
    })
    
    # Show info about existing sample
    output$existing_info <- renderText({
      req(input$data_source == "Use existing dataset", input$existing_sample)
      
      # Check raw files
      raw_artifacts <- mongo_meta$find(
        query = jsonlite::toJSON(list(
          sample_name = input$existing_sample,
          stage_type = "raw_files"
        ), auto_unbox = TRUE)
      )
      
      # Check processed versions
      processed_artifacts <- mongo_meta$find(
        query = jsonlite::toJSON(list(
          sample_name = input$existing_sample,
          stage_type = "binned_dataframe"
        ), auto_unbox = TRUE)
      )
      
      info_parts <- c()
      
      if (nrow(raw_artifacts) > 0) {
        created <- as.character(raw_artifacts$created_at[nrow(raw_artifacts)])
        info_parts <- c(info_parts, 
                       sprintf("✓ Raw files in database (uploaded: %s)", created))
      }
      
      if (nrow(processed_artifacts) > 0) {
        info_parts <- c(info_parts,
          sprintf("\n%d processed version(s) exist: \n", nrow(processed_artifacts)),
          sapply(1:nrow(processed_artifacts), function(i) {
            sprintf("\n - Res: %.0f, SNR: %.1f, Tol: %.1f, Ref: %s",
                   processed_artifacts$resolution[i],
                   processed_artifacts$snr[i], 
                   processed_artifacts$tolerance[i], 
                   processed_artifacts$reference_name[i])
          })
        )
      } else {
        info_parts <- c(info_parts, "\nNo processed versions exist yet")
      }
      
      paste(info_parts, collapse = "\n")
    })
    
    # Get selected reference
    selected_mz <- reactive({
      req(input$ref_source)
      
      if (input$ref_source == "Upload your own") {
        req(input$ref_csv)
        df <- read.csv(input$ref_csv$datapath, stringsAsFactors = FALSE)
        list(
          mz = as.numeric(df$mz),
          name = tools::file_path_sans_ext(basename(input$ref_csv$name)),
          source = "upload"
        )
      } else {
        req(input$ref_csv_mongo)
        ref_doc <- mongo_ref$find(
          query = sprintf('{"reference_name": "%s"}', input$ref_csv_mongo),
          fields = '{"_id": 0, "mz_values": 1}'
        )
        if (nrow(ref_doc) == 0) return(NULL)
        
        list(
          mz = as.numeric(unlist(ref_doc$mz_values[[1]])),
          name = input$ref_csv_mongo,
          source = "database"
        )
      }
    })
    
    # Determine current sample name
    current_sample <- reactive({
      if (input$data_source == "Upload new files") {
        req(input$msi_files)
        imzml_name <- input$msi_files$name[grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)][1]
        
        if (nchar(input$sample_name_upload) > 0) {
          input$sample_name_upload
        } else {
          imzml_name
        }
      } else {
        req(input$existing_sample)
        input$existing_sample
      }
    })
    
    
    # Clear cache
    observeEvent(input$clear_cache, {
      cache_dir <- current_cache_dir()
      
      if (is.null(cache_dir) || !dir.exists(cache_dir)) {
        showNotification("No active cache to clear", type = "message", duration = 3)
        return()
      }
      
      tryCatch({
        # Beregn størrelse
        files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
        cache_size <- sum(file.size(files)) / 1024^2
        
        # Slet cache directory
        unlink(cache_dir, recursive = TRUE)
        current_cache_dir(NULL)
        add_log(sprintf("Cache cleared: %.2f MB freed", cache_size))
        
        # Ryd Cardinal temp files
        temp_files <- list.files(
          tempdir(), 
          pattern = "(imzml_|Cardinal|matter_array)",
          full.names = TRUE, 
          recursive = TRUE
        )
        
        if (length(temp_files) > 0) {
          temp_size <- sum(file.size(temp_files), na.rm = TRUE) / 1024^2
          unlink(temp_files, recursive = TRUE)
          add_log(sprintf("System temp cleaned: %.2f MB", temp_size))
        }
        
        # Nulstil plot reactive values
        plot_top3_raw(NULL)
        plot_top3_norm(NULL)
        plot_distance_binned(NULL)
        plot_distance_scatter(NULL)
        
        # Force garbage collection
        gc()
        
        total_freed <- cache_size + (if(exists("temp_size")) temp_size else 0)
        
        showNotification(
          sprintf("✓ All cleared: %.2f MB freed from disk + plots removed from memory", total_freed), 
          type = "message", 
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("Cache clear error:", e$message), 
                        type = "error", duration = NULL)
      })
    })
    

    # Run processing
    observeEvent(input$run_processing, {
      mz_ref <- selected_mz()
      sample_name <- current_sample()
      
      if (is.null(mz_ref) || is.null(sample_name)) {
        showNotification("Please configure all parameters first", 
                        type = "error", duration = NULL)
        return()
      }
      
      # Check if exact match already exists
      exact_match <- mongo_meta$find(
        query = jsonlite::toJSON(list(
          sample_name = sample_name,
          stage_type = "binned_dataframe",
          resolution = as.numeric(input$resolution),
          snr = as.numeric(input$snr),
          tolerance = as.numeric(input$tolerance),
          reference_name = mz_ref$name
        ), auto_unbox = TRUE)
      )
      
      if (nrow(exact_match) > 0) {
        showNotification(
          "This exact processing already exists. No action needed.",
          type = "warning",
          duration = 10
        )
        return()
      }
      
      # Reset plots at the start of new processing
      plot_top3_raw(NULL)
      plot_top3_norm(NULL)
      plot_distance_binned(NULL)
      plot_distance_scatter(NULL)

      shinyjs::disable("run_processing")
      on.exit({
        shinyjs::enable("run_processing")
      }, add = TRUE)
      
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting processing pipeline...", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      processing_log("")
      cleanup_cardinal_temp()
      
      tryCatch({
        add_log(sprintf("=== PROCESSING STARTED ==="))
        add_log(sprintf("Sample: %s", sample_name))
        add_log(sprintf("Parameters: Resolution=%d, SNR=%.1f, Tol=%.2f, Ref=%s",
                      input$resolution, input$snr, input$tolerance, mz_ref$name))
        
        # Setup cache directory
        cache_base <- file.path(tempdir(), "msi_processing_cache")
        cache_dir <- file.path(cache_base, sanitize_name(sample_name))
        current_cache_dir(cache_dir)
        current_sample_name(sample_name)
        
        if (dir.exists(cache_dir)) {
          add_log("Clearing existing cache...")
          unlink(cache_dir, recursive = TRUE)
        }
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        add_log(sprintf("Cache directory: %s", cache_dir))
        
        progress$set(value = 10, message = "Loading/uploading raw data...")
        
        # STEP 1: Handle raw data
        if (input$data_source == "Upload new files") {
          req(input$msi_files)
          files <- input$msi_files
          imzml_idx <- grepl("\\.imzML$", files$name, ignore.case = TRUE)
          ibd_idx <- grepl("\\.ibd$", files$name, ignore.case = TRUE)
          
          if (!any(imzml_idx) || !any(ibd_idx)) {
            stop("Both imzML and ibd files required")
          }

          # Check if raw files already exist in database
          existing_raw <- mongo_meta$find(
            query = jsonlite::toJSON(list(
              sample_name = sample_name,
              stage_type = "raw_files"
            ), auto_unbox = TRUE)
          )
          
          if (nrow(existing_raw) > 0) {
            add_log("⚠ Raw files already exist in database - skipping upload")
            showNotification(
              "Raw files already exist in database. Reusing existing files instead.",
              type = "message",
              duration = 5
            )
          } else {
            add_log("Uploading raw files to MongoDB...")
            save_raw_pair_to_mongo(
              sample_name = sample_name,
              imzml_path = files$datapath[imzml_idx][1],
              ibd_path = files$datapath[ibd_idx][1],
              db_name = "MSI_database"
            )
            add_log("✓ Raw files saved to database")
          }
        }
        
        progress$set(value = 30, message = "Loading MSI object...")
        add_log("Downloading raw files from database...")
        
        msi_data <- load_raw_object_from_mongo(
          sample_name = sample_name,
          workdir = cache_dir,
          db_name = "MSI_database",
          resolution = as.numeric(input$resolution)
        )
        add_log(sprintf("✓ MSI data loaded: %d pixels, %d m/z values",
                      ncol(msi_data), nrow(msi_data)))
        
        progress$set(value = 50, message = "Processing mean spectrum...")
        
        # STEP 2: Mean spectrum (check for existing)
        mean_artifacts <- mongo_meta$find(
          query = jsonlite::toJSON(list(
            sample_name = sample_name,
            stage_type = "control_mean",
            file_format = "imzML",
            resolution = as.numeric(input$resolution)
          ), auto_unbox = TRUE)
        )
        
        if (nrow(mean_artifacts) > 0) {
          add_log("Loading existing mean spectrum...")
          control_mean <- load_msi_stage_from_mongo(
            sample_name = sample_name,
            stage_type = "control_mean",
            resolution = as.numeric(input$resolution),
            db_name = "MSI_database"
          )
          
          run_id <- mean_artifacts$run_id[nrow(mean_artifacts)]
          if (is.null(run_id) || is.na(run_id) || run_id == "") {
            run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
          }
        } else {
          add_log("Calculating mean spectrum...")
          control_mean <- summarizeFeatures(msi_data, "mean")
          
          run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
          save_msi_stage_to_mongo(
            control_mean,
            run_id,
            "control_mean",
            sample_name = sample_name,
            params = list(
              resolution = as.numeric(input$resolution)
            ),
            db_name = "MSI_database"
          )
        }
        add_log("✓ Mean spectrum ready")
        
        progress$set(value = 70, message = "Applying SNR peak picking and aligning to feature list...")
        
        
        # STEP 4: Alignment
        add_log(sprintf("Applying SNR peak picking (SNR=%.1f)...", input$snr))
        add_log(sprintf("Aligning to reference (Tol=%.2f)...", input$tolerance))
        
        processing_run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        control_MSI_ref <- control_mean %>%
          peakPick(SNR = input$snr) %>%
          peakAlign(ref = mz_ref$mz, tolerance = input$tolerance, units = "mz") %>%
          subsetFeatures() %>%
          process()
        
        add_log("✓ Reference aligned")
        
        progress$set(value = 85, message = "Binning full dataset...")
        
        add_log("Binning MSI data...")
        msi_data_binned <- bin(
          msi_data,
          ref = mz(control_MSI_ref),
          tolerance = input$tolerance,
          units = "mz",
          BPPARAM = BiocParallel::bpparam()
        ) %>% process()
        
        add_log("✓ Data binned")
        

      
      # Lav og gem normaliseret version
      add_log("Normalizing binned data (TIC)...")

      spec_mat <- as.matrix(Cardinal::spectra(msi_data_binned))  # features x pixels
      tic <- colSums(spec_mat, na.rm = TRUE)

      # robust beskyttelse (selvom dine tal ser fine ud)
      tic[!is.finite(tic) | tic <= 0] <- NA_real_

      # TIC-normaliseret matrix (features x pixels)
      spec_mat_tic <- sweep(spec_mat, 2, tic, "/")

      add_log("✓ Normalized matrix ready (TIC)")

      # Generate plots
      add_log("Generating visualization plots...")

      # Top 3 m/z plots (RAW)
      var_intensity <- apply(spec_mat, 1, var, na.rm = TRUE)
      top3_idx <- order(var_intensity, decreasing = TRUE)[1:3]
      top3_mz <- mz(msi_data_binned)[top3_idx]

      # Top 3 m/z plots (NORMALIZED selection)
      norm_var_intensity <- apply(spec_mat_tic, 1, var, na.rm = TRUE)
      norm_top3_idx <- order(norm_var_intensity, decreasing = TRUE)[1:3]
      norm_top3_mz <- mz(msi_data_binned)[norm_top3_idx]

      vizi_style("dark")

      create_raw_plot <- function() {
        image(
          msi_data_binned,
          mz = top3_mz,
          superpose = TRUE,
          contrast.enhance = "suppress",
          normalize.image = "linear",
          col = c("blue", "red", "green")
        )
      }
      plot_top3_raw(create_raw_plot)

      # NB: vi plotter stadig fra msi_data_binned, men top3-mz er valgt ud fra TIC-normaliserede intensiteter
      create_norm_plot <- function() {
        image(
          msi_data_binned,
          mz = norm_top3_mz,
          superpose = TRUE,
          contrast.enhance = "suppress",
          normalize.image = "linear",
          col = c("blue", "red", "green")
        )
      }
      plot_top3_norm(create_norm_plot)

      # Distance calculations baseret på TIC-normaliseret matrix
      add_log("Calculating spatial vs intensity distances...")

      norm_msi_matrix <- t(spec_mat_tic)  # pixels x features
      coords_df <- coord(msi_data_binned)

      norm_msi_matrix <- cbind(
        x = coords_df$x,
        y = coords_df$y,
        norm_msi_matrix
      )
        n_pairs <- 10000
        n <- nrow(norm_msi_matrix)
        
        pairs <- data.frame(
          i = sample(n, n_pairs, replace = TRUE),
          j = sample(n, n_pairs, replace = TRUE)
        )
        pairs <- subset(pairs, i != j)
        pairs$ii <- pmin(pairs$i, pairs$j)
        pairs$jj <- pmax(pairs$i, pairs$j)
        pairs <- unique(pairs[, c("ii", "jj")])
        names(pairs) <- c("i", "j")
        if (nrow(pairs) > n_pairs) pairs <- pairs[1:n_pairs, ]
        
        coords <- norm_msi_matrix[, c("x", "y")]
        space_distance <- sqrt(
          rowSums(
            (coords[pairs$i, , drop = FALSE] -
               coords[pairs$j, , drop = FALSE])^2
          )
        )
        
        intens <- norm_msi_matrix[, -c(1:2), drop = FALSE]
        cosine_distance <- function(a, b) {
          sim <- sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
          return(1 - sim)
        }
        
        intensity_distance <- mapply(
          function(i, j) cosine_distance(intens[i, ], intens[j, ]),
          pairs$i, pairs$j
        )
        
        df_dist <- data.frame(
          space_distance = space_distance,
          intensity_distance = intensity_distance
        )
        
        # Binned plot
        nbins <- 50
        df_binned <- df_dist %>%
          mutate(bin = cut(space_distance, breaks = nbins)) %>%
          group_by(bin) %>%
          summarise(
            space_mid = mean(space_distance),
            int_median = median(intensity_distance),
            int_q25 = quantile(intensity_distance, 0.25),
            int_q75 = quantile(intensity_distance, 0.75),
            .groups = "drop"
          )
        
        p_binned <- ggplot(df_binned, aes(x = space_mid, y = int_median)) +
          geom_line() +
          geom_ribbon(aes(ymin = int_q25, ymax = int_q75), alpha = 0.2) +
          theme_bw() +
          labs(
            x = "Euclidean distance between pixels",
            y = "Cosine distance (median, 25–75% interval)",
            title = "Spatial vs Intensity Distance (binned)"
          )
        plot_distance_binned(p_binned)
        
        # Scatter plot
        p_scatter <- ggplot(df_dist, aes(x = space_distance, y = intensity_distance)) +
          geom_point(alpha = 0.2, size = 1) +
          theme_bw() +
          labs(
            x = "Euclidean distance between pixels",
            y = "Cosine distance in m/z-intensities",
            title = "Spatial vs Intensity Distance (10,000 pixel pairs)"
          )
        plot_distance_scatter(p_scatter)
        
        add_log("✓ All plots generated")

        progress$set(value = 95, message = "Creating feature matrix...")
        
        # STEP 5: Create final dataframe (saved as RDS)
        add_log("Creating feature matrix...")
        msi_matrix <- t(as.matrix(spectra(msi_data_binned)))
        mz_names <- paste0("mz_", mz(msi_data_binned))
        coords <- coord(msi_data_binned)
        run_name <- runNames(msi_data_binned)
        pixel_names <- rep(run_name, nrow(msi_matrix))
        
        full_df <- data.frame(
          runNames = pixel_names,
          x = coords$x,
          y = coords$y,
          msi_matrix
        )
        colnames(full_df) <- c("runNames", "x", "y", mz_names)
        
        # Final dataframe still uses old RDS method
        save_stage_to_mongo(
          full_df,
          processing_run_id,
          "binned_dataframe",
          sample_name = sample_name,
          params = list(
            snr = as.numeric(input$snr),
            tolerance = as.numeric(input$tolerance),
            reference_name = mz_ref$name,
            resolution = as.numeric(input$resolution),
            num_features = ncol(full_df) - 3,
            num_pixels = nrow(full_df)
          ),
          db_name = "MSI_database"
        )
        
        add_log(sprintf("✓ Final dataframe: %d pixels × %d features", 
                      nrow(full_df), sum(grepl("^mz_", names(full_df)))))
        
        progress$set(value = 100, message = "Complete!")
        
        add_log("=== PROCESSING COMPLETE ===")
        add_log(sprintf("Run ID: %s", processing_run_id))
        
        output$pipeline_status <- renderUI({
          div(class = "alert alert-success",
              h4("✅ Processing Complete"),
              p(sprintf("Sample: %s", sample_name)),
              p(sprintf("Run ID: %s", processing_run_id)),
              p(sprintf("Features: %d m/z bins", sum(grepl("^mz_", names(full_df))))),
              p(sprintf("Pixels: %d", nrow(full_df))),
              p(sprintf("Resolution: %d ppm", input$resolution)),
              p(sprintf("SNR: %.1f", input$snr)),
              p(sprintf("Tolerance: %.2f", input$tolerance)),
              p(sprintf("Reference: %s", mz_ref$name))
          )
        })
        
        showNotification(
          sprintf("✅ Processing complete!\nRun ID: %s\n%d features created",
                processing_run_id, sum(grepl("^mz_", names(full_df)))),
          type = "message",
          duration = 10
        )
        
      }, error = function(e) {
        add_log(sprintf("❌ ERROR: %s", e$message))
        showNotification(
          paste("Processing error:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
    
    # Display logs
    output$processing_log <- renderText({
      processing_log()
    })
    
    # Display cache status
    output$cache_status <- renderText({
      cache_dir <- current_cache_dir()
      
      if (is.null(cache_dir) || !dir.exists(cache_dir)) {
        return("No active cache")
      }
      
      files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
      if (length(files) == 0) {
        return(sprintf("Cache directory: %s\n(empty)", cache_dir))
      }
      
      total_size <- sum(file.size(files)) / 1024^2
      
      sprintf(
        "Cache directory: %s\nFiles: %d\nTotal size: %.2f MB\n\nSample: %s",
        cache_dir,
        length(files),
        total_size,
        current_sample_name() %||% "None"
      )
    })
        
   # Render plots from saved objects
    output$top3_raw_plot <- renderPlot({
      req(plot_top3_raw())
      vizi_style("dark")
      plot_top3_raw()()  # Kald funktionen
    })
    
    output$top3_norm_plot <- renderPlot({
      req(plot_top3_norm())
      vizi_style("dark")
      plot_top3_norm()()  # Kald funktionen
    })
    
    output$distance_binned_plot <- renderPlot({
      req(plot_distance_binned())
      plot_distance_binned()
    })
    
    output$distance_scatter_plot <- renderPlot({
      req(plot_distance_scatter())
      plot_distance_scatter()
    })
  })
}


