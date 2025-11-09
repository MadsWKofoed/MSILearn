# R/modules/processing_module.R
processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Processing",
           sidebarLayout(
             sidebarPanel(
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
               actionButton(ns("check_params"), "Check processing status", 
                           class = "btn-info"),
               br(), br(),
               uiOutput(ns("param_check_ui")),
               br(),
               actionButton(ns("run_processing"), "Run Processing", 
                           class = "btn-primary"),
               br(), br(),
               actionButton(ns("clear_cache"), "Clear local cache", 
                           class = "btn-warning"),
               width = 3
             ),
             
             mainPanel(
               h3("Processing Pipeline Status"),
               uiOutput(ns("pipeline_status")),
               hr(),
               h4("Processing Log"),
               verbatimTextOutput(ns("processing_log")),
               hr(),
               h4("Cache Status"),
               verbatimTextOutput(ns("cache_status")),
               width = 9
             )
           )
  )
}

processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MongoDB connections
    mongo_ref <- mongo(collection = "mz_references",
                       db = "msi_project", url = "mongodb://localhost")
    
    mongo_meta <- mongo(collection = "processing_artifacts_metadata",
                       db = "MSI_test_database", url = "mongodb://localhost")
    
    # Reactive values for state
    processing_log <- reactiveVal("")
    current_cache_dir <- reactiveVal(NULL)
    current_sample_name <- reactiveVal(NULL)
    processing_status <- reactiveVal(NULL)
    
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
          sprintf("\n%d processed version(s) exist:", nrow(processed_artifacts)),
          sapply(1:nrow(processed_artifacts), function(i) {
            sprintf("  - SNR: %.1f, Tol: %.2f, Ref: %s",
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
          mz = df$mz,
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
          mz = unlist(ref_doc$mz_values[[1]]),
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
    
    # Check processing status
    observeEvent(input$check_params, {
      mz_ref <- selected_mz()
      sample_name <- current_sample()
      
      if (is.null(mz_ref) || is.null(sample_name)) {
        processing_status(list(
          status = "error",
          message = "Please select data source and reference first"
        ))
        return()
      }
      
      # Check for raw files
      raw_exists <- mongo_meta$find(
        query = jsonlite::toJSON(list(
          sample_name = sample_name,
          stage_type = "raw_files"
        ), auto_unbox = TRUE)
      )
      
      # Check for exact processing match
      exact_match <- mongo_meta$find(
        query = jsonlite::toJSON(list(
          sample_name = sample_name,
          stage_type = "binned_dataframe",
          snr = as.numeric(input$snr),
          tolerance = as.numeric(input$tolerance),
          reference_name = mz_ref$name
        ), auto_unbox = TRUE)
      )
      
      # Check for partial matches (can reuse)
      partial_matches <- mongo_meta$find(
        query = jsonlite::toJSON(list(
          sample_name = sample_name,
          stage_type = c("control_mean", "snr_reference")
        ), auto_unbox = TRUE)
      )
      
      if (nrow(exact_match) > 0) {
        processing_status(list(
          status = "exists",
          message = sprintf(
            "⚠️ EXACT processing already exists!\n\nSample: %s\nResolution: %d ppm\nSNR: %.1f\nTolerance: %.2f\nReference: %s\n\nNo processing needed.",
            sample_name, input$resolution, input$snr, input$tolerance, mz_ref$name
          )
        ))
      } else if (nrow(raw_exists) == 0 && input$data_source == "Use existing dataset") {
        processing_status(list(
          status = "error",
          message = "❌ No raw files found in database for this sample"
        ))
      } else {
        reuse_stages <- c()
        
        if (nrow(raw_exists) > 0) {
          reuse_stages <- c(reuse_stages, "✓ Raw files (will reuse from database)")
        } else if (input$data_source == "Upload new files") {
          reuse_stages <- c(reuse_stages, "• Raw files (will upload)")
        }
        
        # Check for control_mean
        mean_match <- partial_matches[partial_matches$stage_type == "control_mean", ]
        if (nrow(mean_match) > 0) {
          reuse_stages <- c(reuse_stages, "✓ Mean spectrum (will reuse)")
        } else {
          reuse_stages <- c(reuse_stages, "• Mean spectrum (will calculate)")
        }
        
        # Check for SNR reference
        snr_match <- partial_matches[
          partial_matches$stage_type == "snr_reference" & 
          !is.na(partial_matches$snr) &
          abs(partial_matches$snr - input$snr) < 0.01,
        ]
        if (nrow(snr_match) > 0) {
          reuse_stages <- c(reuse_stages, sprintf("✓ SNR reference (SNR=%.1f, will reuse)", input$snr))
        } else {
          reuse_stages <- c(reuse_stages, sprintf("• SNR reference (SNR=%.1f, will calculate)", input$snr))
        }
        
        reuse_stages <- c(reuse_stages, 
                         sprintf("• Binning (Tol=%.2f, will process)", input$tolerance),
                         sprintf("• Final dataframe (Ref=%s, will create)", mz_ref$name))
        
        processing_status(list(
          status = "ready",
          message = sprintf(
            "✅ Ready to process with these parameters:\n\nSample: %s\nResolution: %d ppm\nSNR: %.1f\nTolerance: %.2f\nReference: %s\n\nProcessing plan:\n%s",
            sample_name, input$resolution, input$snr, input$tolerance, mz_ref$name,
            paste(reuse_stages, collapse = "\n")
          )
        ))
      }
    })
    
    # Display parameter check result
    output$param_check_ui <- renderUI({
      status <- processing_status()
      req(status)
      
      if (status$status == "exists") {
        div(class = "alert alert-warning",
            style = "white-space: pre-line;",
            HTML(status$message))
      } else if (status$status == "error") {
        div(class = "alert alert-danger",
            style = "white-space: pre-line;",
            HTML(status$message))
      } else if (status$status == "ready") {
        div(class = "alert alert-success",
            style = "white-space: pre-line;",
            HTML(status$message))
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
        size <- sum(file.size(list.files(cache_dir, full.names = TRUE, recursive = TRUE))) / 1024^2
        unlink(cache_dir, recursive = TRUE)
        current_cache_dir(NULL)
        add_log(sprintf("Cleared cache: %.2f MB freed", size))
        showNotification(sprintf("Cache cleared: %.2f MB freed", size), 
                        type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Cache clear error:", e$message), 
                        type = "error", duration = NULL)
      })
    })
    
    # Run processing
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
      
      shinyjs::disable("run_processing")
      on.exit(shinyjs::enable("run_processing"))
      
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting processing pipeline...", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      processing_log("")  # Clear log
      
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
          
          add_log("Uploading raw files to MongoDB...")
          save_raw_pair_to_mongo(
            sample_name = sample_name,
            imzml_path = files$datapath[imzml_idx][1],
            ibd_path = files$datapath[ibd_idx][1],
            db_name = "MSI_test_database"
          )
          add_log("✓ Raw files saved to database")
        }
        
        progress$set(value = 30, message = "Loading MSI object...")
        add_log("Downloading raw files from database...")
        
        msi_data <- load_raw_object_from_mongo(
          sample_name = sample_name,
          workdir = cache_dir,
          db_name = "MSI_test_database"
        )
        add_log(sprintf("✓ MSI data loaded: %d pixels, %d m/z values",
                      nrow(msi_data), ncol(msi_data)))
        
        progress$set(value = 50, message = "Processing mean spectrum...")
        
        # STEP 2: Mean spectrum (check for existing)
        mean_artifacts <- mongo_meta$find(
          query = jsonlite::toJSON(list(
            sample_name = sample_name,
            stage_type = "control_mean",
            file_format = "imzML"
          ), auto_unbox = TRUE)
        )
        
        if (nrow(mean_artifacts) > 0) {
          add_log("Loading existing mean spectrum...")
          control_mean <- load_msi_stage_from_mongo(
            sample_name = sample_name,
            stage_type = "control_mean",
            db_name = "MSI_test_database"
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
            db_name = "MSI_test_database"
          )
        }
        add_log("✓ Mean spectrum ready")
        
        progress$set(value = 65, message = "Applying SNR peak picking...")
        
        # STEP 3: SNR reference
        snr_artifacts <- mongo_meta$find(
          query = jsonlite::toJSON(list(
            sample_name = sample_name,
            stage_type = "snr_reference",
            file_format = "imzML",
            snr = as.numeric(input$snr)
          ), auto_unbox = TRUE)
        )
        
        if (nrow(snr_artifacts) > 0) {
          add_log(sprintf("Loading existing SNR reference (SNR=%.1f)...", input$snr))
          control_SNR_ref <- load_msi_stage_from_mongo(
            sample_name = sample_name,
            stage_type = "snr_reference",
            db_name = "MSI_test_database"
          )
        } else {
          add_log(sprintf("Applying SNR peak picking (SNR=%.1f)...", input$snr))
          control_SNR_ref <- control_mean %>%
            peakPick(SNR = input$snr)
          
          save_msi_stage_to_mongo(
            control_SNR_ref,
            run_id,
            "snr_reference",
            sample_name = sample_name,
            params = list(snr = as.numeric(input$snr)),
            db_name = "MSI_test_database"
          )
        }
        add_log("✓ SNR reference ready")
        
        progress$set(value = 75, message = "Aligning and binning...")
        
        # STEP 4: Alignment
        add_log(sprintf("Aligning to reference (Tol=%.2f)...", input$tolerance))
        
        processing_run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        control_MSI_ref <- control_SNR_ref %>%
          peakAlign(ref = mz_ref$mz, tolerance = input$tolerance, units = "mz") %>%
          subsetFeatures() %>%
          process()
        
        save_msi_stage_to_mongo(
          control_MSI_ref,
          processing_run_id,
          "aligned_reference",
          sample_name = sample_name,
          params = list(
            snr = as.numeric(input$snr),
            tolerance = as.numeric(input$tolerance),
            reference_name = mz_ref$name
          ),
          db_name = "MSI_test_database"
        )
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
        
        save_msi_stage_to_mongo(
          msi_data_binned,
          processing_run_id,
          "binned_msi",
          sample_name = sample_name,
          params = list(
            snr = as.numeric(input$snr),
            tolerance = as.numeric(input$tolerance),
            reference_name = mz_ref$name
          ),
          db_name = "MSI_test_database"
        )
        add_log("✓ Data binned")
        
        progress$set(value = 95, message = "Creating feature matrix...")
        
        # STEP 5: Create final dataframe (still saved as RDS)
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
          db_name = "MSI_test_database"
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
  })
}


