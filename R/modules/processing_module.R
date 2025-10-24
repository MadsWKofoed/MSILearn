# R/modules/processing_module.R
processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Processing",
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("msi_files"), "Upload imzML + ibd files", multiple = TRUE,
                         accept = c(".imzML", ".ibd")),
               
               radioButtons(
                 ns("ref_source"),
                 "Reference list source:",
                 choices = c("From database", "Upload your own"),
                 selected = "From database"
               ),
               
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'Upload your own'", ns("ref_source")),
                 fileInput(ns("ref_csv"), "Upload list of m/z for alignment",
                           multiple = FALSE, accept = ".csv")
               ),
               
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'From database'", ns("ref_source")),
                 selectInput(ns("ref_csv_mongo"), "Select reference list from DB:", choices = "Loading...")
               ),
               
               numericInput(ns("snr"), "Signal to Noise Ratio (SNR):", value = 3,
                            min = 1.5, max = 30, step = 0.1),
               
               numericInput(ns("tolerance"), "Binning tolerance:", value = 0.5,
                            min = 0.1, max = 3, step = 0.1),
               
               actionButton(ns("run_processing"), "Run Full Processing"),
               br(), br(),
               textOutput(ns("run_status")),
               width = 2
             ),
             mainPanel(
               uiOutput(ns("illustration_layout")),
               width = 10
             )
           )
  )
}


processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mongo_ref <- mongo(collection = "mz_references",
                       db = "msi_project",
                       url = "mongodb://localhost")
    
    ref_docs <- reactive({
      mongo_ref$find(fields = '{"_id": 0, "reference_name": 1}')
    })
    
    observe({
      refs <- unique(ref_docs()$reference_name)
      if (length(refs) == 0) refs <- "No references found"
      updateSelectInput(session, "ref_csv_mongo", choices = refs)
    })
    
    selected_mz <- reactive({
      req(input$ref_source)
      if (input$ref_source == "Upload your own") {
        req(input$ref_csv_upload)
        read.csv(input$ref_csv_upload$datapath, stringsAsFactors = FALSE)
      } else {
        req(input$ref_csv_mongo)
        ref_doc <- mongo_ref$find(
          query = paste0('{"reference_name": "', input$ref_csv_mongo, '"}'),
          fields = '{"_id": 0, "mz_values": 1}'
        )
        if (nrow(ref_doc) == 0) return(NULL)
        data.frame(mz = ref_doc$mz_values[[1]])
      }
    })
    
    # --- Updated processing with progress feedback ---
    observeEvent(input$run_processing, {
      req(input$msi_files)
      
      # Disable button during processing
      shinyjs::disable("run_processing")
      on.exit(shinyjs::enable("run_processing"))
      
      mz_ref <- selected_mz()
      
      imzml_file <- input$msi_files$datapath[grepl("\\.imzML$", input$msi_files$name)]
      ibd_file   <- input$msi_files$datapath[grepl("\\.ibd$", input$msi_files$name)]
      imzml_name <- input$msi_files$name[grepl("\\.imzML$", input$msi_files$name)]
      
      if (is.null(mz_ref)) {
        showNotification("Please select or upload a reference list first.", 
                        type = "error", duration = NULL)
        return()
      }
      
      # Create progress object
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting MSI Processing", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      tryCatch({
        # Get parameters
        ref_name <- if (input$ref_source == "Upload your own") {
          tools::file_path_sans_ext(basename(input$ref_csv_upload$name))
        } else {
          input$ref_csv_mongo
        }
        
        ref_source <- if (input$ref_source == "Upload your own") "upload" else "database"
        snr_val <- as.numeric(input$snr)
        tol_val <- as.numeric(input$tolerance)
        
        progress$set(value = 5, message = "Checking for existing data...")
        
        # Check for duplicates
        existing_binned <- query_artifacts(
          sample_name = imzml_name,
          stage_type = "binned_dataframe"
        )
        
        if (nrow(existing_binned) > 0) {
          for (i in seq_len(nrow(existing_binned))) {
            row <- existing_binned[i, ]
            
            snr_match <- !is.null(row$snr) && 
                         isTRUE(all.equal(as.numeric(row$snr), as.numeric(snr_val)))
            tol_match <- !is.null(row$tolerance) && 
                         isTRUE(all.equal(as.numeric(row$tolerance), as.numeric(tol_val)))
            ref_match <- !is.null(row$reference_name) && 
                         identical(as.character(row$reference_name), as.character(ref_name))
            
            if (snr_match && tol_match && ref_match) {
              showNotification(
                "Processing with identical parameters already exists. No action needed.",
                type = "warning",
                duration = NULL
              )
              return()
            }
          }
        }
        
        existing_stages <- get_existing_stages(imzml_name)
        raw_exists <- !is.null(existing_stages) && 
          any(sapply(existing_stages, function(s) s$stage_type == "raw"))
        
        # STEP 1: Raw data
        if (raw_exists) {
          progress$set(value = 15, message = "♻️ Reusing existing raw data...")
          
          run_id <- find_compatible_run(imzml_name)
          raw_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "raw")
          msi_data <- load_artifact_by_id(raw_artifact$gridfs_id[1])
          
          progress$set(value = 25, message = "📥 Loading control mean...")
          control_mean_artifact <- query_artifacts(sample_name = imzml_name, 
                                                   stage_type = "control_mean")
          
          if (nrow(control_mean_artifact) > 0) {
            control_mean <- load_artifact_by_id(control_mean_artifact$gridfs_id[1])
          } else {
            progress$set(value = 30, message = "📊 Computing control mean...")
            control_mean <- summarizeFeatures(msi_data, "mean")
            save_stage_to_mongo(control_mean, run_id, "control_mean", 
                               sample_name = imzml_name)
          }
          
        } else {
          progress$set(value = 10, message = "🆕 Importing raw data...")
          
          run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
          
          progress$set(value = 15, message = "📖 Reading imzML file (this may take several minutes)...")
          
          base <- tools::file_path_sans_ext(basename(imzml_name))
          temp_dir <- tempfile(); dir.create(temp_dir)
          temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
          temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
          
          file.copy(imzml_file, temp_imzml, overwrite = TRUE)
          file.copy(ibd_file, temp_ibd, overwrite = TRUE)
          
          msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                               mass.range = NULL, resolution = 10, units = c("ppm"),
                               guess.max = 1000L, as = "auto", parse.only = FALSE,
                               verbose = FALSE, chunkopts = list(),
                               BPPARAM = bpparam())
          
          progress$set(value = 35, message = "💾 Saving raw data to database...")
          save_stage_to_mongo(msi_data, run_id, "raw", sample_name = imzml_name)
          
          progress$set(value = 40, message = "📊 Computing mean spectrum...")
          control_mean <- summarizeFeatures(msi_data, "mean")
          
          progress$set(value = 45, message = "💾 Saving control mean...")
          save_stage_to_mongo(control_mean, run_id, "control_mean", 
                             sample_name = imzml_name)
        }
        
        # STEP 2: Reference
        progress$set(value = 50, message = "🔍 Checking for reference spectrum...")
        
        existing_refs <- query_artifacts(
          sample_name = imzml_name,
          stage_type = "mean_snr_reference"
        )
        
        ref_exists <- FALSE
        if (nrow(existing_refs) > 0) {
          for (i in seq_len(nrow(existing_refs))) {
            row <- existing_refs[i, ]
            if (!is.null(row$snr) && 
                isTRUE(all.equal(as.numeric(row$snr), as.numeric(snr_val)))) {
              ref_exists <- TRUE
              existing_ref_id <- row$gridfs_id
              break
            }
          }
        }
        
        if (ref_exists) {
          progress$set(value = 55, message = paste0("♻️ Reusing reference (SNR=", snr_val, ")..."))
          control_SNR_ref <- load_artifact_by_id(existing_ref_id)
        } else {
          progress$set(value = 55, message = paste0("🔬 Creating reference (SNR=", snr_val, ")..."))
          
          control_SNR_ref <- control_mean %>%
            peakPick(SNR = snr_val)
          
          n_peaks <- length(mz(control_SNR_ref))
          
          progress$set(value = 60, 
                      message = paste0("   Found ", n_peaks, " peaks, saving..."))
          
          save_stage_to_mongo(
            control_SNR_ref, 
            run_id, 
            "mean_snr_reference",
            sample_name = imzml_name,
            params = list(snr = snr_val)
          )
        }
        
        # STEP 3: Binning
        progress$set(value = 65, message = "🎯 Aligning to reference database...")
        
        control_MSI_ref <- control_SNR_ref %>%
          peakAlign(ref = mz_ref$mz, tolerance = tol_val, units = "mz") %>%
          subsetFeatures() %>%
          process()
        
        n_aligned <- length(mz(control_MSI_ref))
        
        progress$set(value = 70, 
                    message = paste0("   Aligned to ", n_aligned, " features"))
        
        n_pixels <- nrow(coord(msi_data))
        progress$set(value = 75, 
                    message = paste0("📦 Binning ", n_pixels, " pixels..."))
        
        msi_data_binned <- bin(msi_data, ref = mz(control_MSI_ref),
                              tolerance = tol_val, units = "mz", 
                              BPPARAM = bpparam()) %>%
          process()
        
        progress$set(value = 85, message = "🔢 Building feature matrix...")
        
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
        
        progress$set(value = 95, message = "💾 Saving final dataset...")
        
        save_stage_to_mongo(
          full_df, 
          run_id, 
          "binned_dataframe",
          sample_name = imzml_name,
          params = list(
            snr = snr_val,
            tolerance = tol_val,
            reference_name = ref_name,
            reference_source = ref_source,
            num_features = ncol(full_df) - 3,
            num_pixels = nrow(full_df)
          )
        )
        
        progress$set(value = 100, message = "✅ Complete!")
        
        showNotification(
          paste0("Processing complete! Dataset: ", nrow(full_df), " pixels × ", 
                ncol(full_df) - 3, " features"),
          type = "message",
          duration = 10
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )
      })
      
    })
  })
}



