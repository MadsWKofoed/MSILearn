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
               
               numericInput(ns("snr"), "Signal to Noise Ratio (SNR):", 
                           value = 3, min = 1.5, max = 30, step = 0.1),
               
               numericInput(ns("tolerance"), "Binning tolerance:", 
                           value = 0.5, min = 0.1, max = 3, step = 0.1),
               
               hr(),
               actionButton(ns("check_params"), "Check if processing exists", 
                           class = "btn-info"),
               br(), br(),
               actionButton(ns("run_processing"), "Run Processing", 
                           class = "btn-primary"),
               br(), br(),
               textOutput(ns("param_check_status")),
               width = 3
             ),
             
             mainPanel(
               h3("Processing Pipeline Overview"),
               uiOutput(ns("pipeline_status")),
               hr(),
               verbatimTextOutput(ns("processing_log")),
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
    
    # Reactive values for state
    processing_log <- reactiveVal("")
    current_sample_name <- reactiveVal(NULL)
    
    # Load reference options
    ref_docs <- reactive({
      mongo_ref$find(fields = '{"_id": 0, "reference_name": 1}')
    })
    
    observe({
      refs <- unique(ref_docs()$reference_name)
      if (length(refs) == 0) refs <- "No references found"
      updateSelectInput(session, "ref_csv_mongo", choices = refs)
    })
    
    # Load existing samples
    observe({
      artifacts <- query_artifacts(stage_type = "raw")
      
      if (nrow(artifacts) == 0) {
        updateSelectInput(session, "existing_sample", 
                         choices = "No samples in database")
      } else {
        samples <- unique(artifacts$sample_name)
        updateSelectInput(session, "existing_sample", choices = samples)
      }
    })
    
    # Show info about existing sample
    output$existing_info <- renderText({
      req(input$data_source == "Use existing dataset", input$existing_sample)
      
      artifacts <- query_artifacts(
        sample_name = input$existing_sample,
        stage_type = "binned_dataframe"
      )
      
      if (nrow(artifacts) == 0) {
        return("No processed versions exist yet for this sample.")
      }
      
      info <- sprintf(
        "Found %d processed version(s):\n%s",
        nrow(artifacts),
        paste(
          sprintf("- SNR: %.1f, Tol: %.2f, Ref: %s",
                 artifacts$snr, artifacts$tolerance, artifacts$reference_name),
          collapse = "\n"
        )
      )
      info
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
          mz = ref_doc$mz_values[[1]],
          name = input$ref_csv_mongo,
          source = "database"
        )
      }
    })
    
    # Check if parameters already exist
    observeEvent(input$check_params, {
      mz_ref <- selected_mz()
      
      if (is.null(mz_ref)) {
        output$param_check_status <- renderText(
          "❌ Please select a reference list first"
        )
        return()
      }
      
      # Determine sample name
      if (input$data_source == "Upload new files") {
        req(input$msi_files)
        imzml_name <- input$msi_files$name[grepl("\\.imzML$", input$msi_files$name)]
        
        sample_name <- if (nchar(input$sample_name_upload) > 0) {
          input$sample_name_upload
        } else {
          imzml_name
        }
      } else {
        req(input$existing_sample)
        sample_name <- input$existing_sample
      }
      
      # Check if exact combination exists
      existing <- query_artifacts(
        sample_name = sample_name,
        stage_type = "binned_dataframe",
        snr = as.numeric(input$snr),
        tolerance = as.numeric(input$tolerance),
        reference_name = mz_ref$name
      )
      
      if (nrow(existing) > 0) {
        output$param_check_status <- renderText(
          sprintf(
            "⚠️ Processing with these EXACT parameters already exists!\n\nSample: %s\nSNR: %.1f\nTolerance: %.2f\nReference: %s\n\nNo need to process again.",
            sample_name, input$snr, input$tolerance, mz_ref$name
          )
        )
      } else {
        output$param_check_status <- renderText(
          sprintf(
            "✅ These parameters are NEW for this sample.\n\nSample: %s\nSNR: %.1f\nTolerance: %.2f\nReference: %s\n\nReady to process!",
            sample_name, input$snr, input$tolerance, mz_ref$name
          )
        )
      }
    })
    
    # Run processing
    observeEvent(input$run_processing, {
      mz_ref <- selected_mz()
      
      if (is.null(mz_ref)) {
        showNotification("Please select a reference list first.", 
                        type = "error", duration = NULL)
        return()
      }
      
      shinyjs::disable("run_processing")
      on.exit(shinyjs::enable("run_processing"))
      
      # Prepare parameters based on data source
      if (input$data_source == "Upload new files") {
        req(input$msi_files)
        
        # Get uploaded files
        files <- input$msi_files
        imzml_idx <- grepl("\\.imzML$", files$name, ignore.case = TRUE)
        ibd_idx <- grepl("\\.ibd$", files$name, ignore.case = TRUE)
        
        if (!any(imzml_idx) || !any(ibd_idx)) {
          showNotification("Please upload both imzML and ibd files.", 
                          type = "error", duration = NULL)
          return()
        }
        
        # IMPORTANT: Use the original upload paths directly
        imzml_file <- files$datapath[imzml_idx][1]
        ibd_file <- files$datapath[ibd_idx][1]
        imzml_name <- files$name[imzml_idx][1]
        
        sample_name <- if (nchar(input$sample_name_upload) > 0) {
          input$sample_name_upload
        } else {
          tools::file_path_sans_ext(imzml_name)
        }
      }
      
      current_sample_name(sample_name)
      
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting MSI Processing Pipeline", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      # Create log output
      log_text <- ""
      add_log <- function(msg) {
        log_text <<- paste0(log_text, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
        processing_log(log_text)
      }
      
      tryCatch({
        add_log(sprintf("Starting processing for: %s", sample_name))
        add_log(sprintf("Parameters: SNR=%.1f, Tolerance=%.2f, Reference=%s",
                       input$snr, input$tolerance, mz_ref$name))
        
        progress$set(value = 5, message = "Checking existing stages...")
        
        # Check what stages already exist
        existing_stages <- get_existing_stages(sample_name)
        
        if (!is.null(existing_stages)) {
          stage_types <- sapply(existing_stages, function(s) s$stage_type)
          add_log(sprintf("Found existing stages: %s", 
                         paste(unique(stage_types), collapse = ", ")))
        } else {
          add_log("No existing stages found - full processing required")
        }
        
        progress$set(value = 10, message = "Running pipeline...")
        
        # Run the pipeline
        run_id <- process_msi_pipeline(
          imzml_path = imzml_file,
          ibd_path = ibd_file,
          imzml_name = sample_name,
          ref_mz_values = mz_ref$mz,
          ref_source = mz_ref$source,
          ref_name = mz_ref$name,
          snr = as.numeric(input$snr),
          tolerance = as.numeric(input$tolerance)
        )
        
        progress$set(value = 95, message = "Finalizing...")
        add_log(sprintf("✅ Processing complete! Run ID: %s", run_id))
        
        progress$set(value = 100, message = "Complete!")
        
        showNotification(
          sprintf("Processing complete!\nSample: %s\nRun ID: %s", 
                 sample_name, run_id),
          type = "message",
          duration = 10
        )
        
        # Update pipeline status display
        output$pipeline_status <- renderUI({
          final_artifact <- query_artifacts(
            sample_name = sample_name,
            stage_type = "binned_dataframe",
            snr = as.numeric(input$snr),
            tolerance = as.numeric(input$tolerance),
            reference_name = mz_ref$name
          )
          
          if (nrow(final_artifact) > 0) {
            tagList(
              div(class = "alert alert-success",
                  h4("✅ Processing Complete"),
                  p(sprintf("Sample: %s", sample_name)),
                  p(sprintf("Features: %d", final_artifact$num_features[1])),
                  p(sprintf("Pixels: %d", final_artifact$num_pixels[1])),
                  p(sprintf("Created: %s", final_artifact$created_at[1]))
              )
            )
          }
        })
        
      }, error = function(e) {
        add_log(sprintf("❌ ERROR: %s", e$message))
        
        showNotification(
          paste("Processing error:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
    
    # Display processing log
    output$processing_log <- renderText({
      processing_log()
    })
  })
}



