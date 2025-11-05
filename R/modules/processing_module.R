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
        req(input$ref_csv)
        mz_data <- read.csv(input$ref_csv$datapath)
        return(list(
          source = "uploaded",
          name = input$ref_csv$name,
          values = mz_data[[1]]
        ))
      } else {
        req(input$ref_csv_mongo)
        doc <- mongo_ref$find(paste0('{"reference_name": "', input$ref_csv_mongo, '"}'))
        return(list(
          source = "database",
          name = input$ref_csv_mongo,
          values = doc$mz_values[[1]]
        ))
      }
    })
    
    # --- Updated processing with smart error handling ---
    observeEvent(input$run_processing, {
      req(input$msi_files)
      mz_ref <- selected_mz()
      
      # Get uploaded file info
      files <- input$msi_files
      
      # Find imzML and ibd files (case-insensitive)
      imzml_idx <- grepl("\\.imzML$", files$name, ignore.case = TRUE)
      ibd_idx <- grepl("\\.ibd$", files$name, ignore.case = TRUE)
      
      if (!any(imzml_idx) || !any(ibd_idx)) {
        showNotification("Please upload both imzML and ibd files.", 
                        type = "error", duration = NULL)
        return()
      }
      
      # Use the Shiny-provided temporary paths (these are the actual file locations)
      imzml_file <- files$datapath[imzml_idx][1]
      ibd_file <- files$datapath[ibd_idx][1]
      
      # But use the ORIGINAL filename for naming (not the temp path)
      imzml_name <- files$name[imzml_idx][1]
      
      message("Processing upload:")
      message("  Original imzML name: ", imzml_name)
      message("  Temp imzML path: ", imzml_file)
      message("  Temp ibd path: ", ibd_file)
      
      snr_val <- input$snr
      tol_val <- input$tolerance
      
      output$run_status <- renderText("Checking for existing data...")
      
      run_id <- tryCatch({
        process_msi_pipeline(
          imzml_path = imzml_file,      # Shiny's temp path
          ibd_path   = ibd_file,         # Shiny's temp path  
          imzml_name = imzml_name,       # Original filename
          ref_mz_values = mz_ref$values,
          ref_source = mz_ref$source,
          ref_name = mz_ref$name,
          snr = snr_val,
          tolerance = tol_val
        )
      }, error = function(e) {
        msg <- conditionMessage(e)
        
        if (grepl("identical parameters already exists", msg)) {
          output$run_status <- renderText("⚠️ Data already processed with these exact parameters. No action needed.")
          showNotification(
            "This dataset with identical parameters already exists in the database.",
            type = "warning",
            duration = 8
          )
        } else {
          output$run_status <- renderText(paste("❌ Error:", msg))
          showNotification(
            paste("Processing failed:", msg),
            type = "error",
            duration = 10
          )
        }
        return(NULL)
      })
      
      if (!is.null(run_id)) {
        output$run_status <- renderText(paste("✅ Processing complete. Run ID:", run_id))
        showNotification(
          paste("Processing successful! Run ID:", run_id),
          type = "message",
          duration = 6
        )
      }
    })
  })
}