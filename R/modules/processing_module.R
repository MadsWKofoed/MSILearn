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
      
      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting MSI Processing", value = 0)
      on.exit(progress$close(), add = TRUE)
      
      tryCatch({
        ref_name <- if (input$ref_source == "Upload your own") {
          tools::file_path_sans_ext(basename(input$ref_csv$name))
        } else {
          input$ref_csv_mongo
        }
        
        ref_source <- if (input$ref_source == "Upload your own") "upload" else "database"
        snr_val <- as.numeric(input$snr)
        tol_val <- as.numeric(input$tolerance)
        
        progress$set(value = 10, message = "Processing...")
        
        # Just call the pipeline - it handles everything
        run_id <- process_msi_pipeline(
          imzml_path = imzml_file,
          ibd_path = ibd_file,
          imzml_name = imzml_name,
          ref_mz_values = mz_ref$mz,
          ref_source = ref_source,
          ref_name = ref_name,
          snr = snr_val,
          tolerance = tol_val
        )
        
        progress$set(value = 100, message = "✅ Complete!")
        
        showNotification(
          "Processing complete!",
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



