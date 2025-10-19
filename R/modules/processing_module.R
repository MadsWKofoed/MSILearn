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
               
               # Only show this if "Upload your own" is chosen
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'Upload your own'", ns("ref_source")),
                 fileInput(ns("ref_csv"), "Upload list of m/z for alignment",
                           multiple = FALSE, accept = ".csv")
               ),
               
               # Only show this if "From database" is chosen
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'From database'", ns("ref_source")),
                 selectInput(ns("ref_csv_mongo"), "Select reference list from DB:", choices = NULL)
               ),
               
               numericInput(ns("snr"), "Signal to Noise Ratio (SNR):", value = 3, min = 1.5, max = 30, step = 0.1),
               
               numericInput(ns("tolerance"), "Binning tolerance:", value = 0.5, min = 0.1, max = 3, step = 0.1),
               
               
               actionButton(ns("run_processing"), "Run Processing"),
               

               actionButton(ns("commit_db"), "Commit processed data to MongoDB"),
               width = 2
             ),
             mainPanel(uiOutput(ns("illustration_layout")), width = 10)
             
             # Select among the m/z's to illustrate heatmap of the MSI data colored according to intensity
           )
  )
}

processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Mongo connection
    mongo_ref <- mongo(collection = "mz_references",
                       db = "msi_project",
                       url = "mongodb://localhost"
                       )
    
    # Get reference names from MongoDB
    ref_docs <- reactive({
      mongo_ref$find(fields = '{"_id": 0, "reference_name": 1}')
    })
    
    observe({
      updateSelectInput(session, "ref_csv_mongo",
                        choices = unique(ref_docs()$reference_name))
    })
    
    # Reactive that returns the actual m/z vector depending on source
    selected_mz <- reactive({
      req(input$ref_source)
      if (input$ref_source == "Upload your own") {
        req(input$ref_csv)
        mz_data <- read.csv(input$ref_csv$datapath)
        mz_col <- mz_data[[1]]  # assume first column is mz
        return(mz_col)
      } else {
        req(input$ref_csv_mongo)
        doc <- mongo_ref$find(
          paste0('{"reference_name": "', input$ref_csv_mongo, '"}')
        )
        return(doc$mz_values[[1]])
      }
    })
    
    
    # Example: when running processing
    observeEvent(input$run_processing, {
      mz_ref <- selected_mz()
      
      cat("Using", length(mz_ref), "m/z reference values\n")
      
      # Continue with your alignment / processing logic...
    })
    
  })
  
}

