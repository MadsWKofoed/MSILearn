# R/modules/processing_module.R

processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Processing",
           h3("Processing page"),
           p("Processing of MSI data and save")
  )
}

processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # future server logic goes here...
  })
}