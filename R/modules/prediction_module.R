# R/modules/prediction_module.R

prediction_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Prediction",
           h3("Prediction page"),
           p("This is where tissue classification or other predictions could be implemented.")
  )
}

prediction_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # future server logic goes here...
  })
}
