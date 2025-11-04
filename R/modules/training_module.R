# R/modules/training_module.R

training_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Train Machine Learning Model",
           h3("Training page"),
           p("This is where training of new machine learning models are trained on selected data within the database.")
  )
}

training_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # future server logic goes here...
  })
}
