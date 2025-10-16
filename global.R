# global.R

options(shiny.maxRequestSize = 5000 * 1024^2)
options(shiny.launch.browser = TRUE)

# Parallel settings
bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# MongoDB connection
msi_con <- mongo(
  collection = "msi_data",
  db = "msi_project",
  url = "mongodb://localhost"
)

# Source function files
source("R/clustering_functions.R")
source("R/processing_functions.R")
source("R/mongo_functions.R")

# Source modules
source("R/modules/clustering_module.R")
source("R/modules/prediction_module.R")

# Global UI
ui <- navbarPage(
  title = "MSI Clustering & Prediction",
  tabPanel("Welcome",
           h3("Welcome to the MSI Clustering App"),
           p("Upload imzML + ibd files, perform clustering, and compare to histology.")
  ),
  clustering_module_ui("clustering"),
  prediction_module_ui("prediction")
)

server <- function(input, output, session) {
  clustering_module_server("clustering", msi_con)
  prediction_module_server("prediction")
}
