# global.R

options(shiny.maxRequestSize = 5000 * 1024^2)
options(shiny.launch.browser = TRUE)

# Parallel settings
bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# MongoDB connections
msi_con <- mongo(
  collection = "msi_data",
  db = "msi_project",
  url = "mongodb://localhost"
)

mz_ref_db <- mongo(collection = "mz_references",
                   db = "MSI_database",
                   url = "mongodb://localhost"
                   )

# Source function files
source("R/clustering_functions.R")
source("R/processing_functions.R")
source("R/mongo_functions.R")

# Source modules
source("R/modules/clustering_module.R")
source("R/modules/prediction_module.R")
source("R/modules/processing_module.R")

# Global UI
ui <- navbarPage(
  title = "MSI Clustering & Prediction",
  tabPanel("Welcome",
           h3("Welcome to the MSI Clustering App"),
           p("Upload imzML + ibd files, perform clustering, and compare to histology.")
  ),
  processing_module_ui("processing"),
  clustering_module_ui("clustering"),
  prediction_module_ui("prediction")
)

server <- function(input, output, session) {
  processing_module_server("processing")
  clustering_module_server("clustering", msi_con)
  prediction_module_server("prediction")
}
