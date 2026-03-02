# global.R

options(shiny.maxRequestSize = 5000 * 1024^2)
options(shiny.launch.browser = TRUE)

# Parallel settings
bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# MongoDB connections
msi_con <- mongo(
  collection = "msi_data",
  db = "MSI_database",  
  url = "mongodb://localhost:27018"
)

# Custom CSS for font
custom_css <- tags$head(
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"
  ),
  tags$style(HTML("
    * {
      font-family: 'Roboto', sans-serif !important;
    }
  "))
)

# Source function files
source("R/mongo_schema.R")        # schema initialisation (indexes)
source("R/mongo_functions.R")     # all DB helpers (provenance API + legacy)
source("R/clustering_functions.R")
source("R/processing_functions.R")
source("R/training_functions.R")

# Enforce schema indexes on startup (idempotent)
tryCatch(
  initialise_schema(),
  error = function(e) warning("Schema initialisation failed: ", conditionMessage(e))
)

# Source modules
source("R/modules/clustering_module.R")
source("R/modules/prediction_module.R")
source("R/modules/processing_module.R")
source("R/modules/training_module.R")

# Global UI
ui <- navbarPage(
  title = "MSI Clustering & Prediction",
  custom_css,
  useShinyjs(),  
  tabPanel("Welcome",
           h3("Welcome to the MSI Clustering App"),
           p("Upload imzML + ibd files, perform clustering, and compare to histology.")
  ),
  processing_module_ui("processing"),
  clustering_module_ui("clustering"),
  prediction_module_ui("prediction"),
  training_module_ui("training")
)


server <- function(input, output, session) {
  processing_module_server("processing")
  clustering_module_server("clustering", msi_con)
  prediction_module_server("prediction")
  training_module_server("training")
}
