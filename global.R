# global.R

options(shiny.maxRequestSize = 5000 * 1024^2)
options(shiny.launch.browser = TRUE)

# Parallel settings
bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# MongoDB connections
msi_con <- mongo(
  collection = "msi_data",
  db = "MSI_database_test",  
  url = "mongodb://localhost:27018"
)

# Custom CSS
custom_css <- tags$style(HTML("

/* Welcome cards */
.welcome-card{
  background:#f8f9fa;
  padding:20px;
  border-radius:10px;
  text-align:center;
  box-shadow:0 2px 6px rgba(0,0,0,0.1);
  transition:0.2s;
}

.welcome-card:hover{
  transform:translateY(-5px);
  box-shadow:0 6px 12px rgba(0,0,0,0.15);
}

.welcome-container{
  max-width:1200px;
  margin:auto;
}

.lead{
  font-size:18px;
  color:#555;
}

"))

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
source("R/modules/welcome_module.R")
source("R/modules/clustering_module.R")
source("R/modules/prediction_module.R")
source("R/modules/processing_module.R")
source("R/modules/training_module.R")

# Global UI
ui <- navbarPage(
  title  = "MSI Clustering & Prediction",
  header = tagList(custom_css, useShinyjs()),
  welcome_module_ui("welcome"),
  processing_module_ui("processing"),
  clustering_module_ui("clustering"),
  prediction_module_ui("prediction"),
  training_module_ui("training")
)


server <- function(input, output, session) {
  welcome_module_server("welcome")
  processing_module_server("processing")
  clustering_module_server("clustering")
  prediction_module_server("prediction")
  training_module_server("training")
}
