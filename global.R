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
.welcome-container{
  max-width: 1100px;
  margin: 0 auto;
  padding: 20px 10px 30px 10px;
}

.lead{
  font-size: 18px;
  color: #555;
  margin-bottom: 18px;
}

.lead-small{
  font-size: 15px;
  color: #666;
}

.welcome-card{
  background: #f8f9fa;
  padding: 22px 18px;
  border-radius: 12px;
  text-align: center;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  transition: 0.2s;
  min-height: 170px;
}

.welcome-card:hover{
  transform: translateY(-4px);
  box-shadow: 0 8px 18px rgba(0,0,0,0.12);
}

.welcome-card .btn{
  margin-top: 8px;
}

.welcome-detail-box{
  background: #ffffff;
  border: 1px solid #e3e7ee;
  border-radius: 12px;
  padding: 18px 20px;
  margin-top: 10px;
  margin-bottom: 20px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.06);
}

.small-box{
  min-height: 120px;
}

.data-object-btn{
  width: 100%;
  background: #ffffff;
  border: 1px solid #d9e1ec;
  border-radius: 10px;
  padding: 14px 10px;
  font-weight: 600;
  color: #2c3e50;
  box-shadow: 0 2px 8px rgba(0,0,0,0.04);
}

.data-object-btn:hover{
  background: #f4f8fc;
  border-color: #b8cbe2;
}

.data-object-btn:focus,
.data-object-btn:active{
  outline: none !important;
  box-shadow: 0 0 0 2px rgba(70,130,180,0.15);
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
