# global.R

options(shiny.maxRequestSize = 5000 * 1024^2)
options(shiny.launch.browser = TRUE)

# Parallel settings (shared across app)
bp <- max(1L, parallel::detectCores(logical = FALSE) - 22L)

# One canonical BiocParallel backend for the whole app
msi_bpparam <- BiocParallel::MulticoreParam(workers = bp)
BiocParallel::register(msi_bpparam, default = TRUE)

# Cardinal parallel workers
setCardinalParallel(workers = bp)

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
  margin-bottom: 16px;
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

.soft-box{
  background: #fafbfd;
}

.object-flow-wrap{
  background: #ffffff;
  border: 1px solid #e3e7ee;
  border-radius: 14px;
  padding: 20px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.05);
  margin-bottom: 12px;
}

.object-flow-row{
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 12px;
  flex-wrap: wrap;
}

.flow-break{
  height: 16px;
}

.flow-object-btn{
  background: #f8fbff;
  border: 1px solid #cfdceb;
  border-radius: 12px;
  padding: 12px 18px;
  min-width: 145px;
  font-weight: 600;
  color: #24415f;
  box-shadow: 0 2px 6px rgba(0,0,0,0.04);
}

.flow-object-btn:hover{
  background: #edf5ff;
  border-color: #a9c3e3;
}

.flow-object-btn:focus,
.flow-object-btn:active{
  outline: none !important;
  box-shadow: 0 0 0 2px rgba(70,130,180,0.15);
}

.flow-arrow{
  font-size: 26px;
  font-weight: 700;
  color: #9aa9bb;
  line-height: 1;
  padding: 0 2px;
}

@media (max-width: 992px){
  .object-flow-row{
    flex-direction: column;
  }

  .flow-arrow{
    transform: rotate(90deg);
  }
}
"))

# Source function files
source("R/mongo_schema.R")        # schema initialisation (indexes)
source("R/mongo_functions.R")     # all DB helpers (provenance API + legacy)
source("R/clustering_functions.R")
source("R/processing_functions.R")
source("R/training_functions.R")
source("R/prediction_functions.R")
source("R/ndpi_registration_utils.R")
source("R/database_management_functions.R")

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
source("R/modules/database_management_module.R")

# Global UI
ui <- navbarPage(
  title  = "MSI Clustering & Prediction",
  header = tagList(custom_css, useShinyjs()),
  welcome_module_ui("welcome"),
  processing_module_ui("processing"),
  clustering_module_ui("clustering"),
  training_module_ui("training"),
  prediction_module_ui("prediction"),
  database_management_module_ui("db_management")
)


server <- function(input, output, session) {
  welcome_module_server("welcome")
  processing_module_server("processing")
  clustering_module_server("clustering")
  training_module_server("training")
  prediction_module_server("prediction")
  database_management_module_server("db_management")
}
