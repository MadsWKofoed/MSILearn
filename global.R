# global.R

options(shiny.maxRequestSize = 10000 * 1024^2)
options(shiny.launch.browser = TRUE)

source("R/config.R")

# Parallel settings (shared across app)
bp <- app_worker_count()
parallel_backend <- tolower(.env_or_default("APP_BIOCPARALLEL_BACKEND", "snow"))

# One canonical BiocParallel backend for the whole app
msi_bpparam <- if (bp <= 1L) {
  BiocParallel::SerialParam()
} else if (identical(parallel_backend, "multicore")) {
  BiocParallel::MulticoreParam(workers = bp)
} else {
  BiocParallel::SnowParam(workers = bp, type = "SOCK")
}
BiocParallel::register(msi_bpparam, default = TRUE)

# Cardinal parallel workers
setCardinalParallel(workers = bp)

# Custom CSS
custom_css <- tags$style(HTML("
body{
  background: linear-gradient(180deg, #f6fafb 0%, #eef3f6 100%);
  color: #14213d;
}

.navbar{
  border: 0;
  box-shadow: 0 12px 28px rgba(15, 23, 42, 0.08);
}

.navbar-default{
  background:
    radial-gradient(circle at top left, rgba(15,118,110,0.16), transparent 34%),
    linear-gradient(135deg, #fcfdff 0%, #eef6f6 100%);
}

.navbar-default .navbar-brand{
  color: #14213d !important;
  font-weight: 800;
  letter-spacing: -0.02em;
}

.navbar-default .navbar-nav > li > a{
  color: #52606d !important;
  font-weight: 700;
  font-size: 15px;
  border-radius: 0;
  margin-top: 8px;
  padding-top: 14px;
  padding-bottom: 14px;
  padding-left: 16px;
  padding-right: 16px;
  border-bottom: 3px solid transparent;
}

.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:focus{
  color: #14213d !important;
  background: transparent !important;
  border-bottom-color: rgba(20, 33, 61, 0.18);
}

.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus{
  color: #14213d !important;
  background: transparent !important;
  border-bottom-color: #1d4ed8 !important;
  box-shadow: none;
}

.app-shell{
  --app-ink:#14213d;
  --app-muted:#5b6472;
  --app-border:#d8e0ea;
  --app-soft:#f4f7fb;
  --app-panel:#ffffff;
  --app-accent:#0f766e;
  --app-accent-soft:rgba(15,118,110,0.12);
  --app-accent-warm:#f59e0b;
  --app-danger:#b91c1c;
  --app-shadow:0 16px 40px rgba(15, 23, 42, 0.08);
  padding: 12px 6px 28px 6px;
}

.app-hero{
  background:
    radial-gradient(circle at top left, rgba(15,118,110,0.18), transparent 38%),
    radial-gradient(circle at top right, rgba(245,158,11,0.14), transparent 28%),
    linear-gradient(135deg, #fcfdff 0%, #eef6f6 100%);
  border: 1px solid #d9ece9;
  border-radius: 24px;
  padding: 24px 26px;
  margin-bottom: 18px;
  box-shadow: var(--app-shadow);
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 20px;
  flex-wrap: wrap;
}

.app-hero h3{
  margin: 0 0 8px 0;
  font-size: 30px;
  letter-spacing: -0.03em;
  color: var(--app-ink);
}

.app-hero p{
  margin: 0;
  max-width: 760px;
  color: var(--app-muted);
  line-height: 1.6;
  font-size: 14px;
}

.app-hero-actions .btn{
  border-radius: 999px;
  padding: 10px 18px;
  font-weight: 700;
  box-shadow: 0 8px 18px rgba(15,118,110,0.16);
}

.app-stack > * + *{
  margin-top: 14px;
}

.app-panel{
  background: var(--app-panel);
  border: 1px solid var(--app-border);
  border-radius: 22px;
  box-shadow: var(--app-shadow);
  overflow: hidden;
}

.app-panel-head{
  padding: 16px 18px 12px 18px;
  border-bottom: 1px solid #e6edf5;
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 12px;
  flex-wrap: wrap;
  background: linear-gradient(180deg, #ffffff 0%, #fbfcfe 100%);
}

.app-panel-title{
  font-size: 16px;
  font-weight: 800;
  color: var(--app-ink);
  margin: 0;
}

.app-panel-subtitle{
  font-size: 12px;
  color: var(--app-muted);
  margin-top: 4px;
  line-height: 1.5;
  max-width: 760px;
}

.app-panel-body{
  padding: 16px 18px 18px 18px;
}

.app-accordion-item{
  border: 1px solid var(--app-border);
  border-radius: 20px;
  background: linear-gradient(180deg, rgba(255,255,255,0.98) 0%, rgba(249,251,253,0.98) 100%);
  overflow: hidden;
  position: relative;
  box-shadow: var(--app-shadow);
  transition: transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
}

.app-accordion-item.app-accordion-overflow-visible,
.app-accordion-item.app-accordion-overflow-visible .app-accordion-body{
  overflow: visible;
}

.app-accordion-item:not(:has(.app-accordion-body.in)):hover{
  transform: translateY(-3px);
  box-shadow: 0 18px 34px rgba(15, 23, 42, 0.10);
  border-color: #c8ddd8;
}

.app-accordion-head{
  padding: 14px 16px;
  font-weight: 800;
  font-size: 15px;
  background: linear-gradient(180deg, #ffffff 0%, #f7fafc 100%);
  border-bottom: 1px solid #eef2f7;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
}

.app-accordion-title{
  display: flex;
  align-items: center;
  gap: 10px;
  color: var(--app-ink);
}

.app-step-num{
  width: 26px;
  height: 26px;
  border-radius: 999px;
  background: var(--app-accent-soft);
  color: var(--app-accent);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 12px;
  font-weight: 800;
  flex: 0 0 auto;
  border: 1px solid rgba(15,118,110,0.18);
}

.app-step-status{
  font-size: 11px;
  font-weight: 800;
  padding: 4px 9px;
  border-radius: 999px;
  background: var(--app-accent-soft);
  color: var(--app-accent);
  white-space: nowrap;
}

.app-accordion-body{
  padding: 14px 16px 16px 16px;
}

.app-helper{
  background: linear-gradient(180deg, #f7fafc 0%, #f4f9f8 100%);
  border: 1px solid #dfe8ee;
  border-radius: 16px;
  padding: 12px 14px;
  font-size: 12px;
  color: #435063;
  line-height: 1.6;
}

.app-helper strong{
  color: var(--app-ink);
}

.app-helper-muted,
.app-mini-note{
  font-size: 12px;
  color: var(--app-muted);
  line-height: 1.45;
}

.app-subtitle{
  font-size: 12px;
  font-weight: 800;
  color: #374151;
  margin-top: 10px;
  margin-bottom: 6px;
  text-transform: uppercase;
  letter-spacing: 0.03em;
}

.app-divider{
  margin: 12px 0;
  border-top: 1px solid #eef2f7;
}

.app-session-box{
  background: linear-gradient(135deg, #f7fafc 0%, #fdfdfc 100%);
  border: 1px solid #dfe8ee;
  border-radius: 16px;
  padding: 12px 14px;
  box-shadow: 0 1px 6px rgba(15, 23, 42, 0.05);
}

.app-session-title{
  font-size: 12px;
  font-weight: 800;
  color: var(--app-accent);
  text-transform: uppercase;
  letter-spacing: 0.03em;
  margin-bottom: 6px;
}

.app-session-box table{
  width: 100%;
  font-size: 12px;
  margin-bottom: 0;
}

.app-session-box td{
  padding: 2px 0;
  vertical-align: top;
}

.app-session-box td:first-child{
  color: #64748b;
  width: 38%;
}

.app-session-box td:last-child{
  color: #111827;
  font-weight: 500;
  word-break: break-word;
}

.app-btn-block{
  width: 100%;
  margin-bottom: 8px;
}

.app-plot-wrap{
  background: #fbfcfe;
  border: 1px solid #e4ebf2;
  border-radius: 16px;
  padding: 10px;
}

.app-shell .well{
  background: transparent;
  border: 0;
  box-shadow: none;
  padding: 0;
  margin-bottom: 0;
}

.app-shell .nav-tabs{
  border-bottom: 1px solid #dfe6ee;
}

.app-shell .nav-tabs > li > a{
  color: #506071;
  font-weight: 700;
  border-radius: 12px 12px 0 0;
}

.app-shell .nav-tabs > li.active > a,
.app-shell .nav-tabs > li.active > a:hover,
.app-shell .nav-tabs > li.active > a:focus{
  color: #14213d;
  background: #ffffff;
  border: 1px solid #dfe6ee;
  border-bottom-color: transparent;
}

.app-shell .form-control,
.app-shell .selectize-input{
  border-radius: 14px;
  border-color: #d6e0e8;
  box-shadow: none;
}

.app-shell .app-accordion-overflow-visible .selectize-control{
  position: relative;
  z-index: 25;
}

.app-shell .app-accordion-overflow-visible .selectize-dropdown{
  z-index: 3000;
}

.app-shell .btn-default{
  border-radius: 12px;
  border-color: #d6e0e8;
  background: #ffffff;
  color: #324154;
  font-weight: 700;
}

.app-shell .btn-primary{
  background: #f6ddb0;
  border-color: #e8cea0;
  color: #14213d;
  font-weight: 700;
}

.app-shell .btn-success{
  background: #f8e4bc;
  border-color: #e8cea0;
  color: #14213d;
  font-weight: 700;
}

.app-shell .btn-info{
  background: #d7ece8;
  border-color: #c6e2dc;
  color: #115e59;
  font-weight: 700;
}

.app-shell .btn-warning{
  background: #f59e0b;
  border-color: #f59e0b;
  color: #ffffff;
  font-weight: 700;
}

.welcome-container{
  max-width: 1120px;
  margin: 0 auto;
  padding: 6px 10px 30px 10px;
}

.lead{
  font-size: 18px;
  color: #5b6472;
  margin-bottom: 18px;
  line-height: 1.6;
}

.lead-small{
  font-size: 15px;
  color: #5b6472;
  margin-bottom: 16px;
  line-height: 1.55;
}

.welcome-card{
  background: linear-gradient(180deg, rgba(255,255,255,0.98) 0%, rgba(248,250,252,0.98) 100%);
  padding: 24px 18px;
  border-radius: 20px;
  text-align: center;
  box-shadow: 0 16px 32px rgba(15,23,42,0.08);
  transition: 0.2s;
  min-height: 190px;
  border: 1px solid #d8e0ea;
}

.welcome-card:hover{
  transform: translateY(-4px);
  box-shadow: 0 20px 36px rgba(15,23,42,0.12);
  border-color: #c8ddd8;
}

.welcome-card .btn{
  margin-top: 10px;
  border-radius: 999px;
  padding: 9px 16px;
  font-weight: 700;
}

.welcome-detail-box{
  background: #ffffff;
  border: 1px solid #d8e0ea;
  border-radius: 20px;
  padding: 18px 20px;
  margin-top: 10px;
  margin-bottom: 20px;
  box-shadow: 0 12px 28px rgba(15,23,42,0.06);
}

.soft-box{
  background: linear-gradient(180deg, #f7fafc 0%, #f4f9f8 100%);
}

.object-flow-wrap{
  background: #ffffff;
  border: 1px solid #d8e0ea;
  border-radius: 20px;
  padding: 20px;
  box-shadow: 0 12px 28px rgba(15,23,42,0.06);
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
  background: #f7fafc;
  border: 1px solid #d6e0e8;
  border-radius: 14px;
  padding: 12px 18px;
  min-width: 145px;
  font-weight: 700;
  color: #24415f;
  box-shadow: 0 4px 10px rgba(15,23,42,0.04);
}

.flow-object-btn:hover{
  background: #eef6f6;
  border-color: #bdd8d4;
}

.flow-object-btn:focus,
.flow-object-btn:active{
  outline: none !important;
  box-shadow: 0 0 0 2px rgba(15,118,110,0.12);
}

.flow-arrow{
  font-size: 26px;
  font-weight: 700;
  color: #95a3b3;
  line-height: 1;
  padding: 0 2px;
}

@media (max-width: 1200px){
  .app-hero h3{
    font-size: 26px;
  }
}

@media (max-width: 992px){
  .object-flow-row{
    flex-direction: column;
  }

  .flow-arrow{
    transform: rotate(90deg);
  }
}

@media (max-width: 768px){
  .app-hero{
    padding: 20px 18px;
  }

  .app-hero h3{
    font-size: 24px;
  }
}
"))

app_page_shell <- function(..., class = "") {
  tags$div(class = trimws(paste("app-shell", class)), ...)
}

app_page_hero <- function(title, description, actions = NULL, class = "") {
  tags$div(
    class = trimws(paste("app-hero", class)),
    tags$div(
      tags$h3(title),
      tags$p(description)
    ),
    if (!is.null(actions)) {
      tags$div(class = "app-hero-actions", actions)
    }
  )
}

app_panel <- function(title, ..., subtitle = NULL, class = "", body_class = "", head_extra = NULL) {
  tags$div(
    class = trimws(paste("app-panel", class)),
    tags$div(
      class = "app-panel-head",
      tags$div(
        tags$div(class = "app-panel-title", title),
        if (!is.null(subtitle)) tags$div(class = "app-panel-subtitle", subtitle)
      ),
      head_extra
    ),
    tags$div(class = trimws(paste("app-panel-body", body_class)), ...)
  )
}

app_step_status <- function(label) {
  tags$span(class = "app-step-status", label)
}

app_sidebar_step <- function(id, number, title, ..., status = NULL, open = FALSE, class = "") {
  tags$div(
    class = trimws(paste("app-accordion-item", class)),
    tags$div(
      class = "app-accordion-head",
      `data-toggle` = "collapse",
      `data-target` = paste0("#", id),
      tags$div(
        class = "app-accordion-title",
        tags$span(class = "app-step-num", number),
        tags$span(title)
      ),
      status %||% app_step_status("Section")
    ),
    tags$div(
      id = id,
      class = trimws(paste("app-accordion-body collapse", if (isTRUE(open)) "in" else "")),
      ...
    )
  )
}

# Source function files
source("R/mongo_schema.R")        # schema initialisation (indexes)
source("R/mongo_functions.R")     # all DB helpers (provenance API + legacy)
source("R/alignment_reference_db.R")
source("R/feature_standardization_functions.R")
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

tryCatch(
  seed_default_alignment_references(),
  error = function(e) warning("Alignment reference seeding failed: ", conditionMessage(e))
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
