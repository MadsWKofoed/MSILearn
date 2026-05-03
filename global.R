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

# Shared UI theme helpers
css_classes <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = " ")
}

lookup_study_label <- function(study_id) {
  sid <- as.character(study_id %||% "")
  if (!nzchar(sid)) return("No study selected")
  tryCatch({
    df <- get_studies()
    hit <- df[df$`_id` == sid, , drop = FALSE]
    if (nrow(hit) == 0) sid else as.character(hit$name[1] %||% sid)
  }, error = function(e) sid)
}

lookup_sample_label <- function(study_id, sample_id) {
  sid <- as.character(study_id %||% "")
  samp_id <- as.character(sample_id %||% "")
  if (!nzchar(sid) || !nzchar(samp_id)) return(samp_id)
  tryCatch({
    df <- get_samples(sid)
    hit <- df[df$`_id` == samp_id, , drop = FALSE]
    if (nrow(hit) == 0) samp_id else as.character(hit$sample_name[1] %||% samp_id)
  }, error = function(e) samp_id)
}

lookup_annotation_set_label <- function(study_id, annotation_set_id) {
  sid <- as.character(study_id %||% "")
  ann_id <- as.character(annotation_set_id %||% "")
  if (!nzchar(ann_id)) return("No annotation set selected")
  if (!nzchar(sid)) return(ann_id)
  tryCatch({
    df <- list_annotation_sets(sid)
    hit <- df[df$`_id` == ann_id, , drop = FALSE]
    if (nrow(hit) == 0) ann_id else as.character(hit$name[1] %||% ann_id)
  }, error = function(e) ann_id)
}

lookup_dataset_label <- function(dataset_id) {
  did <- as.character(dataset_id %||% "")
  if (!nzchar(did)) return("No dataset selected")
  tryCatch({
    ds <- get_dataset(did)
    as.character(ds$name[1] %||% did)
  }, error = function(e) did)
}

format_processing_pipeline_label <- function(pipeline_id) {
  pid <- as.character(pipeline_id %||% "")
  if (!nzchar(pid)) return("No pipeline selected")

  tryCatch({
    meta <- get_pipeline(pid)
    params <- extract_params(meta$params)
    ref_name <- as.character(params$reference_name %||% "")
    ref_label <- if (nzchar(ref_name)) {
      tryCatch({
        doc <- load_alignment_reference(ref_name)
        as.character(doc$display_name %||% ref_name)
      }, error = function(e) ref_name)
    } else {
      "reference"
    }

    if (identical(as.character(meta$type[1] %||% ""), "processing")) {
      paste0(
        "SNR ", params$snr %||% "?", " · tol ", params$tolerance %||% "?",
        " · ", params$resolution %||% "?", " ppm · ", ref_label
      )
    } else {
      as.character(meta$name[1] %||% pid)
    }
  }, error = function(e) pid)
}

app_page_header <- function(title, subtitle, badge = NULL, actions = NULL, icon_name = NULL) {
  tags$div(
    class = "app-page-hero",
    tags$div(
      class = "app-page-hero-main",
      if (!is.null(badge)) tags$div(class = "app-page-kicker", badge),
      tags$div(
        class = "app-page-title-row",
        if (!is.null(icon_name)) tags$span(class = "app-page-title-icon", icon(icon_name)),
        h2(class = "app-page-title", title)
      ),
      tags$p(class = "app-page-subtitle", subtitle)
    ),
    if (!is.null(actions)) tags$div(class = "app-page-actions", actions)
  )
}

app_panel <- function(title = NULL,
                      subtitle = NULL,
                      ...,
                      class = NULL,
                      body_class = NULL,
                      actions = NULL) {
  tags$section(
    class = css_classes("app-panel", class),
    if (!is.null(title) || !is.null(subtitle) || !is.null(actions)) {
      tags$div(
        class = "app-panel-head",
        tags$div(
          class = "app-panel-head-main",
          if (!is.null(title)) tags$h3(class = "app-panel-title", title),
          if (!is.null(subtitle)) tags$p(class = "app-panel-subtitle", subtitle)
        ),
        if (!is.null(actions)) tags$div(class = "app-panel-actions", actions)
      )
    },
    tags$div(class = css_classes("app-panel-body", body_class), ...)
  )
}

app_workflow_step <- function(ns,
                              step_id,
                              number,
                              title,
                              ...,
                              status = NULL,
                              open = FALSE,
                              onclick = NULL) {
  tags$div(
    class = "workflow-step",
    tags$div(
      class = "workflow-step-head",
      `data-toggle` = "collapse",
      `data-target` = paste0("#", ns(step_id)),
      onclick = onclick,
      tags$div(
        class = "workflow-step-title",
        tags$span(class = "workflow-step-num", number),
        tags$span(title)
      ),
      status
    ),
    tags$div(
      id = ns(step_id),
      class = css_classes("workflow-step-body collapse", if (isTRUE(open)) "in" else NULL),
      ...
    )
  )
}

app_sidebar_layout <- function(ns,
                               module_key,
                               sidebar_title,
                               sidebar_subtitle,
                               sidebar,
                               main,
                               sidebar_icon = "sliders-h",
                               sidebar_hint = "Controls") {
  tags$div(
    id = ns(paste0(module_key, "_shell")),
    class = "app-module-shell",
    tags$aside(
      class = "app-sidebar-shell",
      tags$div(
        class = "app-sidebar-card",
        tags$div(
          class = "app-sidebar-head",
          tags$div(
            class = "app-sidebar-title-wrap",
            tags$span(class = "app-sidebar-icon", icon(sidebar_icon)),
            tags$div(
              class = "app-sidebar-title-stack",
              tags$div(class = "app-sidebar-title", sidebar_title),
              tags$div(class = "app-sidebar-subtitle", sidebar_subtitle)
            )
          )
        ),
        tags$div(
          class = "app-sidebar-content",
          sidebar
        )
      )
    ),
    tags$div(class = "app-main-shell", main)
  )
}

custom_css <- tags$style(HTML("
:root{
  --app-bg:#eef1ef;
  --app-bg-soft:#f8faf8;
  --app-panel:#ffffff;
  --app-panel-strong:#f4f7f5;
  --app-border:#d7ddd7;
  --app-border-strong:#c7d0ca;
  --app-ink:#1a2523;
  --app-muted:#5d6b66;
  --app-primary:#486a66;
  --app-primary-strong:#35514d;
  --app-primary-soft:#e9f0ee;
  --app-success:#2f6b52;
  --app-warning:#9f6b22;
  --app-danger:#b42318;
  --app-info:#486a66;
  --app-shadow:0 18px 48px rgba(15, 23, 42, 0.08);
  --app-shadow-soft:0 10px 28px rgba(15, 23, 42, 0.06);
}

html,
body{
  min-height:100%;
  background:
    radial-gradient(circle at top left, rgba(72,106,102,0.10), transparent 28%),
    radial-gradient(circle at top right, rgba(159,107,34,0.08), transparent 24%),
    linear-gradient(180deg, #f8faf8 0%, var(--app-bg) 100%);
}

body{
  color:var(--app-ink);
  font-family:\"Avenir Next\", \"Segoe UI Variable\", \"IBM Plex Sans\", \"Helvetica Neue\", sans-serif;
}

.container-fluid{
  max-width:1820px;
}

.navbar{
  min-height:88px;
}

.navbar-default{
  background:rgba(251,252,251,0.90);
  border:none;
  box-shadow:0 12px 34px rgba(15, 23, 42, 0.07);
  backdrop-filter:blur(16px);
}

.navbar-default .navbar-brand{
  color:var(--app-ink);
  font-size:21px;
  font-weight:800;
  letter-spacing:-0.03em;
  height:88px;
  display:flex;
  align-items:center;
}

.navbar-default .navbar-nav{
  margin-top:20px;
  margin-bottom:18px;
}

.navbar-default .navbar-nav > li{
  margin-right:8px;
}

.navbar-default .navbar-nav > li > a{
  color:#44534f;
  font-weight:700;
  padding:12px 18px;
  border-radius:14px;
  transition:color 0.18s ease, background-color 0.18s ease, box-shadow 0.18s ease;
}

.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > .open > a{
  color:var(--app-primary-strong);
  background:#eef3f0;
}

.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus{
  color:#ffffff;
  background:linear-gradient(135deg, var(--app-primary) 0%, var(--app-primary-strong) 100%);
  box-shadow:0 12px 24px rgba(53,81,77,0.18);
}

.tab-content{
  padding-top:18px;
}

.app-page{
  padding:8px 4px 30px 4px;
}

.app-page-hero{
  background:
    radial-gradient(circle at top left, rgba(72,106,102,0.14), transparent 34%),
    radial-gradient(circle at top right, rgba(159,107,34,0.09), transparent 28%),
    linear-gradient(135deg, rgba(255,255,255,0.98) 0%, rgba(245,248,246,0.98) 100%);
  border:1px solid #dce4de;
  border-radius:28px;
  box-shadow:var(--app-shadow);
  padding:24px 28px;
  margin-bottom:20px;
  display:flex;
  align-items:flex-start;
  justify-content:space-between;
  gap:18px;
  flex-wrap:wrap;
}

.app-page-kicker{
  display:inline-flex;
  align-items:center;
  gap:8px;
  padding:7px 12px;
  border-radius:999px;
  background:rgba(53,81,77,0.08);
  color:#415550;
  font-size:11px;
  font-weight:800;
  text-transform:uppercase;
  letter-spacing:0.09em;
  margin-bottom:12px;
}

.app-page-title-row{
  display:flex;
  align-items:center;
  gap:12px;
}

.app-page-title-icon{
  width:48px;
  height:48px;
  border-radius:16px;
  background:linear-gradient(135deg, rgba(72,106,102,0.16) 0%, rgba(159,107,34,0.08) 100%);
  display:inline-flex;
  align-items:center;
  justify-content:center;
  color:var(--app-primary-strong);
  font-size:20px;
  flex:0 0 auto;
}

.app-page-title{
  margin:0;
  color:var(--app-ink);
  font-size:30px;
  line-height:1.05;
  font-weight:900;
  letter-spacing:-0.045em;
}

.app-page-subtitle{
  margin:12px 0 0 0;
  max-width:760px;
  color:var(--app-muted);
  font-size:14px;
  line-height:1.7;
}

.app-module-shell{
  display:flex;
  align-items:flex-start;
  gap:18px;
}

.app-sidebar-shell{
  width:440px;
  flex:0 0 440px;
  position:sticky;
  top:108px;
  transition:flex-basis 0.28s ease, width 0.28s ease;
  z-index:3;
}

.app-sidebar-card{
  background:rgba(255,255,255,0.92);
  border:1px solid #d9e0da;
  border-radius:24px;
  box-shadow:var(--app-shadow);
  overflow:hidden;
}

.app-sidebar-head{
  display:flex;
  align-items:flex-start;
  justify-content:space-between;
  gap:12px;
  padding:16px 18px 14px 18px;
  border-bottom:1px solid #e8eef5;
  background:linear-gradient(180deg, #fbfcfb 0%, #f4f7f5 100%);
}

.app-sidebar-title-wrap{
  display:flex;
  align-items:flex-start;
  gap:12px;
}

.app-sidebar-icon{
  width:38px;
  height:38px;
  border-radius:14px;
  background:var(--app-primary-soft);
  color:var(--app-primary-strong);
  display:inline-flex;
  align-items:center;
  justify-content:center;
  flex:0 0 auto;
}

.app-sidebar-title{
  font-size:16px;
  font-weight:900;
  letter-spacing:-0.02em;
  color:var(--app-ink);
}

.app-sidebar-subtitle{
  margin-top:4px;
  font-size:12px;
  line-height:1.5;
  color:var(--app-muted);
}

.app-sidebar-content{
  padding:18px;
  max-height:calc(100vh - 150px);
  overflow:auto;
  transition:opacity 0.22s ease, padding 0.22s ease;
}

.app-main-shell{
  flex:1 1 auto;
  min-width:0;
}

.app-panel,
.well{
  background:rgba(255,255,255,0.94);
  border:1px solid #d9e4ef;
  border-radius:24px;
  box-shadow:var(--app-shadow-soft);
  overflow:hidden;
  margin-bottom:18px;
}

.app-panel-head{
  padding:16px 18px 14px 18px;
  border-bottom:1px solid #e8eef5;
  display:flex;
  align-items:flex-start;
  justify-content:space-between;
  gap:12px;
  flex-wrap:wrap;
  background:linear-gradient(180deg, #ffffff 0%, #f8fbfe 100%);
}

.app-panel-title,
.well h4:first-child{
  margin:0;
  color:var(--app-ink);
  font-size:17px;
  font-weight:900;
  letter-spacing:-0.02em;
}

.app-panel-subtitle{
  margin:4px 0 0 0;
  color:var(--app-muted);
  font-size:12px;
  line-height:1.6;
}

.app-panel-body{
  padding:18px;
}

.well{
  padding:18px 18px 16px 18px;
  margin-bottom:18px;
}

.well hr,
.app-panel hr{
  border-color:#e6edf5;
}

.control-label,
label{
  color:#203247;
  font-weight:700;
  font-size:12px;
  letter-spacing:0.01em;
}

.form-group{
  margin-bottom:14px;
}

.form-control,
.selectize-input,
.selectize-control.single .selectize-input.input-active,
.selectize-dropdown,
.well .form-control{
  border-radius:14px;
  border:1px solid var(--app-border-strong);
  box-shadow:none;
  min-height:44px;
  padding-top:10px;
  padding-bottom:10px;
  background:#ffffff;
}

.selectize-input{
  padding:10px 12px;
}

.selectize-dropdown{
  box-shadow:0 22px 48px rgba(15, 23, 42, 0.12);
}

.form-control:focus,
.selectize-input.focus,
.btn:focus,
.btn:active:focus{
  border-color:#9bb8ff;
  box-shadow:0 0 0 4px rgba(21,94,239,0.12);
}

.radio,
.checkbox{
  margin-top:8px;
  margin-bottom:8px;
}

.help-block,
.text-muted,
p small,
small{
  color:var(--app-muted);
  line-height:1.55;
}

.btn{
  border-radius:14px;
  font-weight:800;
  border:1px solid transparent;
  transition:transform 0.18s ease, box-shadow 0.18s ease, background-color 0.18s ease, border-color 0.18s ease;
}

.btn:hover{
  transform:translateY(-1px);
}

.btn-default{
  background:#eef4fa;
  border-color:#d6e0eb;
  color:#203247;
}

.btn-default:hover{
  background:#e4edf7;
  border-color:#cad7e5;
  color:#16283a;
}

.btn-primary{
  background:linear-gradient(135deg, var(--app-primary) 0%, var(--app-primary-strong) 100%);
  box-shadow:0 12px 28px rgba(53,81,77,0.22);
}

.btn-success{
  background:linear-gradient(135deg, #4e8867 0%, var(--app-success) 100%);
  box-shadow:0 10px 24px rgba(47,107,82,0.16);
}

.btn-warning{
  background:linear-gradient(135deg, #c88d39 0%, var(--app-warning) 100%);
  box-shadow:0 10px 24px rgba(159,107,34,0.16);
  color:#ffffff;
}

.btn-info{
  background:linear-gradient(135deg, #5f8278 0%, #486a66 100%);
  box-shadow:0 10px 24px rgba(72,106,102,0.16);
}

.btn-danger{
  background:linear-gradient(135deg, #d64545 0%, var(--app-danger) 100%);
  box-shadow:0 10px 24px rgba(180,35,24,0.18);
}

.nav-tabs{
  border-bottom:1px solid #e6edf5;
  margin-bottom:14px;
}

.nav-tabs > li{
  margin-bottom:-1px;
}

.nav-tabs > li > a{
  border:none !important;
  border-radius:999px;
  color:#46586b;
  font-weight:800;
  background:#eef4fa;
  margin-right:8px;
  padding:10px 16px;
}

.nav-tabs > li > a:hover{
  background:#e5edf7;
  color:#23374b;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus{
  background:#314541;
  color:#ffffff;
  box-shadow:0 10px 24px rgba(49,69,65,0.18);
}

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter{
  margin-bottom:10px;
}

.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select{
  border-radius:999px;
  border:1px solid var(--app-border-strong);
  background:#ffffff;
  min-height:38px;
}

table.dataTable thead th,
table.dataTable thead td{
  background:#f7fafc;
  color:#23374b;
  font-weight:900;
  border-bottom:1px solid #dde7f0 !important;
}

table.dataTable tbody td{
  border-top:1px solid #edf3f8;
}

table.dataTable.stripe tbody tr.odd,
table.dataTable.display tbody tr.odd{
  background:#fcfdff;
}

table.dataTable.hover tbody tr:hover,
table.dataTable.display tbody tr:hover{
  background:#eef5ff !important;
}

pre.shiny-text-output{
  background:linear-gradient(180deg, #122033 0%, #17283d 100%);
  color:#e8f0fb;
  border:1px solid rgba(255,255,255,0.07);
  border-radius:18px;
  padding:14px 16px;
  box-shadow:inset 0 1px 0 rgba(255,255,255,0.04);
  white-space:pre-wrap;
  word-break:break-word;
}

.shiny-notification{
  border:none;
  border-radius:18px;
  box-shadow:0 18px 40px rgba(15, 23, 42, 0.18);
  padding:14px 16px;
  min-width:320px;
}

.shiny-notification-message{
  background:#edf6f0;
  color:#204838;
  border-left:5px solid var(--app-success);
}

.shiny-notification-warning{
  background:#fbf3e7;
  color:#7f571d;
  border-left:5px solid var(--app-warning);
}

.shiny-notification-error{
  background:#fff0ef;
  color:#8d1d17;
  border-left:5px solid var(--app-danger);
}

.recalculating{
  opacity:0.62;
  transition:opacity 0.18s ease;
}

.welcome-container{
  max-width:1220px;
  margin:0 auto;
  padding:12px 4px 34px 4px;
}

.lead{
  font-size:18px;
  color:#425467;
  line-height:1.7;
  margin-bottom:18px;
}

.lead-small{
  font-size:14px;
  color:var(--app-muted);
  margin-bottom:16px;
}

.welcome-card{
  background:linear-gradient(180deg, rgba(255,255,255,0.98) 0%, rgba(245,249,253,0.98) 100%);
  padding:24px 18px;
  border-radius:22px;
  text-align:center;
  border:1px solid #dce6ef;
  box-shadow:var(--app-shadow-soft);
  transition:transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
  min-height:205px;
}

.welcome-card:hover{
  transform:translateY(-4px);
  box-shadow:var(--app-shadow);
  border-color:#c8d8e8;
}

.welcome-card .fa-2x{
  color:var(--app-primary-strong);
  margin:10px 0 12px 0;
}

.workflow-step{
  border:1px solid #dde4de;
  border-radius:18px;
  margin-bottom:14px;
  background:#ffffff;
  overflow:hidden;
  box-shadow:0 4px 16px rgba(15,23,42,0.05);
  transition:transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
}

.workflow-step-head{
  padding:13px 15px;
  font-weight:800;
  font-size:15px;
  background:#f5f8f6;
  border-bottom:1px solid #e7eee8;
  cursor:pointer;
  display:flex;
  align-items:center;
  justify-content:space-between;
  gap:10px;
  transition:background-color 0.18s ease;
}

.workflow-step:hover{
  border-color:#cfd9d2;
}

.workflow-step-title{
  display:flex;
  align-items:center;
  gap:10px;
}

.workflow-step-num{
  width:26px;
  height:26px;
  border-radius:999px;
  background:#e8f0ee;
  color:#35514d;
  display:inline-flex;
  align-items:center;
  justify-content:center;
  font-size:12px;
  font-weight:800;
  flex:0 0 auto;
  border:1px solid #cbd8d3;
}

.workflow-step-status{
  font-size:11px;
  font-weight:800;
  padding:5px 9px;
  border-radius:999px;
  background:#edf2ef;
  color:#425550;
  white-space:nowrap;
}

.workflow-step-body{
  padding:14px 15px 16px 15px;
}

.workflow-step-body .well{
  border:none;
  box-shadow:none;
  background:transparent;
  padding:0;
  margin-bottom:0;
}

.workflow-lead{
  font-size:13px;
  color:#53635e;
  margin-bottom:10px;
  line-height:1.5;
}

.workflow-subtitle{
  font-size:12px;
  font-weight:800;
  color:#314541;
  margin-top:10px;
  margin-bottom:6px;
  text-transform:uppercase;
  letter-spacing:0.03em;
}

.helper-box{
  background:#f5f8f6;
  border:1px solid #dbe4de;
  border-radius:12px;
  padding:10px 12px;
  font-size:12px;
  color:#3f504b;
  line-height:1.55;
  margin-bottom:10px;
}

.helper-box strong{
  color:#2f4742;
}

.helper-muted{
  font-size:12px;
  color:#6a7773;
  margin-top:4px;
  margin-bottom:8px;
  line-height:1.4;
}

.mini-note{
  font-size:11px;
  color:#6a7773;
  margin-top:4px;
  line-height:1.4;
}

.btn-blockish{
  width:100%;
  margin-bottom:8px;
}

.section-divider{
  margin:10px 0 12px 0;
  border-top:1px solid #e8eeea;
}

.compact-help summary{
  cursor:pointer;
  font-weight:700;
  color:#314541;
  margin-bottom:8px;
}

.compact-help ol{
  margin-bottom:0;
  padding-left:18px;
  font-size:12px;
  color:#52635e;
  line-height:1.55;
}

.welcome-detail-box{
  background:rgba(255,255,255,0.96);
  border:1px solid #dde6ef;
  border-radius:20px;
  padding:20px 22px;
  margin-top:12px;
  margin-bottom:20px;
  box-shadow:var(--app-shadow-soft);
}

.soft-box{
  background:linear-gradient(180deg, #fbfdff 0%, #f5f8fc 100%);
}

.object-flow-wrap{
  background:rgba(255,255,255,0.96);
  border:1px solid #dde6ef;
  border-radius:22px;
  padding:22px;
  box-shadow:var(--app-shadow-soft);
  margin-bottom:12px;
}

.object-flow-row{
  display:flex;
  align-items:center;
  justify-content:center;
  gap:12px;
  flex-wrap:wrap;
}

.flow-break{
  height:16px;
}

.flow-object-btn{
  background:#f6faff;
  border:1px solid #d6e2ef;
  border-radius:16px;
  padding:12px 18px;
  min-width:150px;
  font-weight:800;
  color:#23405d;
  box-shadow:0 6px 16px rgba(15, 23, 42, 0.05);
}

.flow-object-btn:hover{
  background:#ecf4ff;
  border-color:#b8cfe8;
}

.flow-object-btn:focus,
.flow-object-btn:active{
  outline:none !important;
  box-shadow:0 0 0 4px rgba(21,94,239,0.12);
}

.flow-arrow{
  font-size:26px;
  font-weight:800;
  color:#9aa9bb;
  line-height:1;
  padding:0 2px;
}

@media (max-width: 1199px){
  .app-module-shell{
    flex-direction:column;
  }

  .app-sidebar-shell{
    width:100%;
    flex-basis:auto;
    position:static;
  }
}

@media (max-width: 992px){
  .object-flow-row{
    flex-direction:column;
  }

  .flow-arrow{
    transform:rotate(90deg);
  }
}

@media (max-width: 768px){
  .app-page-hero{
    padding:20px;
    border-radius:24px;
  }

  .app-page-title{
    font-size:26px;
  }

  .navbar-default .navbar-brand{
    font-size:18px;
  }
}
"))

custom_js <- tags$script(HTML("
(function(){})();
"))

# Source function files
source("R/mongo_schema.R")        # schema initialisation (indexes)
source("R/mongo_functions.R")     # all DB helpers (provenance API + legacy)
source("R/alignment_reference_db.R")
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
  header = tagList(custom_css, custom_js, useShinyjs()),
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
