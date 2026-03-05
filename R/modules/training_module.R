# R/modules/training_module.R
#
# Training module UI and server.
#
# Design constraints enforced here:
#   - Training can only be initiated from an explicitly selected dataset_id.
#   - No artifact loading outside of load_dataset_for_training().
#   - All hyperparameters are captured in the UI and stored with the run.

training_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Training",
    fluidRow(

      # ── Left: dataset creation + selection + hyperparams ───────────────
      column(4,

        # ── 0. Create Dataset ──────────────────────────────────────────
        wellPanel(
          h4("0. Create Dataset"),
          p(tags$small("Pin samples, pipeline, annotation set and split seed into a frozen snapshot.")),

          selectInput(ns("ds_study"), "Study:",
                      choices = c("(loading...)" = ""), width = "100%"),
          actionButton(ns("ds_refresh_study"), "↺", class = "btn-xs"),
          br(), br(),

          selectInput(ns("ds_pipeline"), "Processing pipeline:",
                      choices = c("— select study first —" = ""), width = "100%"),

          selectInput(ns("ds_ann_set"), "Annotation set:",
                      choices = c("— select study first —" = ""), width = "100%"),

          tags$label("Samples (select multiple):"),
          selectInput(ns("ds_samples"), NULL,
                      choices  = c("— select study first —" = ""),
                      multiple = TRUE, width = "100%"),

          numericInput(ns("ds_train_frac"), "Train fraction:", value = 0.8,
                       min = 0.5, max = 0.95, step = 0.05),
          numericInput(ns("ds_seed"), "Split seed:", value = 42, min = 1),
          textInput(ns("ds_name"), "Dataset name:", placeholder = "e.g. SSC_cohort_RF_v1"),

          actionButton(ns("create_dataset_btn"), "Create Dataset",
                       class = "btn-success btn-sm", style = "width:100%"),
          uiOutput(ns("create_dataset_status"))
        ),

        # ── 1. Select Dataset ──────────────────────────────────────────
        h4("1. Select Dataset"),
        p(tags$small(
          "Datasets are frozen snapshots that pin samples, pipeline, ",
          "annotation set, and split seed. Select one to train on it."
        )),
        actionButton(ns("refresh_datasets"), "Refresh dataset list",
                     class = "btn-sm btn-default"),
        br(), br(),
        selectInput(ns("dataset_id"), "Dataset:",
                    choices = c("(loading...)" = ""), width = "100%"),
        uiOutput(ns("dataset_info_ui")),

        hr(),
        h4("2. Hyperparameters"),
        numericInput(ns("mtry"),          "mtry",                value = 31,   min = 1),
        numericInput(ns("num_trees"),     "num.trees",           value = 500,  min = 10),
        numericInput(ns("min_node_size"), "min.node.size",       value = 10,   min = 1),
        selectInput( ns("splitrule"),     "splitrule",
                     choices = c("gini", "extratrees"), selected = "gini"),
        numericInput(ns("cv_folds"),      "CV folds (0 = none)", value = 10,   min = 0),
        numericInput(ns("seed"),          "Random seed",         value = 1234, min = 1),

        hr(),
        actionButton(ns("run_training"), "Train model",
                     class = "btn-primary btn-lg", style = "width:100%"),
        br(), br(),
        verbatimTextOutput(ns("training_log"))
      ),

      # ── Right: results ─────────────────────────────────────────────────
      column(8,
        h4("3. Model Runs for Selected Dataset"),
        actionButton(ns("refresh_runs"), "Refresh run list",
                     class = "btn-sm btn-default"),
        br(), br(),
        tableOutput(ns("run_table")),
        hr(),
        h4("Metrics for last run"),
        verbatimTextOutput(ns("last_metrics"))
      )
    )
  )
}


training_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    log_val     <- reactiveVal("")
    last_run_id <- reactiveVal(NULL)

    add_log <- function(msg) {
      log_val(paste0(log_val(), "\n", format(Sys.time(), "[%H:%M:%S] "), msg))
    }

    # ── Populate study dropdown ──────────────────────────────────────────
    load_ds_studies <- function() {
      tryCatch({
        df <- get_studies()
        if (nrow(df) == 0 || !("_id" %in% names(df))) {
          updateSelectInput(session, "ds_study", choices = c("No studies" = ""))
          return()
        }
        ch <- setNames(df[["_id"]], df$name)
        updateSelectInput(session, "ds_study",
                          choices = c("— select —" = "", ch))
      }, error = function(e)
        updateSelectInput(session, "ds_study", choices = c("Error" = ""))
      )
    }

    observeEvent(input$ds_refresh_study, load_ds_studies(), ignoreInit = FALSE)
    session$onFlushed(load_ds_studies, once = TRUE)

    # ── Study → samples + pipeline + annotation set ──────────────────────
    observeEvent(input$ds_study, {
      sid <- input$ds_study
      if (!nzchar(sid %||% "")) return()

      # Samples
      tryCatch({
        samp_df <- get_samples(sid)
        if (nrow(samp_df) == 0 || !("_id" %in% names(samp_df))) {
          updateSelectInput(session, "ds_samples",
                            choices = c("No samples" = ""))
        } else {
          ch <- setNames(samp_df[["_id"]], samp_df$sample_name)
          updateSelectInput(session, "ds_samples", choices = ch)
        }
      }, error = function(e)
        updateSelectInput(session, "ds_samples", choices = c("Error" = ""))
      )

      # Pipelines (binned_dataframe artifacts in this study)
      tryCatch({
        arts <- query_artifacts(study_id = sid, stage_type = "binned_dataframe")
        pids <- unique(arts$pipeline_id)
        if (length(pids) == 0) {
          updateSelectInput(session, "ds_pipeline",
                            choices = c("No processed artifacts" = ""))
        } else {
          labels <- vapply(pids, function(pid) {
            tryCatch({
              m <- get_pipeline(pid)
              paste0(m$name[1], " (", substr(pid, 1, 8), "\u2026)")
            }, error = function(e) substr(pid, 1, 16))
          }, character(1))
          updateSelectInput(session, "ds_pipeline",
                            choices = c("— select —" = "", setNames(pids, labels)))
        }
      }, error = function(e)
        updateSelectInput(session, "ds_pipeline", choices = c("Error" = ""))
      )

      # Annotation sets
      tryCatch({
        ann_df <- list_annotation_sets(sid)
        if (nrow(ann_df) == 0 || !("_id" %in% names(ann_df))) {
          updateSelectInput(session, "ds_ann_set",
                            choices = c("No annotation sets" = ""))
        } else {
          ch <- setNames(ann_df[["_id"]], ann_df$name)
          updateSelectInput(session, "ds_ann_set",
                            choices = c("— select —" = "", ch))
        }
      }, error = function(e)
        updateSelectInput(session, "ds_ann_set", choices = c("Error" = ""))
      )
    }, ignoreInit = TRUE)

    # ── Create dataset ───────────────────────────────────────────────────
    observeEvent(input$create_dataset_btn, {
      sid      <- input$ds_study
      samp_ids <- input$ds_samples
      pid      <- input$ds_pipeline
      ann_id   <- input$ds_ann_set
      nm       <- trimws(input$ds_name)

      if (!nzchar(sid %||% "")) {
        showNotification("Select a study.", type = "warning"); return()
      }
      if (length(samp_ids) == 0) {
        showNotification("Select at least one sample.", type = "warning"); return()
      }
      if (!nzchar(pid %||% "")) {
        showNotification("Select a pipeline.", type = "warning"); return()
      }
      if (!nzchar(ann_id %||% "")) {
        showNotification("Select an annotation set.", type = "warning"); return()
      }
      if (!nzchar(nm)) {
        showNotification("Enter a dataset name.", type = "warning"); return()
      }

      tryCatch({
        dataset_id <- create_dataset(
          study_id          = sid,
          sample_ids        = samp_ids,
          pipeline_id       = pid,
          annotation_set_id = ann_id,
          split             = list(strategy   = "random",
                                   seed       = as.integer(input$ds_seed),
                                   train_frac = input$ds_train_frac),
          name              = nm
        )

        output$create_dataset_status <- renderUI(
          tags$div(class = "alert alert-success", style = "padding:6px; margin-top:6px;",
            tags$b("\u2713 Dataset created"), tags$br(),
            tags$small(substr(dataset_id, 1, 16), "\u2026")
          )
        )
        showNotification(paste0("Dataset created: ", dataset_id), type = "message")

        # Auto-refresh the dataset selector
        ds <- list_datasets()
        if (nrow(ds) > 0 && "_id" %in% names(ds)) {
          choices <- setNames(ds[["_id"]], paste0(ds$name, " [", ds$study_id, "]"))
          updateSelectInput(session, "dataset_id", choices = choices,
                            selected = dataset_id)
        }
      }, error = function(e) {
        output$create_dataset_status <- renderUI(
          tags$div(class = "alert alert-danger", style = "padding:6px; margin-top:6px;",
            tags$b("Error: "), conditionMessage(e)
          )
        )
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    # ── Dataset list ─────────────────────────────────────────────────────
    datasets_rv <- reactiveVal(data.frame())

    observe({
      input$refresh_datasets
      tryCatch({
        ds <- list_datasets()
        datasets_rv(ds)
        if (nrow(ds) == 0) {
          updateSelectInput(session, "dataset_id",
                            choices = c("No datasets found" = ""))
        } else {
          choices <- setNames(ds[["_id"]], paste0(ds$name, " [", ds$study_id, "]"))
          updateSelectInput(session, "dataset_id", choices = choices)
        }
      }, error = function(e)
        updateSelectInput(session, "dataset_id",
                          choices = c("Error loading datasets" = ""))
      )
    })

    # ── Dataset info card ────────────────────────────────────────────────
    output$dataset_info_ui <- renderUI({
      req(input$dataset_id, nchar(input$dataset_id) > 0)
      tryCatch({
        ds <- get_dataset(input$dataset_id)
        n  <- length(unlist(ds$sample_ids))
        sp <- if (is.data.frame(ds$split)) as.list(ds$split[1,]) else ds$split[[1]]
        tagList(
          tags$small(
            tags$b("Study: "),    ds$study_id,  tags$br(),
            tags$b("Samples: "),  n,            tags$br(),
            tags$b("Pipeline: "), substr(ds$pipeline_id, 1, 12), "...", tags$br(),
            tags$b("Ann. set: "), substr(ds$annotation_set_id, 1, 12), "...", tags$br(),
            tags$b("Stage: "),    ds$stage_type, tags$br(),
            tags$b("Split: "),    sp$train_frac * 100, "% train | seed=", sp$seed
          )
        )
      }, error = function(e)
        tags$small(style = "color:red", "Could not load dataset info.")
      )
    })

    # ── Train ────────────────────────────────────────────────────────────
    observeEvent(input$run_training, {
      dataset_id <- input$dataset_id
      if (is.null(dataset_id) || !nzchar(dataset_id)) {
        showNotification("Select a dataset before training.", type = "warning")
        return()
      }
      log_val("")
      add_log(paste0("Starting training on dataset: ", dataset_id))
      shinyjs::disable("run_training")
      on.exit(shinyjs::enable("run_training"), add = TRUE)

      tryCatch({
        run_id <- train_ranger_from_dataset(
          dataset_id    = dataset_id,
          mtry          = input$mtry,
          splitrule     = input$splitrule,
          min_node_size = input$min_node_size,
          num_trees     = input$num_trees,
          cv_folds      = input$cv_folds,
          seed          = input$seed
        )
        last_run_id(run_id)
        add_log(paste0("✓ Training complete. model_run_id: ", run_id))
        showNotification(paste0("Training complete! Run ID: ", run_id), type = "message")
      }, error = function(e) {
        add_log(paste0("✗ Error: ", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    output$training_log <- renderText({ log_val() })

    # ── Run table ────────────────────────────────────────────────────────
    runs_rv <- reactiveVal(data.frame())

    observe({
      input$refresh_runs
      req(input$dataset_id, nchar(input$dataset_id) > 0)
      tryCatch({
        runs <- list_model_runs(input$dataset_id)
        runs_rv(runs)
      }, error = function(e) runs_rv(data.frame()))
    })

    observeEvent(last_run_id(), {
      req(input$dataset_id)
      tryCatch(runs_rv(list_model_runs(input$dataset_id)), error = function(e) NULL)
    })

    output$run_table <- renderTable({
      df <- runs_rv()
      if (nrow(df) == 0 || !("_id" %in% names(df)))
        return(data.frame(message = "No runs yet for this dataset."))

      extract_metric <- function(m, key) {
        tryCatch({
          v <- if (is.data.frame(m))   m[[key]][1]
               else if (is.list(m))    m[[key]]
               else if (!is.null(names(m))) m[key]   # named atomic vector
               else NA_real_
          if (is.null(v) || length(v) == 0) NA_real_ else round(as.numeric(v[[1]]), 4)
        }, error = function(e) NA_real_)
      }

      out <- data.frame(
        run_id     = df[["_id"]],
        model_type = df$model_type,
        accuracy   = vapply(df$metrics, extract_metric, numeric(1), key = "accuracy"),
        kappa      = vapply(df$metrics, extract_metric, numeric(1), key = "kappa"),
        created_at = df$created_at,
        stringsAsFactors = FALSE
      )
      out[order(out$created_at, decreasing = TRUE), ]
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Last metrics ──────────────────────────────────────────────────────
    output$last_metrics <- renderText({
      req(last_run_id())
      tryCatch({
        runs <- list_model_runs(input$dataset_id)
        row  <- runs[runs[["_id"]] == last_run_id(), ]
        if (nrow(row) == 0) return("Run not found.")
        m <- row$metrics[[1]]

        get_m <- function(key) {
          v <- if (is.data.frame(m)) m[[key]][1]
               else if (is.list(m))  m[[key]]
               else m[key]
          if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v[[1]])
        }

        paste0(
          "Accuracy : ", round(get_m("accuracy"), 4), "\n",
          "Kappa    : ", round(get_m("kappa"),    4), "\n",
          if (!is.null(m[["cv_mean_accuracy"]]))
            paste0("CV acc   : ", round(get_m("cv_mean_accuracy"), 4), "\n")
          else ""
        )
      }, error = function(e) conditionMessage(e))
    })
  })
}