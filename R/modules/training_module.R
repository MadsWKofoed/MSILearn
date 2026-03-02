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
      # ── Left: dataset selection + hyperparams ──────────────────────────
      column(4,
        h4("1. Select Dataset"),
        p(tags$small(
          "Datasets are frozen snapshots that pin samples, pipeline, ",
          "annotation set, and split seed. Select one to train on it."
        )),
        actionButton(ns("refresh_datasets"), "Refresh dataset list",
                     class = "btn-sm btn-default"),
        br(), br(),
        selectInput(ns("dataset_id"), "Dataset",
                    choices  = c("(loading...)" = ""),
                    width    = "100%"),
        uiOutput(ns("dataset_info_ui")),

        hr(),
        h4("2. Hyperparameters"),
        numericInput(ns("mtry"),          "mtry",           value = 31,  min = 1),
        numericInput(ns("num_trees"),     "num.trees",      value = 500, min = 10),
        numericInput(ns("min_node_size"), "min.node.size",  value = 10,  min = 1),
        selectInput( ns("splitrule"),     "splitrule",
                     choices = c("gini", "extratrees"), selected = "gini"),
        numericInput(ns("cv_folds"),      "CV folds (0 = none)", value = 10, min = 0),
        numericInput(ns("seed"),          "Random seed",    value = 1234, min = 1),

        hr(),
        actionButton(ns("run_training"), "Train model",
                     class = "btn-primary btn-lg",
                     style = "width:100%"),
        br(), br(),
        verbatimTextOutput(ns("training_log"))
      ),

      # ── Right: results / model run list ────────────────────────────────
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

    log_val    <- reactiveVal("")
    last_run_id <- reactiveVal(NULL)

    add_log <- function(msg) {
      log_val(paste0(log_val(), "\n", format(Sys.time(), "[%H:%M:%S] "), msg))
    }

    # ── Dataset list ────────────────────────────────────────────────────
    datasets_rv <- reactiveVal(data.frame())

    observe({
      input$refresh_datasets   # trigger on button click and on load
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
      }, error = function(e) {
        updateSelectInput(session, "dataset_id",
                          choices = c("Error loading datasets" = ""))
      })
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
            tags$b("Study: "),   ds$study_id, tags$br(),
            tags$b("Samples: "), n,           tags$br(),
            tags$b("Pipeline: "), substr(ds$pipeline_id, 1, 12), "...", tags$br(),
            tags$b("Ann. set: "), substr(ds$annotation_set_id, 1, 12), "...", tags$br(),
            tags$b("Stage: "),   ds$stage_type, tags$br(),
            tags$b("Split: "),   sp$train_frac * 100, "% train | seed=", sp$seed
          )
        )
      }, error = function(e) tags$small(style = "color:red", "Could not load dataset info."))
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

    # Re-refresh automatically after a successful training run
    observeEvent(last_run_id(), {
      req(input$dataset_id)
      tryCatch({
        runs_rv(list_model_runs(input$dataset_id))
      }, error = function(e) NULL)
    })

    output$run_table <- renderTable({
      df <- runs_rv()
      if (nrow(df) == 0) return(data.frame(message = "No runs yet for this dataset."))
      out <- data.frame(
        run_id     = df[["_id"]],
        model_type = df$model_type,
        accuracy   = vapply(df$metrics, function(m) {
          v <- if (is.data.frame(m)) m$accuracy[1] else m$accuracy
          if (is.null(v)) NA_real_ else round(as.numeric(v), 4)
        }, numeric(1)),
        kappa      = vapply(df$metrics, function(m) {
          v <- if (is.data.frame(m)) m$kappa[1] else m$kappa
          if (is.null(v)) NA_real_ else round(as.numeric(v), 4)
        }, numeric(1)),
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
        paste0(
          "Accuracy : ", round(m$accuracy, 4), "\n",
          "Kappa    : ", round(m$kappa,    4), "\n",
          if (!is.null(m$cv_mean_accuracy))
            paste0("CV acc   : ", round(m$cv_mean_accuracy, 4), "\n")
          else ""
        )
      }, error = function(e) conditionMessage(e))
    })
  })
}

