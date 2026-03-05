# R/modules/training_module.R

training_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Training",
    fluidRow(

      column(4,
        wellPanel(
          h4("0. Create Dataset"),
          p(tags$small("Pin samples, pipeline, annotation set and split seed into a frozen snapshot.")),
          selectInput(ns("ds_study"), "Study:",
                      choices = c("(loading...)" = ""), width = "100%"),
          actionButton(ns("ds_refresh_study"), "\u21ba", class = "btn-xs"),
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

      column(8,
        h4("3. Model Runs for Selected Dataset"),
        p(tags$small("Click a row to see full metrics below.")),
        actionButton(ns("refresh_runs"), "Refresh run list",
                     class = "btn-sm btn-default"),
        br(), br(),
        DT::dataTableOutput(ns("run_table")),
        hr(),
        h4("Run Details"),
        uiOutput(ns("run_details_ui"))
      )
    )
  )
}


training_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    log_val        <- reactiveVal("")
    last_run_id    <- reactiveVal(NULL)
    selected_run   <- reactiveVal(NULL)   # full row from runs_rv()
    selected_run_id <- reactiveVal(NULL)

    add_log <- function(msg) {
      log_val(paste0(log_val(), "\n", format(Sys.time(), "[%H:%M:%S] "), msg))
    }

    # ── Study dropdown ────────────────────────────────────────────────────
    load_ds_studies <- function() {
      tryCatch({
        df <- get_studies()
        if (nrow(df) == 0 || !("_id" %in% names(df))) {
          updateSelectInput(session, "ds_study", choices = c("No studies" = ""))
          return()
        }
        ch <- setNames(df[["_id"]], df$name)
        updateSelectInput(session, "ds_study", choices = c("— select —" = "", ch))
      }, error = function(e)
        updateSelectInput(session, "ds_study", choices = c("Error" = ""))
      )
    }

    observeEvent(input$ds_refresh_study, load_ds_studies(), ignoreInit = FALSE)
    session$onFlushed(load_ds_studies, once = TRUE)

    # ── Study → samples + pipelines + annotation sets ─────────────────────
    observeEvent(input$ds_study, {
      sid <- input$ds_study
      if (!nzchar(sid %||% "")) return()

      tryCatch({
        samp_df <- get_samples(sid)
        if (nrow(samp_df) == 0 || !("_id" %in% names(samp_df))) {
          updateSelectInput(session, "ds_samples", choices = c("No samples" = ""))
        } else {
          updateSelectInput(session, "ds_samples",
                            choices = setNames(samp_df[["_id"]], samp_df$sample_name))
        }
      }, error = function(e)
        updateSelectInput(session, "ds_samples", choices = c("Error" = ""))
      )

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

      tryCatch({
        ann_df <- list_annotation_sets(sid)
        if (nrow(ann_df) == 0 || !("_id" %in% names(ann_df))) {
          updateSelectInput(session, "ds_ann_set",
                            choices = c("No annotation sets" = ""))
        } else {
          updateSelectInput(session, "ds_ann_set",
                            choices = c("— select —" = "",
                                        setNames(ann_df[["_id"]], ann_df$name)))
        }
      }, error = function(e)
        updateSelectInput(session, "ds_ann_set", choices = c("Error" = ""))
      )
    }, ignoreInit = TRUE)

    # ── Create dataset ────────────────────────────────────────────────────
    observeEvent(input$create_dataset_btn, {
      sid      <- input$ds_study
      samp_ids <- input$ds_samples
      pid      <- input$ds_pipeline
      ann_id   <- input$ds_ann_set
      nm       <- trimws(input$ds_name)

      if (!nzchar(sid %||% ""))    { showNotification("Select a study.",            type = "warning"); return() }
      if (length(samp_ids) == 0)   { showNotification("Select at least one sample.", type = "warning"); return() }
      if (!nzchar(pid %||% ""))    { showNotification("Select a pipeline.",          type = "warning"); return() }
      if (!nzchar(ann_id %||% "")) { showNotification("Select an annotation set.",   type = "warning"); return() }
      if (!nzchar(nm))             { showNotification("Enter a dataset name.",        type = "warning"); return() }

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
        ds <- list_datasets()
        if (nrow(ds) > 0 && "_id" %in% names(ds)) {
          choices <- setNames(ds[["_id"]], paste0(ds$name, " [", ds$study_id, "]"))
          updateSelectInput(session, "dataset_id", choices = choices, selected = dataset_id)
        }
      }, error = function(e) {
        output$create_dataset_status <- renderUI(
          tags$div(class = "alert alert-danger", style = "padding:6px; margin-top:6px;",
            tags$b("Error: "), conditionMessage(e))
        )
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    # ── Dataset list ──────────────────────────────────────────────────────
    datasets_rv <- reactiveVal(data.frame())

    observe({
      input$refresh_datasets
      tryCatch({
        ds <- list_datasets()
        datasets_rv(ds)
        if (nrow(ds) == 0) {
          updateSelectInput(session, "dataset_id", choices = c("No datasets found" = ""))
        } else {
          choices <- setNames(ds[["_id"]], paste0(ds$name, " [", ds$study_id, "]"))
          updateSelectInput(session, "dataset_id", choices = choices)
        }
      }, error = function(e)
        updateSelectInput(session, "dataset_id", choices = c("Error loading datasets" = ""))
      )
    })

    # ── Dataset info card ─────────────────────────────────────────────────
    output$dataset_info_ui <- renderUI({
      req(input$dataset_id, nchar(input$dataset_id) > 0)
      tryCatch({
        ds <- get_dataset(input$dataset_id)
        n  <- length(unlist(ds$sample_ids))
        sp <- if (is.data.frame(ds$split)) as.list(ds$split[1,]) else ds$split[[1]]
        tagList(tags$small(
          tags$b("Study: "),    ds$study_id,  tags$br(),
          tags$b("Samples: "),  n,            tags$br(),
          tags$b("Pipeline: "), substr(ds$pipeline_id, 1, 12), "...", tags$br(),
          tags$b("Ann. set: "), substr(ds$annotation_set_id, 1, 12), "...", tags$br(),
          tags$b("Stage: "),    ds$stage_type, tags$br(),
          tags$b("Split: "),    sp$train_frac * 100, "% train | seed=", sp$seed
        ))
      }, error = function(e)
        tags$small(style = "color:red", "Could not load dataset info.")
      )
    })

    # ── Train ─────────────────────────────────────────────────────────────
    observeEvent(input$run_training, {
      dataset_id <- input$dataset_id
      if (is.null(dataset_id) || !nzchar(dataset_id)) {
        showNotification("Select a dataset before training.", type = "warning"); return()
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
        add_log(paste0("\u2713 Training complete. model_run_id: ", run_id))
        showNotification(paste0("Training complete! Run ID: ", run_id), type = "message")
      }, error = function(e) {
        add_log(paste0("\u2717 Error: ", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    output$training_log <- renderText({ log_val() })

    # ── Run list ──────────────────────────────────────────────────────────
    runs_rv <- reactiveVal(data.frame())

    observe({
      input$refresh_runs
      req(input$dataset_id, nchar(input$dataset_id) > 0)
      tryCatch({
        runs_rv(list_model_runs(input$dataset_id))
      }, error = function(e) runs_rv(data.frame()))
    })

    observeEvent(last_run_id(), {
      req(input$dataset_id)
      tryCatch(runs_rv(list_model_runs(input$dataset_id)), error = function(e) NULL)
    })

    # ── Run table (DT, clickable) ─────────────────────────────────────────
    output$run_table <- DT::renderDataTable({
      df <- runs_rv()
      if (nrow(df) == 0 || !("_id" %in% names(df))) {
        return(DT::datatable(data.frame(message = "No runs yet."), rownames = FALSE))
      }

      # ---- SAFE accessors (metrics/hyperparams kan være NULL eller mærkeligt formateret)
      as_list1 <- function(x) {
        if (is.null(x)) return(list())
        if (is.data.frame(x)) return(as.list(x[1, , drop = FALSE]))
        if (is.list(x)) return(x)
        list(value = x)
      }

      m_get <- function(m, key) {
        m <- as_list1(m)
        v <- m[[key]]
        if (is.null(v) || length(v) == 0) return(NA_real_)
        suppressWarnings(as.numeric(v[1]))
      }

      hp_get <- function(hp, key) {
        hp <- as_list1(hp)
        v <- hp[[key]]
        if (is.null(v) || length(v) == 0) return(NA_character_)
        as.character(v[1])
      }

      # sortér runs “sikkert”
      df_sorted <- df[order(df$created_at, decreasing = TRUE), , drop = FALSE]
      n <- nrow(df_sorted)

      tbl <- data.frame(
        run_id_full = df_sorted[["_id"]],
        run_id      = substr(df_sorted[["_id"]], 1, 30),
        model_type  = df_sorted$model_type,
        mtry        = vapply(seq_len(n), \(i) hp_get(df_sorted$hyperparams[[i]], "mtry"), character(1)),
        trees       = vapply(seq_len(n), \(i) hp_get(df_sorted$hyperparams[[i]], "num_trees"), character(1)),
        node        = vapply(seq_len(n), \(i) hp_get(df_sorted$hyperparams[[i]], "min_node_size"), character(1)),
        rule        = vapply(seq_len(n), \(i) hp_get(df_sorted$hyperparams[[i]], "splitrule"), character(1)),
        cv          = vapply(seq_len(n), \(i) hp_get(df_sorted$hyperparams[[i]], "cv_folds"), character(1)),
        test_acc    = vapply(seq_len(n), \(i) m_get(df_sorted$metrics[[i]], "test_accuracy"), numeric(1)),
        test_kappa  = vapply(seq_len(n), \(i) m_get(df_sorted$metrics[[i]], "test_kappa"), numeric(1)),
        cv_acc      = vapply(seq_len(n), \(i) m_get(df_sorted$metrics[[i]], "cv_mean_accuracy"), numeric(1)),
        created_at  = df_sorted$created_at,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        tbl,
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 10,
          scrollX = TRUE,
          # Skjul run_id_full i tabellen, men behold den i data
          columnDefs = list(list(targets = 0, visible = FALSE))
        ),
        class = "compact stripe hover"
      ) |>
        DT::formatRound(c("test_acc", "test_kappa", "cv_acc"), digits = 4)
    })

    # ── Capture row click → selected_run ─────────────────────────────────
    observeEvent(input$run_table_rows_selected, {
      idx <- input$run_table_rows_selected
      if (is.null(idx) || length(idx) == 0) {
        selected_run_id(NULL)
        return()
      }

      # hent den viste tabel igen (DT sender ikke row data),
      # så vi bruger runs_rv() + samme sortering som i tabellen:
      df <- runs_rv()
      if (nrow(df) == 0) { selected_run_id(NULL); return() }

      df_sorted <- df[order(df$created_at, decreasing = TRUE), , drop = FALSE]
      rid <- df_sorted[["_id"]][idx]
      selected_run_id(rid)
    })

    # Also auto-select last trained run
    observeEvent(last_run_id(), {
      df <- runs_rv()
      if (nrow(df) == 0 || !("_id" %in% names(df))) return()
      row <- df[df[["_id"]] == last_run_id(), , drop = FALSE]
      if (nrow(row) > 0) selected_run(row)
    })

    # ── Run details panel ─────────────────────────────────────────────────
    output$run_details_ui <- renderUI({
      rid <- selected_run_id()
      if (is.null(rid) || !nzchar(rid)) {
        return(tags$p(style = "color:#888", "Click a row above to see details."))
      }

      # Find run i runs_rv() (billigt). Alternativt: hent fra DB hvis du vil være 100% sikker.
      df <- runs_rv()
      row <- df[df[["_id"]] == rid, , drop = FALSE]
      if (nrow(row) == 0) {
        return(tags$p(style = "color:#c00", "Selected run not found in run list (refresh?)."))
      }

      m  <- row$metrics[[1]]
      hp <- row$hyperparams[[1]]

      cat("DEBUG metrics class:", class(row$metrics[[1]]), "\n")
      cat("DEBUG hyperparams class:", class(row$hyperparams[[1]]), "\n")

      if (is.data.frame(m))  m  <- as.list(m[1,  , drop = FALSE])
      if (is.data.frame(hp)) hp <- as.list(hp[1, , drop = FALSE])
      if (is.null(m))  m  <- list()
      if (is.null(hp)) hp <- list()

      first_chr <- function(x, default = "—") {
        if (is.null(x) || length(x) == 0) return(default)
        as.character(x[1])
      }
      first_num <- function(x) {
        if (is.null(x) || length(x) == 0) return(NA_real_)
        suppressWarnings(as.numeric(x[1]))
      }
      fmt_num <- function(x, digits = 4) {
        v <- first_num(x)
        if (!is.finite(v)) return(tags$span(style="color:#aaa", "—"))
        tags$b(round(v, digits))
      }

      lo <- first_num(m[["test_acc_lower"]])
      hi <- first_num(m[["test_acc_upper"]])

      # Per-class keys (kan være 0)
      bc_keys <- grep("^byclass_Sensitivity__", names(m), value = TRUE)
      class_names <- gsub("^byclass_Sensitivity__", "", bc_keys)

      perclass_tbl <- if (length(class_names) > 0) {
        safe_key <- function(metric, cls) {
          k <- paste0("byclass_", metric, "__", cls)
          v <- first_num(m[[k]])
          if (!is.finite(v)) "—" else sprintf("%.4f", v)
        }
        tags$table(
          class="table table-condensed table-bordered",
          style="font-size:13px; margin-top:8px;",
          tags$thead(tags$tr(
            tags$th("Class"),
            tags$th("Sensitivity"), tags$th("Specificity"),
            tags$th("Precision"), tags$th("Recall"),
            tags$th("F1"), tags$th("Bal. Accuracy")
          )),
          tags$tbody(lapply(class_names, function(cls) {
            tags$tr(
              tags$td(tags$b(gsub("_"," ", cls))),
              tags$td(safe_key("Sensitivity", cls)),
              tags$td(safe_key("Specificity", cls)),
              tags$td(safe_key("Precision", cls)),
              tags$td(safe_key("Recall", cls)),
              tags$td(safe_key("F1", cls)),
              tags$td(safe_key("Balanced_Accuracy", cls))
            )
          }))
        )
      } else {
        tags$p(style="color:#aaa", "No per-class metrics stored.")
      }

      tagList(
        tags$div(
          style="background:#f8f9fa; border:1px solid #dee2e6; border-radius:6px; padding:14px; margin-bottom:12px;",
          tags$h5(style="margin-top:0", tags$code(rid)),

          fluidRow(
            column(4,
              tags$h6(tags$b("Hyperparameters")),
              tags$table(class="table table-condensed", style="font-size:13px;",
                tags$tr(tags$td("Model"), tags$td(first_chr(row$model_type))),
                tags$tr(tags$td("mtry"), tags$td(first_chr(hp[["mtry"]]))),
                tags$tr(tags$td("num.trees"), tags$td(first_chr(hp[["num_trees"]]))),
                tags$tr(tags$td("min.node.size"), tags$td(first_chr(hp[["min_node_size"]]))),
                tags$tr(tags$td("splitrule"), tags$td(first_chr(hp[["splitrule"]]))),
                tags$tr(tags$td("CV folds"), tags$td(first_chr(hp[["cv_folds"]]))),
                tags$tr(tags$td("Seed"), tags$td(first_chr(hp[["seed"]])))
              )
            ),
            column(4,
              tags$h6(tags$b("Test Set Metrics")),
              tags$table(class="table table-condensed", style="font-size:13px;",
                tags$tr(tags$td("Accuracy"), tags$td(fmt_num(m[["test_accuracy"]]))),
                tags$tr(tags$td("95% CI"), tags$td({
                  if (is.finite(lo) && is.finite(hi)) paste0("[", round(lo,4), ", ", round(hi,4), "]") else "—"
                })),
                tags$tr(tags$td("Kappa"), tags$td(fmt_num(m[["test_kappa"]])))
              )
            ),
            column(4,
              tags$h6(tags$b("Cross-Validation Metrics")),
              if (!is.null(m[["cv_mean_accuracy"]])) {
                tags$table(class="table table-condensed", style="font-size:13px;",
                  tags$tr(tags$td("CV Accuracy"), tags$td(fmt_num(m[["cv_mean_accuracy"]]))),
                  tags$tr(tags$td("CV Acc SD"), tags$td(fmt_num(m[["cv_acc_sd"]]))),
                  tags$tr(tags$td("CV Kappa"), tags$td(fmt_num(m[["cv_mean_kappa"]]))),
                  tags$tr(tags$td("CV Mean F1"), tags$td(fmt_num(m[["cv_mean_f1"]])))
                )
              } else {
                tags$p(style="color:#aaa; font-size:13px;", "No CV metrics stored.")
              }
            )
          )
        ),
        tags$h6(tags$b("Per-Class Metrics (Test Set)")),
        perclass_tbl
      )
    })
  })
}