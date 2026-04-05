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
          selectInput(
            ns("ds_split_strategy"),
            "Split strategy:",
            choices = c(
              "Random" = "random",
              "Spatial block" = "spatial_block"
            ),
            selected = "random"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'leave_one_sample_out'", ns("ds_split_strategy")),
            numericInput(ns("ds_train_frac"), "Train fraction:", value = 0.8,
                         min = 0.5, max = 0.95, step = 0.05)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'spatial_block'", ns("ds_split_strategy")),
            numericInput(ns("ds_block_size"), "Block size (pixels):", value = 25, min = 2, step = 1),
            numericInput(ns("ds_buffer_radius"), "Buffer radius (pixels):", value = 0, min = 0, step = 1)
          ),
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
        selectInput(
          ns("normalize"),
          "Normalisation",
          choices = c(
            "None"   = "none",
            "TIC"    = "tic",
            "Median" = "median",
            "RMS"    = "rms"
          ),
          selected = "none"
        ),
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

    ns <- session$ns

    log_val        <- reactiveVal("")
    last_run_id    <- reactiveVal(NULL)
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

    observeEvent(input$ds_samples, {
      n_samples <- length(input$ds_samples %||% character(0))
      choices <- c(
        "Random" = "random",
        "Spatial block" = "spatial_block"
      )
      if (n_samples >= 3) {
        choices <- c(choices, "Leave-one-sample-out" = "leave_one_sample_out")
      }
      selected <- input$ds_split_strategy
      if (is.null(selected) || !(selected %in% unname(choices))) selected <- unname(choices)[1]
      updateSelectInput(session, "ds_split_strategy", choices = choices, selected = selected)
    }, ignoreInit = FALSE)

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
        split_strategy <- input$ds_split_strategy %||% "random"
        split_obj <- list(
          strategy = split_strategy,
          seed = as.integer(input$ds_seed)
        )
        if (split_strategy != "leave_one_sample_out") {
          split_obj$train_frac <- as.numeric(input$ds_train_frac)
        }
        if (split_strategy == "spatial_block") {
          split_obj$block_size <- as.integer(input$ds_block_size)
          split_obj$buffer_radius <- as.numeric(input$ds_buffer_radius)
        }

        if (split_strategy == "leave_one_sample_out" && length(samp_ids) < 3) {
          showNotification("Leave-one-sample-out requires at least 3 samples.", type = "warning")
          return()
        }

        dataset_id <- create_dataset(
          study_id          = sid,
          sample_ids        = samp_ids,
          pipeline_id       = pid,
          annotation_set_id = ann_id,
          split             = split_obj,
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

    # Update run list when dataset changes
    observeEvent(input$dataset_id, {
      req(input$dataset_id, nzchar(input$dataset_id))
      tryCatch({
        runs_rv(list_model_runs(input$dataset_id))
        selected_run_id(NULL)  # reset selection når dataset skifter
      }, error = function(e) {
        runs_rv(data.frame())
        selected_run_id(NULL)
      })
    }, ignoreInit = TRUE)

    # ── Dataset list ──────────────────────────────────────────────────────
    observe({
      input$refresh_datasets
      tryCatch({
        ds <- list_datasets()
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
          tags$b("Split strategy: "), sp$strategy %||% "random", tags$br(),
          tags$b("Split: "),    if (!is.null(sp$train_frac)) paste0(sp$train_frac * 100, "% train | ") else "", "seed=", sp$seed,
          if (!is.null(sp$block_size)) tagList(tags$br(), tags$b("Block size: "), sp$block_size),
          if (!is.null(sp$buffer_radius)) tagList(tags$br(), tags$b("Buffer radius: "), sp$buffer_radius)
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
          dataset_id        = dataset_id,
          normalize_method  = input$normalize,
          mtry              = input$mtry,
          splitrule         = input$splitrule,
          min_node_size     = input$min_node_size,
          num_trees         = input$num_trees,
          cv_folds          = input$cv_folds,
          seed              = input$seed
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

      # --- hyperparams/metrics kan være data.frame (kolonner = keys) ELLER list-column ---
      hp_obj <- df_sorted$hyperparams
      m_obj  <- df_sorted$metrics

      # Hyperparams kolonner
      if (is.data.frame(hp_obj)) {
        norm  <- as.character(hp_obj$normalize_method %||% NA)
        mtry  <- as.character(hp_obj$mtry %||% NA)
        trees <- as.character(hp_obj$num_trees %||% NA)
        node  <- as.character(hp_obj$min_node_size %||% NA)
        rule  <- as.character(hp_obj$splitrule %||% NA)
        cv    <- as.character(hp_obj$cv_folds %||% NA)
      } else {
        norm  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "normalize_method"), character(1))
        mtry  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "mtry"), character(1))
        trees <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "num_trees"), character(1))
        node  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "min_node_size"), character(1))
        rule  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "splitrule"), character(1))
        cv    <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "cv_folds"), character(1))
      }

      # Metrics kolonner
      if (is.data.frame(m_obj)) {
        test_acc   <- suppressWarnings(as.numeric(m_obj$test_accuracy %||% NA))
        test_kappa <- suppressWarnings(as.numeric(m_obj$test_kappa %||% NA))
        cv_acc     <- suppressWarnings(as.numeric(m_obj$cv_mean_accuracy %||% NA))
      } else {
        test_acc   <- vapply(seq_len(n), \(i) m_get(m_obj[[i]], "test_accuracy"), numeric(1))
        test_kappa <- vapply(seq_len(n), \(i) m_get(m_obj[[i]], "test_kappa"), numeric(1))
        cv_acc     <- vapply(seq_len(n), \(i) m_get(m_obj[[i]], "cv_mean_accuracy"), numeric(1))
      }

      tbl <- data.frame(
        run_id_full = df_sorted[["_id"]],
        run_id      = substr(df_sorted[["_id"]], 1, 30),
        model_type  = df_sorted$model_type,
        normalisation = norm,
        mtry        = mtry,
        trees       = trees,
        node        = node,
        rule        = rule,
        cv          = cv,
        test_acc    = test_acc,
        test_kappa  = test_kappa,
        cv_acc      = cv_acc,
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
      rid <- last_run_id()
      if (!is.null(rid) && nzchar(rid)) selected_run_id(rid)
    })

    # ── Shared helper (used by run_details_ui, cm_plot, roc_plot) ─────────
    extract_subdoc <- function(row, field) {
      x <- row[[field]]
      if (is.character(x) && length(x) == 1 && grepl("^\\s*\\{", x)) {
        return(jsonlite::fromJSON(x, simplifyVector = TRUE))
      }
      if (is.null(x)) return(list())
      if (is.data.frame(x)) return(as.list(x[1, , drop = FALSE]))
      if (is.list(x) && length(x) == 1 && is.list(x[[1]])) return(x[[1]])
      if (is.list(x)) return(x)
      list(value = x)
    }

    # ── Run details panel ─────────────────────────────────────────────────
    output$run_details_ui <- renderUI({
      rid <- selected_run_id()
      if (is.null(rid) || !nzchar(rid)) {
        return(tags$p(style="color:#888", "Click a run to see details."))
      }
      
      row <- get_model_run(rid)
        if (is.null(row) || nrow(row) == 0) {
          return(tags$p(style="color:#c00", "Could not load run from DB. Try refresh."))
        }

      m  <- extract_subdoc(row, "metrics")
      hp <- extract_subdoc(row, "hyperparams")

      # force to list (hvis mongo returner data.frame eller weird)
      if (is.data.frame(m))  m  <- as.list(m[1, , drop=FALSE])
      if (is.data.frame(hp)) hp <- as.list(hp[1, , drop=FALSE])
      if (is.null(m))  m  <- list()
      if (is.null(hp)) hp <- list()
      if (!is.list(m))  m  <- list(value = m)
      if (!is.list(hp)) hp <- list(value = hp)

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
          v <- tryCatch(m[[k]], error = function(e) NULL)
          num <- first_num(v)
          if (!is.finite(num)) "—" else sprintf("%.4f", num)
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
                tags$tr(tags$td("Normalisation"), tags$td(first_chr(hp[["normalize_method"]]))),
                tags$tr(tags$td("mtry"), tags$td(first_chr(hp[["mtry"]]))),
                tags$tr(tags$td("num.trees"), tags$td(first_chr(hp[["num_trees"]]))),
                tags$tr(tags$td("min.node.size"), tags$td(first_chr(hp[["min_node_size"]]))),
                tags$tr(tags$td("splitrule"), tags$td(first_chr(hp[["splitrule"]]))),
                tags$tr(tags$td("CV folds"), tags$td(first_chr(hp[["cv_folds"]]))),
                tags$tr(tags$td("Seed"), tags$td(first_chr(hp[["seed"]]))),
                tags$tr(tags$td("Split strategy"), tags$td(first_chr(hp[["split_strategy"]]))),
                tags$tr(tags$td("Block size"), tags$td(first_chr(hp[["split_block_size"]]))),
                tags$tr(tags$td("Buffer radius"), tags$td(first_chr(hp[["split_buffer_radius"]]))),
                tags$tr(tags$td("PCA Moran PCs"), tags$td(first_chr(hp[["pca_moran_n_pcs"]]))),
                tags$tr(tags$td("PCA Moran max points"), tags$td(first_chr(hp[["pca_moran_max_points"]])))
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
                  tags$tr(tags$td("CV Mean F1"), tags$td(fmt_num(m[["cv_mean_f1"]]))),
                  tags$tr(tags$td("Suggested buffer radius"), tags$td(fmt_num(m[["recommended_buffer_radius"]], digits = 2)))
                )
              } else {
                tags$p(style="color:#aaa; font-size:13px;", "No CV metrics stored.")
              }
            )
          )
        ),
        
        tags$h6(tags$b("Per-Class Metrics (Test Set)")),
        perclass_tbl,

        tags$hr(),

        tags$div(
          style="max-width:1100px; margin:auto;",

          tags$div(
            style="margin-bottom:10px;",
            tags$h4(
              "Confusion Matrix",
              style="font-weight:600; margin-bottom:6px;"
            )
          ),

          fluidRow(
            column(3),
            column(
              6,
              plotOutput(ns("cm_plot"), height = "300px")
            ),
            column(3)
          ),

          tags$br(),

          tags$div(
            style="margin-bottom:10px;",
            tags$h4(
              "PCA Moran Correlogram (PC1–PC5)",
              style="font-weight:600; margin-bottom:6px;"
            ),
            tags$p(
              style = "font-size:13px; color:#666; margin-bottom:8px;",
              "Used to estimate a reasonable pixel buffer where spatial autocorrelation falls toward zero."
            )
          ),

          fluidRow(
            column(1),
            column(
              10,
              plotOutput(ns("moran_plot"), height = "450px")
            ),
            column(1)
          ),

          tags$br(),

          tags$div(
            style="margin-bottom:10px;",
            tags$h4(
              "ROC Curves",
              style="font-weight:600; margin-bottom:6px;"
            )
          ),

          fluidRow(
            column(2),
            column(
              8,
              plotOutput(ns("roc_plot"), height = "650px")
            ),
            column(2)
          )
        )
      )
    })

        # ── Confusion matrix plot ─────────────────────────────────────────────
    output$cm_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      row <- get_model_run(rid)
      req(!is.null(row) && nrow(row) > 0)

      m <- extract_subdoc(row, "metrics")
      if (is.data.frame(m)) m <- as.list(m[1, , drop = FALSE])

      cm_raw <- m[["cm_table"]]
      if (is.null(cm_raw)) return(NULL)

      # Unpack all nesting layers mongolite may introduce
      cm_df <- cm_raw
      while (is.list(cm_df) && !is.data.frame(cm_df)) cm_df <- cm_df[[1]]
      req(is.data.frame(cm_df))

      # Recompute Rel_Freq in case it was lost during serialisation
      if (!"Rel_Freq" %in% names(cm_df)) {
        cm_df <- cm_df |>
          dplyr::group_by(Reference) |>
          dplyr::mutate(Rel_Freq = Freq / sum(Freq)) |>
          dplyr::ungroup()
      }
      cm_df$Text_Color <- ifelse(cm_df$Rel_Freq > 0.5, "white", "black")

      ggplot2::ggplot(cm_df,
                      ggplot2::aes(x = Prediction, y = Reference, fill = Rel_Freq)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.3f", Rel_Freq), color = Text_Color),
          size = 5, fontface = "bold", show.legend = FALSE
        ) +
        ggplot2::scale_color_identity() +
        ggplot2::scale_fill_gradient(low = "white", high = "navy",
                                     name = "Relative\nFreq") +
        ggplot2::labs(x = "Predicted", y = "Actual") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(face = "bold", size = 11),
          axis.text.y  = ggplot2::element_text(face = "bold", size = 11),
          plot.title   = ggplot2::element_text(face = "bold", hjust = 0.5),
          panel.grid   = ggplot2::element_blank()
        )
    })

    # ── PCA Moran correlogram plot ───────────────────────────────────────
    output$moran_plot <- renderPlot({
      first_num_local <- function(x) {
        if (is.null(x) || length(x) == 0) return(NA_real_)
        suppressWarnings(as.numeric(x[1]))
      }

      rid <- selected_run_id()
      req(rid, nzchar(rid))
      row <- get_model_run(rid)
      req(!is.null(row) && nrow(row) > 0)

      m <- extract_subdoc(row, "metrics")
      if (is.data.frame(m)) m <- as.list(m[1, , drop = FALSE])

      moran_raw <- m[["pca_moran_correlogram"]]
      if (is.null(moran_raw)) return(NULL)

      moran_df <- moran_raw
      while (is.list(moran_df) && !is.data.frame(moran_df)) moran_df <- moran_df[[1]]
      req(is.data.frame(moran_df), nrow(moran_df) > 0)

      range_raw <- m[["pca_moran_range_summary"]]
      range_df <- range_raw
      while (is.list(range_df) && !is.data.frame(range_df)) range_df <- range_df[[1]]
      if (!is.data.frame(range_df)) {
        range_df <- data.frame(pc = character(0), range_estimate = numeric(0))
      }

      varexp_raw <- m[["pca_var_explained"]]
      varexp_df <- varexp_raw
      while (is.list(varexp_df) && !is.data.frame(varexp_df)) varexp_df <- varexp_df[[1]]
      if (is.data.frame(varexp_df) && nrow(varexp_df) > 0) {
        moran_df <- dplyr::left_join(moran_df, varexp_df, by = "pc")
        moran_df$pc_label <- sprintf(
          "%s (%.1f%% var)",
          moran_df$pc,
          100 * moran_df$variance_explained
        )
      } else {
        moran_df$pc_label <- moran_df$pc
      }

      p <- ggplot2::ggplot(
        moran_df,
        ggplot2::aes(x = distance_mid, y = moran_i, color = pc_label)
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(
          x = "Pixel distance",
          y = "Moran's I",
          color = NULL
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 13),
          axis.text = ggplot2::element_text(size = 11)
        )

      if (nrow(range_df) > 0) {
        if (is.data.frame(varexp_df) && nrow(varexp_df) > 0) {
          range_df <- dplyr::left_join(range_df, varexp_df, by = "pc")
          range_df$pc_label <- sprintf(
            "%s (%.1f%% var)",
            range_df$pc,
            100 * range_df$variance_explained
          )
        } else {
          range_df$pc_label <- range_df$pc
        }
        p <- p + ggplot2::geom_vline(
          data = range_df,
          ggplot2::aes(xintercept = range_estimate, color = pc_label),
          linetype = "dotted",
          alpha = 0.7,
          show.legend = FALSE
        )
      }

      rec_buf <- first_num_local(m[["recommended_buffer_radius"]])
      if (is.finite(rec_buf)) {
        p <- p + ggplot2::annotate(
          "text",
          x = rec_buf,
          y = max(moran_df$moran_i, na.rm = TRUE),
          label = paste0("Suggested buffer ≈ ", round(rec_buf, 1), " px"),
          vjust = -0.5,
          hjust = 0,
          size = 4
        ) +
          ggplot2::geom_vline(xintercept = rec_buf, linetype = "longdash", alpha = 0.6)
      }

      p
    })

    # ── ROC curve plot ────────────────────────────────────────────────────
    output$roc_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      row <- get_model_run(rid)
      req(!is.null(row) && nrow(row) > 0)

      m <- extract_subdoc(row, "metrics")
      if (is.data.frame(m)) m <- as.list(m[1, , drop = FALSE])

      roc_raw <- m[["roc_data"]]
      if (is.null(roc_raw)) return(NULL)

      # --- robust unwrapping ---
      unwrap_once <- function(x) {
        if (is.list(x) && length(x) == 1) return(x[[1]])
        x
      }

      roc_list <- roc_raw
      repeat {
        new_obj <- unwrap_once(roc_list)
        if (identical(new_obj, roc_list)) break
        roc_list <- new_obj
      }

      # Case 1: already a data.frame with one row per class
      if (is.data.frame(roc_list)) {
        roc_entries <- lapply(seq_len(nrow(roc_list)), function(i) {
          list(
            class = as.character(roc_list$class[i]),
            auc   = suppressWarnings(as.numeric(roc_list$auc[i])),
            sensitivities = unlist(roc_list$sensitivities[[i]]),
            specificities = unlist(roc_list$specificities[[i]])
          )
        })

      # Case 2: list of per-class objects
      } else if (is.list(roc_list)) {
        roc_entries <- lapply(roc_list, function(r) {

          # if one entry is itself a 1-row data.frame
          if (is.data.frame(r)) {
            return(list(
              class = as.character(r$class[1]),
              auc   = suppressWarnings(as.numeric(r$auc[1])),
              sensitivities = unlist(r$sensitivities[[1]]),
              specificities = unlist(r$specificities[[1]])
            ))
          }

          # if one entry is a plain list
          if (is.list(r)) {
            return(list(
              class = as.character(r$class[1]),
              auc   = suppressWarnings(as.numeric(r$auc[1])),
              sensitivities = unlist(r$sensitivities),
              specificities = unlist(r$specificities)
            ))
          }

          NULL
        })

        roc_entries <- Filter(Negate(is.null), roc_entries)

      } else {
        return(NULL)
      }

      roc_df <- do.call(rbind, lapply(roc_entries, function(r) {
        if (length(r$sensitivities) == 0 || length(r$specificities) == 0) return(NULL)

        n <- min(length(r$sensitivities), length(r$specificities))
        data.frame(
          class = r$class,
          fpr   = 1 - as.numeric(r$specificities[seq_len(n)]),
          tpr   = as.numeric(r$sensitivities[seq_len(n)]),
          auc   = as.numeric(r$auc),
          stringsAsFactors = FALSE
        )
      }))

      req(!is.null(roc_df), nrow(roc_df) > 0)

      auc_labels <- roc_df |>
        dplyr::group_by(class) |>
        dplyr::summarise(auc = dplyr::first(auc), .groups = "drop") |>
        dplyr::mutate(label = sprintf("%s (AUC = %.3f)", class, auc))

      roc_df <- dplyr::left_join(roc_df, auc_labels[, c("class", "label")], by = "class")

      n_classes <- length(unique(roc_df$class))
      legend_nrow <- ceiling(n_classes / 3)

      ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr, color = label)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_abline(
          slope = 1,
          intercept = 0,
          linetype = "dashed",
          color = "grey60"
        ) +
        ggplot2::scale_x_continuous(
          limits = c(0, 1),
          labels = scales::percent_format()
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          labels = scales::percent_format()
        ) +
        ggplot2::labs(
          x = "False Positive Rate",
          y = "True Positive Rate",
          color = NULL
        ) +
        ggplot2::guides(
          color = ggplot2::guide_legend(
            nrow = legend_nrow,
            byrow = TRUE
          )
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 13),
          legend.title = ggplot2::element_blank(),
          legend.key.width = grid::unit(1.6, "cm"),
          legend.key.height = grid::unit(0.7, "cm"),
          axis.title = ggplot2::element_text(size = 13),
          axis.text = ggplot2::element_text(size = 11),
          plot.margin = ggplot2::margin(10, 15, 10, 15)
        )
    })
  })
}