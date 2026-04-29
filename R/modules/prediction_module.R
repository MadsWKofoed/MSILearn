# R/modules/prediction_module.R

prediction_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Prediction",
    fluidRow(
      column(
        4,
        wellPanel(
          h4("1. Select Model Run"),
          actionButton(ns("refresh_runs"), "Refresh model runs", class = "btn-sm btn-default"),
          br(), br(),
          selectInput(
            ns("run_filter_study"),
            "Study:",
            choices = c("All studies" = "__all__"),
            width = "100%"
          ),
          selectInput(
            ns("run_filter_dataset"),
            "Dataset:",
            choices = c("All datasets" = "__all__"),
            width = "100%"
          ),
          selectInput(
            ns("run_filter_eval_mode"),
            "Evaluation mode:",
            choices = c(
              "All" = "__all__",
              "CV only" = "cv_only",
              "CV + held-out test" = "cv_plus_test"
            ),
            width = "100%"
          ),
          selectInput(
            ns("run_filter_pipeline"),
            "Processing pipeline:",
            choices = c("All pipelines" = "__all__"),
            width = "100%"
          ),
          selectInput(
            ns("run_filter_normalize"),
            "Normalisation:",
            choices = c("All" = "__all__"),
            width = "100%"
          ),
          selectInput(
            ns("run_filter_model_type"),
            "Model type:",
            choices = c("All" = "__all__"),
            width = "100%"
          ),
          DT::dataTableOutput(ns("run_table")),
          tags$div(style = "display:none;",
          selectInput(
            ns("run_id"),
            "Trained model:",
            choices = c("(loading...)" = ""),
            width = "100%"
          )),
          uiOutput(ns("run_info_ui"))
        ),

        wellPanel(
          h4("2. Upload New Raw Data"),
          fileInput(
            ns("pred_files"),
            "Upload one .imzML and one .ibd",
            multiple = TRUE,
            accept = c(".imzML", ".ibd")
          ),
          actionButton(
            ns("run_prediction"),
            "Run Prediction",
            class = "btn-primary btn-lg",
            style = "width:100%;"
          )
        )
      ),

      column(
        3,
        h4("Resolved Data Structure"),
        uiOutput(ns("pipeline_info_ui")),
        hr(),
        h4("Prediction Log"),
        verbatimTextOutput(ns("prediction_log")),
        hr(),
        h4("Class Counts"),
        DT::DTOutput(ns("class_count_table"))
      ),

      column(
        5,
        wellPanel(
          h4("Prediction Map"),
          plotOutput(ns("prediction_plot"), height = "700px")
        )
      )
    )
  )
}


prediction_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pred_log <- reactiveVal("")
    pred_res <- reactiveVal(NULL)
    run_browser_rv <- reactiveVal(data.frame())
    run_table_proxy <- DT::dataTableProxy(ns("run_table"))

    add_log <- function(msg) {
      pred_log(
        paste0(
          pred_log(),
          format(Sys.time(), "[%H:%M:%S] "), msg, "\n"
        )
      )
    }

    first_chr <- function(x, default = "—") {
      if (is.null(x) || length(x) == 0) return(default)
      as.character(x[[1]])
    }

    normalize_eval_mode <- function(x) {
      mode <- as.character(x %||% "cv_plus_test")
      if (!mode %in% c("cv_plus_test", "cv_only")) mode <- "cv_plus_test"
      mode
    }

    evaluation_mode_label <- function(x) {
      mode <- normalize_eval_mode(x)
      if (identical(mode, "cv_only")) "CV only" else "CV + held-out test"
    }

    run_filter_all_value <- "__all__"

    empty_run_summary <- function() {
      data.frame(
        run_id = character(0),
        run_id_short = character(0),
        dataset_id = character(0),
        dataset_name = character(0),
        dataset_label = character(0),
        study_id = character(0),
        study = character(0),
        pipeline_id = character(0),
        pipeline_name = character(0),
        model_type = character(0),
        evaluation_mode = character(0),
        evaluation_mode_label = character(0),
        normalisation = character(0),
        primary_score = numeric(0),
        created_at = character(0),
        stringsAsFactors = FALSE
      )
    }

    run_filter_config <- list(
      run_filter_study = list(
        all_label = "All studies",
        value_col = "study_id",
        label_col = "study"
      ),
      run_filter_dataset = list(
        all_label = "All datasets",
        value_col = "dataset_id",
        label_col = "dataset_label"
      ),
      run_filter_eval_mode = list(
        all_label = "All",
        value_col = "evaluation_mode",
        label_col = "evaluation_mode_label"
      ),
      run_filter_pipeline = list(
        all_label = "All pipelines",
        value_col = "pipeline_id",
        label_col = "pipeline_name"
      ),
      run_filter_normalize = list(
        all_label = "All",
        value_col = "normalisation",
        label_col = "normalisation"
      ),
      run_filter_model_type = list(
        all_label = "All",
        value_col = "model_type",
        label_col = "model_type"
      )
    )

    normalize_filter_value <- function(x) {
      val <- as.character(x %||% run_filter_all_value)
      if (!nzchar(val) || identical(val, run_filter_all_value)) run_filter_all_value else val
    }

    current_run_filter_values <- function() {
      list(
        run_filter_study = normalize_filter_value(input$run_filter_study),
        run_filter_dataset = normalize_filter_value(input$run_filter_dataset),
        run_filter_eval_mode = normalize_filter_value(input$run_filter_eval_mode),
        run_filter_pipeline = normalize_filter_value(input$run_filter_pipeline),
        run_filter_normalize = normalize_filter_value(input$run_filter_normalize),
        run_filter_model_type = normalize_filter_value(input$run_filter_model_type)
      )
    }

    build_run_summary <- function(run_df) {
      if (is.null(run_df) || !is.data.frame(run_df) || nrow(run_df) == 0 || !all(c("_id", "dataset_id") %in% names(run_df))) {
        return(empty_run_summary())
      }

      studies_df <- tryCatch(get_studies(), error = function(e) data.frame())
      study_map <- if (is.data.frame(studies_df) && nrow(studies_df) > 0 && all(c("_id", "name") %in% names(studies_df))) {
        stats::setNames(as.character(studies_df$name), as.character(studies_df[["_id"]]))
      } else {
        setNames(character(0), character(0))
      }

      dataset_ids <- unique(as.character(run_df$dataset_id %||% character(0)))
      dataset_ids <- dataset_ids[nzchar(dataset_ids)]
      dataset_docs <- lapply(dataset_ids, function(did) {
        tryCatch(get_dataset(did), error = function(e) NULL)
      })
      names(dataset_docs) <- dataset_ids

      pipeline_ids <- unique(vapply(dataset_docs, function(ds) {
        if (is.null(ds) || !is.data.frame(ds) || nrow(ds) == 0) return("")
        as.character(ds$pipeline_id[1] %||% "")
      }, character(1)))
      pipeline_ids <- pipeline_ids[nzchar(pipeline_ids)]
      pipeline_name_map <- stats::setNames(
        vapply(pipeline_ids, function(pid) {
          tryCatch(as.character(get_pipeline(pid)$name[1] %||% pid), error = function(e) pid)
        }, character(1)),
        pipeline_ids
      )

      rows <- lapply(seq_len(nrow(run_df)), function(i) {
        row <- run_df[i, , drop = FALSE]
        run_id <- as.character(row$`_id`[1] %||% "")
        dataset_id <- as.character(row$dataset_id[1] %||% "")
        ds <- dataset_docs[[dataset_id]]
        if (is.null(ds) || !is.data.frame(ds) || nrow(ds) == 0) return(NULL)

        hp <- extract_params(row$hyperparams)
        metrics <- extract_params(row$metrics)

        study_id <- as.character(ds$study_id[1] %||% "")
        study_name <- as.character(study_map[[study_id]] %||% study_id)
        dataset_name <- as.character(ds$name[1] %||% dataset_id)
        pipeline_id <- as.character(ds$pipeline_id[1] %||% "")
        pipeline_name <- as.character(pipeline_name_map[[pipeline_id]] %||% pipeline_id)
        eval_mode <- normalize_eval_mode(hp$evaluation_mode)
        test_acc <- suppressWarnings(as.numeric(hp$test_accuracy %||% metrics$test_accuracy %||% NA_real_))
        cv_acc <- suppressWarnings(as.numeric(hp$cv_mean_accuracy %||% metrics$cv_mean_accuracy %||% NA_real_))
        primary_score <- if (identical(eval_mode, "cv_only")) cv_acc else test_acc
        norm_method <- as.character(hp$normalize_method[1] %||% "none")
        if (!nzchar(norm_method)) norm_method <- "none"

        data.frame(
          run_id = run_id,
          run_id_short = substr(run_id, 1, 18),
          dataset_id = dataset_id,
          dataset_name = dataset_name,
          dataset_label = paste0(dataset_name, " [", study_name, "]"),
          study_id = study_id,
          study = study_name,
          pipeline_id = pipeline_id,
          pipeline_name = pipeline_name,
          model_type = as.character(row$model_type[1] %||% ""),
          evaluation_mode = eval_mode,
          evaluation_mode_label = evaluation_mode_label(eval_mode),
          normalisation = norm_method,
          primary_score = primary_score,
          created_at = as.character(row$created_at[1] %||% ""),
          stringsAsFactors = FALSE
        )
      })

      summary_df <- dplyr::bind_rows(Filter(Negate(is.null), rows))
      if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0) {
        return(empty_run_summary())
      }

      summary_df[order(summary_df$created_at, decreasing = TRUE), , drop = FALSE]
    }

    filtered_run_summary <- function(exclude_filter = NULL, filters = NULL) {
      df <- run_browser_rv()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(empty_run_summary())
      }

      if (is.null(filters)) {
        filters <- current_run_filter_values()
      }

      out <- df
      for (filter_id in names(run_filter_config)) {
        if (identical(filter_id, exclude_filter)) next
        selected <- as.character(filters[[filter_id]] %||% run_filter_all_value)
        if (identical(selected, run_filter_all_value) || !nzchar(selected)) next

        value_col <- run_filter_config[[filter_id]]$value_col
        out <- out[out[[value_col]] == selected, , drop = FALSE]
      }

      out[order(out$created_at, decreasing = TRUE), , drop = FALSE]
    }

    build_run_filter_choices <- function(filter_id, filters = NULL) {
      cfg <- run_filter_config[[filter_id]]
      choices <- stats::setNames(run_filter_all_value, cfg$all_label)
      df <- filtered_run_summary(exclude_filter = filter_id, filters = filters)

      if (nrow(df) == 0) return(choices)

      vals <- unique(df[, c(cfg$value_col, cfg$label_col), drop = FALSE])
      vals <- vals[nzchar(as.character(vals[[cfg$value_col]] %||% "")), , drop = FALSE]
      if (nrow(vals) == 0) return(choices)

      vals[[cfg$label_col]] <- as.character(vals[[cfg$label_col]] %||% vals[[cfg$value_col]])
      vals <- vals[order(vals[[cfg$label_col]], vals[[cfg$value_col]]), , drop = FALSE]
      c(choices, stats::setNames(as.character(vals[[cfg$value_col]]), as.character(vals[[cfg$label_col]])))
    }

    sync_run_browser_filters <- function() {
      filters <- current_run_filter_values()

      for (filter_id in names(run_filter_config)) {
        choices <- build_run_filter_choices(filter_id, filters = filters)
        selected <- as.character(filters[[filter_id]] %||% run_filter_all_value)
        if (!(selected %in% unname(choices))) {
          selected <- run_filter_all_value
        }
        filters[[filter_id]] <- selected
        updateSelectInput(session, filter_id, choices = choices, selected = selected)
      }

      filters
    }

    sync_run_id_input <- function(selected_run_id = NULL) {
      browser_df <- run_browser_rv()
      if (is.null(browser_df) || !is.data.frame(browser_df) || nrow(browser_df) == 0) {
        updateSelectInput(session, "run_id", choices = c("No model runs found" = ""), selected = "")
        return(invisible(""))
      }

      choices <- setNames(
        browser_df$run_id,
        paste0(browser_df$dataset_name, " | ", browser_df$pipeline_name, " | ", browser_df$created_at)
      )
      selected <- selected_run_id %||% input$run_id %||% ""
      if (!nzchar(selected) || !(selected %in% browser_df$run_id)) {
        selected <- ""
      }

      updateSelectInput(
        session,
        "run_id",
        choices = c("— select —" = "", choices),
        selected = selected
      )

      invisible(selected)
    }

    refresh_run_browser <- function(selected_run_id = NULL) {
      df <- list_all_model_runs()
      run_browser_rv(build_run_summary(df))
      sync_run_id_input(selected_run_id = selected_run_id)
    }

    observeEvent(c(TRUE, input$refresh_runs), {
      tryCatch({
        refresh_run_browser(selected_run_id = input$run_id %||% "")
      }, error = function(e) {
        run_browser_rv(empty_run_summary())
        updateSelectInput(session, "run_id", choices = c("Error loading model runs" = ""), selected = "")
        showNotification(conditionMessage(e), type = "error")
      })
    }, ignoreInit = FALSE)

    observe({
      run_browser_rv()
      input$run_filter_study
      input$run_filter_dataset
      input$run_filter_eval_mode
      input$run_filter_pipeline
      input$run_filter_normalize
      input$run_filter_model_type

      sync_run_browser_filters()
      sync_run_id_input(selected_run_id = input$run_id %||% "")
    })

    filtered_runs <- reactive({
      filtered_run_summary()
    })

    output$run_table <- DT::renderDataTable({
      df <- filtered_runs()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(message = "No model runs match the current filters."), rownames = FALSE))
      }

      tbl <- df[, c(
        "run_id", "dataset_name", "study", "pipeline_name",
        "model_type", "evaluation_mode_label", "normalisation",
        "primary_score", "created_at"
      ), drop = FALSE]

      names(tbl) <- c(
        "run_id", "Dataset", "Study", "Processing pipeline",
        "Model type", "Evaluation mode", "Normalisation",
        "Primary score", "Created at"
      )

      DT::datatable(
        tbl,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))
        ),
        class = "compact stripe hover"
      )
    })

    observe({
      rid <- input$run_id %||% ""
      df <- filtered_runs()
      if (!nzchar(rid) || is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        DT::selectRows(run_table_proxy, NULL)
        return()
      }
      idx <- which(df$run_id == rid)
      if (length(idx) == 1) {
        DT::selectRows(run_table_proxy, idx)
      } else {
        DT::selectRows(run_table_proxy, NULL)
      }
    })

    observeEvent(input$run_table_rows_selected, {
      idx <- input$run_table_rows_selected
      df <- filtered_runs()
      if (is.null(idx) || length(idx) == 0 || is.null(df) || nrow(df) == 0) return()
      rid <- df$run_id[idx[1]]
      sync_run_id_input(selected_run_id = rid)
    })

    selected_context <- reactive({
      req(input$run_id, nzchar(input$run_id))
      get_prediction_context(input$run_id)
    })

    output$run_info_ui <- renderUI({
      ctx <- selected_context()
      hp  <- ctx$hyperparams
      run_df <- run_browser_rv()
      run_row <- run_df[run_df$run_id == input$run_id, , drop = FALSE]
      dataset_label <- if (nrow(run_row) == 1) run_row$dataset_name[1] else ctx$dataset_id
      study_label <- if (nrow(run_row) == 1) run_row$study[1] else "—"
      eval_label <- if (nrow(run_row) == 1) run_row$evaluation_mode_label[1] else "—"

      tags$div(
        style = "margin-top:8px;",
        tags$div(
          style = "margin-bottom:6px;",
          tags$b("Selected model run: "),
          input$run_id
        ),
        tags$small(
          tags$b("Dataset: "), dataset_label, tags$br(),
          tags$b("Study: "), study_label, tags$br(),
          tags$b("Pipeline: "), ctx$pipeline_name, tags$br(),
          tags$b("Evaluation mode: "), eval_label, tags$br(),
          tags$b("Model type: "), first_chr(ctx$run_row$model_type), tags$br(),
          tags$b("Normalisation: "), first_chr(hp$normalize_method, "none"), tags$br(),
          tags$b("mtry: "), first_chr(hp$mtry), tags$br(),
          tags$b("Trees: "), first_chr(hp$num_trees)
        )
      )
    })

    output$pipeline_info_ui <- renderUI({
      ctx <- selected_context()
      p   <- ctx$pipeline_params
      hp  <- ctx$hyperparams

      tags$table(
        class = "table table-condensed table-bordered",
        tags$tbody(
          tags$tr(tags$td("dataset_id"),        tags$td(ctx$dataset_id)),
          tags$tr(tags$td("pipeline_id"),       tags$td(ctx$pipeline_id)),
          tags$tr(tags$td("pipeline_name"),     tags$td(ctx$pipeline_name)),
          tags$tr(tags$td("snr"),               tags$td(first_chr(p$snr))),
          tags$tr(tags$td("tolerance"),         tags$td(first_chr(p$tolerance))),
          tags$tr(tags$td("resolution"),        tags$td(first_chr(p$resolution))),
          tags$tr(tags$td("reference_name"),    tags$td(first_chr(p$reference_name))),
          tags$tr(tags$td("normalize_method"),  tags$td(first_chr(hp$normalize_method, "none")))
        )
      )
    })

    observeEvent(input$run_prediction, {
      req(input$run_id, nzchar(input$run_id))

      files <- input$pred_files
      if (is.null(files) || nrow(files) == 0) {
        showNotification("Upload exactly one .imzML and one .ibd file.", type = "warning")
        return()
      }

      ext <- tolower(tools::file_ext(files$name))
      imz_idx <- which(ext == "imzml")
      ibd_idx <- which(ext == "ibd")

      if (length(imz_idx) != 1 || length(ibd_idx) != 1) {
        showNotification("Please upload exactly one .imzML and one .ibd file.", type = "error")
        return()
      }

      pred_log("")
      pred_res(NULL)

      add_log(paste0("Selected model_run_id: ", input$run_id))
      add_log("Resolving model_run -> dataset -> pipeline ...")

      tryCatch({
        ctx <- selected_context()
        add_log(paste0("dataset_id = ", ctx$dataset_id))
        add_log(paste0("pipeline_id = ", ctx$pipeline_id))
        add_log(paste0("reference_name = ", first_chr(ctx$pipeline_params$reference_name)))
        add_log(paste0("normalize_method = ", first_chr(ctx$hyperparams$normalize_method, "none")))
        add_log("Processing uploaded raw data with pipeline-tied parameters ...")

        res <- run_prediction_from_upload(
          run_id     = input$run_id,
          imzml_path = files$datapath[imz_idx][1],
          ibd_path   = files$datapath[ibd_idx][1],
          imzml_name = files$name[imz_idx][1]
        )

        pred_res(res)

        cls_tab <- sort(table(res$prediction_df$Predicted), decreasing = TRUE)
        add_log(paste0("Prediction complete. Pixels predicted: ", nrow(res$prediction_df)))
        add_log(
          paste(
            paste(names(cls_tab), as.integer(cls_tab), sep = "="),
            collapse = ", "
          )
        )

        showNotification("Prediction complete.", type = "message")
      }, error = function(e) {
        add_log(paste0("ERROR: ", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    output$prediction_log <- renderText({
      pred_log()
    })

    output$class_count_table <- DT::renderDT({
      res <- pred_res()

      if (is.null(res)) {
        return(
          DT::datatable(
            data.frame(message = "No prediction yet."),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }

      tab <- as.data.frame(sort(table(res$prediction_df$Predicted), decreasing = TRUE))
      names(tab) <- c("Class", "Pixels")

      DT::datatable(
        tab,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 20)
      )
    })

    output$prediction_plot <- renderPlot({
      res <- pred_res()

      shiny::req(!is.null(res))
      shiny::req(!is.null(res$prediction_df))

      df <- res$prediction_df

      shiny::validate(
        shiny::need(is.data.frame(df), "No prediction yet."),
        shiny::need(nrow(df) > 0, "No prediction yet."),
        shiny::need("runNames" %in% names(df), "Prediction data missing runNames."),
        shiny::need(
          all(c("x", "y", "Predicted") %in% names(df)),
          "Prediction data missing required columns."
        )
      )

      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = Predicted)) +
        ggplot2::geom_tile() +
        ggplot2::scale_y_reverse() +
        ggplot2::coord_fixed() +
        ggplot2::facet_wrap(~ runNames) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = "bottom",
          strip.text = ggplot2::element_text(face = "bold")
        ) +
        ggplot2::labs(
          x = "x",
          y = "y",
          fill = "Predicted"
        )
    })
  })
}
