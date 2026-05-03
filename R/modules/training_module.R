training_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Training",
    div(
      class = "app-page",
      app_page_header(
        title = "Model Training Studio",
        subtitle = "Freeze datasets, configure evaluation settings, and compare machine learning runs without changing any underlying training logic.",
        badge = "Step 3 of 4",
        icon_name = "brain"
      ),
      app_sidebar_layout(
        ns = ns,
        module_key = "training_sidebar",
        sidebar_title = "Training Controls",
        sidebar_subtitle = "Use the sidebar as a guided workflow, and keep larger diagnostics and results in the main workspace.",
        sidebar_icon = "sliders-h",
        sidebar_hint = "Training",
        sidebar = tagList(
          app_workflow_step(
            ns = ns,
            step_id = "step_dataset_create",
            number = "1",
            title = "Create dataset",
            status = uiOutput(ns("step_dataset_create_status")),
            open = TRUE,
            tags$div(
              class = "workflow-lead",
              "Freeze the study, processed MSI pipeline, annotation set, and split settings into a reproducible training dataset."
            ),
            selectInput(ns("ds_study"), "Study:",
              choices = c("(loading...)" = ""), width = "100%"
            ),
            actionButton(ns("ds_refresh_study"), "\u21ba", class = "btn-xs"),
            br(), br(),
            selectInput(ns("ds_pipeline"), "Processing pipeline:",
              choices = c("— select study first —" = ""), width = "100%"
            ),
            selectInput(ns("ds_ann_set"), "Annotation set:",
              choices = c("— select study first —" = ""), width = "100%"
            ),
            tags$label("Samples (select multiple):"),
            selectInput(ns("ds_samples"), NULL,
              choices = c("— select study first —" = ""),
              multiple = TRUE, width = "100%"
            ),
            selectInput(
              ns("ds_evaluation_mode"),
              "Evaluation mode:",
              choices = c(
                "CV + held-out test" = "cv_plus_test",
                "CV only" = "cv_only"
              ),
              selected = "cv_plus_test"
            ),
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
              condition = sprintf(
                "input['%s'] == 'cv_plus_test' && input['%s'] != 'leave_one_sample_out'",
                ns("ds_evaluation_mode"),
                ns("ds_split_strategy")
              ),
              numericInput(ns("ds_train_frac"), "Train fraction:", value = 0.8,
                min = 0.5, max = 0.95, step = 0.05
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'spatial_block'", ns("ds_split_strategy")),
              radioButtons(
                ns("spatial_buffer_mode"),
                "Buffer / block setup:",
                choices = c(
                  "Estimate from Moran's I" = "estimate",
                  "Set manually" = "manual"
                ),
                selected = "estimate",
                inline = FALSE
              ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'estimate'", ns("spatial_buffer_mode")),
              actionButton(ns("estimate_spatial_btn"), "Estimate from Moran's I + correlogram",
                class = "btn-default btn-sm", style = "width:100%; margin-bottom:8px;"
              ),
              uiOutput(ns("estimate_spatial_text"))
            ),
            numericInput(ns("ds_block_size"), "Block size (pixels):", value = 25, min = 2, step = 1),
            numericInput(ns("ds_buffer_radius"), "Buffer radius (pixels):", value = 0, min = 0, step = 1)
            ),
            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'spatial_block' || input['%s'] == 'leave_one_sample_out'",
                ns("ds_split_strategy"),
                ns("ds_split_strategy")
              ),
              actionButton(ns("run_spatial_preview_btn"), "Preview spatial split",
                class = "btn-info btn-sm", style = "width:100%; margin-top:4px;"
              ),
              uiOutput(ns("spatial_preview_ui"))
            ),
            numericInput(ns("ds_seed"), "Dataset split seed:", value = 42, min = 1),
            numericInput(ns("ds_cv_folds"), "CV folds (0 = none):", value = 10, min = 0),
            tags$small(style = "color:#666; display:block; margin-top:4px;",
              "CV folds are stored in the dataset snapshot. CV-only mode requires folds greater than 1."
            ),
            textInput(ns("ds_name"), "Dataset name:", placeholder = "e.g. SSC_cohort_RF_v1"),
            actionButton(ns("create_dataset_btn"), "Create Dataset",
              class = "btn-success btn-sm", style = "width:100%"
            ),
            uiOutput(ns("create_dataset_status"))
          ),
          app_workflow_step(
            ns = ns,
            step_id = "step_dataset_select",
            number = "2",
            title = "Select dataset",
            status = uiOutput(ns("step_dataset_select_status")),
            tags$div(
              class = "workflow-lead",
              "Review the frozen datasets available for training and choose the one you want to compare runs on."
            ),
            p(tags$small(
              "Datasets are frozen snapshots that pin samples, pipeline, ",
              "annotation set, and split seed. Select one to train on it."
            )),
            actionButton(ns("refresh_datasets"), "Refresh dataset list",
              class = "btn-sm btn-default"
            ),
            br(), br(),
            selectInput(ns("dataset_filter_study"), "Study:",
              choices = c("All studies" = "__all__"), width = "100%"
            ),
            selectInput(ns("dataset_filter_eval_mode"), "Evaluation mode:",
              choices = c(
                "All" = "__all__",
                "CV only" = "cv_only",
                "CV + held-out test" = "cv_plus_test"
              ),
              width = "100%"
            ),
            selectInput(ns("dataset_filter_split_strategy"), "Split strategy:",
              choices = c(
                "All" = "__all__",
                "Random" = "random",
                "Spatial block" = "spatial_block",
                "Grouped sample-out" = "leave_one_sample_out"
              ),
              width = "100%"
            ),
            selectInput(ns("dataset_filter_pipeline"), "Processing pipeline:",
              choices = c("All pipelines" = "__all__"), width = "100%"
            ),
            selectInput(ns("dataset_filter_ann_set"), "Annotation set:",
              choices = c("All annotation sets" = "__all__"), width = "100%"
            ),
            DT::dataTableOutput(ns("dataset_table")),
            tags$div(
              style = "display:none;",
              selectInput(ns("dataset_id"), "Dataset:",
                choices = c("(loading...)" = ""), width = "100%"
              )
            ),
            uiOutput(ns("dataset_info_ui"))
          ),
          app_workflow_step(
            ns = ns,
            step_id = "step_training_run",
            number = "3",
            title = "Configure and train",
            status = uiOutput(ns("step_training_run_status")),
            tags$div(
              class = "workflow-lead",
              "Choose model fitting settings for the selected dataset, then train and compare model runs."
            ),
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
            numericInput(ns("mtry"), "mtry", value = 31, min = 1),
            numericInput(ns("num_trees"), "num.trees", value = 500, min = 10),
            numericInput(ns("min_node_size"), "min.node.size", value = 10, min = 1),
            selectInput(ns("splitrule"), "splitrule",
              choices = c("gini", "extratrees"), selected = "gini"
            ),
            numericInput(ns("seed"), "Training / CV seed", value = 1234, min = 1),
            hr(),
            actionButton(ns("run_training"), "Train model",
              class = "btn-primary btn-lg", style = "width:100%"
            ),
            br(), br(),
            verbatimTextOutput(ns("training_log"))
          )
        ),
        main = tagList(
          app_panel(
            title = "Spatial Diagnostic",
            subtitle = "The Moran's I and correlogram diagnostic is shown here at full width when using spatial block estimation.",
            uiOutput(ns("estimate_plot_caption_ui")),
            plotOutput(ns("estimate_moran_plot"), height = "420px")
          ),
          app_panel(
            title = "Model Runs for Selected Dataset",
            subtitle = "Filter the stored datasets in the left sidebar, train on the selected snapshot, and compare the resulting runs here.",
            actions = actionButton(ns("refresh_runs"), "Refresh run list", class = "btn-sm btn-default"),
            DT::dataTableOutput(ns("run_table"))
          ),
          app_panel(
            title = "Run Details",
            subtitle = "Click any model run row to inspect the full metrics, split information, and saved hyperparameters.",
            uiOutput(ns("run_details_ui"))
          )
        )
      )
    )
  )
}


training_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    normalize_eval_mode <- function(x) {
      mode <- as.character(x %||% "cv_plus_test")
      if (!mode %in% c("cv_plus_test", "cv_only")) mode <- "cv_plus_test"
      mode
    }

    evaluation_mode_label <- function(x) {
      mode <- normalize_eval_mode(x)
      if (identical(mode, "cv_only")) "CV only" else "CV + held-out test"
    }

    log_val          <- reactiveVal("")
    last_run_id      <- reactiveVal(NULL)
    selected_run_id  <- reactiveVal(NULL)
    estimate_diag_rv <- reactiveVal(NULL)
    estimate_diag_label_rv <- reactiveVal("")
    estimate_params_rv <- reactiveVal(NULL)
    estimate_in_progress_rv <- reactiveVal(FALSE)
    preview_src_rv   <- reactiveVal(NULL)
    preview_src_key_rv <- reactiveVal(NULL)
    preview_result_rv <- reactiveVal(NULL)
    cv_fold_suggestion_key_rv <- reactiveVal(NULL)
    dataset_browser_rv <- reactiveVal(data.frame())
    dataset_table_proxy <- DT::dataTableProxy(ns("dataset_table"))
    study_map_rv <- reactiveVal(setNames(character(0), character(0)))
    pending_dataset_selection_rv <- reactiveVal(NULL)
    pending_dataset_reveal_rv <- reactiveVal(NULL)

    selected_dataset_split_strategy <- reactive({
      did <- input$dataset_id %||% ""
      if (!nzchar(did)) return(NULL)
      tryCatch({
        ds <- get_dataset(did)
        sp <- if (is.data.frame(ds$split)) as.list(ds$split[1, ]) else ds$split[[1]]
        as.character(sp$strategy %||% "random")
      }, error = function(e) NULL)
    })

    output$step_dataset_create_status <- renderUI({
      sid <- input$ds_study %||% ""
      pid <- input$ds_pipeline %||% ""
      ann_id <- input$ds_ann_set %||% ""
      samp_ids <- input$ds_samples %||% character(0)
      if (nzchar(sid) && nzchar(pid) && nzchar(ann_id) && length(samp_ids) > 0) {
        step_badge_ui("Ready")
      } else {
        step_badge_ui("Setup needed")
      }
    })

    output$step_dataset_select_status <- renderUI({
      if (nzchar(input$dataset_id %||% "")) step_badge_ui("Selected") else step_badge_ui("Choose dataset")
    })

    output$step_training_run_status <- renderUI({
      if (nzchar(input$dataset_id %||% "")) step_badge_ui("Ready") else step_badge_ui("Waiting")
    })

    output$estimate_plot_caption_ui <- renderUI({
      diag <- estimate_diag_rv()
      if (is.null(diag)) {
        return(tags$p(class = "helper-muted", "Run the Moran's I estimate from the dataset creation step to show the diagnostic here."))
      }
      tags$p(
        class = "helper-muted",
        paste0("Current estimate context: ", estimate_diag_label_rv())
      )
    })

    add_log <- function(msg) {
      log_val(paste0(log_val(), "\n", format(Sys.time(), "[%H:%M:%S] "), msg))
    }

    step_badge_ui <- function(text) {
      tags$span(class = "workflow-step-status", text)
    }

    open_sidebar_step <- function(step = c("dataset_create", "dataset_select", "training_run")) {
      step <- match.arg(step)

      ids <- c(
        dataset_create = ns("step_dataset_create"),
        dataset_select = ns("step_dataset_select"),
        training_run = ns("step_training_run")
      )

      target <- ids[[step]]
      js <- sprintf(
        "$('#%s').collapse('hide');
        $('#%s').collapse('hide');
        $('#%s').collapse('hide');
        $('#%s').collapse('show');",
        ids[["dataset_create"]], ids[["dataset_select"]], ids[["training_run"]], target
      )

      shinyjs::runjs(js)
    }

    split_strategy_label <- function(x) {
      val <- as.character(x %||% "random")
      if (identical(val, "leave_one_sample_out")) return("Grouped sample-out")
      if (identical(val, "spatial_block")) return("Spatial block")
      if (identical(val, "random")) return("Random")
      val
    }

    empty_dataset_summary <- function() {
      data.frame(
        dataset_id = character(0),
        dataset_name = character(0),
        study_id = character(0),
        study = character(0),
        pipeline_id = character(0),
        pipeline_label = character(0),
        annotation_set_id = character(0),
        annotation_set_label = character(0),
        created_at = character(0),
        evaluation_mode = character(0),
        evaluation_mode_label = character(0),
        split_strategy = character(0),
        split_strategy_label = character(0),
        cv_folds = integer(0),
        n_samples = integer(0),
        stringsAsFactors = FALSE
      )
    }

    dataset_filter_all_value <- "__all__"

    dataset_filter_config <- list(
      dataset_filter_study = list(
        all_label = "All studies",
        value_col = "study_id",
        label_col = "study"
      ),
      dataset_filter_eval_mode = list(
        all_label = "All",
        value_col = "evaluation_mode",
        label_col = "evaluation_mode_label"
      ),
      dataset_filter_split_strategy = list(
        all_label = "All",
        value_col = "split_strategy",
        label_col = "split_strategy_label"
      ),
      dataset_filter_pipeline = list(
        all_label = "All pipelines",
        value_col = "pipeline_id",
        label_col = "pipeline_label"
      ),
      dataset_filter_ann_set = list(
        all_label = "All annotation sets",
        value_col = "annotation_set_id",
        label_col = "annotation_set_label"
      )
    )

    current_dataset_filter_values <- function() {
      normalize_filter_value <- function(x) {
        val <- as.character(x %||% dataset_filter_all_value)
        if (!nzchar(val) || identical(val, dataset_filter_all_value)) dataset_filter_all_value else val
      }

      list(
        dataset_filter_study = normalize_filter_value(input$dataset_filter_study),
        dataset_filter_eval_mode = normalize_filter_value(input$dataset_filter_eval_mode),
        dataset_filter_split_strategy = normalize_filter_value(input$dataset_filter_split_strategy),
        dataset_filter_pipeline = normalize_filter_value(input$dataset_filter_pipeline),
        dataset_filter_ann_set = normalize_filter_value(input$dataset_filter_ann_set)
      )
    }

    dataset_summary_row <- function(ds_row, ds_doc = NULL,
                                   pipeline_name_map = NULL,
                                   annotation_set_name_map = NULL) {
      if (is.null(ds_row) || !is.data.frame(ds_row) || nrow(ds_row) == 0) return(NULL)

      row <- ds_row[1, , drop = FALSE]
      if (is.null(ds_doc)) {
        ds_doc <- tryCatch(get_dataset(as.character(row$`_id`[1])), error = function(e) NULL)
      }
      if (is.null(ds_doc) || !is.data.frame(ds_doc) || nrow(ds_doc) == 0) return(NULL)

      ds <- ds_doc[1, , drop = FALSE]
      sp <- if (is.data.frame(ds$split)) as.list(ds$split[1, , drop = FALSE]) else ds$split[[1]]
      if (is.null(sp) || !is.list(sp)) sp <- list()

      study_id <- as.character(row$study_id[1] %||% ds$study_id[1] %||% "")
      study_map <- study_map_rv()
      evaluation_mode <- normalize_eval_mode(sp$evaluation_mode)
      split_strategy <- as.character(sp$strategy %||% "random")
      pipeline_id <- as.character(row$pipeline_id[1] %||% ds$pipeline_id[1] %||% "")
      annotation_set_id <- as.character(row$annotation_set_id[1] %||% ds$annotation_set_id[1] %||% "")
      annotation_key <- paste(study_id, annotation_set_id, sep = "::")

      pipeline_label <- as.character(pipeline_name_map[[pipeline_id]] %||% "")
      if (!nzchar(pipeline_label)) {
        pipeline_label <- format_processing_pipeline_label(pipeline_id)
      }

      annotation_set_label <- as.character(annotation_set_name_map[[annotation_key]] %||% "")
      if (!nzchar(annotation_set_label)) {
        annotation_set_label <- "Unnamed annotation set"
      }

      data.frame(
        dataset_id = as.character(row$`_id`[1] %||% ds$`_id`[1] %||% ""),
        dataset_name = as.character(row$name[1] %||% ds$name[1] %||% ""),
        study_id = study_id,
        study = unname(study_map[[study_id]] %||% lookup_study_label(study_id)),
        pipeline_id = pipeline_id,
        pipeline_label = pipeline_label,
        annotation_set_id = annotation_set_id,
        annotation_set_label = annotation_set_label,
        created_at = as.character(row$created_at[1] %||% ds$created_at[1] %||% ""),
        evaluation_mode = evaluation_mode,
        evaluation_mode_label = evaluation_mode_label(evaluation_mode),
        split_strategy = split_strategy,
        split_strategy_label = split_strategy_label(split_strategy),
        cv_folds = suppressWarnings(as.integer(sp$cv_folds %||% NA_integer_)),
        n_samples = length(unlist(ds$sample_ids[[1]])),
        stringsAsFactors = FALSE
      )
    }

    build_dataset_summary <- function(ds_list) {
      if (is.null(ds_list) || !is.data.frame(ds_list) || nrow(ds_list) == 0 || !("_id" %in% names(ds_list))) {
        return(empty_dataset_summary())
      }

      pipeline_ids <- unique(as.character(ds_list$pipeline_id %||% character(0)))
      pipeline_ids <- pipeline_ids[nzchar(pipeline_ids)]
      pipeline_name_map <- stats::setNames(
        vapply(pipeline_ids, function(pid) {
          format_processing_pipeline_label(pid)
        }, character(1)),
        pipeline_ids
      )

      annotation_set_name_map <- setNames(character(0), character(0))
      if (all(c("study_id", "annotation_set_id") %in% names(ds_list))) {
        ann_pairs <- unique(data.frame(
          study_id = as.character(ds_list$study_id %||% character(0)),
          annotation_set_id = as.character(ds_list$annotation_set_id %||% character(0)),
          stringsAsFactors = FALSE
        ))
        ann_pairs <- ann_pairs[nzchar(ann_pairs$study_id) & nzchar(ann_pairs$annotation_set_id), , drop = FALSE]

        study_ids <- unique(ann_pairs$study_id)
        for (study_id in study_ids) {
          ann_df <- tryCatch(list_annotation_sets(study_id), error = function(e) data.frame())
          if (!is.data.frame(ann_df) || nrow(ann_df) == 0 || !all(c("_id", "name") %in% names(ann_df))) next

          ann_ids <- as.character(ann_df[["_id"]])
          ann_names <- as.character(ann_df$name %||% ann_ids)
          keys <- paste(study_id, ann_ids, sep = "::")
          annotation_set_name_map[keys] <- ann_names
        }
      }

      rows <- lapply(seq_len(nrow(ds_list)), function(i) {
        tryCatch(
          dataset_summary_row(
            ds_list[i, , drop = FALSE],
            pipeline_name_map = pipeline_name_map,
            annotation_set_name_map = annotation_set_name_map
          ),
          error = function(e) NULL
        )
      })
      summary_df <- dplyr::bind_rows(Filter(Negate(is.null), rows))
      if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0) {
        return(empty_dataset_summary())
      }
      summary_df
    }

    filtered_dataset_summary <- function(exclude_filter = NULL, filters = NULL) {
      df <- dataset_browser_rv()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(empty_dataset_summary())
      }

      if (is.null(filters)) {
        filters <- current_dataset_filter_values()
      }

      out <- df
      for (filter_id in names(dataset_filter_config)) {
        if (identical(filter_id, exclude_filter)) next
        selected <- as.character(filters[[filter_id]] %||% dataset_filter_all_value)
        if (identical(selected, dataset_filter_all_value) || !nzchar(selected)) next

        value_col <- dataset_filter_config[[filter_id]]$value_col
        out <- out[out[[value_col]] == selected, , drop = FALSE]
      }

      out[order(out$created_at, decreasing = TRUE), , drop = FALSE]
    }

    build_dataset_filter_choices <- function(filter_id, filters = NULL) {
      cfg <- dataset_filter_config[[filter_id]]
      choices <- stats::setNames(dataset_filter_all_value, cfg$all_label)
      df <- filtered_dataset_summary(exclude_filter = filter_id, filters = filters)

      if (nrow(df) == 0) {
        return(choices)
      }

      vals <- unique(df[, c(cfg$value_col, cfg$label_col), drop = FALSE])
      vals <- vals[nzchar(vals[[cfg$value_col]] %||% ""), , drop = FALSE]
      if (nrow(vals) == 0) {
        return(choices)
      }

      vals[[cfg$label_col]] <- as.character(vals[[cfg$label_col]] %||% vals[[cfg$value_col]])
      vals <- vals[order(vals[[cfg$label_col]], vals[[cfg$value_col]]), , drop = FALSE]

      c(choices, stats::setNames(as.character(vals[[cfg$value_col]]), as.character(vals[[cfg$label_col]])))
    }

    sync_dataset_browser_filters <- function(reveal_dataset_id = NULL) {
      filters <- current_dataset_filter_values()
      browser_df <- dataset_browser_rv()

      if (!is.null(reveal_dataset_id) && nzchar(reveal_dataset_id) &&
          is.data.frame(browser_df) && nrow(browser_df) > 0) {
        visible_ids <- filtered_dataset_summary(filters = filters)$dataset_id
        if (!(reveal_dataset_id %in% visible_ids)) {
          row <- browser_df[browser_df$dataset_id == reveal_dataset_id, , drop = FALSE]
          if (nrow(row) == 1) {
            filters$dataset_filter_study <- row$study_id[1] %||% ""
            filters$dataset_filter_eval_mode <- row$evaluation_mode[1] %||% ""
            filters$dataset_filter_split_strategy <- row$split_strategy[1] %||% ""
            filters$dataset_filter_pipeline <- row$pipeline_id[1] %||% ""
            filters$dataset_filter_ann_set <- row$annotation_set_id[1] %||% ""
          }
        }
      }

      for (filter_id in names(dataset_filter_config)) {
        choices <- build_dataset_filter_choices(filter_id, filters = filters)
        selected <- as.character(filters[[filter_id]] %||% dataset_filter_all_value)
        if (!(selected %in% unname(choices))) {
          selected <- dataset_filter_all_value
        }
        filters[[filter_id]] <- selected
        updateSelectInput(session, filter_id, choices = choices, selected = selected)
      }

      filters
    }

    sync_dataset_id_input <- function(selected_dataset_id = NULL) {
      browser_df <- dataset_browser_rv()
      if (is.null(browser_df) || !is.data.frame(browser_df) || nrow(browser_df) == 0) {
        updateSelectInput(session, "dataset_id", choices = c("No datasets found" = ""), selected = "")
        return(invisible(""))
      }

      choices <- setNames(browser_df$dataset_id, paste0(browser_df$dataset_name, " [", browser_df$study, "]"))
      selected <- selected_dataset_id %||% input$dataset_id %||% ""
      if (!nzchar(selected) || !(selected %in% browser_df$dataset_id)) {
        selected <- ""
      }

      updateSelectInput(
        session,
        "dataset_id",
        choices = if (length(choices) > 0) choices else c("No datasets found" = ""),
        selected = selected
      )

      invisible(selected)
    }

    refresh_dataset_browser <- function(selected_dataset_id = NULL, reveal_dataset_id = NULL) {
      ds_list <- list_datasets()
      browser_df <- build_dataset_summary(ds_list)
      dataset_browser_rv(browser_df)
      pending_dataset_selection_rv(selected_dataset_id %||% reveal_dataset_id %||% input$dataset_id %||% NULL)
      pending_dataset_reveal_rv(reveal_dataset_id %||% NULL)

      if (nrow(browser_df) == 0) {
        pending_dataset_selection_rv(NULL)
        pending_dataset_reveal_rv(NULL)
        updateSelectInput(session, "dataset_id", choices = c("No datasets found" = ""), selected = "")
        return(invisible(NULL))
      }

      invisible(browser_df)
    }

    current_cv_suggestion_context_key <- reactive({
      paste(
        normalize_eval_mode(input$ds_evaluation_mode),
        input$ds_split_strategy %||% "random",
        input$ds_study %||% "",
        input$ds_pipeline %||% "",
        input$ds_ann_set %||% "",
        paste(sort(as.character(input$ds_samples %||% character(0))), collapse = ","),
        suppressWarnings(as.integer(input$ds_block_size %||% NA_integer_)),
        suppressWarnings(as.numeric(input$ds_buffer_radius %||% NA_real_)),
        suppressWarnings(as.numeric(input$ds_train_frac %||% NA_real_)),
        suppressWarnings(as.integer(input$ds_seed %||% NA_integer_)),
        sep = "|"
      )
    })

    maybe_apply_cv_fold_suggestion <- function(suggested_folds) {
      suggested_folds <- suppressWarnings(as.integer(suggested_folds))
      if (!is.finite(suggested_folds) || suggested_folds < 2L) return(invisible(FALSE))

      suggestion_key <- paste(current_cv_suggestion_context_key(), suggested_folds, sep = "|")
      if (identical(cv_fold_suggestion_key_rv(), suggestion_key)) {
        return(invisible(FALSE))
      }

      updateNumericInput(session, "ds_cv_folds", value = suggested_folds)
      cv_fold_suggestion_key_rv(suggestion_key)
      invisible(TRUE)
    }

    materialize_preview_source <- function(sample_ids,
                                           pipeline_id,
                                           annotation_set_id,
                                           stage_type = "binned_dataframe") {
      stopifnot(length(sample_ids) > 0)

      all_labels <- vector("list", length(sample_ids))
      all_meta   <- vector("list", length(sample_ids))

      for (i in seq_along(sample_ids)) {
        sid <- sample_ids[i]

        feat_df <- load_artifact_by_pipeline(sid, stage_type, pipeline_id)
        ann_df  <- load_annotation(sid, annotation_set_id)

        merged <- merge(
          feat_df[, c("x", "y"), drop = FALSE],
          ann_df[, c("x", "y", "Class"), drop = FALSE],
          by = c("x", "y"),
          all = FALSE
        )

        if (nrow(merged) == 0) {
          stop("No pixel overlap after joining features and annotations for sample_id=", sid)
        }

        all_labels[[i]] <- merged$Class
        all_meta[[i]] <- data.frame(
          sample_id = sid,
          x = merged$x,
          y = merged$y,
          stringsAsFactors = FALSE
        )
      }

      list(
        y = as.factor(do.call(c, all_labels)),
        meta = do.call(rbind, all_meta)
      )
    }

    make_class_count_wide <- function(labels, groups, all_levels = NULL) {
      labels <- as.factor(labels)
      if (is.null(all_levels)) {
        all_levels <- levels(labels)
      }

      df <- data.frame(
        group = as.character(groups),
        class = as.character(labels),
        stringsAsFactors = FALSE
      )

      out <- as.data.frame.matrix(table(
        factor(df$group),
        factor(df$class, levels = all_levels)
      ))

      out <- cbind(group = rownames(out), out, row.names = NULL, stringsAsFactors = FALSE)
      names(out)[1] <- "Group"
      out
    }

    make_fold_preview_table <- function(cv_idx, y, all_levels = NULL) {
      y <- as.factor(y)
      if (is.null(all_levels)) {
        all_levels <- levels(y)
      }

      all_idx <- seq_along(y)

      fold_rows <- lapply(seq_along(cv_idx$index), function(i) {
        tr_idx <- cv_idx$index[[i]]
        te_idx <- cv_idx$indexOut[[i]]
        excl_idx <- setdiff(all_idx, union(tr_idx, te_idx))

        tr_tab <- table(factor(y[tr_idx], levels = all_levels))
        te_tab <- table(factor(y[te_idx], levels = all_levels))
        excl_tab <- table(factor(y[excl_idx], levels = all_levels))

        row <- data.frame(
          Fold = i,
          Train_n = length(tr_idx),
          Test_n = length(te_idx),
          Excluded_n = length(excl_idx),
          stringsAsFactors = FALSE
        )

        for (cls in all_levels) {
          row[[paste0("Train_", cls)]] <- as.integer(tr_tab[[cls]])
          row[[paste0("Test_", cls)]]  <- as.integer(te_tab[[cls]])
          row[[paste0("Excluded_", cls)]] <- as.integer(excl_tab[[cls]])
        }

        row
      })

      dplyr::bind_rows(fold_rows)
    }

    render_preview_table <- function(df, title_text, subtitle_text = NULL) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      tagList(
        tags$div(
          style = "margin-top:8px; margin-bottom:4px;",
          tags$b(title_text),
          if (!is.null(subtitle_text)) {
            tagList(
              tags$br(),
              tags$small(style = "color:#666;", subtitle_text)
            )
          }
        ),
        tags$div(
          style = "max-height:220px; overflow-y:auto; border:1px solid #ddd; padding:4px; background:#fff;",
          tags$table(
            class = "table table-condensed table-bordered",
            style = "font-size:12px; margin-bottom:0;",
            tags$thead(
              tags$tr(lapply(names(df), tags$th))
            ),
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                tags$tr(lapply(df[i, , drop = FALSE], function(x) tags$td(as.character(x[[1]]))))
              })
            )
          )
        )
      )
    }

    compute_spatial_preview_tables <- function(src, params, evaluation_mode = "cv_plus_test", cv_folds = NA_integer_) {
      req(!is.null(src), length(src$y) > 0, is.data.frame(src$meta))

      block_size <- suppressWarnings(as.integer(params$block_size))
      buffer_radius <- suppressWarnings(as.numeric(params$buffer_radius))
      train_frac <- suppressWarnings(as.numeric(params$train_frac))
      split_seed <- suppressWarnings(as.integer(params$split_seed))
      cv_seed <- suppressWarnings(as.integer(params$cv_seed))
      cv_folds <- suppressWarnings(as.integer(cv_folds))
      evaluation_mode <- normalize_eval_mode(evaluation_mode)

      req(is.finite(block_size), block_size >= 2)
      req(is.finite(buffer_radius), buffer_radius >= 0)
      if (identical(evaluation_mode, "cv_plus_test")) {
        req(is.finite(train_frac), train_frac > 0, train_frac < 1)
      }
      req(is.finite(split_seed))
      req(is.finite(cv_seed))

      y_all <- as.factor(src$y)
      class_levels <- levels(y_all)

      if (identical(evaluation_mode, "cv_only")) {
        tr_idx <- seq_len(nrow(src$meta))
        te_idx <- integer(0)
        excl_idx <- integer(0)
        outer_tbl <- data.frame(
          Group = c("Final train", "Final test"),
          stringsAsFactors = FALSE
        )
        train_tab <- table(factor(y_all[tr_idx], levels = class_levels))
        zero_tab <- stats::setNames(integer(length(class_levels)), class_levels)
        for (cls in class_levels) {
          outer_tbl[[cls]] <- c(as.integer(train_tab[[cls]]), zero_tab[[cls]])
        }
        warning_msg <- NULL
      } else {
        split_idx <- make_outer_split_indices(
          meta = src$meta,
          split_strategy = "spatial_block",
          split_seed = split_seed,
          train_frac = train_frac,
          block_size = block_size,
          buffer_radius = buffer_radius
        )

        tr_idx <- split_idx$train_idx
        te_idx <- split_idx$test_idx
        excl_idx <- setdiff(seq_len(nrow(src$meta)), c(tr_idx, te_idx))

        outer_groups <- rep(NA_character_, nrow(src$meta))
        outer_groups[tr_idx] <- "Final train"
        outer_groups[te_idx] <- "Final test"
        if (length(excl_idx) > 0) {
          outer_groups[excl_idx] <- "Buffer excluded"
        }

        outer_tbl <- make_class_count_wide(
          labels = y_all,
          groups = outer_groups,
          all_levels = class_levels
        )
        warning_msg <- split_idx$split_details$warning_msg %||% NULL
      }

      train_meta_preview <- src$meta[tr_idx, , drop = FALSE]
      train_y_preview <- y_all[tr_idx]

      spatial_rec <- recommend_spatial_params(
        meta = train_meta_preview,
        block_size = block_size,
        merge_frac = 0.60,
        max_cv_folds = 10L
      )

      cv_tbl <- NULL
      recommended_folds <- suppressWarnings(as.integer(spatial_rec$recommended_cv_folds))
      selected_cv_folds <- cv_folds
      effective_cv_folds <- NA_integer_

      if (is.finite(selected_cv_folds) && selected_cv_folds >= 2 && length(tr_idx) > 0) {
        split_info_preview <- list(
          strategy = "spatial_block",
          block_size = block_size,
          buffer_radius = buffer_radius,
          min_pixels_per_block = block_merge_threshold(block_size, frac = 0.60)
        )

        cv_idx <- tryCatch(
          build_cv_indices_from_split(
            train_meta = train_meta_preview,
            split_info = split_info_preview,
            cv_folds = selected_cv_folds,
            seed = cv_seed
          ),
          error = function(e) NULL
        )

        if (!is.null(cv_idx)) {
          effective_cv_folds <- length(cv_idx$index)
          cv_tbl <- make_fold_preview_table(
            cv_idx = cv_idx,
            y = train_y_preview,
            all_levels = class_levels
          )
        }
      }

      list(
        outer_tbl = outer_tbl,
        cv_tbl = cv_tbl,
        selected_cv_folds = selected_cv_folds,
        effective_cv_folds = effective_cv_folds,
        recommended_folds = recommended_folds,
        warning_msg = warning_msg,
        evaluation_mode = evaluation_mode,
        no_test_message = if (identical(evaluation_mode, "cv_only")) {
          "No held-out test set will be created. The final model will train on all available non-excluded pixels."
        } else {
          NULL
        },
        params = list(
          block_size = block_size,
          buffer_radius = buffer_radius,
          train_frac = train_frac,
          split_seed = split_seed,
          cv_seed = cv_seed
        )
      )
    }

    compute_grouped_preview_tables <- function(src, params, evaluation_mode = "cv_plus_test", cv_folds = NA_integer_) {
      req(!is.null(src), length(src$y) > 0, is.data.frame(src$meta))

      train_frac <- suppressWarnings(as.numeric(params$train_frac))
      split_seed <- suppressWarnings(as.integer(params$split_seed))
      cv_seed <- suppressWarnings(as.integer(params$cv_seed))
      cv_folds <- suppressWarnings(as.integer(cv_folds))
      evaluation_mode <- normalize_eval_mode(evaluation_mode)

      if (identical(evaluation_mode, "cv_plus_test")) {
        req(is.finite(train_frac), train_frac > 0, train_frac < 1)
      }
      req(is.finite(split_seed))
      req(is.finite(cv_seed))

      y_all <- as.factor(src$y)
      class_levels <- levels(y_all)

      if (identical(evaluation_mode, "cv_only")) {
        tr_idx <- seq_len(nrow(src$meta))
        te_idx <- integer(0)
        outer_tbl <- data.frame(
          Group = c("Final train", "Final test"),
          stringsAsFactors = FALSE
        )
        train_tab <- table(factor(y_all[tr_idx], levels = class_levels))
        zero_tab <- stats::setNames(integer(length(class_levels)), class_levels)
        for (cls in class_levels) {
          outer_tbl[[cls]] <- c(as.integer(train_tab[[cls]]), zero_tab[[cls]])
        }
      } else {
        split_idx <- make_outer_split_indices(
          meta = src$meta,
          split_strategy = "leave_one_sample_out",
          split_seed = split_seed,
          train_frac = train_frac
        )

        tr_idx <- split_idx$train_idx
        te_idx <- split_idx$test_idx

        outer_groups <- rep(NA_character_, nrow(src$meta))
        outer_groups[tr_idx] <- "Final train"
        outer_groups[te_idx] <- "Final test"

        outer_tbl <- make_class_count_wide(
          labels = y_all,
          groups = outer_groups,
          all_levels = class_levels
        )
      }

      train_meta_preview <- src$meta[tr_idx, , drop = FALSE]
      train_y_preview <- y_all[tr_idx]
      n_training_samples <- length(unique(as.character(train_meta_preview$sample_id)))

      selected_cv_folds <- cv_folds
      suggested_cv_folds <- n_training_samples
      effective_cv_folds <- if (is.finite(selected_cv_folds) && selected_cv_folds >= 2) {
        min(selected_cv_folds, n_training_samples)
      } else {
        NA_integer_
      }

      cv_tbl <- NULL
      if (is.finite(selected_cv_folds) && selected_cv_folds >= 2 && n_training_samples >= 2) {
        cv_idx <- tryCatch(
          make_loso_cv_indices(
            train_meta = train_meta_preview,
            cv_folds = selected_cv_folds,
            seed = cv_seed
          ),
          error = function(e) NULL
        )

        if (!is.null(cv_idx)) {
          effective_cv_folds <- length(cv_idx$index)
          cv_tbl <- make_fold_preview_table(
            cv_idx = cv_idx,
            y = train_y_preview,
            all_levels = class_levels
          )
        }
      }

      list(
        outer_tbl = outer_tbl,
        cv_tbl = cv_tbl,
        selected_cv_folds = selected_cv_folds,
        effective_cv_folds = effective_cv_folds,
        recommended_folds = suggested_cv_folds,
        warning_msg = NULL,
        evaluation_mode = evaluation_mode,
        no_test_message = if (identical(evaluation_mode, "cv_only")) {
          "No held-out test set will be created. The final model will train on all available non-excluded pixels."
        } else {
          NULL
        },
        params = list(
          train_frac = train_frac,
          split_seed = split_seed,
          cv_seed = cv_seed
        )
      )
    }

    load_ds_studies <- function() {
      tryCatch({
        df <- get_studies()
        if (nrow(df) == 0 || !("_id" %in% names(df))) {
          study_map_rv(setNames(character(0), character(0)))
          updateSelectInput(session, "ds_study", choices = c("No studies" = ""))
          return()
        }
        study_map_rv(stats::setNames(as.character(df$name), as.character(df[["_id"]])))
        ch <- setNames(df[["_id"]], df$name)
        updateSelectInput(session, "ds_study", choices = c("— select —" = "", ch))
      }, error = function(e)
        {
          study_map_rv(setNames(character(0), character(0)))
          updateSelectInput(session, "ds_study", choices = c("Error" = ""))
        }
      )
    }

    observeEvent(input$ds_refresh_study, load_ds_studies(), ignoreInit = FALSE)
    session$onFlushed(load_ds_studies, once = TRUE)

    observeEvent(input$ds_study, {
      sid <- input$ds_study
      if (!nzchar(sid %||% "")) return()
      open_sidebar_step("dataset_create")

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
            format_processing_pipeline_label(pid)
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

    observeEvent(input$ds_pipeline, {
      if (nzchar(input$ds_pipeline %||% "")) open_sidebar_step("dataset_create")
    }, ignoreInit = TRUE)

    observeEvent(input$ds_ann_set, {
      if (nzchar(input$ds_ann_set %||% "")) open_sidebar_step("dataset_create")
    }, ignoreInit = TRUE)

    observeEvent(list(input$ds_samples, input$ds_evaluation_mode), {
      n_samples <- length(input$ds_samples %||% character(0))
      evaluation_mode <- normalize_eval_mode(input$ds_evaluation_mode)
      choices <- c(
        "Random" = "random",
        "Spatial block" = "spatial_block"
      )
      min_grouped_samples <- if (identical(evaluation_mode, "cv_only")) 2 else 3
      if (n_samples >= min_grouped_samples) {
        choices <- c(choices, "Grouped sample-out" = "leave_one_sample_out")
      }
      selected <- input$ds_split_strategy
      if (is.null(selected) || !(selected %in% unname(choices))) selected <- unname(choices)[1]
      updateSelectInput(session, "ds_split_strategy", choices = choices, selected = selected)
    }, ignoreInit = FALSE)

    observeEvent(
      list(
        input$ds_study, input$ds_pipeline, input$ds_ann_set,
        input$ds_samples, input$ds_split_strategy, input$ds_evaluation_mode,
        input$spatial_buffer_mode
      ),
      {
        estimate_diag_rv(NULL)
        estimate_diag_label_rv("")
        estimate_params_rv(NULL)
        estimate_in_progress_rv(FALSE)
        preview_src_rv(NULL)
        preview_src_key_rv(NULL)
        preview_result_rv(NULL)
        cv_fold_suggestion_key_rv(NULL)
        output$estimate_spatial_text <- renderUI(NULL)
      },
      ignoreInit = TRUE
    )

    observe({
      strategy <- input$ds_split_strategy %||% "random"
      mode <- input$spatial_buffer_mode %||% "estimate"

      enabled <-
        identical(strategy, "spatial_block") &&
        identical(mode, "estimate") &&
        nzchar(input$ds_study %||% "") &&
        length(input$ds_samples %||% character(0)) > 0 &&
        nzchar(input$ds_pipeline %||% "") &&
        nzchar(input$ds_ann_set %||% "") &&
        !isTRUE(estimate_in_progress_rv())

      shinyjs::toggleState("estimate_spatial_btn", condition = enabled)
    })

    observe({
      strategy <- input$ds_split_strategy %||% "random"
      mode <- input$spatial_buffer_mode %||% "estimate"

      base_ready <-
        strategy %in% c("spatial_block", "leave_one_sample_out") &&
        nzchar(input$ds_study %||% "") &&
        length(input$ds_samples %||% character(0)) > 0 &&
        nzchar(input$ds_pipeline %||% "") &&
        nzchar(input$ds_ann_set %||% "") &&
        !isTRUE(estimate_in_progress_rv())

      estimate_ready <- !is.null(estimate_diag_rv()) && !is.null(estimate_params_rv())

      enabled <- if (identical(strategy, "spatial_block") && identical(mode, "estimate")) {
        base_ready && estimate_ready
      } else {
        base_ready
      }

      shinyjs::toggleState("run_spatial_preview_btn", condition = enabled)
    })

    observe({
      strategy <- input$ds_split_strategy %||% "random"
      label <- if (identical(strategy, "leave_one_sample_out")) {
        "Preview grouped sample-out split"
      } else {
        "Preview spatial split"
      }
      shiny::updateActionButton(session, "run_spatial_preview_btn", label = label)
    })

    observe({
      input$refresh_datasets
      tryCatch({
        refresh_dataset_browser()
      }, error = function(e) {
        dataset_browser_rv(empty_dataset_summary())
        pending_dataset_selection_rv(NULL)
        pending_dataset_reveal_rv(NULL)
        updateSelectInput(session, "dataset_id", choices = c("Error loading datasets" = ""))
      }
      )
    })

    observe({
      dataset_browser_rv()
      input$dataset_filter_study
      input$dataset_filter_eval_mode
      input$dataset_filter_split_strategy
      input$dataset_filter_pipeline
      input$dataset_filter_ann_set

      sync_dataset_browser_filters(reveal_dataset_id = pending_dataset_reveal_rv())
      selected_id <- pending_dataset_selection_rv() %||% pending_dataset_reveal_rv() %||% input$dataset_id %||% ""
      sync_dataset_id_input(selected_dataset_id = selected_id)

      reveal_id <- pending_dataset_reveal_rv()
      if (!is.null(reveal_id) && nzchar(reveal_id)) {
        visible_ids <- filtered_dataset_summary(filters = current_dataset_filter_values())$dataset_id
        if (reveal_id %in% visible_ids) {
          pending_dataset_reveal_rv(NULL)
        }
      }

      requested_id <- pending_dataset_selection_rv()
      if (!is.null(requested_id) && nzchar(requested_id) &&
          identical(input$dataset_id %||% "", requested_id)) {
        pending_dataset_selection_rv(NULL)
      }
    })

    filtered_datasets <- reactive({
      filtered_dataset_summary()
    })

    output$dataset_table <- DT::renderDataTable({
      df <- filtered_datasets()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(message = "No datasets match the current filters."), rownames = FALSE))
      }

      tbl <- df[, c(
        "dataset_id", "dataset_name", "study", "evaluation_mode_label",
        "split_strategy_label", "cv_folds", "n_samples", "pipeline_label",
        "annotation_set_label", "created_at"
      ), drop = FALSE]

      names(tbl) <- c(
        "dataset_id", "Dataset name", "Study", "Evaluation mode",
        "Split strategy", "CV folds", "Number of samples", "Processing pipeline",
        "Annotation set", "Created at"
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
      rid <- input$dataset_id %||% ""
      df <- filtered_datasets()
      if (!nzchar(rid) || is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        DT::selectRows(dataset_table_proxy, NULL)
        return()
      }
      idx <- which(df$dataset_id == rid)
      if (length(idx) == 1) {
        DT::selectRows(dataset_table_proxy, idx)
      } else {
        DT::selectRows(dataset_table_proxy, NULL)
      }
    })

    observeEvent(input$dataset_table_rows_selected, {
      idx <- input$dataset_table_rows_selected
      df <- filtered_datasets()
      if (is.null(idx) || length(idx) == 0 || is.null(df) || nrow(df) == 0) return()
      rid <- df$dataset_id[idx[1]]
      choices <- setNames(dataset_browser_rv()$dataset_id, paste0(dataset_browser_rv()$dataset_name, " [", dataset_browser_rv()$study, "]"))
      updateSelectInput(session, "dataset_id", choices = choices, selected = rid)
    })

    output$dataset_info_ui <- renderUI({
      req(input$dataset_id, nchar(input$dataset_id) > 0)
      tryCatch({
        ds <- get_dataset(input$dataset_id)
        n  <- length(unlist(ds$sample_ids))
        sp <- if (is.data.frame(ds$split)) as.list(ds$split[1,]) else ds$split[[1]]
        eval_mode <- normalize_eval_mode(sp$evaluation_mode)
        is_spatial_split <- identical(sp$strategy, "spatial_block")
        split_label <- if (identical(sp$strategy, "leave_one_sample_out")) {
          "Grouped sample-out"
        } else if (identical(sp$strategy, "spatial_block")) {
          "Spatial block"
        } else {
          sp$strategy %||% "random"
        }
        dataset_name <- as.character(ds$name[1] %||% input$dataset_id)
        study_label <- lookup_study_label(ds$study_id[1] %||% "")
        pipeline_label <- format_processing_pipeline_label(ds$pipeline_id[1] %||% "")
        ann_set_label <- lookup_annotation_set_label(ds$study_id[1] %||% "", ds$annotation_set_id[1] %||% "")
        tagList(
          tags$div(
            style = "margin-bottom:6px;",
            tags$b("Selected dataset: "),
            dataset_name
          ),
          tags$small(
            tags$b("Study: "),    study_label,  tags$br(),
            tags$b("Samples: "),  n,            tags$br(),
            tags$b("Pipeline: "), pipeline_label, tags$br(),
            tags$b("Annotation set: "), ann_set_label, tags$br(),
            tags$b("Stage: "),    ds$stage_type, tags$br(),
            tags$b("Evaluation mode: "), evaluation_mode_label(eval_mode), tags$br(),
            tags$b("Split strategy: "), split_label, tags$br(),
            tags$b("Split: "),
            if (identical(eval_mode, "cv_plus_test") && !is.null(sp$train_frac)) {
              paste0(sp$train_frac * 100, "% train | ")
            } else {
              ""
            },
            "seed=", sp$seed,
            if (!is.null(sp$cv_folds)) tagList(tags$br(), tags$b("CV folds: "), sp$cv_folds),
            if (is_spatial_split && !is.null(sp$block_size)) tagList(tags$br(), tags$b("Block size: "), sp$block_size),
            if (is_spatial_split && !is.null(sp$buffer_radius)) tagList(tags$br(), tags$b("Buffer radius: "), sp$buffer_radius),
            if (is_spatial_split && !is.null(sp$min_pixels_per_block)) tagList(tags$br(), tags$b("Min pixels per merged block: "), sp$min_pixels_per_block),
            if (is_spatial_split && !is.null(sp$diagnostic_method)) tagList(tags$br(), tags$b("Diagnostic: "), sp$diagnostic_method),
            if (is_spatial_split && !is.null(sp$diagnostic_n_features)) tagList(tags$br(), tags$b("Features evaluated: "), sp$diagnostic_n_features),
            if (is_spatial_split && !is.null(sp$diagnostic_n_correlogram_features)) tagList(tags$br(), tags$b("Correlogram features: "), sp$diagnostic_n_correlogram_features),
            if (is_spatial_split && !is.null(sp$diagnostic_recommended_buffer)) tagList(tags$br(), tags$b("Estimated buffer: "), sp$diagnostic_recommended_buffer)
          )
        )
      }, error = function(e)
        tags$small(style = "color:red", "Could not load dataset info.")
      )
    })

    output$spatial_preview_ui <- renderUI({
      strategy <- input$ds_split_strategy %||% "random"
      if (!(strategy %in% c("spatial_block", "leave_one_sample_out"))) return(NULL)

      mode <- input$spatial_buffer_mode %||% "estimate"
      evaluation_mode <- normalize_eval_mode(input$ds_evaluation_mode)

      prev <- preview_result_rv()

      if (isTRUE(estimate_in_progress_rv())) {
        return(
          tags$div(
            class = "alert alert-secondary",
            style = "padding:6px; margin-top:8px;",
            tags$small("Estimating Moran's I + correlogram. Preview is available after estimation finishes.")
          )
        )
      }

      if (is.null(prev)) {
        msg <- if (identical(mode, "estimate")) {
          "Click 'Estimate from Moran's I + correlogram' first, then click 'Preview spatial split'."
        } else {
          "Set parameters and click 'Preview spatial split'."
        }

        return(
          tags$div(
            class = "alert alert-secondary",
            style = "padding:6px; margin-top:8px;",
            tags$small(msg)
          )
        )
      }

      tagList(
        tags$div(
          style = "margin-top:6px; margin-bottom:2px;",
          tags$small(
            style = "color:#666;",
            paste0(
              if (identical(strategy, "spatial_block")) paste0("Preview snapshot — block=", prev$params$block_size, ", buffer=", prev$params$buffer_radius) else "Preview snapshot — grouped sample-out",
              if (identical(evaluation_mode, "cv_plus_test")) paste0(", train_frac=", prev$params$train_frac) else "",
              ", split_seed=", prev$params$split_seed,
              ", cv_seed=", prev$params$cv_seed
            )
          )
        ),
        if (identical(evaluation_mode, "cv_only")) {
          tags$div(
            class = "alert alert-info",
            style = "padding:8px; margin-top:6px;",
            tags$b("CV-only fold preview"),
            tags$br(),
            tags$small("No held-out test set will be created. The final model will train on all available non-excluded pixels.")
          )
        },
        if (!is.null(prev$warning_msg) && nzchar(prev$warning_msg)) {
          tags$div(
            class = "alert alert-warning",
            style = "padding:6px; margin-top:6px;",
            tags$small(prev$warning_msg)
          )
        },
        render_preview_table(
          prev$outer_tbl,
          title_text = if (identical(evaluation_mode, "cv_only")) "CV-only available training pool" else "Final split class counts",
          subtitle_text = if (identical(evaluation_mode, "cv_only")) {
            "Counts per class in the full training pool and a zero-count held-out test row for CV-only mode."
          } else {
            if (identical(strategy, "spatial_block")) {
              "Counts per class in final train, final test, and buffer-excluded pixels."
            } else {
              "Counts per class in final train and final held-out grouped sample-out test split."
            }
          }
        ),
        render_preview_table(
          prev$cv_tbl,
          title_text = if (identical(strategy, "leave_one_sample_out")) "Grouped sample-out fold preview" else if (identical(evaluation_mode, "cv_only")) "CV-only fold preview" else "CV fold class counts",
          subtitle_text = paste0(
            "Selected CV folds input: ", if (is.finite(prev$selected_cv_folds)) prev$selected_cv_folds else "not available",
            " | Effective CV folds used: ", if (is.finite(prev$effective_cv_folds)) prev$effective_cv_folds else "not available",
            " | Suggested CV folds: ",
            if (is.finite(prev$recommended_folds)) prev$recommended_folds else "not available"
          )
        )
      )
    })

    observeEvent(input$estimate_spatial_btn, {
      sid      <- input$ds_study
      samp_ids <- input$ds_samples
      pid      <- input$ds_pipeline
      ann_id   <- input$ds_ann_set
      evaluation_mode <- normalize_eval_mode(input$ds_evaluation_mode)

      estimate_in_progress_rv(TRUE)
      estimate_diag_rv(NULL)
      estimate_diag_label_rv("")
      estimate_params_rv(NULL)
      preview_result_rv(NULL)
      output$estimate_spatial_text <- renderUI(
        tags$div(
          class = "alert alert-info",
          style = "padding:8px; margin-top:4px; margin-bottom:8px;",
          "Estimating Moran's I + correlogram..."
        )
      )
      on.exit(estimate_in_progress_rv(FALSE), add = TRUE)

      if ((input$ds_split_strategy %||% "random") != "spatial_block") {
        showNotification("Choose 'Spatial block' to estimate buffer from Moran's I.", type = "warning")
        return()
      }
      if (!nzchar(sid %||% ""))    { showNotification("Select a study.", type = "warning"); return() }
      if (length(samp_ids) == 0)   { showNotification("Select at least one sample.", type = "warning"); return() }
      if (!nzchar(pid %||% ""))    { showNotification("Select a pipeline.", type = "warning"); return() }
      if (!nzchar(ann_id %||% "")) { showNotification("Select an annotation set.", type = "warning"); return() }

      tryCatch({
        src <- materialize_training_source(
          sample_ids = samp_ids,
          pipeline_id = pid,
          annotation_set_id = ann_id,
          stage_type = "binned_dataframe"
        )

        X_est <- normalize_feature_matrix(src$X, input$normalize %||% "none")

        diag_info <- compute_feature_moran_diagnostics(
          X = X_est,
          meta = src$meta,
          max_points = 800L,
          lag_breaks = NULL,
          max_dist = 100,
          max_pairs_per_bin = 300L,
          local_decay_threshold = 0.2,
          seed = as.integer(input$ds_seed %||% 42L),
          workers = max(1L, min(8L, parallel::detectCores(logical = FALSE) - 1L))
        )

        src_key <- paste(
          sid,
          paste(sort(as.character(samp_ids)), collapse = ","),
          pid,
          ann_id,
          "estimate",
          sep = "|"
        )

        preview_src_rv(list(
          y = src$y,
          meta = src$meta,
          key = src_key
        ))
        preview_src_key_rv(src_key)

        rec_buf <- suppressWarnings(as.numeric(diag_info$recommended_buffer_radius[1]))
        rec_block <- suppressWarnings(as.numeric(diag_info$recommended_block_size[1]))

        if (!is.finite(rec_block) || rec_block < 2) {
          rec_block <- max(4, ceiling(rec_buf))
        }

        if (is.finite(rec_buf) && rec_buf > 0) {
          updateNumericInput(session, "ds_buffer_radius", value = round(rec_buf, 1))
        }

        if (is.finite(rec_block) && rec_block >= 2) {
          updateNumericInput(session, "ds_block_size", value = as.integer(rec_block))
        }

        estimate_params_rv(list(
          block_size = as.integer(rec_block),
          buffer_radius = as.numeric(rec_buf)
        ))

        estimate_diag_rv(diag_info)
        estimate_diag_label_rv(
          paste0(
            length(samp_ids), " sample(s), ",
            nrow(src$X), " annotated pixels, normalize=",
            input$normalize %||% "none"
          )
        )

        train_meta_preview <- if (identical(evaluation_mode, "cv_only")) {
          src$meta
        } else {
          split_idx <- make_outer_split_indices(
            meta = src$meta,
            split_strategy = "spatial_block",
            split_seed = as.integer(input$ds_seed %||% 42L),
            train_frac = as.numeric(input$ds_train_frac),
            block_size = as.integer(rec_block),
            buffer_radius = as.numeric(rec_buf)
          )
          src$meta[split_idx$train_idx, , drop = FALSE]
        }

        spatial_rec <- recommend_spatial_params(
          meta = train_meta_preview,
          block_size = as.integer(rec_block),
          merge_frac = 0.60,
          max_cv_folds = 10L
        )

        rec_folds2 <- suppressWarnings(as.integer(spatial_rec$recommended_cv_folds))

        if (is.finite(spatial_rec$recommended_cv_folds) && spatial_rec$recommended_cv_folds >= 2) {
          maybe_apply_cv_fold_suggestion(spatial_rec$recommended_cv_folds)
        }

        output$estimate_spatial_text <- renderUI({
          moran_tbl <- diag_info$feature_moran_summary
          range_tbl <- diag_info$feature_range_summary
          rec_buf2  <- suppressWarnings(as.numeric(diag_info$recommended_buffer_radius[1]))
          rec_block2 <- suppressWarnings(as.numeric(diag_info$recommended_block_size[1]))

          tagList(
            tags$div(
              class = "alert alert-info",
              style = "padding:8px; margin-top:4px; margin-bottom:8px;",

              tags$b("Suggested buffer radius: "),
              if (is.finite(rec_buf2)) paste0(round(rec_buf2, 2), " px") else "not available",
              tags$br(),

              tags$b("Suggested block size: "),
              if (is.finite(rec_block2)) paste0(as.integer(rec_block2), " px") else "not available",
              tags$br(),

              tags$b("Suggested CV folds: "),
              if (is.finite(rec_folds2)) rec_folds2 else "not available",
              tags$br(),

              tags$small(estimate_diag_label_rv()),

              if (is.data.frame(moran_tbl) && nrow(moran_tbl) > 0) {
                tags$div(
                  style = "margin-top:4px;",
                  tags$small(
                    paste0(
                      "Features evaluated: ", nrow(moran_tbl),
                      " | median Moran's I: ", round(stats::median(moran_tbl$moran_i, na.rm = TRUE), 4)
                    )
                  )
                )
              },

              if (is.data.frame(range_tbl) && nrow(range_tbl) > 0) {
                tags$div(
                  style = "margin-top:4px;",
                  tags$small(
                    paste0(
                      "Features contributing to correlogram/buffer: ", nrow(range_tbl),
                      " | median local decay: ", round(stats::median(range_tbl$range_estimate, na.rm = TRUE), 2), " px",
                      " | 75% local decay: ", round(stats::quantile(range_tbl$range_estimate, 0.75, na.rm = TRUE), 2), " px"
                    )
                  )
                )
              }
            )
          )
        })
      }, error = function(e) {
        estimate_diag_rv(NULL)
        estimate_diag_label_rv("")
        estimate_params_rv(NULL)
        preview_src_rv(NULL)
        preview_src_key_rv(NULL)
        preview_result_rv(NULL)
        output$estimate_spatial_text <- renderUI(
          tags$div(class = "alert alert-danger", style = "padding:8px; margin-top:4px;",
            tags$b("Estimate failed: "), conditionMessage(e))
        )
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    observeEvent(input$run_spatial_preview_btn, {
      sid      <- input$ds_study
      samp_ids <- input$ds_samples
      pid      <- input$ds_pipeline
      ann_id   <- input$ds_ann_set
      strategy <- input$ds_split_strategy %||% "random"
      mode     <- input$spatial_buffer_mode %||% "estimate"
      evaluation_mode <- normalize_eval_mode(input$ds_evaluation_mode)
      cv_folds <- suppressWarnings(as.integer(input$ds_cv_folds %||% NA_integer_))

      if (!(strategy %in% c("spatial_block", "leave_one_sample_out"))) {
        showNotification("Choose 'Spatial block' or 'Grouped sample-out' to preview the dataset split.", type = "warning")
        return()
      }
      if (!nzchar(sid %||% ""))    { showNotification("Select a study.", type = "warning"); return() }
      if (length(samp_ids) == 0)    { showNotification("Select at least one sample.", type = "warning"); return() }
      if (!nzchar(pid %||% ""))    { showNotification("Select a pipeline.", type = "warning"); return() }
      if (!nzchar(ann_id %||% "")) { showNotification("Select an annotation set.", type = "warning"); return() }

      if (identical(strategy, "spatial_block") && identical(mode, "estimate") &&
          (is.null(estimate_diag_rv()) || is.null(estimate_params_rv()))) {
        showNotification("Run Moran estimate first in estimate mode.", type = "warning")
        return()
      }

      params <- list(
        block_size = suppressWarnings(as.integer(input$ds_block_size)),
        buffer_radius = suppressWarnings(as.numeric(input$ds_buffer_radius)),
        train_frac = suppressWarnings(as.numeric(input$ds_train_frac)),
        split_seed = suppressWarnings(as.integer(input$ds_seed)),
        cv_seed = suppressWarnings(as.integer(input$seed %||% 1234L))
      )

      src_key <- paste(
        sid,
        paste(sort(as.character(samp_ids)), collapse = ","),
        pid,
        ann_id,
        mode,
        sep = "|"
      )

      tryCatch({
        src <- preview_src_rv()

        if (identical(mode, "manual") || is.null(src) || !identical(preview_src_key_rv(), src_key)) {
          src <- materialize_preview_source(
            sample_ids = samp_ids,
            pipeline_id = pid,
            annotation_set_id = ann_id,
            stage_type = "binned_dataframe"
          )
          src$key <- src_key
          preview_src_rv(src)
          preview_src_key_rv(src_key)
        }

        prev <- if (identical(strategy, "leave_one_sample_out")) {
          compute_grouped_preview_tables(
            src = src,
            params = params,
            evaluation_mode = evaluation_mode,
            cv_folds = cv_folds
          )
        } else {
          compute_spatial_preview_tables(
            src = src,
            params = params,
            evaluation_mode = evaluation_mode,
            cv_folds = cv_folds
          )
        }
        maybe_apply_cv_fold_suggestion(prev$recommended_folds)
        preview_result_rv(prev)
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

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
        evaluation_mode <- normalize_eval_mode(input$ds_evaluation_mode)
        cv_folds <- as.integer(input$ds_cv_folds %||% 0L)
        split_obj <- list(
          strategy = split_strategy,
          seed = as.integer(input$ds_seed),
          cv_folds = cv_folds,
          evaluation_mode = evaluation_mode
        )
        if (identical(evaluation_mode, "cv_only") && cv_folds <= 1L) {
          showNotification("CV-only mode requires CV folds greater than 1.", type = "warning")
          return()
        }
        if (identical(evaluation_mode, "cv_plus_test") && split_strategy != "leave_one_sample_out") {
          split_obj$train_frac <- as.numeric(input$ds_train_frac)
        }
        if (split_strategy == "spatial_block") {
          split_obj$block_size <- as.integer(input$ds_block_size)
          split_obj$buffer_radius <- as.numeric(input$ds_buffer_radius)
          split_obj$min_pixels_per_block <- block_merge_threshold(
            as.integer(input$ds_block_size),
            frac = 0.60
          )

          diag_info <- estimate_diag_rv()
          if (!is.null(diag_info)) {
            split_obj$diagnostic_method <- "sampled_moran_correlogram"

            moran_tbl <- diag_info$feature_moran_summary
            range_tbl <- diag_info$feature_range_summary

            split_obj$diagnostic_n_features <- if (is.data.frame(moran_tbl)) nrow(moran_tbl) else NA_integer_
            split_obj$diagnostic_n_correlogram_features <- if (is.data.frame(range_tbl)) nrow(range_tbl) else NA_integer_
            split_obj$diagnostic_recommended_buffer <- as.numeric(diag_info$recommended_buffer_radius)
          }
        }

        min_grouped_samples <- if (identical(evaluation_mode, "cv_only")) 2 else 3
        if (split_strategy == "leave_one_sample_out" && length(samp_ids) < min_grouped_samples) {
          showNotification(
            paste0("Grouped sample-out requires at least ", min_grouped_samples, " samples in this evaluation mode."),
            type = "warning"
          )
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
            tags$small(nm)
          )
        )
        showNotification(paste0("Dataset created: ", nm), type = "message")
        open_sidebar_step("dataset_select")
        refresh_dataset_browser(selected_dataset_id = dataset_id, reveal_dataset_id = dataset_id)
      }, error = function(e) {
        output$create_dataset_status <- renderUI(
          tags$div(class = "alert alert-danger", style = "padding:6px; margin-top:6px;",
            tags$b("Error: "), conditionMessage(e))
        )
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    observeEvent(input$dataset_id, {
      req(input$dataset_id, nzchar(input$dataset_id))
      tryCatch({
        runs_rv(list_model_runs(input$dataset_id))
        selected_run_id(NULL)
        open_sidebar_step("training_run")
      }, error = function(e) {
        runs_rv(data.frame())
        selected_run_id(NULL)
      })
    }, ignoreInit = TRUE)

    output$training_log <- renderText({ log_val() })

    output$estimate_moran_plot <- renderPlot({
      diag_info <- estimate_diag_rv()
      req(!is.null(diag_info))

      corr_df <- diag_info$feature_correlogram
      req(is.data.frame(corr_df), nrow(corr_df) > 0)

      range_df <- diag_info$feature_range_summary
      if (!is.data.frame(range_df)) {
        range_df <- data.frame(feature = character(0), range_estimate = numeric(0))
      }

      rec_buf <- suppressWarnings(as.numeric(diag_info$recommended_buffer_radius[1]))

      p <- ggplot2::ggplot(
        corr_df,
        ggplot2::aes(x = distance_mid, y = moran_i, group = feature, color = feature)
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        ggplot2::geom_line(linewidth = 0.2, alpha = 0.55) +
        ggplot2::labs(
          title = "Sampled Moran correlogram",
          subtitle = "All features shown; fixed pair sampling per lag bin; suggested buffer based on first local Moran decay",
          x = "Pixel distance",
          y = "Moran's I"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13),
          plot.subtitle = ggplot2::element_text(size = 10),
          legend.position = "none"
        )

      if (nrow(range_df) > 0) {
        p <- p + ggplot2::geom_vline(
          data = range_df,
          ggplot2::aes(xintercept = range_estimate, group = feature),
          inherit.aes = FALSE,
          linetype = "dotted",
          alpha = 0.08,
          color = "grey30"
        )
      }

      if (is.finite(rec_buf) && rec_buf > 0) {
        p <- p +
          ggplot2::geom_vline(xintercept = rec_buf, linetype = "longdash", alpha = 0.9, linewidth = 0.7) +
          ggplot2::annotate(
            "text",
            x = rec_buf,
            y = max(corr_df$moran_i, na.rm = TRUE),
            label = paste0("Suggested ≈ ", round(rec_buf, 1), " px"),
            vjust = -0.4,
            hjust = 0,
            size = 3.6
          )
      }

      p
    })

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

    observeEvent(input$run_training, {
      dataset_id <- input$dataset_id
      if (is.null(dataset_id) || !nzchar(dataset_id)) {
        showNotification("Select a dataset before training.", type = "warning"); return()
      }
      log_val("")
      add_log(paste0("Starting training on dataset: ", lookup_dataset_label(dataset_id)))
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
          seed              = input$seed
        )
        last_run_id(run_id)
        add_log("\u2713 Training complete.")
        showNotification("Training complete.", type = "message")
      }, error = function(e) {
        add_log(paste0("\u2717 Error: ", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 15)
      })
    })

    output$run_table <- DT::renderDataTable({
      df <- runs_rv()
      if (nrow(df) == 0 || !("_id" %in% names(df))) {
        return(DT::datatable(data.frame(message = "No runs yet."), rownames = FALSE))
      }

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

      eval_mode_display <- function(x) {
        evaluation_mode_label(x)
      }

      df_sorted <- df[order(df$created_at, decreasing = TRUE), , drop = FALSE]
      n <- nrow(df_sorted)

      hp_obj <- df_sorted$hyperparams
      m_obj  <- df_sorted$metrics

      if (is.data.frame(hp_obj)) {
        norm  <- as.character(hp_obj$normalize_method %||% NA)
        mtry  <- as.character(hp_obj$mtry %||% NA)
        trees <- as.character(hp_obj$num_trees %||% NA)
        node  <- as.character(hp_obj$min_node_size %||% NA)
        rule  <- as.character(hp_obj$splitrule %||% NA)
        cv    <- as.character(hp_obj$cv_folds %||% NA)
        eval_mode <- vapply(hp_obj$evaluation_mode %||% rep("cv_plus_test", n), eval_mode_display, character(1))
      } else {
        norm  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "normalize_method"), character(1))
        mtry  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "mtry"), character(1))
        trees <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "num_trees"), character(1))
        node  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "min_node_size"), character(1))
        rule  <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "splitrule"), character(1))
        cv    <- vapply(seq_len(n), \(i) hp_get(hp_obj[[i]], "cv_folds"), character(1))
        eval_mode <- vapply(seq_len(n), \(i) eval_mode_display(hp_get(hp_obj[[i]], "evaluation_mode")), character(1))
      }

      if (is.data.frame(m_obj)) {
        test_acc   <- suppressWarnings(as.numeric(m_obj$test_accuracy %||% NA))
        test_kappa <- suppressWarnings(as.numeric(m_obj$test_kappa %||% NA))
        cv_acc     <- suppressWarnings(as.numeric(m_obj$cv_mean_accuracy %||% NA))
      } else {
        test_acc   <- vapply(seq_len(n), \(i) m_get(m_obj[[i]], "test_accuracy"), numeric(1))
        test_kappa <- vapply(seq_len(n), \(i) m_get(m_obj[[i]], "test_kappa"), numeric(1))
        cv_acc     <- vapply(seq_len(n), \(i) m_get(m_obj[[i]], "cv_mean_accuracy"), numeric(1))
      }

      primary_score <- ifelse(grepl("^CV only$", eval_mode), cv_acc, test_acc)

      tbl <- data.frame(
        run_id_full = df_sorted[["_id"]],
        model_type  = df_sorted$model_type,
        evaluation  = eval_mode,
        normalisation = norm,
        mtry        = mtry,
        trees       = trees,
        node        = node,
        rule        = rule,
        cv          = cv,
        primary_score = primary_score,
        test_acc    = test_acc,
        test_kappa  = test_kappa,
        cv_acc      = cv_acc,
        created_at  = df_sorted$created_at,
        stringsAsFactors = FALSE
      )

      names(tbl) <- c(
        "run_id_full", "Model type", "Evaluation mode", "Normalisation",
        "mtry", "Trees", "Min node size", "Split rule", "CV folds",
        "Primary score", "Test accuracy", "Test kappa", "CV mean accuracy", "Created at"
      )

      DT::datatable(
        tbl,
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))
        ),
        class = "compact stripe hover"
      ) |>
        DT::formatRound(c("primary_score", "test_acc", "test_kappa", "cv_acc"), digits = 4)
    })

    observeEvent(input$run_table_rows_selected, {
      idx <- input$run_table_rows_selected
      if (is.null(idx) || length(idx) == 0) {
        selected_run_id(NULL)
        return()
      }

      df <- runs_rv()
      if (nrow(df) == 0) { selected_run_id(NULL); return() }

      df_sorted <- df[order(df$created_at, decreasing = TRUE), , drop = FALSE]
      rid <- df_sorted[["_id"]][idx]
      selected_run_id(rid)
    })

    observeEvent(last_run_id(), {
      rid <- last_run_id()
      if (!is.null(rid) && nzchar(rid)) selected_run_id(rid)
    })

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

    unwrap_nested_metric <- function(x) {
      obj <- x
      repeat {
        if (is.data.frame(obj) || is.null(obj) || !is.list(obj) || length(obj) != 1) break
        obj <- obj[[1]]
      }
      obj
    }

    get_run_payload <- function(rid) {
      row <- get_model_run(rid)
      if (is.null(row) || nrow(row) == 0) return(NULL)

      m <- extract_subdoc(row, "metrics")
      hp <- extract_subdoc(row, "hyperparams")

      if (is.data.frame(m)) m <- as.list(m[1, , drop = FALSE])
      if (is.data.frame(hp)) hp <- as.list(hp[1, , drop = FALSE])
      if (is.null(m)) m <- list()
      if (is.null(hp)) hp <- list()
      if (!is.list(m)) m <- list(value = m)
      if (!is.list(hp)) hp <- list(value = hp)

      list(row = row, metrics = m, hyperparams = hp)
    }

    extract_cm_df_from_metrics <- function(metrics, key) {
      cm_obj <- unwrap_nested_metric(metrics[[key]])
      if (is.null(cm_obj) || !is.data.frame(cm_obj) || nrow(cm_obj) == 0) return(NULL)

      cm_df <- cm_obj
      if (!"Rel_Freq" %in% names(cm_df) && all(c("Reference", "Freq") %in% names(cm_df))) {
        cm_df <- cm_df |>
          dplyr::group_by(Reference) |>
          dplyr::mutate(Rel_Freq = Freq / sum(Freq)) |>
          dplyr::ungroup()
      }
      cm_df
    }

    extract_roc_df_from_metrics <- function(metrics, key) {
      roc_raw <- metrics[[key]]
      if (is.null(roc_raw)) return(NULL)

      roc_list <- unwrap_nested_metric(roc_raw)

      if (is.data.frame(roc_list)) {
        roc_entries <- lapply(seq_len(nrow(roc_list)), function(i) {
          list(
            class = as.character(roc_list$class[i]),
            auc = suppressWarnings(as.numeric(roc_list$auc[i])),
            sensitivities = unlist(roc_list$sensitivities[[i]]),
            specificities = unlist(roc_list$specificities[[i]])
          )
        })
      } else if (is.list(roc_list)) {
        roc_entries <- lapply(roc_list, function(r) {
          r <- unwrap_nested_metric(r)
          if (is.data.frame(r)) {
            return(list(
              class = as.character(r$class[1]),
              auc = suppressWarnings(as.numeric(r$auc[1])),
              sensitivities = unlist(r$sensitivities[[1]]),
              specificities = unlist(r$specificities[[1]])
            ))
          }
          if (is.list(r)) {
            return(list(
              class = as.character(r$class[1]),
              auc = suppressWarnings(as.numeric(r$auc[1])),
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

      roc_rows <- lapply(roc_entries, function(r) {
        if (length(r$sensitivities) == 0 || length(r$specificities) == 0) return(NULL)
        n <- min(length(r$sensitivities), length(r$specificities))
        data.frame(
          class = r$class,
          fpr = 1 - as.numeric(r$specificities[seq_len(n)]),
          tpr = as.numeric(r$sensitivities[seq_len(n)]),
          auc = as.numeric(r$auc),
          stringsAsFactors = FALSE
        )
      })

      roc_df <- dplyr::bind_rows(Filter(Negate(is.null), roc_rows))
      if (nrow(roc_df) == 0) return(NULL)

      auc_labels <- roc_df |>
        dplyr::group_by(class) |>
        dplyr::summarise(auc = dplyr::first(auc), .groups = "drop") |>
        dplyr::mutate(label = sprintf("%s (AUC = %.3f)", class, auc))

      dplyr::left_join(roc_df, auc_labels[, c("class", "label")], by = "class")
    }

    extract_byclass_df_from_metrics <- function(metrics, key = "byclass_table") {
      byclass_obj <- unwrap_nested_metric(metrics[[key]])
      if (is.null(byclass_obj) || !is.data.frame(byclass_obj) || nrow(byclass_obj) == 0) return(NULL)
      if (!"Class" %in% names(byclass_obj)) return(NULL)

      out <- byclass_obj
      out$Class <- as.character(out$Class)
      numeric_cols <- setdiff(names(out), "Class")
      for (col in numeric_cols) {
        out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
      }
      out
    }

    extract_importance_df_from_metrics <- function(metrics, key = "permutation_importance") {
      imp_obj <- unwrap_nested_metric(metrics[[key]])
      if (is.null(imp_obj) || !is.data.frame(imp_obj) || nrow(imp_obj) == 0) return(NULL)
      if (!all(c("feature", "importance") %in% names(imp_obj))) return(NULL)

      imp_df <- imp_obj[, c("feature", "importance"), drop = FALSE]
      imp_df$feature <- as.character(imp_df$feature)
      imp_df$importance <- suppressWarnings(as.numeric(imp_df$importance))
      imp_df <- imp_df[order(imp_df$importance, decreasing = TRUE), , drop = FALSE]
      rownames(imp_df) <- NULL
      imp_df
    }

    build_cm_plot <- function(cm_df) {
      cm_df$Text_Color <- ifelse(cm_df$Rel_Freq > 0.5, "white", "black")

      ggplot2::ggplot(cm_df, ggplot2::aes(x = Prediction, y = Reference, fill = Rel_Freq)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.3f", Rel_Freq), color = Text_Color),
          size = 5, fontface = "bold", show.legend = FALSE
        ) +
        ggplot2::scale_color_identity() +
        ggplot2::scale_fill_gradient(low = "white", high = "navy", name = "Relative\nFreq") +
        ggplot2::labs(x = "Predicted", y = "Actual") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(face = "bold", size = 11),
          axis.text.y = ggplot2::element_text(face = "bold", size = 11),
          panel.grid = ggplot2::element_blank()
        )
    }

    build_importance_plot <- function(imp_df) {
      top_df <- imp_df[is.finite(imp_df$importance), , drop = FALSE]
      top_df <- head(top_df, 10)
      if (nrow(top_df) == 0) return(NULL)

      top_df$feature <- factor(top_df$feature, levels = rev(top_df$feature))

      ggplot2::ggplot(top_df, ggplot2::aes(x = importance, y = feature)) +
        ggplot2::geom_col(fill = "#2c7fb8") +
        ggplot2::labs(
          title = "Top 10 Features by Permutation Importance",
          x = "Permutation importance",
          y = "Feature"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          axis.text.y = ggplot2::element_text(size = 11)
        )
    }

    build_roc_plot <- function(roc_df) {
      n_classes <- length(unique(roc_df$class))
      legend_nrow <- ceiling(n_classes / 3)

      ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr, color = label)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
        ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
        ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate", color = NULL) +
        ggplot2::guides(color = ggplot2::guide_legend(nrow = legend_nrow, byrow = TRUE)) +
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
    }

    output$run_details_ui <- renderUI({
      rid <- selected_run_id()
      if (is.null(rid) || !nzchar(rid)) {
        return(tags$p(style = "color:#888", "Click a run to see details."))
      }

      payload <- get_run_payload(rid)
      if (is.null(payload)) {
        return(tags$p(style = "color:#c00", "Could not load run from DB. Try refresh."))
      }

      row <- payload$row
      m <- payload$metrics
      hp <- payload$hyperparams

      first_chr <- function(x, default = "—") {
        if (is.null(x) || length(x) == 0) return(default)
        as.character(x[1])
      }
      split_label <- function(x) {
        if (is.null(x) || length(x) == 0) return("—")
        val <- as.character(x[1])
        if (identical(val, "leave_one_sample_out")) "Grouped sample-out" else val
      }
      is_spatial_strategy <- function(x) {
        identical(first_chr(x, ""), "spatial_block")
      }
      first_num <- function(x) {
        if (is.null(x) || length(x) == 0) return(NA_real_)
        suppressWarnings(as.numeric(x[1]))
      }
      fmt_num <- function(x, digits = 4) {
        v <- first_num(x)
        if (!is.finite(v)) return(tags$span(style = "color:#aaa", "—"))
        tags$b(round(v, digits))
      }

      evaluation_mode <- normalize_eval_mode(hp[["evaluation_mode"]] %||% m[["evaluation_mode"]])
      show_spatial_rows <- is_spatial_strategy(hp[["split_strategy"]])
      has_test_set <- {
        flag <- first_chr(m[["has_test_set"]], "")
        if (nzchar(flag)) {
          tolower(flag) %in% c("true", "1")
        } else {
          is.finite(first_num(m[["test_accuracy"]])) || !is.null(extract_cm_df_from_metrics(m, "cm_table"))
        }
      }

      lo <- first_num(m[["test_acc_lower"]])
      hi <- first_num(m[["test_acc_upper"]])

      cv_cm_df <- extract_cm_df_from_metrics(m, "cv_cm_table")
      cv_roc_df <- extract_roc_df_from_metrics(m, "cv_roc_data")
      test_cm_df <- extract_cm_df_from_metrics(m, "cm_table")
      test_roc_df <- extract_roc_df_from_metrics(m, "roc_data")
      byclass_df <- extract_byclass_df_from_metrics(m)
      importance_df <- extract_importance_df_from_metrics(m)
      importance_message <- first_chr(m[["permutation_importance_message"]], "")
      cv_warning <- first_chr(m[["cv_warning"]], "")

      bc_keys <- grep("^byclass_Sensitivity__", names(m), value = TRUE)
      class_names <- gsub("^byclass_Sensitivity__", "", bc_keys)

      perclass_tbl <- if (has_test_set && !is.null(byclass_df) && nrow(byclass_df) > 0) {
        metric_value <- function(row, candidates) {
          hit <- candidates[candidates %in% names(row)][1]
          if (is.na(hit) || !nzchar(hit)) return("—")
          val <- suppressWarnings(as.numeric(row[[hit]][1]))
          if (!is.finite(val)) "—" else sprintf("%.4f", val)
        }

        tags$table(
          class = "table table-condensed table-bordered",
          style = "font-size:13px; margin-top:8px;",
          tags$thead(tags$tr(
            tags$th("Class"),
            tags$th("Sensitivity"), tags$th("Specificity"),
            tags$th("Precision"), tags$th("Recall"),
            tags$th("F1"), tags$th("Bal. Accuracy")
          )),
          tags$tbody(lapply(seq_len(nrow(byclass_df)), function(i) {
            row_i <- byclass_df[i, , drop = FALSE]
            tags$tr(
              tags$td(tags$b(as.character(row_i$Class[1]))),
              tags$td(metric_value(row_i, c("Sensitivity"))),
              tags$td(metric_value(row_i, c("Specificity"))),
              tags$td(metric_value(row_i, c("Precision", "Pos Pred Value"))),
              tags$td(metric_value(row_i, c("Recall", "Sensitivity"))),
              tags$td(metric_value(row_i, c("F1"))),
              tags$td(metric_value(row_i, c("Balanced Accuracy")))
            )
          }))
        )
      } else if (has_test_set && length(class_names) > 0) {
        safe_key <- function(metric, cls) {
          key <- paste0("byclass_", metric, "__", cls)
          num <- first_num(m[[key]])
          if (!is.finite(num)) "—" else sprintf("%.4f", num)
        }
        tags$table(
          class = "table table-condensed table-bordered",
          style = "font-size:13px; margin-top:8px;",
          tags$thead(tags$tr(
            tags$th("Class"),
            tags$th("Sensitivity"), tags$th("Specificity"),
            tags$th("Precision"), tags$th("Recall"),
            tags$th("F1"), tags$th("Bal. Accuracy")
          )),
          tags$tbody(lapply(class_names, function(cls) {
            tags$tr(
              tags$td(tags$b(gsub("_", " ", cls))),
              tags$td(safe_key("Sensitivity", cls)),
              tags$td(safe_key("Specificity", cls)),
              tags$td(safe_key("Precision", cls)),
              tags$td(safe_key("Recall", cls)),
              tags$td(safe_key("F1", cls)),
              tags$td(safe_key("Balanced_Accuracy", cls))
            )
          }))
        )
      } else if (has_test_set) {
        tags$p(style = "color:#aaa", "No per-class held-out test metrics stored.")
      } else {
        NULL
      }

      cv_section <- tagList(
        tags$h5("Cross-validation evaluation"),
        if (!is.null(m[["cv_mean_accuracy"]])) {
          tags$table(class = "table table-condensed", style = "font-size:13px;",
            tags$tr(tags$td("CV accuracy"), tags$td(fmt_num(m[["cv_mean_accuracy"]]))),
            tags$tr(tags$td("CV accuracy SD"), tags$td(fmt_num(m[["cv_acc_sd"]]))),
            tags$tr(tags$td("CV kappa"), tags$td(fmt_num(m[["cv_mean_kappa"]]))),
            tags$tr(tags$td("CV mean F1"), tags$td(fmt_num(m[["cv_mean_f1"]]))),
            tags$tr(tags$td("CV macro F1 from predictions"), tags$td(fmt_num(m[["cv_macro_f1_from_predictions"]]))),
            tags$tr(tags$td("CV predictions"), tags$td(first_chr(m[["n_cv_predictions"]])))
          )
        } else {
          tags$p(style = "color:#aaa", "No CV scalar metrics stored.")
        },
        if (nzchar(cv_warning)) {
          tags$div(class = "alert alert-warning", style = "padding:8px; margin-top:8px;", cv_warning)
        } else if (is.null(cv_cm_df) && is.null(cv_roc_df) && !is.null(m[["cv_mean_accuracy"]])) {
          tags$div(
            class = "alert alert-secondary",
            style = "padding:8px; margin-top:8px;",
            "CV confusion matrix / ROC were not stored for this run."
          )
        },
        if (!is.null(cv_cm_df)) {
          tagList(
            tags$h6("CV confusion matrix"),
            plotOutput(ns("cv_cm_plot"), height = "300px")
          )
        },
        if (!is.null(cv_roc_df)) {
          tagList(
            tags$h6("CV ROC curves"),
            plotOutput(ns("cv_roc_plot"), height = "650px")
          )
        }
      )

      test_section <- if (has_test_set) {
        tagList(
          tags$h5("Held-out test evaluation"),
          tags$table(class = "table table-condensed", style = "font-size:13px;",
            tags$tr(tags$td("Accuracy"), tags$td(fmt_num(m[["test_accuracy"]]))),
            tags$tr(tags$td("95% CI"), tags$td({
              if (is.finite(lo) && is.finite(hi)) paste0("[", round(lo, 4), ", ", round(hi, 4), "]") else "—"
            })),
            tags$tr(tags$td("Kappa"), tags$td(fmt_num(m[["test_kappa"]]))),
            tags$tr(tags$td("Test pixels"), tags$td(first_chr(m[["n_test"]])))
          ),
          tags$h6("Per-class metrics"),
          perclass_tbl,
          if (!is.null(test_cm_df)) {
            tagList(
              tags$h6("Held-out confusion matrix"),
              plotOutput(ns("test_cm_plot"), height = "300px")
            )
          },
          if (!is.null(test_roc_df)) {
            tagList(
              tags$h6("Held-out ROC curves"),
              plotOutput(ns("test_roc_plot"), height = "650px")
            )
          }
        )
      } else {
        tagList(
          tags$h5("Held-out test evaluation"),
          tags$div(
            class = "alert alert-info",
            style = "padding:8px; margin-top:8px;",
            "No held-out test set was used for this run."
          )
        )
      }

      importance_section <- tagList(
        tags$h5("Permutation feature importance"),
        if (!is.null(importance_df) && nrow(importance_df) > 0 && any(is.finite(importance_df$importance))) {
          tagList(
            plotOutput(ns("permutation_importance_plot"), height = "420px"),
            tags$h6("All feature importances"),
            DT::DTOutput(ns("permutation_importance_table"))
          )
        } else {
          tags$div(
            class = "alert alert-info",
            style = "padding:8px; margin-top:8px;",
            if (nzchar(importance_message)) importance_message else "Permutation importance is unavailable for this run."
          )
        }
      )

      tagList(
        tags$div(
          style = "background:#f8f9fa; border:1px solid #dee2e6; border-radius:6px; padding:14px; margin-bottom:12px;",
          tags$h5(style = "margin-top:0", tags$code(rid)),
          fluidRow(
            column(4,
              tags$h6(tags$b("Run setup")),
              tags$table(class = "table table-condensed", style = "font-size:13px;",
                tags$tr(tags$td("Model"), tags$td(first_chr(row$model_type))),
                tags$tr(tags$td("Evaluation mode"), tags$td(evaluation_mode_label(evaluation_mode))),
                tags$tr(tags$td("Normalisation"), tags$td(first_chr(hp[["normalize_method"]]))),
                tags$tr(tags$td("mtry"), tags$td(first_chr(hp[["mtry"]]))),
                tags$tr(tags$td("num.trees"), tags$td(first_chr(hp[["num_trees"]]))),
                tags$tr(tags$td("min.node.size"), tags$td(first_chr(hp[["min_node_size"]]))),
                tags$tr(tags$td("splitrule"), tags$td(first_chr(hp[["splitrule"]])))
              )
            ),
            column(4,
              tags$h6(tags$b("Split / CV setup")),
              tags$table(class = "table table-condensed", style = "font-size:13px;",
                tags$tr(tags$td("CV folds"), tags$td(first_chr(hp[["cv_folds"]]))),
                tags$tr(tags$td("Seed"), tags$td(first_chr(hp[["seed"]]))),
                tags$tr(tags$td("Split strategy"), tags$td(split_label(hp[["split_strategy"]]))),
                if (show_spatial_rows) tags$tr(tags$td("Block size"), tags$td(first_chr(hp[["split_block_size"]]))),
                if (show_spatial_rows) tags$tr(tags$td("Buffer radius"), tags$td(first_chr(hp[["split_buffer_radius"]]))),
                if (show_spatial_rows) tags$tr(tags$td("Min pixels per merged block"), tags$td(first_chr(hp[["split_min_pixels_per_block"]])))
              )
            ),
            column(4,
              tags$h6(tags$b("Dataset size")),
              tags$table(class = "table table-condensed", style = "font-size:13px;",
                tags$tr(tags$td("Train pixels"), tags$td(first_chr(m[["n_train"]]))),
                tags$tr(tags$td("Test pixels"), tags$td(first_chr(m[["n_test"]]))),
                tags$tr(tags$td("Classes"), tags$td(first_chr(m[["n_classes"]]))),
                tags$tr(tags$td("Features"), tags$td(first_chr(m[["n_features"]]))),
                tags$tr(tags$td("Primary score"), tags$td(
                  if (identical(evaluation_mode, "cv_only")) fmt_num(m[["cv_mean_accuracy"]]) else fmt_num(m[["test_accuracy"]])
                ))
              )
            )
          )
        ),
        tags$div(style = "max-width:1100px; margin:auto;", cv_section, tags$hr(), test_section, tags$hr(), importance_section)
      )
    })

    output$cv_cm_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      payload <- get_run_payload(rid)
      req(!is.null(payload))
      cm_df <- extract_cm_df_from_metrics(payload$metrics, "cv_cm_table")
      req(!is.null(cm_df), nrow(cm_df) > 0)
      build_cm_plot(cm_df)
    })

    output$test_cm_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      payload <- get_run_payload(rid)
      req(!is.null(payload))
      cm_df <- extract_cm_df_from_metrics(payload$metrics, "cm_table")
      req(!is.null(cm_df), nrow(cm_df) > 0)
      build_cm_plot(cm_df)
    })

    output$cv_roc_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      payload <- get_run_payload(rid)
      req(!is.null(payload))
      roc_df <- extract_roc_df_from_metrics(payload$metrics, "cv_roc_data")
      req(!is.null(roc_df), nrow(roc_df) > 0)
      build_roc_plot(roc_df)
    })

    output$test_roc_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      payload <- get_run_payload(rid)
      req(!is.null(payload))
      roc_df <- extract_roc_df_from_metrics(payload$metrics, "roc_data")
      req(!is.null(roc_df), nrow(roc_df) > 0)
      build_roc_plot(roc_df)
    })

    output$permutation_importance_plot <- renderPlot({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      payload <- get_run_payload(rid)
      req(!is.null(payload))
      imp_df <- extract_importance_df_from_metrics(payload$metrics)
      req(!is.null(imp_df), nrow(imp_df) > 0, any(is.finite(imp_df$importance)))
      build_importance_plot(imp_df)
    })

    output$permutation_importance_table <- DT::renderDT({
      rid <- selected_run_id()
      req(rid, nzchar(rid))
      payload <- get_run_payload(rid)
      req(!is.null(payload))
      imp_df <- extract_importance_df_from_metrics(payload$metrics)

      if (is.null(imp_df) || nrow(imp_df) == 0) {
        return(
          DT::datatable(
            data.frame(message = "Permutation importance is unavailable for this run."),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }

      DT::datatable(
        imp_df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        class = "compact stripe hover"
      )
    })
  })
}
