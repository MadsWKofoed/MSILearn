# R/modules/prediction_module.R

prediction_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Prediction",
    fluidRow(
      column(
        3,
        wellPanel(
          h4("1. Select Model Run"),
          actionButton(ns("refresh_runs"), "Refresh model runs", class = "btn-sm btn-default"),
          br(), br(),
          selectInput(
            ns("run_id"),
            "Trained model:",
            choices = c("(loading...)" = ""),
            width = "100%"
          ),
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
        4,
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

    observeEvent(c(TRUE, input$refresh_runs), {
      tryCatch({
        df <- list_all_model_runs()

        if (is.null(df) || nrow(df) == 0) {
          updateSelectInput(
            session, "run_id",
            choices = c("No model runs found" = "")
          )
          return()
        }

        df <- df[order(df$created_at, decreasing = TRUE), , drop = FALSE]

        labels <- vapply(seq_len(nrow(df)), function(i) {
          rid <- as.character(df$`_id`[i])
          did <- as.character(df$dataset_id[i])
          created <- as.character(df$created_at[i])
          paste0(substr(rid, 1, 18), "… | dataset=", substr(did, 1, 12), "… | ", created)
        }, character(1))

        updateSelectInput(
          session,
          "run_id",
          choices = c("— select —" = "", stats::setNames(df$`_id`, labels))
        )
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error")
      })
    }, ignoreInit = FALSE)

    selected_context <- reactive({
      req(input$run_id, nzchar(input$run_id))
      get_prediction_context(input$run_id)
    })

    output$run_info_ui <- renderUI({
      ctx <- selected_context()
      hp  <- ctx$hyperparams

      tags$div(
        tags$small(
          tags$b("Run ID: "), input$run_id, tags$br(),
          tags$b("Dataset ID: "), ctx$dataset_id, tags$br(),
          tags$b("Pipeline ID: "), ctx$pipeline_id, tags$br(),
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