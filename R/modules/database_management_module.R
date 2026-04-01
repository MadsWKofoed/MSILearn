# R/modules/database_management_module.R

database_management_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Database Management",
    fluidRow(
      column(
        3,
        tags$style(HTML(" 
          .dbm-card{
            border: 1px solid #e5e7eb;
            border-radius: 14px;
            margin-bottom: 14px;
            background: #ffffff;
            overflow: hidden;
            box-shadow: 0 2px 10px rgba(0,0,0,0.04);
          }
          .dbm-card-head{
            padding: 12px 14px;
            font-weight: 700;
            font-size: 15px;
            background: #f8fafc;
            border-bottom: 1px solid #eef2f7;
            display: flex;
            align-items: center;
            justify-content: space-between;
            gap: 10px;
          }
          .dbm-card-body{
            padding: 12px 14px 14px 14px;
          }
          .dbm-lead{
            font-size: 13px;
            color: #4b5563;
            margin-bottom: 10px;
            line-height: 1.45;
          }
          .dbm-subtitle{
            font-size: 12px;
            font-weight: 700;
            color: #374151;
            margin-top: 10px;
            margin-bottom: 6px;
            text-transform: uppercase;
            letter-spacing: 0.02em;
          }
          .dbm-helper{
            background: #f8fbff;
            border: 1px solid #dbeafe;
            border-radius: 10px;
            padding: 10px 12px;
            font-size: 12px;
            color: #334155;
            line-height: 1.5;
            margin-bottom: 10px;
          }
          .dbm-helper strong{ color:#1e3a8a; }
          .dbm-session-box{
            background: linear-gradient(135deg, #f8fbff 0%, #fdfdff 100%);
            border: 1px solid #dbeafe;
            border-radius: 12px;
            padding: 10px 12px;
            margin-bottom: 14px;
            box-shadow: 0 1px 6px rgba(37,99,235,0.05);
          }
          .dbm-session-title{
            font-size: 12px;
            font-weight: 700;
            color: #3730a3;
            text-transform: uppercase;
            letter-spacing: 0.03em;
            margin-bottom: 6px;
          }
          .dbm-session-box table{ width:100%; font-size:12px; margin-bottom:0; }
          .dbm-session-box td{ padding:2px 0; vertical-align:top; }
          .dbm-session-box td:first-child{ color:#64748b; width:42%; }
          .dbm-session-box td:last-child{ color:#111827; font-weight:500; word-break:break-word; }
          .dbm-btn-block{ width:100%; margin-bottom:8px; }
          .dbm-danger-note{
            color:#991b1b; font-size:12px; line-height:1.45; margin-top:4px;
          }
        ")),

        uiOutput(ns("dbm_session_ui")),

        tags$div(
          class = "dbm-card",
          tags$div(class = "dbm-card-head", "Filters & browser"),
          tags$div(
            class = "dbm-card-body",
            tags$div(
              class = "dbm-lead",
              "Browse database collections, inspect records, and remove selected objects when needed."
            ),
            actionButton(ns("refresh_all"), "↺ Refresh everything", class = "btn btn-default btn-sm dbm-btn-block"),
            div(class = "dbm-subtitle", "Collection"),
            selectInput(ns("collection"), "Collection", choices = setNames(dbm_catalog()$key, dbm_catalog()$label), width = "100%"),
            div(class = "dbm-subtitle", "Optional filters"),
            uiOutput(ns("filter_ui")),
            checkboxInput(ns("show_raw_json"), "Show raw JSON in details", value = FALSE),
            tags$div(
              class = "dbm-helper",
              tags$strong("How to use this page: "),
              "First choose a collection. Then use the filters only to narrow the table. ",
              "To inspect or delete something, click a row in the Records table below."
            )
          )
        ),

        tags$div(
          class = "dbm-card",
          tags$div(class = "dbm-card-head", "Selected record"),
          tags$div(
            class = "dbm-card-body",
            uiOutput(ns("selected_summary_ui")),
            tags$div(
              id = ns("delete_btn_wrap"),
              style = "display:none;",
              actionButton(
                ns("delete_selected"),
                "Delete selected record",
                class = "btn btn-danger btn-sm dbm-btn-block"
              )
            ),
            tags$div(
              class = "dbm-danger-note",
              "Deletion is permanent. Cascading cleanup is applied for dependent records such as artifacts, annotations, datasets, and model runs."
            )
          )
        )
      ),

      column(
        9,
        tags$div(
          class = "dbm-card",
          tags$div(class = "dbm-card-head", "Overview"),
          tags$div(
            class = "dbm-card-body",
            tags$p(class = "dbm-lead", "Collection sizes in the current MongoDB database."),
            DT::DTOutput(ns("counts_table"))
          )
        ),

        tags$div(
          class = "dbm-card",
          tags$div(class = "dbm-card-head", "Records"),
          tags$div(
            class = "dbm-card-body",
            tags$div(
              class = "dbm-helper",
              tags$strong("Important: "),
              "The filters do not choose what gets deleted. ",
              "To inspect or delete a record, click directly on a row in the Records table."
            ),
            DT::DTOutput(ns("records_table"))
          )
        ),

        tags$div(
          class = "dbm-card",
          tags$div(class = "dbm-card-head", "Details"),
          tags$div(
            class = "dbm-card-body",
            uiOutput(ns("details_ui"))
          )
        )
      )
    )
  )
}


database_management_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    counts_rv <- reactiveVal(data.frame())
    records_raw_rv <- reactiveVal(data.frame())
    records_display_rv <- reactiveVal(data.frame())
    selected_id_rv <- reactiveVal(NULL)
    selected_record_rv <- reactiveVal(NULL)

    load_studies_for_filter <- function(selected = NULL) {
      df <- tryCatch(get_studies(), error = function(e) data.frame())
      if (nrow(df) == 0 || !all(c("_id", "name") %in% names(df))) {
        updateSelectInput(session, "study_filter", choices = c("All studies" = ""), selected = "")
      } else {
        ch <- c("All studies" = "", setNames(as.character(df$`_id`), as.character(df$name)))
        updateSelectInput(session, "study_filter", choices = ch, selected = selected %||% "")
      }
    }

    load_samples_for_filter <- function(study_id = NULL, selected = NULL) {
      df <- tryCatch({
        if (!is.null(study_id) && nzchar(study_id)) get_samples(study_id) else dbm_safe_find("samples")
      }, error = function(e) data.frame())

      if (nrow(df) == 0 || !all(c("_id", "sample_name") %in% names(df))) {
        updateSelectInput(session, "sample_filter", choices = c("All samples" = ""), selected = "")
      } else {
        ch <- c("All samples" = "", setNames(as.character(df$`_id`), as.character(df$sample_name)))
        updateSelectInput(session, "sample_filter", choices = ch, selected = selected %||% "")
      }
    }

    refresh_counts <- function() {
      counts_rv(dbm_collection_counts())
    }

    refresh_records <- function(collection = NULL, study_id = NULL, sample_id = NULL) {
      if (is.null(collection)) {
        collection <- isolate(input$collection %||% dbm_catalog()$key[1])
      }
      if (is.null(study_id)) {
        study_id <- isolate(input$study_filter %||% NULL)
      }
      if (is.null(sample_id)) {
        sample_id <- isolate(input$sample_filter %||% NULL)
      }

      raw <- dbm_get_collection_data(
        collection = collection,
        study_id = study_id,
        sample_id = sample_id
      )
      display <- dbm_prepare_display(raw, collection)
      records_raw_rv(raw)
      records_display_rv(display)
      selected_id_rv(NULL)
      selected_record_rv(NULL)
    }

    observeEvent(input$refresh_all, {
      refresh_counts()
      load_studies_for_filter(input$study_filter %||% "")
      load_samples_for_filter(input$study_filter %||% NULL, input$sample_filter %||% "")
      refresh_records()
      showNotification("Database view refreshed.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$study_filter, {
      load_samples_for_filter(input$study_filter %||% NULL)
      refresh_records()
    }, ignoreInit = TRUE)

    observeEvent(input$sample_filter, {
      refresh_records()
    }, ignoreInit = TRUE)

    observeEvent(input$collection, {
      refresh_records()
    }, ignoreInit = TRUE)

    session$onFlushed(function() {
      refresh_counts()
      load_studies_for_filter()
      load_samples_for_filter()
      refresh_records(
        collection = dbm_catalog()$key[1],
        study_id = NULL,
        sample_id = NULL
      )
    }, once = TRUE)

    observeEvent(input$records_table_rows_selected, {
      idx <- input$records_table_rows_selected
      df_disp <- records_display_rv()
      df_raw  <- records_raw_rv()

      if (is.null(idx) || length(idx) == 0 || nrow(df_disp) == 0) {
        selected_id_rv(NULL)
        selected_record_rv(NULL)
        return()
      }

      i <- idx[1]
      rid <- as.character(df_disp$id[i])
      selected_id_rv(rid)

      if (!is.null(df_raw) && nrow(df_raw) >= i) {
        selected_record_rv(df_raw[i, , drop = FALSE])
      } else {
        selected_record_rv(dbm_fetch_record(input$collection, rid))
      }
    }, ignoreInit = TRUE)

    output$dbm_session_ui <- renderUI({
      counts <- counts_rv()
      total_records <- if (nrow(counts) > 0) sum(counts$records, na.rm = TRUE) else 0
      tags$div(
        class = "dbm-session-box",
        tags$div(class = "dbm-session-title", "Database session"),
        tags$table(
          tags$tr(tags$td("Database"), tags$td(DB_NAME)),
          tags$tr(tags$td("Mongo URL"), tags$td(MONGO_URL)),
          tags$tr(tags$td("Collections tracked"), tags$td(nrow(dbm_catalog()))),
          tags$tr(tags$td("Total records"), tags$td(total_records)),
          tags$tr(tags$td("Selected collection"), tags$td(dbm_catalog()$label[match(input$collection, dbm_catalog()$key)] %||% "—"))
        )
      )
    })

    output$filter_ui <- renderUI({
      coll <- input$collection %||% "studies"

      if (coll %in% c("studies", "pipelines", "model_runs", "datasets")) {
        return(
          tagList(
            if (coll == "datasets") {
              selectInput(ns("study_filter"), "Study", choices = c("All studies" = ""), width = "100%")
            },
            if (coll %in% c("studies", "pipelines", "model_runs")) {
              tags$div(class = "mini-note", style = "font-size:12px; color:#6b7280;",
                      "No extra filters are needed for this collection.")
            }
          )
        )
      }

      if (coll %in% c("samples", "annotation_sets")) {
        return(
          tagList(
            selectInput(ns("study_filter"), "Study", choices = c("All studies" = ""), width = "100%")
          )
        )
      }

      tagList(
        selectInput(ns("study_filter"), "Study", choices = c("All studies" = ""), width = "100%"),
        selectInput(ns("sample_filter"), "Sample", choices = c("All samples" = ""), width = "100%")
      )
    })

    output$selected_summary_ui <- renderUI({
      rec <- selected_record_rv()

      if (is.null(rec) || nrow(rec) == 0) {
        return(
          tags$div(
            class = "dbm-helper",
            tags$strong("No record selected."), tags$br(),
            "Click a row in the Records table to inspect it and enable deletion."
          )
        )
      }

      title <- dbm_record_title(input$collection, rec)
      rid <- selected_id_rv() %||% "—"
      created <- if ("created_at" %in% names(rec)) as.character(rec$created_at[1]) else "—"

      extra_note <- NULL
      if (identical(input$collection, "studies")) {
        extra_note <- tags$div(
          style = "margin-top:8px; color:#991b1b;",
          tags$strong("Warning: "),
          "Deleting a study also removes dependent samples, artifacts, annotations, datasets, model runs, and related metadata."
        )
      }

      tags$div(
        class = "dbm-helper",
        tags$strong("Selected for deletion/inspection"), tags$br(),
        tags$strong(title), tags$br(),
        tags$strong("ID: "), rid, tags$br(),
        tags$strong("Created: "), created,
        extra_note
      )
    })

    observe({
      rec <- selected_record_rv()
      coll <- input$collection %||% ""

      label <- if (identical(coll, "studies")) {
        "Delete selected study"
      } else {
        "Delete selected record"
      }

      updateActionButton(session, "delete_selected", label = label)

      if (is.null(rec) || nrow(rec) == 0) {
        shinyjs::hide("delete_btn_wrap")
      } else {
        shinyjs::show("delete_btn_wrap")
      }
    })

    output$counts_table <- DT::renderDT({
      df <- counts_rv()
      if (nrow(df) == 0) df <- data.frame(collection = "No data", records = 0, description = "", stringsAsFactors = FALSE)
      DT::datatable(
        df[, c("collection", "records", "description"), drop = FALSE],
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(pageLength = 15, dom = "t", ordering = TRUE, scrollX = TRUE)
      )
    }, server = FALSE)

    output$records_table <- DT::renderDT({
      df <- records_display_rv()
      if (nrow(df) == 0) {
        df <- data.frame(message = "No records for the current filters.", stringsAsFactors = FALSE)
      }
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        class = "compact stripe hover",
        options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE)
      )
    }, server = FALSE)

    output$details_ui <- renderUI({
      rec <- selected_record_rv()
      if (is.null(rec) || nrow(rec) == 0) {
        return(tags$p(style = "color:#6b7280;", "Select a record in the table to inspect it."))
      }

      rec_list <- as.list(rec[1, , drop = FALSE])
      pretty <- jsonlite::prettify(jsonlite::toJSON(rec_list, auto_unbox = TRUE, null = "null", pretty = TRUE))

      if (isTRUE(input$show_raw_json)) {
        return(tags$pre(style = "max-height:500px; overflow:auto; background:#f8fafc; border:1px solid #e5e7eb; padding:12px; border-radius:10px;", pretty))
      }

      kv <- lapply(names(rec_list), function(nm) {
        val <- rec_list[[nm]]
        val_txt <- if (is.list(val) || is.data.frame(val)) {
          paste0(substr(jsonlite::toJSON(val, auto_unbox = TRUE), 1, 160), if (nchar(jsonlite::toJSON(val, auto_unbox = TRUE)) > 160) "…" else "")
        } else {
          as.character(dbm_null_to_na(val))
        }
        tags$tr(tags$td(tags$b(nm)), tags$td(val_txt %||% "NA"))
      })

      tags$table(class = "table table-condensed table-bordered", kv)
    })


    observeEvent(input$delete_selected, {
      rec <- selected_record_rv()
      rid <- selected_id_rv()
      req(!is.null(rec), !is.null(rid), nzchar(rid))

      extra_warning <- NULL
      if (identical(input$collection, "studies")) {
        extra_warning <- tags$div(
          class = "alert alert-danger",
          style = "margin-top:10px; margin-bottom:10px;",
          tags$b("Study deletion is cascading."),
          tags$br(),
          "This will also remove dependent samples, artifacts, annotations, datasets, model runs, and related metadata."
        )
      }

      showModal(modalDialog(
        title = "Confirm deletion",
        tags$p(dbm_record_title(input$collection, rec)),
        tags$p(tags$b("This operation cannot be undone.")),
        extra_warning,
        textInput(ns("confirm_delete_text"), "Type DELETE to confirm", value = ""),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_btn"), "Delete permanently", class = "btn-danger")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_delete_btn, {
      if (!identical(trimws(input$confirm_delete_text %||% ""), "DELETE")) {
        showNotification("Type DELETE exactly to confirm deletion.", type = "warning", duration = 5)
        return()
      }

      rid <- selected_id_rv()
      collection <- input$collection
      removeModal()

      tryCatch({
        report <- dbm_delete_record(collection, rid)
        msg <- dbm_delete_report_text(report)

        refresh_counts()
        load_studies_for_filter(input$study_filter %||% "")
        load_samples_for_filter(input$study_filter %||% NULL, input$sample_filter %||% "")
        refresh_records()

        showNotification(paste("Deleted.", msg), type = "message", duration = 10)
      }, error = function(e) {
        showNotification(paste("Delete failed:", conditionMessage(e)), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)
  })
}
