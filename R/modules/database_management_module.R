# R/modules/database_management_module.R

database_management_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Database Management",
    tags$div(
      class = "dbm-shell",
      tags$style(HTML("
        .dbm-shell{
          --dbm-ink:#14213d;
          --dbm-muted:#5b6472;
          --dbm-border:#d8e0ea;
          --dbm-soft:#f4f7fb;
          --dbm-panel:#ffffff;
          --dbm-accent:#0f766e;
          --dbm-accent-2:#f59e0b;
          --dbm-danger:#b91c1c;
          --dbm-shadow:0 16px 40px rgba(15, 23, 42, 0.08);
          padding: 12px 6px 28px 6px;
        }
        .dbm-hero{
          background:
            radial-gradient(circle at top left, rgba(15,118,110,0.18), transparent 38%),
            radial-gradient(circle at top right, rgba(245,158,11,0.14), transparent 28%),
            linear-gradient(135deg, #fcfdff 0%, #eef6f6 100%);
          border: 1px solid #d9ece9;
          border-radius: 24px;
          padding: 24px 26px;
          margin-bottom: 18px;
          box-shadow: var(--dbm-shadow);
          display:flex;
          align-items:flex-start;
          justify-content:space-between;
          gap:20px;
          flex-wrap:wrap;
        }
        .dbm-hero h3{
          margin:0 0 8px 0;
          font-size:30px;
          letter-spacing:-0.03em;
          color:var(--dbm-ink);
        }
        .dbm-hero p{
          margin:0;
          max-width:760px;
          color:var(--dbm-muted);
          line-height:1.6;
          font-size:14px;
        }
        .dbm-hero-actions .btn{
          border-radius:999px;
          padding:10px 18px;
          font-weight:700;
          box-shadow:0 8px 18px rgba(15,118,110,0.16);
        }
        .dbm-grid-gap{ margin-bottom:18px; }
        .dbm-panel{
          background:var(--dbm-panel);
          border:1px solid var(--dbm-border);
          border-radius:22px;
          box-shadow:var(--dbm-shadow);
          overflow:hidden;
        }
        .dbm-panel-browser,
        .dbm-panel-browser .dbm-panel-body{
          overflow:visible;
        }
        .dbm-panel-head{
          padding:16px 18px 12px 18px;
          border-bottom:1px solid #e6edf5;
          display:flex;
          align-items:flex-start;
          justify-content:space-between;
          gap:12px;
          flex-wrap:wrap;
          background:linear-gradient(180deg, #ffffff 0%, #fbfcfe 100%);
        }
        .dbm-panel-title{
          font-size:16px;
          font-weight:800;
          color:var(--dbm-ink);
          margin:0;
        }
        .dbm-panel-subtitle{
          font-size:12px;
          color:var(--dbm-muted);
          margin-top:4px;
          line-height:1.5;
          max-width:720px;
        }
        .dbm-panel-body{ padding:16px 18px 18px 18px; }
        .dbm-metric-grid{
          display:grid;
          grid-template-columns:repeat(4, minmax(0, 1fr));
          gap:14px;
          margin-bottom:18px;
        }
        .dbm-metric{
          border-radius:20px;
          padding:16px 18px;
          border:1px solid #d9e6e6;
          background:
            linear-gradient(180deg, rgba(255,255,255,0.96) 0%, rgba(247,250,252,0.96) 100%);
          min-height:120px;
          position:relative;
          overflow:hidden;
        }
        .dbm-metric:before{
          content:'';
          position:absolute;
          top:-36px;
          right:-24px;
          width:96px;
          height:96px;
          border-radius:999px;
          background:rgba(15,118,110,0.08);
        }
        .dbm-metric-kicker{
          text-transform:uppercase;
          letter-spacing:0.08em;
          font-size:11px;
          font-weight:800;
          color:#58706c;
          margin-bottom:10px;
        }
        .dbm-metric-value{
          font-size:34px;
          line-height:1;
          font-weight:900;
          color:var(--dbm-ink);
          letter-spacing:-0.04em;
          margin-bottom:8px;
        }
        .dbm-metric-note{
          font-size:12px;
          color:var(--dbm-muted);
          line-height:1.5;
        }
        .dbm-mini-grid{
          display:grid;
          grid-template-columns:repeat(2, minmax(0, 1fr));
          gap:14px;
        }
        .dbm-chip{
          display:inline-flex;
          align-items:center;
          gap:8px;
          background:#f3f7fb;
          border:1px solid #dde6ee;
          border-radius:999px;
          padding:7px 12px;
          font-size:12px;
          color:#435063;
          margin-right:8px;
          margin-bottom:8px;
        }
        .dbm-chip strong{ color:var(--dbm-ink); }
        .dbm-helper{
          background:linear-gradient(180deg, #f7fafc 0%, #f4f9f8 100%);
          border:1px solid #dfe8ee;
          border-radius:16px;
          padding:12px 14px;
          font-size:12px;
          color:#435063;
          line-height:1.6;
        }
        .dbm-helper strong{ color:var(--dbm-ink); }
        .dbm-stack > * + *{ margin-top:12px; }
        .dbm-label{
          font-size:11px;
          font-weight:800;
          letter-spacing:0.08em;
          text-transform:uppercase;
          color:#617080;
          margin:0 0 7px 0;
        }
        .dbm-record-toolbar{
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:12px;
          flex-wrap:wrap;
          margin-bottom:12px;
        }
        .dbm-record-meta{
          font-size:12px;
          color:var(--dbm-muted);
        }
        .dbm-selected{
          border-radius:18px;
          border:1px solid #d7e4ea;
          background:#fcfdff;
          padding:14px 16px;
        }
        .dbm-selected h4{
          margin:0 0 8px 0;
          color:var(--dbm-ink);
          font-size:16px;
        }
        .dbm-selected p{
          margin:0 0 4px 0;
          color:var(--dbm-muted);
          font-size:12px;
          line-height:1.55;
        }
        .dbm-danger-note{
          color:var(--dbm-danger);
          font-size:12px;
          line-height:1.55;
        }
        .dbm-filter-hidden{
          display:none;
        }
        .dbm-panel-browser .selectize-control{
          margin-bottom:0;
        }
        .dbm-panel-browser .selectize-dropdown{
          z-index:5000;
          border-radius:16px;
          border:1px solid #d8e0ea;
          box-shadow:0 18px 42px rgba(15, 23, 42, 0.16);
        }
        .dbm-panel-browser .selectize-dropdown-content{
          max-height:280px;
        }
        .dbm-safe-note{
          color:#0f766e;
          font-size:12px;
          line-height:1.55;
        }
        .dbm-kv{
          width:100%;
          border-collapse:separate;
          border-spacing:0 8px;
          font-size:12px;
        }
        .dbm-kv td{
          vertical-align:top;
          padding:0;
        }
        .dbm-kv td:first-child{
          width:38%;
          color:#6b7280;
          font-weight:700;
          padding-right:12px;
        }
        .dbm-kv td:last-child{
          color:var(--dbm-ink);
          word-break:break-word;
        }
        .dbm-plot-wrap{
          background:#fbfcfe;
          border:1px solid #e4ebf2;
          border-radius:16px;
          padding:10px;
        }
        .dbm-quiet{
          color:var(--dbm-muted);
          font-size:12px;
        }
        .dbm-actions .btn{
          width:100%;
          border-radius:14px;
          font-weight:700;
          padding:10px 14px;
        }
        .dbm-table-wrap .dataTables_wrapper .dataTables_length,
        .dbm-table-wrap .dataTables_wrapper .dataTables_filter{
          margin-bottom:10px;
        }
        @media (max-width: 1200px){
          .dbm-metric-grid{ grid-template-columns:repeat(2, minmax(0, 1fr)); }
        }
        @media (max-width: 768px){
          .dbm-metric-grid,
          .dbm-mini-grid{ grid-template-columns:1fr; }
          .dbm-hero h3{ font-size:24px; }
        }
      ")),
      fluidRow(
        column(
          12,
          tags$div(
            class = "dbm-hero",
            tags$div(
              tags$h3("Database Console"),
              tags$p(
                "Browse, inspect, and safely clean up the main database objects used across processing, clustering, annotation, training, prediction, and alignment. ",
                "Built-in alignment references stay protected, while uploaded and user-created objects can be removed with explicit confirmation."
              )
            ),
            tags$div(
              class = "dbm-hero-actions",
              actionButton(ns("refresh_all"), "Refresh database view", class = "btn btn-primary")
            )
          )
        )
      ),
      fluidRow(
        column(12, uiOutput(ns("overview_cards_ui")))
      ),
      fluidRow(
        column(
          7,
          tags$div(
            class = "dbm-panel dbm-grid-gap",
            tags$div(
              class = "dbm-panel-head",
              tags$div(
                tags$div(class = "dbm-panel-title", "Database Overview"),
                tags$div(
                  class = "dbm-panel-subtitle",
                  "Track collection sizes and quickly spot where most stored objects live."
                )
              )
            ),
            tags$div(
              class = "dbm-panel-body",
              div(class = "dbm-plot-wrap", plotOutput(ns("overview_counts_plot"), height = "300px")),
              tags$div(style = "margin-top:14px;", class = "dbm-table-wrap", DT::DTOutput(ns("counts_table")))
            )
          )
        ),
        column(
          5,
          tags$div(
            class = "dbm-panel dbm-grid-gap",
            tags$div(
              class = "dbm-panel-head",
              tags$div(
                tags$div(class = "dbm-panel-title", "Database Composition"),
                tags$div(
                  class = "dbm-panel-subtitle",
                  "A high-level view of where tracked objects are concentrated across the app database."
                )
              )
            ),
            tags$div(
              class = "dbm-panel-body dbm-stack",
              div(class = "dbm-plot-wrap", plotOutput(ns("composition_plot"), height = "260px")),
              uiOutput(ns("dbm_session_ui"))
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          tags$div(
            class = "dbm-panel dbm-grid-gap dbm-panel-browser",
            tags$div(
              class = "dbm-panel-head",
              tags$div(
                tags$div(class = "dbm-panel-title", "Collection Browser"),
                tags$div(
                  class = "dbm-panel-subtitle",
                  "Choose an object type, optionally narrow the results, and then inspect records from the table."
                )
              )
            ),
            tags$div(
              class = "dbm-panel-body dbm-stack",
              tags$div(
                class = "dbm-helper",
                tags$strong("Workflow"),
                tags$br(),
                "1. Pick a collection.",
                tags$br(),
                "2. Use filters only to narrow the record list.",
                tags$br(),
                "3. Click a row in the table to inspect it or delete it."
              ),
              tags$div(
                tags$div(class = "dbm-label", "Collection"),
                selectizeInput(
                  ns("collection"),
                  NULL,
                  choices = setNames(dbm_catalog()$key, dbm_catalog()$label),
                  width = "100%",
                  options = list(
                    dropdownParent = "body",
                    maxOptions = 100,
                    placeholder = "Choose a collection"
                  )
                )
              ),
              uiOutput(ns("collection_summary_ui")),
              tags$div(
                tags$div(class = "dbm-label", "Filters"),
                uiOutput(ns("filter_ui"))
              )
            )
          ),
          tags$div(
            class = "dbm-panel dbm-grid-gap",
            tags$div(
              class = "dbm-panel-head",
              tags$div(
                tags$div(class = "dbm-panel-title", "Selected Object"),
                tags$div(
                  class = "dbm-panel-subtitle",
                  "Deletion only applies to the currently selected row. Built-in alignment references are protected."
                )
              )
            ),
            tags$div(
              class = "dbm-panel-body dbm-stack",
              uiOutput(ns("selected_summary_ui")),
              tags$div(
                id = ns("delete_btn_wrap"),
                class = "dbm-actions",
                style = "display:none;",
                actionButton(ns("delete_selected"), "Delete selected record", class = "btn btn-danger")
              ),
              tags$div(
                class = "dbm-danger-note",
                "Deletion is permanent. Cascading cleanup is applied where records have dependent datasets, Pipeline Outputs, NDPI images, annotations, model runs, or related metadata."
              ),
              uiOutput(ns("delete_report_ui"))
            )
          )
        ),
        column(
          8,
          tags$div(
            class = "dbm-panel dbm-grid-gap",
            tags$div(
              class = "dbm-panel-head",
              tags$div(
                tags$div(class = "dbm-panel-title", "Records"),
                tags$div(class = "dbm-panel-subtitle", uiOutput(ns("records_meta_ui")))
              )
            ),
            tags$div(
              class = "dbm-panel-body",
              tags$div(class = "dbm-record-toolbar",
                tags$div(class = "dbm-helper", style = "margin-bottom:0; flex:1 1 340px;",
                  tags$strong("Safety reminder"),
                  tags$br(),
                  "Filtering changes which rows are visible, not what is selected for deletion. Always click the exact record you want to inspect or remove."
                )
              ),
              tags$div(class = "dbm-table-wrap", DT::DTOutput(ns("records_table")))
            )
          ),
          tags$div(
            class = "dbm-panel",
            tags$div(
              class = "dbm-panel-head",
              tags$div(
                tags$div(class = "dbm-panel-title", "Details"),
                tags$div(
                  class = "dbm-panel-subtitle",
                  "Use the structured view for quick inspection or raw JSON for the original stored payload."
                )
              ),
              checkboxInput(ns("show_raw_json"), "Raw JSON", value = FALSE)
            ),
            tags$div(
              class = "dbm-panel-body",
              uiOutput(ns("details_ui"))
            )
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
    overview_rv <- reactiveVal(NULL)
    records_raw_rv <- reactiveVal(data.frame())
    records_display_rv <- reactiveVal(data.frame())
    selected_id_rv <- reactiveVal(NULL)
    selected_record_rv <- reactiveVal(NULL)
    delete_plan_rv <- reactiveVal(NULL)
    last_delete_report_rv <- reactiveVal(NULL)

    dbm_selectize_options <- function(placeholder) {
      list(
        dropdownParent = "body",
        maxOptions = 500,
        placeholder = placeholder,
        selectOnTab = TRUE,
        closeAfterSelect = TRUE
      )
    }

    build_single_filter_choices <- function(ids, labels, all_label) {
      ids <- as.character(ids %||% character(0))
      labels <- as.character(labels %||% character(0))
      keep <- nzchar(ids) & !duplicated(ids)
      ids <- ids[keep]
      labels <- labels[keep]

      if (length(ids) == 0) {
        return(stats::setNames("", all_label))
      }

      labels[is.na(labels) | !nzchar(labels)] <- ids[is.na(labels) | !nzchar(labels)]
      ord <- order(tolower(labels), tolower(ids))
      c(stats::setNames("", all_label), stats::setNames(ids[ord], labels[ord]))
    }

    sync_filter_inputs <- function(collection = NULL,
                                   selected_study = NULL,
                                   selected_sample = NULL) {
      if (is.null(collection) || !nzchar(collection)) {
        collection <- dbm_catalog()$key[1]
      }
      selected_study <- as.character(selected_study %||% "")
      selected_sample <- as.character(selected_sample %||% "")

      use_study <- dbm_supports_study_filter(collection)
      use_sample <- dbm_supports_sample_filter(collection)
      filter_index <- dbm_filter_index(collection)

      study_choices <- build_single_filter_choices(
        filter_index$study_id,
        filter_index$study_label,
        "All studies"
      )
      if (!use_study || !(selected_study %in% unname(study_choices))) {
        selected_study <- ""
      }
      updateSelectizeInput(
        session,
        "study_filter",
        choices = study_choices,
        selected = selected_study,
        options = dbm_selectize_options("All studies"),
        server = TRUE
      )

      sample_index <- filter_index
      if (use_sample && nzchar(selected_study)) {
        sample_index <- sample_index[sample_index$study_id == selected_study, , drop = FALSE]
      }
      sample_choices <- build_single_filter_choices(
        sample_index$sample_id,
        sample_index$sample_label,
        "All samples"
      )
      if (!use_sample || !(selected_sample %in% unname(sample_choices))) {
        selected_sample <- ""
      }
      updateSelectizeInput(
        session,
        "sample_filter",
        choices = sample_choices,
        selected = selected_sample,
        options = dbm_selectize_options("All samples"),
        server = TRUE
      )

      list(
        study_id = if (use_study && nzchar(selected_study)) selected_study else NULL,
        sample_id = if (use_sample && nzchar(selected_sample)) selected_sample else NULL
      )
    }

    refresh_counts <- function() {
      counts_rv(dbm_collection_counts())
    }

    refresh_overview <- function() {
      overview_rv(dbm_overview_stats())
    }

    refresh_dashboard <- function() {
      refresh_counts()
      refresh_overview()
    }

    db_change_snapshot <- reactivePoll(
      intervalMillis = 4000,
      session = session,
      checkFunc = function() {
        counts <- dbm_collection_counts()
        overview <- dbm_overview_stats()
        paste(
          paste(counts$key, counts$records, sep = "=", collapse = "|"),
          overview$total_db_bytes %||% "NA",
          overview$managed_file_bytes %||% "NA",
          overview$gridfs_file_count %||% "NA",
          overview$alignment_reference_total %||% "NA",
          sep = "||"
        )
      },
      valueFunc = function() {
        list(
          counts = dbm_collection_counts(),
          overview = dbm_overview_stats()
        )
      }
    )

    observeEvent(db_change_snapshot(), {
      snapshot <- db_change_snapshot()
      counts_rv(snapshot$counts)
      overview_rv(snapshot$overview)
    }, ignoreInit = FALSE)

    current_collection_label <- reactive({
      idx <- match(input$collection %||% dbm_catalog()$key[1], dbm_catalog()$key)
      if (is.na(idx)) "—" else dbm_catalog()$label[idx]
    })

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

    refresh_records_view <- function(collection = NULL, study_id = NULL, sample_id = NULL) {
      refresh_records(
        collection = collection,
        study_id = study_id,
        sample_id = sample_id
      )

      selected_id_rv(NULL)
      selected_record_rv(NULL)

      session$sendInputMessage(ns("records_table_rows_selected"), NULL)

      try({
        DT::dataTableProxy("records_table", session = session) |>
          DT::selectRows(NULL)
      }, silent = TRUE)
    }

    observeEvent(input$refresh_all, {
      refresh_dashboard()
      selected_filters <- sync_filter_inputs(
        collection = input$collection %||% dbm_catalog()$key[1],
        selected_study = input$study_filter %||% "",
        selected_sample = input$sample_filter %||% ""
      )
      refresh_records(
        collection = input$collection %||% dbm_catalog()$key[1],
        study_id = selected_filters$study_id,
        sample_id = selected_filters$sample_id
      )
      showNotification("Database view refreshed.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$study_filter, {
      selected_filters <- sync_filter_inputs(
        collection = input$collection %||% dbm_catalog()$key[1],
        selected_study = input$study_filter %||% "",
        selected_sample = input$sample_filter %||% ""
      )
      refresh_records(
        collection = input$collection %||% dbm_catalog()$key[1],
        study_id = selected_filters$study_id,
        sample_id = selected_filters$sample_id
      )
    }, ignoreInit = TRUE)

    observeEvent(input$sample_filter, {
      selected_filters <- sync_filter_inputs(
        collection = input$collection %||% dbm_catalog()$key[1],
        selected_study = input$study_filter %||% "",
        selected_sample = input$sample_filter %||% ""
      )
      refresh_records(
        collection = input$collection %||% dbm_catalog()$key[1],
        study_id = selected_filters$study_id,
        sample_id = selected_filters$sample_id
      )
    }, ignoreInit = TRUE)

    observeEvent(input$collection, {
      selected_collection <- input$collection %||% dbm_catalog()$key[1]
      session$onFlushed(function() {
        selected_filters <- sync_filter_inputs(
          collection = selected_collection,
          selected_study = "",
          selected_sample = ""
        )
        refresh_records(
          collection = selected_collection,
          study_id = selected_filters$study_id,
          sample_id = selected_filters$sample_id
        )
      }, once = TRUE)
    }, ignoreInit = TRUE)

    session$onFlushed(function() {
      refresh_dashboard()
      sync_filter_inputs(
        collection = dbm_catalog()$key[1],
        selected_study = "",
        selected_sample = ""
      )
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
      raw_row <- if (!is.null(df_raw) && nrow(df_raw) >= i) df_raw[i, , drop = FALSE] else NULL

      rid <- if ("id" %in% names(df_disp) && nrow(df_disp) >= i) as.character(df_disp$id[i]) else NA_character_
      if (is.na(rid) || !nzchar(rid)) {
        rid <- dbm_record_id(input$collection %||% "", raw_row)
      }

      selected_id_rv(if (!is.na(rid) && nzchar(rid)) rid else NULL)

      if (!is.null(raw_row) && nrow(raw_row) > 0) {
        selected_record_rv(raw_row)
      } else if (!is.na(rid) && nzchar(rid)) {
        selected_record_rv(dbm_fetch_record(input$collection, rid))
      } else {
        selected_record_rv(NULL)
      }
    }, ignoreInit = TRUE)

    output$dbm_session_ui <- renderUI({
      counts <- counts_rv()
      overview <- overview_rv()
      req(!is.null(overview))
      total_records <- if (nrow(counts) > 0) sum(counts$records, na.rm = TRUE) else 0
      largest_collection <- overview$largest_collection
      if (!length(largest_collection) || is.na(largest_collection) || !nzchar(largest_collection)) {
        largest_collection <- "—"
      }
      tags$div(
        class = "dbm-helper",
        tags$div(class = "dbm-label", style = "margin-bottom:10px;", "Session"),
        tags$div(class = "dbm-chip", tags$strong("Database"), DB_NAME),
        tags$div(class = "dbm-chip", tags$strong("Collections"), nrow(dbm_catalog())),
        tags$div(class = "dbm-chip", tags$strong("Total records"), total_records),
        tags$div(class = "dbm-chip", tags$strong("Mongo footprint"), dbm_bytes_label(overview$total_db_bytes)),
        tags$div(class = "dbm-chip", tags$strong("Largest collection"), largest_collection),
        tags$div(class = "dbm-chip", tags$strong("Selected"), current_collection_label()),
        tags$p(class = "dbm-quiet", style = "margin:10px 0 0 0;",
          "Mongo URL: ", MONGO_URL
        )
      )
    })

    output$overview_cards_ui <- renderUI({
      counts_rv()
      stats <- overview_rv()
      req(!is.null(stats))
      largest_name <- stats$largest_collection
      if (!length(largest_name) || is.na(largest_name) || !nzchar(largest_name)) {
        largest_name <- "—"
      }
      tags$div(
        class = "dbm-metric-grid",
        tags$div(
          class = "dbm-metric",
          tags$div(class = "dbm-metric-kicker", "Total Objects"),
          tags$div(class = "dbm-metric-value", format(stats$total_records, big.mark = ",")),
          tags$div(class = "dbm-metric-note", "Across all tracked collections in the shared application database.")
        ),
        tags$div(
          class = "dbm-metric",
          tags$div(class = "dbm-metric-kicker", "Collections In Use"),
          tags$div(class = "dbm-metric-value", stats$non_empty_collections),
          tags$div(class = "dbm-metric-note",
            "Tracked collections with at least 1 record. Largest: ",
            tags$strong(largest_name),
            " (", stats$largest_collection_records, ")."
          )
        ),
        tags$div(
          class = "dbm-metric",
          tags$div(class = "dbm-metric-kicker", "Mongo Footprint"),
          tags$div(class = "dbm-metric-value", dbm_bytes_label(stats$total_db_bytes)),
          tags$div(class = "dbm-metric-note",
            "Estimated physical space used by MongoDB for documents and indexes."
          )
        ),
        tags$div(
          class = "dbm-metric",
          tags$div(class = "dbm-metric-kicker", "Stored File Payload"),
          tags$div(class = "dbm-metric-value", dbm_bytes_label(stats$managed_file_bytes)),
          tags$div(class = "dbm-metric-note",
            "Logical size of stored files: ", stats$gridfs_file_count,
            " GridFS file(s) plus inline references. This can be larger or smaller than Mongo footprint."
          )
        )
      )
    })

    output$filter_ui <- renderUI({
      coll <- input$collection %||% "studies"
      use_study <- dbm_supports_study_filter(coll)
      use_sample <- dbm_supports_sample_filter(coll)
      no_filters <- !use_study && !use_sample

      tagList(
        if (no_filters) {
          tags$div(
            class = "dbm-quiet",
            "No extra filters are needed for this collection."
          )
        },
        tags$div(
          class = if (use_study) NULL else "dbm-filter-hidden",
          selectizeInput(
            ns("study_filter"),
            "Study",
            choices = c("All studies" = ""),
            width = "100%",
            options = dbm_selectize_options("All studies")
          )
        ),
        tags$div(
          class = if (use_sample) NULL else "dbm-filter-hidden",
          selectizeInput(
            ns("sample_filter"),
            "Sample",
            choices = c("All samples" = ""),
            width = "100%",
            options = dbm_selectize_options("All samples")
          )
        )
      )
    })

    output$collection_summary_ui <- renderUI({
      coll <- input$collection %||% dbm_catalog()$key[1]
      cat_row <- dbm_catalog()[match(coll, dbm_catalog()$key), , drop = FALSE]
      counts <- counts_rv()
      total_in_collection <- if (nrow(counts) > 0) counts$records[match(coll, counts$key)] else NA_real_
      if (!length(total_in_collection) || is.na(total_in_collection)) total_in_collection <- 0
      policy <- dbm_delete_policy(coll)

      chips <- list(
        tags$div(class = "dbm-chip", tags$strong("Collection"), cat_row$label[1]),
        tags$div(class = "dbm-chip", tags$strong("Stored"), total_in_collection)
      )

      if (identical(coll, "alignment_references")) {
        ref_stats <- dbm_alignment_reference_stats()
        chips <- c(
          chips,
          list(
            tags$div(class = "dbm-chip", tags$strong("Built-in"), ref_stats$built_in),
            tags$div(class = "dbm-chip", tags$strong("Uploaded"), ref_stats$uploaded)
          )
        )
      }

      tags$div(
        class = "dbm-helper",
        do.call(tagList, chips),
        tags$p(style = "margin:8px 0 0 0;", cat_row$description[1]),
        tags$p(
          class = if (isTRUE(policy$allowed)) "dbm-safe-note" else "dbm-danger-note",
          style = "margin:8px 0 0 0;",
          if (isTRUE(policy$allowed)) {
            "Selected records from this collection can be deleted after confirmation."
          } else {
            policy$reason %||% "Deletion is disabled for this collection."
          }
        )
      )
    })

    output$selected_summary_ui <- renderUI({
      rec <- selected_record_rv()
      policy <- dbm_delete_policy(input$collection %||% "", rec)

      if (is.null(rec) || nrow(rec) == 0) {
        return(
          tags$div(
            class = "dbm-selected",
            tags$h4("No object selected"),
            tags$p("Click a row in the records table to inspect it and enable any relevant actions.")
          )
        )
      }

      title <- dbm_record_title(input$collection, rec)
      rid <- selected_id_rv() %||% "—"
      created <- if ("created_at" %in% names(rec)) as.character(rec$created_at[1]) else "—"

      tags$div(
        class = "dbm-selected",
        tags$h4(title),
        tags$p(tags$strong("ID: "), rid),
        tags$p(tags$strong("Created: "), created),
        if ("description" %in% names(rec) && nzchar(as.character(rec$description[1] %||% ""))) {
          tags$p(tags$strong("Description: "), as.character(rec$description[1]))
        },
        if (identical(input$collection, "alignment_references")) {
          tags$p(
            tags$strong("Source: "),
            if (dbm_is_protected_record("alignment_references", rec)) "Built-in default" else "Uploaded by user"
          )
        },
        tags$p(
          class = if (isTRUE(policy$allowed)) "dbm-safe-note" else "dbm-danger-note",
          if (identical(input$collection, "studies")) {
            "Deleting a study cascades to dependent samples, Pipeline Outputs, NDPI images, annotations, datasets, model runs, and related metadata."
          } else if (isTRUE(policy$allowed)) {
            "This selected object can be deleted after confirmation."
          } else {
            policy$reason
          }
        )
      )
    })

    output$delete_report_ui <- renderUI({
      report <- last_delete_report_rv()
      if (is.null(report) || length(report) == 0) {
        return(NULL)
      }

      tags$div(
        class = "dbm-helper",
        tags$div(class = "dbm-label", style = "margin-bottom:10px;", "Last Delete Report"),
        tags$pre(
          style = "margin:0; white-space:pre-wrap; word-break:break-word; background:#f8fafc; border:1px solid #e5e7eb; padding:12px; border-radius:10px;",
          paste(dbm_delete_report_lines(report), collapse = "\n")
        )
      )
    })

    observe({
      rec <- selected_record_rv()
      coll <- input$collection %||% ""
      policy <- dbm_delete_policy(coll, rec)

      updateActionButton(session, "delete_selected", label = policy$label %||% "Delete selected record")

      if (is.null(rec) || nrow(rec) == 0 || !isTRUE(policy$allowed)) {
        shinyjs::hide("delete_btn_wrap")
      } else {
        shinyjs::show("delete_btn_wrap")
      }
    })

    output$records_meta_ui <- renderUI({
      df <- records_display_rv()
      coll <- current_collection_label()
      tags$span(
        class = "dbm-record-meta",
        format(nrow(df), big.mark = ","), " matching record(s) in ", coll, "."
      )
    })

    output$counts_table <- DT::renderDT({
      df <- counts_rv()
      if (nrow(df) == 0) df <- data.frame(collection = "No data", records = 0, description = "", stringsAsFactors = FALSE)
      DT::datatable(
        df[, c("collection", "records", "description"), drop = FALSE],
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(pageLength = 8, dom = "tip", ordering = TRUE, scrollX = TRUE)
      )
    }, server = FALSE)

    output$overview_counts_plot <- renderPlot({
      df <- counts_rv()
      req(nrow(df) > 0)
      df_plot <- df[df$records > 0, , drop = FALSE]
      if (nrow(df_plot) == 0) {
        plot.new()
        text(0.5, 0.5, "No records in tracked collections.")
        return(invisible(NULL))
      }
      df_plot$collection <- factor(df_plot$collection, levels = df_plot$collection[order(df_plot$records)])

      ggplot(df_plot, aes(x = collection, y = records, fill = records)) +
        geom_col(width = 0.72, show.legend = FALSE) +
        coord_flip() +
        scale_fill_gradient(low = "#9bd5d0", high = "#0f766e") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_text(color = "#1f2937", face = "bold"),
          axis.text.x = element_text(color = "#52606d"),
          plot.margin = margin(8, 12, 8, 8)
        )
    })

    output$composition_plot <- renderPlot({
      df_plot <- dbm_domain_mix(counts_rv())
      df_plot <- df_plot[df_plot$records > 0, , drop = FALSE]

      if (nrow(df_plot) == 0) {
        plot.new()
        text(0.5, 0.5, "No tracked objects stored yet.")
        return(invisible(NULL))
      }

      total_records <- sum(df_plot$records, na.rm = TRUE)
      df_plot$share <- if (total_records > 0) 100 * df_plot$records / total_records else 0
      df_plot$label <- paste0(df_plot$records, " object", ifelse(df_plot$records == 1, "", "s"),
                              "  (", sprintf("%.0f%%", df_plot$share), ")")
      df_plot$domain <- factor(df_plot$domain, levels = df_plot$domain[order(df_plot$records)])

      ggplot(df_plot, aes(x = domain, y = records, fill = domain)) +
        geom_col(width = 0.68, show.legend = FALSE) +
        geom_text(
          aes(label = label),
          hjust = -0.08,
          color = "#14213d",
          size = 4.1,
          fontface = "bold",
        ) +
        scale_fill_manual(values = c(
          "Study setup" = "#0f766e",
          "Processing" = "#1d8f88",
          "Annotation" = "#4fb3ad",
          "Modeling" = "#80cbc4",
          "References" = "#f59e0b"
        )) +
        coord_flip(clip = "off") +
        expand_limits(y = max(df_plot$records, 1) * 1.24) +
        labs(
          x = NULL,
          y = "Objects",
          subtitle = paste0(total_records, " tracked objects across ", nrow(df_plot), " active domains")
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(color = "#1f2937", face = "bold"),
          axis.text.x = element_text(color = "#52606d"),
          plot.subtitle = element_text(color = "#5b6472", margin = margin(b = 10)),
          plot.margin = margin(8, 44, 8, 8)
        )
    })

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
        options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE, dom = "tip")
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

      tags$table(class = "dbm-kv", kv)
    })


    observeEvent(input$delete_selected, {
      rec <- selected_record_rv()
      collection <- input$collection %||% ""
      policy <- dbm_delete_policy(collection, rec)

      if (is.null(rec) || nrow(rec) == 0) {
        showNotification(
          "Select a row in the Records table before deleting.",
          type = "warning",
          duration = 5
        )
        return()
      }

      if (!isTRUE(policy$allowed)) {
        showNotification(policy$reason %||% "This object cannot be deleted.", type = "warning", duration = 8)
        return()
      }

      plan <- dbm_plan_deletion(collection = collection, record = rec)
      delete_plan_rv(plan)

      if (!isTRUE(plan$requested$found)) {
        showNotification(plan$reason %||% "The selected object no longer exists.", type = "warning", duration = 8)
        return()
      }

      preview_tbl <- dbm_plan_summary_table(plan)
      preview_total <- sum(preview_tbl$count, na.rm = TRUE)

      preview_ui <- if (nrow(preview_tbl) > 0) {
        tags$ul(
          style = "margin-bottom:0;",
          lapply(seq_len(nrow(preview_tbl)), function(i) {
            tags$li(paste0(preview_tbl$label[i], ": ", preview_tbl$count[i]))
          })
        )
      } else {
        tags$p("No dependent objects were found.")
      }

      warning_ui <- tags$div(
        class = "alert alert-danger",
        style = "margin-top:10px; margin-bottom:10px;",
        tags$b("Cascade preview"),
        tags$br(),
        paste0(
          "This action will remove ",
          format(preview_total, big.mark = ","),
          " object(s) across ",
          format(nrow(preview_tbl), big.mark = ","),
          " collection(s)."
        )
      )

      showModal(modalDialog(
        title = "Confirm deletion",
        tags$p(plan$requested$title %||% dbm_record_title(collection, rec)),
        tags$p(tags$b("This operation cannot be undone.")),
        warning_ui,
        preview_ui,
        if (length(plan$errors %||% character(0)) > 0) {
          tags$div(
            class = "alert alert-warning",
            style = "margin-top:10px; margin-bottom:0;",
            paste(plan$errors, collapse = "\n")
          )
        },
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_btn"), "Yes, delete", class = "btn-danger")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_delete_btn, {
      collection <- input$collection %||% ""
      rec <- selected_record_rv()
      if (is.null(rec) || nrow(rec) == 0) {
        removeModal()
        showNotification("The selected row is no longer available. Refresh the table and try again.", type = "warning", duration = 6)
        return()
      }

      removeModal()

      tryCatch({
        plan <- delete_plan_rv() %||% dbm_plan_deletion(collection = collection, record = rec)
        report <- dbm_execute_deletion_plan(plan)
        last_delete_report_rv(report)

        cur_study <- isolate(input$study_filter %||% "")
        cur_sample <- isolate(input$sample_filter %||% "")

        selected_id_rv(NULL)
        selected_record_rv(NULL)
        delete_plan_rv(NULL)

        refresh_dashboard()

        selected_filters <- sync_filter_inputs(
          collection = collection,
          selected_study = cur_study,
          selected_sample = cur_sample
        )

        refresh_records_view(
          collection = collection,
          study_id = selected_filters$study_id,
          sample_id = selected_filters$sample_id
        )

        showNotification(
          if (report$total_deleted > 0) {
            paste0(
              "Delete finished. ",
              format(report$total_deleted, big.mark = ","),
              " object(s) were removed."
            )
          } else {
            report$reason %||% "Delete finished, but no objects were removed."
          },
          type = if (report$total_deleted > 0) "message" else "warning",
          duration = 12
        )
      }, error = function(e) {
        showNotification(
          paste("Delete failed:", conditionMessage(e)),
          type = "error",
          duration = 12
        )
      })
    }, ignoreInit = TRUE)
  })
}
