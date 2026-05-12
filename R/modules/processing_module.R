# R/modules/processing_module.R
#
# Processing page – fully provenance-aware.
#
# Flow:
#   1. User chooses "Create new Study" OR "Add to existing Study".
#   2. Sample is registered (or retrieved) via upsert_sample().
#   3. Parameters → deterministic pipeline_id shown before running.
#   4. Pipeline Output table shows all existing (sample, pipeline_id) combos.
#   5. Run Processing → saves via save_pipeline_output(); errors on exact duplicate.
#   6. No "most recent" logic anywhere.

processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Processing",
    app_page_shell(
      app_page_hero(
        "Processing Studio",
        "Prepare raw MSI data, configure the feature-generation pipeline, and inspect the resulting Pipeline Outputs and quality-control plots in one consistent workspace."
      ),
      fluidRow(
        column(
          3,
          tags$div(
            class = "app-stack",
            app_sidebar_step(
              ns("step_study_sample"),
              "1",
              "Study & Sample",
              status = app_step_status("Setup"),
              open = TRUE,
              radioButtons(
                ns("study_mode"),
                "Study:",
                choices = c("Create new Study" = "new", "Add to existing Study" = "existing"),
                selected = "new"
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'new'", ns("study_mode")),
                textInput(ns("new_study_name"), "Study name:", placeholder = "e.g. SSC_cohort_2025"),
                textInput(ns("new_study_desc"), "Description (optional):"),
                actionButton(ns("create_study_btn"), "Create Study", class = "btn-sm btn-success")
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'existing'", ns("study_mode")),
                actionButton(ns("refresh_studies"), "Refresh", class = "btn-xs btn-default"),
                selectInput(
                  ns("existing_study_id"),
                  "Select Study:",
                  choices = c("(loading...)" = ""),
                  width = "100%"
                )
              ),
              uiOutput(ns("study_badge")),
              tags$div(class = "app-divider"),
              textInput(
                ns("sample_name_input"),
                "Sample name:",
                placeholder = "Leave empty to use filename"
              ),
              uiOutput(ns("sample_duplicate_warning"))
            ),
            app_sidebar_step(
              ns("step_data_source"),
              "2",
              "Data Source",
              status = app_step_status("Input"),
              class = "app-accordion-overflow-visible",
              radioButtons(
                ns("data_source"),
                NULL,
                choices = c("Upload new files", "Use existing dataset"),
                selected = "Upload new files"
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Upload new files'", ns("data_source")),
                uiOutput(ns("msi_upload_ui"))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Use existing dataset'", ns("data_source")),
                uiOutput(ns("existing_sample_ui"))
              )
            ),
            app_sidebar_step(
              ns("step_processing_params"),
              "3",
              "Processing Parameters",
              status = app_step_status("Tuning"),
              numericInput(ns("resolution"), "Resolution (ppm):", value = 10, min = 1, max = 100, step = 1),
              numericInput(ns("snr"), "SNR:", value = 3, min = 1.5, max = 30, step = 0.1),
              numericInput(
                ns("tolerance"),
                "Binning tolerance (mz):",
                value = 0.5,
                min = 0.01,
                max = 3,
                step = 0.01
              ),
              radioButtons(
                ns("ref_source"),
                "Reference list:",
                choices = c("From database", "Upload your own"),
                selected = "From database"
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Upload your own'", ns("ref_source")),
                textInput(
                  ns("ref_upload_name"),
                  "Reference name (optional):",
                  placeholder = "Defaults to the uploaded filename"
                ),
                fileInput(ns("ref_csv"), "Upload .csv", multiple = FALSE, accept = ".csv"),
                actionButton(
                  ns("save_uploaded_reference"),
                  "Save uploaded reference",
                  class = "btn-sm btn-default"
                ),
                tags$p(
                  class = "help-block",
                  "Saved uploads become available in the shared reference list for future runs."
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'From database'", ns("ref_source")),
                selectInput(ns("ref_csv_mongo"), "Select reference:", choices = "Loading...")
              ),
              tags$div(class = "app-divider"),
              tags$div(
                class = "app-helper",
                tags$strong("Preview"),
                tags$br(),
                "The pipeline fingerprint updates with your current inputs so duplicate processing runs can be blocked before execution."
              ),
              verbatimTextOutput(ns("pipeline_id_preview")),
              actionButton(
                ns("run_processing"),
                "Run Processing",
                class = "btn-primary btn-lg app-btn-block"
              ),
              actionButton(
                ns("clear_cache"),
                "Clear local cache",
                class = "btn-warning btn-sm app-btn-block"
              )
            )
          )
        ),
        column(
          4,
          tags$div(
            class = "app-stack",
            app_panel(
              "Existing Pipeline Outputs",
              subtitle = "Pipeline Outputs for the current study and sample. Processing is blocked for exact duplicate pipeline IDs.",
              DT::DTOutput(ns("pipeline_output_table")),
              tags$div(style = "margin-top:12px;", actionButton(ns("refresh_pipeline_outputs"), "Refresh", class = "btn-xs btn-default"))
            ),
            app_panel(
              "Processing Log",
              subtitle = "Execution updates and validation messages for the current run.",
              verbatimTextOutput(ns("processing_log"))
            ),
            app_panel(
              "Cache Status",
              subtitle = "Local cache state for reusable intermediate files.",
              verbatimTextOutput(ns("cache_status"))
            )
          )
        ),
        column(
          5,
          tags$div(
            class = "app-stack",
            app_panel(
              "Pipeline Status",
              subtitle = "Current registration status for the selected study, sample, and pipeline.",
              uiOutput(ns("pipeline_status"))
            ),
            app_panel(
              "MSI Images - Top 3 m/z (by variance)",
              subtitle = "Compare raw and normalized views of the highest-variance features.",
              tabsetPanel(
                tabPanel("Raw", plotOutput(ns("top3_raw_plot"), height = "400px")),
                tabPanel("Normalized", plotOutput(ns("top3_norm_plot"), height = "400px"))
              )
            ),
            app_panel(
              "Spatial vs Intensity Distance",
              subtitle = "Inspect how spatial structure and intensity relationships shift across the generated feature matrix.",
              tabsetPanel(
                tabPanel("Binned", plotOutput(ns("distance_binned_plot"), height = "400px")),
                tabPanel("Scatter", plotOutput(ns("distance_scatter_plot"), height = "400px"))
              )
            )
          )
        )
      )
    )
  )
}


processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ─────────────────────────────────────────────────────
    processing_log      <- reactiveVal("")
    current_cache_dir   <- reactiveVal(NULL)
    current_sample_name <- reactiveVal(NULL)
    plot_top3_raw       <- reactiveVal(NULL)
    plot_top3_norm      <- reactiveVal(NULL)
    plot_distance_binned  <- reactiveVal(NULL)
    plot_distance_scatter <- reactiveVal(NULL)
    raw_upload_validation <- reactiveVal(empty_uploaded_raw_pair_validation())
    msi_upload_reset_counter <- reactiveVal(0L)

    # The study_id that is currently "active" (resolved after create / select)
    active_study_id <- reactiveVal(NULL)
    reference_refresh <- reactiveVal(0L)

    add_log <- function(msg) {
      processing_log(paste0(processing_log(),
                             format(Sys.time(), "[%H:%M:%S]"), " ", msg, "\n"))
    }

    cleanup_cardinal_temp <- function() {
      tryCatch({
        tmp <- list.files(tempdir(),
          pattern = "(imzml_|Cardinal|matter_array|msi_run_)",
          full.names = TRUE, recursive = TRUE)
        if (length(tmp) > 0) { s <- sum(file.size(tmp), na.rm=TRUE)/1024^2
          unlink(tmp, recursive = TRUE)
          add_log(sprintf("✓ System temp cleaned: %.2f MB", s)) }
        gc()
      }, error = function(e) invisible(NULL))
    }

    output$msi_upload_ui <- renderUI({
      msi_upload_reset_counter()
      fileInput(
        ns("msi_files"),
        "Upload imzML + ibd files",
        multiple = TRUE,
        accept = c(".imzML", ".ibd")
      )
    })

    uploaded_reference_display_name <- reactive({
      custom_name <- trimws(input$ref_upload_name %||% "")
      if (nzchar(custom_name)) {
        return(custom_name)
      }
      if (!is.null(input$ref_csv) && !is.null(input$ref_csv$name) && nzchar(input$ref_csv$name)) {
        return(default_alignment_reference_display_name(input$ref_csv$name))
      }
      ""
    })

    refresh_reference_choices <- function(selected = NULL, notify = FALSE) {
      refs <- tryCatch(
        list_alignment_references(),
        error = function(e) {
          if (isTRUE(notify)) {
            showNotification(
              paste("Could not load alignment references:", e$message),
              type = "error",
              duration = NULL
            )
          }
          data.frame(stringsAsFactors = FALSE)
        }
      )

      choices <- alignment_reference_choices(refs)
      selected_value <- selected %||% isolate(input$ref_csv_mongo)
      if (!(selected_value %in% unname(choices))) {
        selected_value <- unname(choices)[1] %||% ""
      }

      updateSelectInput(
        session,
        "ref_csv_mongo",
        choices = choices,
        selected = selected_value
      )
    }

    save_current_uploaded_reference <- function(show_success = TRUE,
                                                switch_to_database = TRUE) {
      saved_ref <- tryCatch(
        save_uploaded_alignment_reference(
          input$ref_csv,
          display_name = uploaded_reference_display_name()
        ),
        error = function(e) {
          showNotification(
            paste("Reference save failed:", e$message),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )

      if (is.null(saved_ref)) {
        return(NULL)
      }

      reference_refresh(reference_refresh() + 1L)
      refresh_reference_choices(selected = saved_ref$reference_name, notify = FALSE)

      if (isTRUE(switch_to_database)) {
        updateRadioButtons(session, "ref_source", selected = "From database")
      }

      if (isTRUE(show_success)) {
        showNotification(
          paste0("Reference saved: ", saved_ref$display_name),
          type = "message",
          duration = 8
        )
      }

      list(
        mz = saved_ref$mz_values,
        name = saved_ref$reference_name,
        display_name = saved_ref$display_name,
        built_in = saved_ref$built_in
      )
    }

    observe({
      if (!identical(input$data_source, "Upload new files")) {
        shinyjs::enable("run_processing")
        return()
      }

      files <- input$msi_files
      if (is.null(files) || NROW(files) == 0) {
        raw_upload_validation(empty_uploaded_raw_pair_validation())
        shinyjs::disable("run_processing")
        return()
      }

      validation <- raw_upload_validation()

      if (isTRUE(validation$valid)) {
        shinyjs::enable("run_processing")
      } else {
        shinyjs::disable("run_processing")
      }
    })

    observeEvent(input$msi_files, {
      if (!identical(input$data_source, "Upload new files")) {
        return()
      }

      files <- input$msi_files
      if (is.null(files) || NROW(files) == 0) {
        return()
      }

      validation <- validate_uploaded_raw_pair(files)
      raw_upload_validation(validation)

      if (!isTRUE(validation$valid)) {
        showNotification(
          paste("Raw file upload failed:", validation$message),
          type = "error",
          duration = NULL
        )
        msi_upload_reset_counter(msi_upload_reset_counter() + 1L)
      }
    }, ignoreInit = TRUE)

    # ── Reference dropdown ─────────────────────────────────────────────────
    observe({
      reference_refresh()
      refresh_reference_choices(notify = TRUE)
    })

    observeEvent(input$save_uploaded_reference, {
      validation <- validate_alignment_reference_upload(input$ref_csv)
      if (!isTRUE(validation$valid)) {
        showNotification(
          paste("Reference upload failed:", validation$message),
          type = "error",
          duration = NULL
        )
        return()
      }
      save_current_uploaded_reference(show_success = TRUE, switch_to_database = TRUE)
    })

    # ── Studies dropdown ───────────────────────────────────────────────────
    observe({
      input$refresh_studies
      input$study_mode          # also re-fetch when switching to 'existing'
      studies_df <- tryCatch(
        get_studies(),
        error = function(e) {
          showNotification(paste("Could not load studies:", e$message), type = "error")
          data.frame()
        }
      )
      has_ids <- nrow(studies_df) > 0 && "_id" %in% names(studies_df)
      if (!has_ids) {
        updateSelectInput(session, "existing_study_id",
                          choices = c("No studies found" = ""))
      } else {
        ch <- setNames(studies_df[["_id"]],
                       paste0(studies_df$name, " [", studies_df[["_id"]], "]"))
        updateSelectInput(session, "existing_study_id", choices = ch)
      }
    })

    # ── Create study button ────────────────────────────────────────────────
    observeEvent(input$create_study_btn, {
      nm <- trimws(input$new_study_name)
      if (!nzchar(nm)) {
        showNotification("Enter a study name first.", type = "warning"); return()
      }
      sid <- tryCatch(
        create_study(nm, input$new_study_desc),
        error = function(e) { showNotification(e$message, type="error"); NULL }
      )
      if (!is.null(sid)) {
        active_study_id(sid)
        showNotification(paste0("\u2713 Study created: ", sid), type = "message")
        # Refresh the 'existing' dropdown so it is immediately available
        studies_df <- tryCatch(get_studies(), error = function(e) data.frame())
        if (nrow(studies_df) > 0 && "_id" %in% names(studies_df)) {
          ch <- setNames(studies_df[["_id"]],
                         paste0(studies_df$name, " [", studies_df[["_id"]], "]"))
          updateSelectInput(session, "existing_study_id", choices = ch)
        }
      }
    })

    # Resolve active study from dropdown when in "existing" mode
    observe({
      req(input$study_mode == "existing", input$existing_study_id,
          nzchar(input$existing_study_id))
      active_study_id(input$existing_study_id)
    })

    # ── Study badge ────────────────────────────────────────────────────────
    output$study_badge <- renderUI({
      sid <- active_study_id()
      if (is.null(sid)) return(tags$small(style="color:grey", "No study selected"))
      tags$div(class="alert alert-info", style="padding:6px;margin:4px 0",
               tags$b("Active study: "), sid)
    })

    # ── Existing-sample dropdown (for "Use existing dataset") ──────────────
    output$existing_sample_ui <- renderUI({
      sid <- active_study_id()
      if (is.null(sid)) return(tags$small("Select a study first."))
      samp_df <- tryCatch(get_samples(sid), error = function(e) data.frame())
      if (nrow(samp_df) == 0 || !("_id" %in% names(samp_df)))
        return(tags$small("No samples in this study yet."))
      selectInput(ns("existing_sample"),  "Select sample:",
                  choices = setNames(samp_df[["_id"]], samp_df$sample_name),
                  width   = "100%")
    })

    # ── Duplicate-name warning (for upload path) ───────────────────────────
    output$sample_duplicate_warning <- renderUI({
      sid <- active_study_id()
      nm  <- trimws(input$sample_name_input)
      if (is.null(sid) || !nzchar(nm)) return(NULL)
      if (sample_name_exists(sid, nm)) {
        tags$div(class = "alert alert-warning", style = "padding:4px; font-size:12px",
                 "⚠ A sample with this name already exists in this study.",
                 " Processing will attach a new Pipeline Output to the existing sample.")
      } else NULL
    })

    # ── Resolve current sample name ────────────────────────────────────────
    current_sample_name_resolved <- reactive({
      if (input$data_source == "Upload new files") {
        nm <- trimws(input$sample_name_input)
        if (nzchar(nm)) {
          return(nm)
        }
        validation <- raw_upload_validation()
        req(isTRUE(validation$valid))
        validation$sample_name_default
      } else {
        req(input$existing_sample)
        # existing_sample value is sample_id – fetch sample_name
        samp_col <- mongolite::mongo(collection = "samples",
                                     db  = DB_NAME,
                                     url = "mongodb://localhost:27018")
        row <- samp_col$find(
          sprintf('{"_id": "%s"}', input$existing_sample),
          fields = '{"sample_name":1}'
        )
        if (nrow(row) == 0) stop("Sample not found")
        row$sample_name[1]
      }
    })

    # ── Selected reference ─────────────────────────────────────────────────
    selected_mz <- reactive({
      req(input$ref_source)
      if (input$ref_source == "Upload your own") {
        req(input$ref_csv)
        uploaded_ref <- prepare_uploaded_alignment_reference(
          input$ref_csv,
          display_name = uploaded_reference_display_name()
        )
        list(
          mz = uploaded_ref$mz_values,
          name = uploaded_ref$reference_name,
          display_name = uploaded_ref$display_name,
          built_in = FALSE
        )
      } else {
        req(input$ref_csv_mongo, nzchar(input$ref_csv_mongo))
        doc <- load_alignment_reference(input$ref_csv_mongo)
        list(
          mz = doc$mz_values,
          name = doc$reference_name,
          display_name = doc$display_name,
          built_in = doc$built_in
        )
      }
    })

    # ── Compute pipeline_id preview ────────────────────────────────────────
    current_pipeline_params <- reactive({
      mz_ref <- selected_mz()
      req(mz_ref)
      list(
        snr            = as.numeric(input$snr),
        tolerance      = as.numeric(input$tolerance),
        resolution     = as.numeric(input$resolution),
        reference_name = mz_ref$name
      )
    })

    output$pipeline_id_preview <- renderText({
      params <- tryCatch(current_pipeline_params(), error = function(e) NULL)
      mz_ref <- tryCatch(selected_mz(), error = function(e) NULL)
      if (is.null(params)) return("(configure parameters above)")
      ref_label <- if (!is.null(mz_ref) && !is.null(mz_ref$display_name) && nzchar(mz_ref$display_name)) {
        mz_ref$display_name
      } else {
        params$reference_name
      }
      pid <- compute_pipeline_id("processing", params)
      paste0("pipeline_id:\n", substr(pid, 1, 16), "...\n\n",
             "snr=",        params$snr,        "\n",
             "tol=",        params$tolerance,  "\n",
             "res=",        params$resolution, " ppm\n",
             "ref=",        ref_label)
    })

    # ── Pipeline Output table for current study + sample ──────────────────────────
    output$pipeline_output_table <- DT::renderDT({
      input$refresh_pipeline_outputs
      sid <- active_study_id()
      
      if (is.null(sid)) {
        return(
          DT::datatable(
            data.frame(message = "No study selected"),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE, searching = FALSE)
          )
        )
      }
      
      nm <- tryCatch(current_sample_name_resolved(), error = function(e) NULL)
      if (is.null(nm)) {
        return(
          DT::datatable(
            data.frame(message = "No sample name"),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE, searching = FALSE)
          )
        )
      }
      
      sample_id <- get_sample_id(sid, nm)
      outputs <- query_pipeline_outputs(
        sample_id  = sample_id,
        stage_type = "binned_dataframe"
      )
      
      if (nrow(outputs) == 0) {
        return(
          DT::datatable(
            data.frame(message = "No Pipeline Outputs yet"),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE, searching = FALSE)
          )
        )
      }
      
      pipes <- lapply(outputs$pipeline_id, function(pid) {
        tryCatch({
          p  <- get_pipeline(pid)
          pa <- extract_params(p$params)
          ref_display <- tryCatch({
            ref_name <- pa$reference_name %||% NA_character_
            if (is.na(ref_name) || !nzchar(ref_name)) {
              return(NA_character_)
            }
            ref_doc <- load_alignment_reference(ref_name)
            suffix <- if (isTRUE(ref_doc$built_in)) " [built-in]" else " [uploaded]"
            paste0(ref_doc$display_name, suffix)
          }, error = function(e) {
            pa$reference_name %||% NA_character_
          })
          
          data.frame(
            pipeline_id = substr(pid, 1, 12),
            snr         = pa$snr %||% NA,
            tolerance   = pa$tolerance %||% NA,
            resolution  = pa$resolution %||% NA,
            reference   = ref_display,
            created_at  = outputs$created_at[outputs$pipeline_id == pid][1],
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          data.frame(
            pipeline_id = substr(pid, 1, 12),
            snr         = NA,
            tolerance   = NA,
            resolution  = NA,
            reference   = NA,
            created_at  = NA,
            stringsAsFactors = FALSE
          )
        })
      })
      
      df <- do.call(rbind, pipes)
      
      DT::datatable(
        df,
        rownames = FALSE,
        class = "display cell-border",
        options = list(
          scrollX = TRUE,
          scrollY = "180px",
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = TRUE,
          fixedHeader = TRUE,
          dom = "t"
        )
      )
    }, server = FALSE)

    # ── Clear cache ────────────────────────────────────────────────────────
    observeEvent(input$clear_cache, {
      d <- current_cache_dir()
      if (is.null(d) || !dir.exists(d)) {
        showNotification("No active cache", type = "message"); return()
      }
      fls  <- list.files(d, full.names = TRUE, recursive = TRUE)
      mb   <- sum(file.size(fls)) / 1024^2
      unlink(d, recursive = TRUE); current_cache_dir(NULL)
      plot_top3_raw(NULL); plot_top3_norm(NULL)
      plot_distance_binned(NULL); plot_distance_scatter(NULL)
      gc()
      showNotification(sprintf("✓ Cleared %.2f MB", mb), type = "message")
    })

    # ── MAIN PROCESSING PIPELINE ───────────────────────────────────────────
    observeEvent(input$run_processing, {
      upload_validation <- NULL
      if (identical(input$data_source, "Upload new files")) {
        upload_validation <- validate_uploaded_raw_pair(input$msi_files)
        raw_upload_validation(upload_validation)
        if (!isTRUE(upload_validation$valid)) {
          showNotification(
            paste("Raw file upload failed:", upload_validation$message),
            type = "error",
            duration = NULL
          )
          return()
        }
      }

      study_id <- active_study_id()
      if (is.null(study_id) || !nzchar(study_id)) {
        showNotification("Select or create a Study first.", type = "error"); return()
      }

      mz_ref <- if (identical(input$ref_source, "Upload your own")) {
        save_current_uploaded_reference(show_success = TRUE, switch_to_database = FALSE)
      } else {
        tryCatch(
          selected_mz(),
          error = function(e) {
            showNotification(
              paste("Reference load failed:", e$message),
              type = "error",
              duration = NULL
            )
            NULL
          }
        )
      }
      sample_name <- tryCatch(current_sample_name_resolved(), error = function(e) NULL)

      if (is.null(mz_ref) || is.null(sample_name)) {
        showNotification("Configure all parameters first.", type = "error", duration = NULL)
        return()
      }

      params <- list(
        snr            = as.numeric(input$snr),
        tolerance      = as.numeric(input$tolerance),
        resolution     = as.numeric(input$resolution),
        reference_name = mz_ref$name
      )
      pipeline_id <- compute_pipeline_id("processing", params)

      # Resolve sample_id (creates sample document if new)
      sample_id <- upsert_sample(study_id, sample_name)

      # Block exact duplicate
      outputs <- query_pipeline_outputs(sample_id = sample_id, stage_type = "binned_dataframe",
                               pipeline_id = pipeline_id)
      if (nrow(outputs) > 0) {
        showNotification(
          paste0("Pipeline Output already exists for this exact pipeline_id:\n", pipeline_id),
          type = "warning", duration = 12
        )
        return()
      }

      plot_top3_raw(NULL); plot_top3_norm(NULL)
      plot_distance_binned(NULL); plot_distance_scatter(NULL)
      shinyjs::disable("run_processing")
      on.exit(shinyjs::enable("run_processing"), add = TRUE)

      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting…", value = 0)
      on.exit(progress$close(), add = TRUE)

      processing_log("")
      cleanup_cardinal_temp()

      tryCatch({
        add_log("=== PROCESSING STARTED ===")
        add_log(sprintf("Study:  %s", study_id))
        add_log(sprintf("Sample: %s  [id: %s]", sample_name, sample_id))
        add_log(sprintf("pipeline_id: %s", pipeline_id))

        # ── Work dir ───────────────────────────────────────────────────
        work_dir <- tempfile("msi_run_")
        dir.create(work_dir, recursive = TRUE)
        on.exit(tryCatch(unlink(work_dir, recursive = TRUE), error = function(e) NULL),
                add = TRUE)
        current_cache_dir(work_dir)
        current_sample_name(sample_name)

        # ── STEP 1: Raw files ──────────────────────────────────────────
        progress$set(value = 10, message = "Handling raw data...")

        if (input$data_source == "Upload new files") {
          req(input$msi_files)
          files     <- input$msi_files
          imzml_idx <- upload_validation$imzml_idx
          ibd_idx   <- upload_validation$ibd_idx

          # Reuse the existing raw-file storage path for uploaded imzML/ibd pairs.
          existing_raw <- query_legacy_pipeline_outputs(sample_name = sample_name,
                                                 stage_type  = "raw_files")
          if (nrow(existing_raw) > 0) {
            add_log("⚠ Raw files already in database — skipping upload")
          } else {
            add_log("Uploading raw files to MongoDB...")
            raw_refs <- save_raw_pair_to_mongo(
              sample_name = sample_name,
              imzml_path  = files$datapath[imzml_idx[1]],
              ibd_path    = files$datapath[ibd_idx[1]]
            )
            # Back-fill raw_ref into the sample document
            mongolite::mongo(collection = "samples", db = DB_NAME,
                             url = "mongodb://localhost:27018")$update(
              sprintf('{"_id": "%s"}', sample_id),
              jsonlite::toJSON(list(`$set` = list(raw_ref = raw_refs)),
                               auto_unbox = TRUE)
            )
            add_log("✓ Raw files saved")
          }
        }

        # ── STEP 2: Load raw MSI object ────────────────────────────────
        progress$set(value = 25, message = "Loading MSI object...")
        add_log("Downloading raw files from MongoDB...")
        msi_data <- load_raw_object_from_mongo(
          sample_name = sample_name,
          workdir     = work_dir,
          db_name     = DB_NAME,
          resolution  = as.numeric(input$resolution)
        )
        add_log(sprintf("✓ MSI loaded: %d pixels × %d m/z values",
                        ncol(msi_data), nrow(msi_data)))

        # ── STEP 3: Mean → peakPick → align ───────────────────────────
        progress$set(value = 45, message = "Mean spectrum + peak picking + alignment...")
        add_log("Computing mean spectrum...")
        control_mean <- Cardinal::summarizeFeatures(msi_data, "mean")

        add_log(sprintf("Peak picking (SNR=%.1f) + aligning (tol=%.2f)...",
                        input$snr, input$tolerance))
        control_MSI_ref <- control_mean |>
          Cardinal::peakPick(SNR = input$snr) |>
          Cardinal::peakAlign(ref = mz_ref$mz, tolerance = input$tolerance,
                              units = "mz") |>
          Cardinal::subsetFeatures() |>
          Cardinal::process()
        add_log(sprintf("✓ Reference aligned: %d m/z bins", nrow(control_MSI_ref)))

        # ── STEP 4: Bin full dataset ───────────────────────────────────
        progress$set(value = 70, message = "Binning full dataset...")
        msi_data_binned <- Cardinal::bin(
          msi_data,
          ref       = Cardinal::mz(control_MSI_ref),
          tolerance = input$tolerance,
          units     = "mz",
          BPPARAM   = msi_bpparam
        ) |> Cardinal::process()
        add_log("✓ Data binned")

        # ── STEP 5: TIC normalization + plots ──────────────────────────
        progress$set(value = 82, message = "Generating plots...")
        spec_mat <- as.matrix(Cardinal::spectra(msi_data_binned))
        tic      <- colSums(spec_mat, na.rm = TRUE)
        tic[!is.finite(tic) | tic <= 0] <- NA_real_
        spec_mat_tic <- sweep(spec_mat, 2, tic, "/")
        mz_vals  <- Cardinal::mz(msi_data_binned)

        var_raw  <- apply(spec_mat,     1, var, na.rm = TRUE)
        var_norm <- apply(spec_mat_tic, 1, var, na.rm = TRUE)
        top3_mz      <- mz_vals[order(var_raw,  decreasing = TRUE)[1:3]]
        norm_top3_mz <- mz_vals[order(var_norm, decreasing = TRUE)[1:3]]
        coords_df    <- Cardinal::coord(msi_data_binned)

        make_image_df <- function(idx, coords, mat) {
          data.frame(x=coords$x, y=coords$y,
                     mz1=mat[idx[1],], mz2=mat[idx[2],], mz3=mat[idx[3],])
        }
        img_df_raw  <- make_image_df(match(top3_mz,      mz_vals), coords_df, spec_mat)
        img_df_norm <- make_image_df(match(norm_top3_mz, mz_vals), coords_df, spec_mat_tic)

        make_overlay_plot <- function(df, lbl, title) {
          s01 <- function(v) {
            v[!is.finite(v)] <- 0
            lo <- min(v); hi <- max(v)
            if (hi == lo) return(rep(0, length(v)))
            (v - lo) / (hi - lo)
          }
          r <- s01(df$mz1); g <- s01(df$mz2); b <- s01(df$mz3)
          list(x = df$x, y = df$y,
               cols  = rgb(r, g, b, alpha = pmax(r,g,b)*0.9+0.1, maxColorValue=1),
               lbl   = lbl, title = title)
        }
        plot_top3_raw( make_overlay_plot(img_df_raw,  paste0("mz=",round(top3_mz,     2)),
                                         "Top 3 m/z (raw variance)"))
        plot_top3_norm(make_overlay_plot(img_df_norm, paste0("mz=",round(norm_top3_mz,2)),
                                         "Top 3 m/z (TIC-normalized variance)"))

        # ── STEP 6: Spatial distance plots ────────────────────────────
        add_log("Calculating spatial vs intensity distances...")
        norm_mat  <- cbind(x=coords_df$x, y=coords_df$y, t(spec_mat_tic))
        valid_r   <- complete.cases(norm_mat)
        norm_mat  <- norm_mat[valid_r, , drop = FALSE]

        n_pairs   <- 10000L
        n         <- nrow(norm_mat)
        pairs     <- data.frame(i=sample(n,n_pairs,replace=TRUE),
                                j=sample(n,n_pairs,replace=TRUE))
        pairs     <- subset(pairs, i != j)
        pairs     <- unique(data.frame(i=pmin(pairs$i,pairs$j),
                                       j=pmax(pairs$i,pairs$j)))
        if (nrow(pairs) > n_pairs) pairs <- pairs[seq_len(n_pairs),]

        xy      <- norm_mat[, c("x","y"), drop=FALSE]
        intens  <- norm_mat[, -c(1,2),   drop=FALSE]
        space_d <- sqrt(rowSums((xy[pairs$i,,drop=FALSE]-xy[pairs$j,,drop=FALSE])^2))
        cos_d   <- mapply(function(i,j) {
          a <- intens[i,]; b <- intens[j,]
          na <- sqrt(sum(a^2,na.rm=TRUE)); nb <- sqrt(sum(b^2,na.rm=TRUE))
          if (!is.finite(na)||!is.finite(nb)||na==0||nb==0) return(NA_real_)
          1 - sum(a*b,na.rm=TRUE)/(na*nb)
        }, pairs$i, pairs$j)

        df_dist <- data.frame(space_distance    = space_d,
                              intensity_distance = cos_d)
        df_dist <- df_dist[is.finite(df_dist$space_distance) &
                           is.finite(df_dist$intensity_distance), ]

        df_bin <- df_dist |>
          dplyr::mutate(bin = cut(space_distance, breaks = 50L)) |>
          dplyr::group_by(bin) |>
          dplyr::summarise(
            space_mid  = mean(space_distance,       na.rm = TRUE),
            int_median = median(intensity_distance, na.rm = TRUE),
            int_q25    = quantile(intensity_distance, 0.25, na.rm = TRUE),
            int_q75    = quantile(intensity_distance, 0.75, na.rm = TRUE),
            .groups    = "drop"
          )

        plot_distance_binned(
          ggplot(df_bin, aes(x=space_mid, y=int_median)) +
            geom_line() +
            geom_ribbon(aes(ymin=int_q25, ymax=int_q75), alpha=0.2) +
            theme_bw() +
            labs(x="Euclidean pixel distance",
                 y="Cosine distance (median ± IQR)",
                 title="Spatial vs Intensity Distance (binned)")
        )
        plot_distance_scatter(
          ggplot(df_dist, aes(x=space_distance, y=intensity_distance)) +
            geom_point(alpha=0.2, size=1) +
            theme_bw() +
            labs(x="Euclidean pixel distance",
                 y="Cosine distance",
                 title="Spatial vs Intensity Distance (10k pairs)")
        )

        # ── STEP 7: Build feature matrix ───────────────────────────────
        progress$set(value = 92, message = "Building feature matrix...")
        mz_names    <- paste0("mz_", mz_vals)
        pixel_names <- rep(Cardinal::runNames(msi_data_binned), nrow(t(spec_mat)))
        full_df     <- data.frame(
          runNames = pixel_names,
          x        = coords_df$x,
          y        = coords_df$y,
          t(spec_mat),
          check.names = FALSE
        )
        colnames(full_df) <- c("runNames", "x", "y", mz_names)
        add_log(sprintf("Feature matrix: %d pixels × %d features",
                        nrow(full_df), length(mz_names)))

        # ── STEP 8: Register pipeline + save Pipeline Output ──────────────────
        progress$set(value = 96, message = "Saving to MongoDB...")

        upsert_pipeline(
          type         = "processing",
          name         = paste0("proc_snr", input$snr, "_tol", input$tolerance,
                                "_res", input$resolution, "_", mz_ref$name),
          params       = params,
          code_version = "dev"
        )

        pipeline_output_id <- save_pipeline_output(
          obj         = full_df,
          study_id    = study_id,
          sample_id   = sample_id,
          pipeline_id = pipeline_id,
          stage_type  = "binned_dataframe",
          extra_meta  = list(
            num_features = length(mz_names),
            num_pixels   = nrow(full_df)
          )
        )

        progress$set(value = 100, message = "Complete!")
        add_log(sprintf("✓ Pipeline Output ID: %s", pipeline_output_id))
        add_log(sprintf("✓ pipeline_id: %s", pipeline_id))
        add_log("=== PROCESSING COMPLETE ===")

        output$pipeline_status <- renderUI({
          div(class = "alert alert-success",
            h4("✅ Processing Complete"),
            p(strong("Study:   "), study_id),
            p(strong("Sample:  "), sample_name),
            p(strong("sample_id: "), sample_id),
            p(strong("pipeline_id: "), pipeline_id),
            p(strong("Pipeline Output ID: "), pipeline_output_id),
            p(strong("Features: "), length(mz_names), " m/z bins"),
            p(strong("Pixels:   "), nrow(full_df))
          )
        })

        showNotification(
          sprintf("✅ Processing complete! %d features", length(mz_names)),
          type = "message", duration = 10
        )

      }, error = function(e) {
        add_log(sprintf("❌ ERROR: %s", e$message))
        showNotification(paste("Processing error:", e$message),
                         type = "error", duration = NULL)
      })
    })

    # ── Render outputs ─────────────────────────────────────────────────────
    output$processing_log <- renderText({ processing_log() })

    output$cache_status <- renderText({
      d <- current_cache_dir()
      if (is.null(d) || !dir.exists(d)) return("No active cache")
      fls <- list.files(d, full.names = TRUE, recursive = TRUE)
      if (length(fls) == 0) return(sprintf("Work dir: %s\n(empty)", d))
      sprintf("Work dir: %s\nFiles: %d | Size: %.2f MB\nSample: %s",
              d, length(fls), sum(file.size(fls))/1024^2,
              current_sample_name() %||% "None")
    })

    render_overlay <- function(rv) {
      renderPlot({
        req(rv())
        p <- rv()
        par(bg="black", col.axis="white", col.lab="white",
            col.main="white", fg="white", mar=c(3,3,2,1))
        plot(p$x, p$y, col=p$cols, pch=15, cex=0.5,
             xlab="x", ylab="y", main=p$title, asp=1, axes=FALSE)
        axis(1, col="white", col.ticks="white", col.axis="white")
        axis(2, col="white", col.ticks="white", col.axis="white")
        legend("topright", legend=p$lbl, col=c("red","green","blue"),
               pch=15, pt.cex=1.5, text.col="white",
               bg=adjustcolor("black", alpha.f=0.6), box.col="white")
      })
    }
    output$top3_raw_plot   <- render_overlay(plot_top3_raw)
    output$top3_norm_plot  <- render_overlay(plot_top3_norm)

    output$distance_binned_plot <- renderPlot({
      req(plot_distance_binned()); plot_distance_binned()
    })
    output$distance_scatter_plot <- renderPlot({
      req(plot_distance_scatter()); plot_distance_scatter()
    })
  })
}
