# R/modules/processing_module.R
#
# Processing page – fully provenance-aware.
#
# Flow:
#   1. User chooses "Create new Study" OR "Add to existing Study".
#   2. Sample is registered (or retrieved) via upsert_sample().
#   3. Parameters → deterministic pipeline_id shown before running.
#   4. Artifact table shows all existing (sample, pipeline_id) combos.
#   5. Run Processing → saves via save_artifact(); errors on exact duplicate.
#   6. No "most recent" logic anywhere.

processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Processing",
    fluidRow(

      # ── LEFT SIDEBAR (3 / 12) ──────────────────────────────────────────
      column(3,

        # ── 0. Study / Sample selection ────────────────────────────────
        wellPanel(
          h4("Study & Sample"),
          radioButtons(ns("study_mode"), "Study:",
            choices  = c("Create new Study" = "new",
                         "Add to existing Study" = "existing"),
            selected = "new"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'new'", ns("study_mode")),
            textInput(ns("new_study_name"), "Study name:", placeholder = "e.g. SSC_cohort_2025"),
            textInput(ns("new_study_desc"), "Description (optional):"),
            actionButton(ns("create_study_btn"), "Create Study",
                         class = "btn-sm btn-success")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'existing'", ns("study_mode")),
            actionButton(ns("refresh_studies"), "Refresh", class = "btn-xs btn-default"),
            selectInput(ns("existing_study_id"), "Select Study:",
                        choices = c("(loading...)" = ""), width = "100%")
          ),
          uiOutput(ns("study_badge")),
          hr(),
          textInput(ns("sample_name_input"), "Sample name:",
                    placeholder = "Leave empty to use filename"),
          uiOutput(ns("sample_duplicate_warning"))
        ),

        # ── 1. File source ─────────────────────────────────────────────
        wellPanel(
          h4("Data Source"),
          radioButtons(ns("data_source"), NULL,
            choices  = c("Upload new files", "Use existing dataset"),
            selected = "Upload new files"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Upload new files'", ns("data_source")),
            fileInput(ns("msi_files"), "Upload imzML + ibd files",
                      multiple = TRUE, accept = c(".imzML", ".ibd"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Use existing dataset'", ns("data_source")),
            uiOutput(ns("existing_sample_ui"))
          )
        ),

        # ── 2. Processing parameters ───────────────────────────────────
        wellPanel(
          h4("Processing Parameters"),
          numericInput(ns("resolution"), "Resolution (ppm):",
                       value = 10, min = 1, max = 100, step = 1),
          numericInput(ns("snr"),        "SNR:",
                       value = 3,  min = 1.5, max = 30, step = 0.1),
          numericInput(ns("tolerance"),  "Binning tolerance (mz):",
                       value = 0.5, min = 0.01, max = 3, step = 0.01),
          radioButtons(ns("ref_source"), "Reference list:",
            choices  = c("From database", "Upload your own"),
            selected = "From database"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Upload your own'", ns("ref_source")),
            fileInput(ns("ref_csv"), "Upload .csv", multiple = FALSE, accept = ".csv")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'From database'", ns("ref_source")),
            selectInput(ns("ref_csv_mongo"), "Select reference:", choices = "Loading...")
          )
        ),

        # ── 3. Pipeline preview + run ──────────────────────────────────
        wellPanel(
          h4("Pipeline"),
          verbatimTextOutput(ns("pipeline_id_preview")),
          actionButton(ns("run_processing"), "Run Processing",
                       class = "btn-primary btn-lg", style = "width:100%"),
          br(), br(),
          actionButton(ns("clear_cache"), "Clear local cache",
                       class = "btn-warning btn-sm")
        )
      ),

      # ── CENTRE: Log / status (4 / 12) ─────────────────────────────────
      column(4,
        h4("Existing Artifacts"),
        p(tags$small("Artifacts for the current study + sample.
                      Processing is blocked for exact duplicate pipeline_ids.")),
        tableOutput(ns("artifact_table")),
        actionButton(ns("refresh_artifacts"), "Refresh",
                     class = "btn-xs btn-default"),
        hr(),
        h4("Processing Log"),
        verbatimTextOutput(ns("processing_log")),
        hr(),
        h4("Cache Status"),
        verbatimTextOutput(ns("cache_status"))
      ),

      # ── RIGHT: Plots (5 / 12) ──────────────────────────────────────────
      column(5,
        uiOutput(ns("pipeline_status")),
        wellPanel(
          h4("MSI Images – Top 3 m/z (by variance)"),
          tabsetPanel(
            tabPanel("Raw",        plotOutput(ns("top3_raw_plot"),  height = "400px")),
            tabPanel("Normalized", plotOutput(ns("top3_norm_plot"), height = "400px"))
          )
        ),
        wellPanel(
          h4("Spatial vs Intensity Distance"),
          tabsetPanel(
            tabPanel("Binned",  plotOutput(ns("distance_binned_plot"),  height = "400px")),
            tabPanel("Scatter", plotOutput(ns("distance_scatter_plot"), height = "400px"))
          )
        )
      )
    )
  )
}


processing_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── MongoDB connections ────────────────────────────────────────────────
    mongo_ref <- mongolite::mongo(collection = "mz_references",
                                  db  = "msi_project",
                                  url = "mongodb://localhost:27018")

    # ── Reactive state ─────────────────────────────────────────────────────
    processing_log      <- reactiveVal("")
    current_cache_dir   <- reactiveVal(NULL)
    current_sample_name <- reactiveVal(NULL)
    plot_top3_raw       <- reactiveVal(NULL)
    plot_top3_norm      <- reactiveVal(NULL)
    plot_distance_binned  <- reactiveVal(NULL)
    plot_distance_scatter <- reactiveVal(NULL)

    # The study_id that is currently "active" (resolved after create / select)
    active_study_id <- reactiveVal(NULL)

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

    # ── Reference dropdown ─────────────────────────────────────────────────
    observe({
      refs <- tryCatch(
        unique(mongo_ref$find(fields = '{"_id":0,"reference_name":1}')$reference_name),
        error = function(e) character(0)
      )
      if (length(refs) == 0) refs <- c("No references found" = "")
      updateSelectInput(session, "ref_csv_mongo", choices = refs)
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
                 " Processing will attach a new artifact to the existing sample.")
      } else NULL
    })

    # ── Resolve current sample name ────────────────────────────────────────
    current_sample_name_resolved <- reactive({
      if (input$data_source == "Upload new files") {
        req(input$msi_files)
        file_name <- input$msi_files$name[
          grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)][1]
        nm <- trimws(input$sample_name_input)
        if (nzchar(nm)) nm else tools::file_path_sans_ext(basename(file_name))
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
        df <- read.csv(input$ref_csv$datapath, stringsAsFactors = FALSE)
        list(mz   = as.numeric(df$mz),
             name = tools::file_path_sans_ext(basename(input$ref_csv$name)))
      } else {
        req(input$ref_csv_mongo, nzchar(input$ref_csv_mongo))
        doc <- mongo_ref$find(
          sprintf('{"reference_name": "%s"}', input$ref_csv_mongo),
          fields = '{"_id":0,"mz_values":1}'
        )
        if (nrow(doc) == 0) return(NULL)
        list(mz   = as.numeric(unlist(doc$mz_values[[1]])),
             name = input$ref_csv_mongo)
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
      if (is.null(params)) return("(configure parameters above)")
      pid <- compute_pipeline_id("processing", params)
      paste0("pipeline_id:\n", substr(pid, 1, 16), "...\n\n",
             "snr=",        params$snr,        "\n",
             "tol=",        params$tolerance,  "\n",
             "res=",        params$resolution, " ppm\n",
             "ref=",        params$reference_name)
    })

    # ── Artifact table for current study + sample ──────────────────────────
    output$artifact_table <- renderTable({
      input$refresh_artifacts
      sid <- active_study_id()
      if (is.null(sid)) return(data.frame(message = "No study selected"))
      nm <- tryCatch(current_sample_name_resolved(), error = function(e) NULL)
      if (is.null(nm)) return(data.frame(message = "No sample name"))
      sample_id <- get_sample_id(sid, nm)
      arts <- query_artifacts(sample_id  = sample_id,
                               stage_type = "binned_dataframe")
      if (nrow(arts) == 0) return(data.frame(message = "No artifacts yet"))
      pipes <- lapply(arts$pipeline_id, function(pid) {
        tryCatch({
          p <- get_pipeline(pid)
          pa <- extract_params(p$params)
          data.frame(
            pipeline_id  = substr(pid, 1, 12),
            snr          = pa$snr       %||% NA,
            tolerance    = pa$tolerance %||% NA,
            resolution   = pa$resolution %||% NA,
            reference    = pa$reference_name %||% NA,
            created_at   = arts$created_at[arts$pipeline_id == pid][1],
            stringsAsFactors = FALSE
          )
        }, error = function(e) data.frame(pipeline_id = substr(pid,1,12),
                                          snr=NA, tolerance=NA,
                                          resolution=NA, reference=NA,
                                          created_at=NA))
      })
      do.call(rbind, pipes)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "–")

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

      study_id <- active_study_id()
      if (is.null(study_id) || !nzchar(study_id)) {
        showNotification("Select or create a Study first.", type = "error"); return()
      }

      mz_ref      <- selected_mz()
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
      arts <- query_artifacts(sample_id = sample_id, stage_type = "binned_dataframe",
                               pipeline_id = pipeline_id)
      if (nrow(arts) > 0) {
        showNotification(
          paste0("Artifact already exists for this exact pipeline_id:\n", pipeline_id),
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
          imzml_idx <- grepl("\\.imzML$", files$name, ignore.case = TRUE)
          ibd_idx   <- grepl("\\.ibd$",   files$name, ignore.case = TRUE)
          if (!any(imzml_idx) || !any(ibd_idx))
            stop("Both imzML and ibd files are required.")

          # Check legacy raw_files in processing_artifacts_metadata
          existing_raw <- query_legacy_artifacts(sample_name = sample_name,
                                                 stage_type  = "raw_files")
          if (nrow(existing_raw) > 0) {
            add_log("⚠ Raw files already in database — skipping upload")
          } else {
            add_log("Uploading raw files to MongoDB...")
            raw_refs <- save_raw_pair_to_mongo(
              sample_name = sample_name,
              imzml_path  = files$datapath[imzml_idx][1],
              ibd_path    = files$datapath[ibd_idx][1]
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
          BPPARAM   = BiocParallel::bpparam()
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

        # ── STEP 8: Register pipeline + save artifact ──────────────────
        progress$set(value = 96, message = "Saving to MongoDB...")

        upsert_pipeline(
          type         = "processing",
          name         = paste0("proc_snr", input$snr, "_tol", input$tolerance,
                                "_res", input$resolution, "_", mz_ref$name),
          params       = params,
          code_version = "dev"
        )

        artifact_id <- save_artifact(
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

        # Also keep legacy record for processing_module compat
        run_id_legacy <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        save_stage_to_mongo(
          full_df, run_id_legacy, "binned_dataframe",
          sample_name = sample_name,
          params = list(
            snr            = as.numeric(input$snr),
            tolerance      = as.numeric(input$tolerance),
            reference_name = mz_ref$name,
            resolution     = as.numeric(input$resolution),
            num_features   = length(mz_names),
            num_pixels     = nrow(full_df)
          )
        )

        progress$set(value = 100, message = "Complete!")
        add_log(sprintf("✓ artifact_id: %s", artifact_id))
        add_log(sprintf("✓ pipeline_id: %s", pipeline_id))
        add_log("=== PROCESSING COMPLETE ===")

        output$pipeline_status <- renderUI({
          div(class = "alert alert-success",
            h4("✅ Processing Complete"),
            p(strong("Study:   "), study_id),
            p(strong("Sample:  "), sample_name),
            p(strong("sample_id: "), sample_id),
            p(strong("pipeline_id: "), pipeline_id),
            p(strong("artifact_id: "), artifact_id),
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
