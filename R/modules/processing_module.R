# R/modules/processing_module.R

processing_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Processing",
    fluidRow(
      # ── Sidebar (2/12) ──────────────────────────────────────────────────────
      column(2,
        h4("Data Source"),
        radioButtons(ns("data_source"), "Select data source:",
          choices  = c("Upload new files", "Use existing dataset"),
          selected = "Upload new files"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'Upload new files'", ns("data_source")),
          fileInput(ns("msi_files"), "Upload imzML + ibd files",
                    multiple = TRUE, accept = c(".imzML", ".ibd")),
          textInput(ns("sample_name_upload"), "Sample name (optional):",
                    placeholder = "Leave empty to use filename")
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'Use existing dataset'", ns("data_source")),
          selectInput(ns("existing_sample"), "Select sample:", choices = "Loading..."),
          textOutput(ns("existing_info"))
        ),

        hr(),
        h4("Processing Parameters"),
        numericInput(ns("resolution"), "Resolution (ppm):",
                     value = 10, min = 1, max = 100, step = 1),
        numericInput(ns("snr"), "SNR:",
                     value = 3, min = 1.5, max = 30, step = 0.1),
        numericInput(ns("tolerance"), "Binning tolerance:",
                     value = 0.5, min = 0.1, max = 3, step = 0.1),

        radioButtons(ns("ref_source"), "Reference list source:",
          choices  = c("From database", "Upload your own"),
          selected = "From database"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Upload your own'", ns("ref_source")),
          fileInput(ns("ref_csv"), "Upload m/z reference list (.csv)",
                    multiple = FALSE, accept = ".csv")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'From database'", ns("ref_source")),
          selectInput(ns("ref_csv_mongo"), "Select reference list:", choices = "Loading...")
        ),

        hr(),
        actionButton(ns("run_processing"), "Run Processing", class = "btn-primary"),
        br(), br(),
        actionButton(ns("clear_cache"), "Clear local cache", class = "btn-warning")
      ),

      # ── Log / status (4/12) ─────────────────────────────────────────────────
      column(4,
        h3("Processing Pipeline Status"),
        uiOutput(ns("pipeline_status")),
        hr(),
        h4("Processing Log"),
        verbatimTextOutput(ns("processing_log")),
        hr(),
        h4("Cache Status"),
        verbatimTextOutput(ns("cache_status"))
      ),

      # ── Plots (6/12) ────────────────────────────────────────────────────────
      column(6,
        wellPanel(
          h4("MSI Images - Top 3 m/z (by variance)"),
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

    # ── MongoDB ───────────────────────────────────────────────────────────────
    mongo_ref  <- mongo(collection = "mz_references",
                        db = "msi_project", url = "mongodb://localhost:27018")
    mongo_meta <- mongo(collection = "processing_artifacts_metadata",
                        db = "MSI_database", url = "mongodb://localhost:27018")

    # ── Reactive state ────────────────────────────────────────────────────────
    processing_log      <- reactiveVal("")
    current_cache_dir   <- reactiveVal(NULL)
    current_sample_name <- reactiveVal(NULL)
    plot_top3_raw       <- reactiveVal(NULL)
    plot_top3_norm      <- reactiveVal(NULL)
    plot_distance_binned  <- reactiveVal(NULL)
    plot_distance_scatter <- reactiveVal(NULL)

    # ── Helpers ───────────────────────────────────────────────────────────────
    add_log <- function(msg) {
      processing_log(paste0(processing_log(),
                             format(Sys.time(), "[%H:%M:%S]"), " ", msg, "\n"))
    }

    cleanup_cardinal_temp <- function() {
      tryCatch({
        temp_files <- list.files(
          tempdir(),
          pattern   = "(imzml_|Cardinal|matter_array|msi_run_)",
          full.names = TRUE, recursive = TRUE
        )
        if (length(temp_files) > 0) {
          total_mb <- sum(file.size(temp_files), na.rm = TRUE) / 1024^2
          unlink(temp_files, recursive = TRUE)
          add_log(sprintf("✓ System temp cleaned: %.2f MB", total_mb))
        }
        gc()
      }, error = function(e) invisible(NULL))
    }

    # ── Reference dropdown ────────────────────────────────────────────────────
    observe({
      refs <- unique(mongo_ref$find(
        fields = '{"_id": 0, "reference_name": 1}'
      )$reference_name)
      if (length(refs) == 0) refs <- "No references found"
      updateSelectInput(session, "ref_csv_mongo", choices = refs)
    })

    # ── Sample dropdown (existing) ────────────────────────────────────────────
    available_samples <- reactive({
      input$data_source
      arts <- mongo_meta$find(
        query  = '{"stage_type": "raw_files"}',
        fields = '{"_id": 0, "sample_name": 1}'
      )
      if (nrow(arts) == 0) "No samples in database" else unique(arts$sample_name)
    })

    observe({
      updateSelectInput(session, "existing_sample", choices = available_samples())
    })

    # ── Existing sample info ──────────────────────────────────────────────────
    output$existing_info <- renderText({
      req(input$data_source == "Use existing dataset", input$existing_sample)

      raw_arts <- mongo_meta$find(jsonlite::toJSON(list(
        sample_name = input$existing_sample, stage_type = "raw_files"
      ), auto_unbox = TRUE))

      proc_arts <- mongo_meta$find(jsonlite::toJSON(list(
        sample_name = input$existing_sample, stage_type = "binned_dataframe"
      ), auto_unbox = TRUE))

      parts <- character(0)

      if (nrow(raw_arts) > 0) {
        parts <- c(parts, sprintf("✓ Raw files in database (uploaded: %s)",
                                  as.character(raw_arts$created_at[nrow(raw_arts)])))
      }

      if (nrow(proc_arts) > 0) {
        parts <- c(parts,
          sprintf("\n%d processed version(s):", nrow(proc_arts)),
          sapply(seq_len(nrow(proc_arts)), function(i) {
            sprintf("  - Res: %.0f ppm, SNR: %.1f, Tol: %.2f, Ref: %s",
                    proc_arts$resolution[i], proc_arts$snr[i],
                    proc_arts$tolerance[i], proc_arts$reference_name[i])
          })
        )
      } else {
        parts <- c(parts, "\nNo processed versions exist yet")
      }

      paste(parts, collapse = "\n")
    })

    # ── Selected reference ────────────────────────────────────────────────────
    selected_mz <- reactive({
      req(input$ref_source)
      if (input$ref_source == "Upload your own") {
        req(input$ref_csv)
        df <- read.csv(input$ref_csv$datapath, stringsAsFactors = FALSE)
        list(mz   = as.numeric(df$mz),
             name = tools::file_path_sans_ext(basename(input$ref_csv$name)))
      } else {
        req(input$ref_csv_mongo)
        doc <- mongo_ref$find(
          sprintf('{"reference_name": "%s"}', input$ref_csv_mongo),
          fields = '{"_id": 0, "mz_values": 1}'
        )
        if (nrow(doc) == 0) return(NULL)
        list(mz   = as.numeric(unlist(doc$mz_values[[1]])),
             name = input$ref_csv_mongo)
      }
    })

    # ── Current sample name ───────────────────────────────────────────────────
    current_sample <- reactive({
      if (input$data_source == "Upload new files") {
        req(input$msi_files)
        imzml_name <- input$msi_files$name[
          grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)][1]
        if (nchar(input$sample_name_upload) > 0) input$sample_name_upload else imzml_name
      } else {
        req(input$existing_sample)
        input$existing_sample
      }
    })

    # ── Clear cache button ────────────────────────────────────────────────────
    observeEvent(input$clear_cache, {
      cache_dir <- current_cache_dir()
      if (is.null(cache_dir) || !dir.exists(cache_dir)) {
        showNotification("No active cache to clear", type = "message", duration = 3)
        return()
      }
      files      <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
      cache_size <- sum(file.size(files)) / 1024^2
      unlink(cache_dir, recursive = TRUE)
      current_cache_dir(NULL)

      temp_files <- list.files(tempdir(),
        pattern = "(imzml_|Cardinal|matter_array|msi_run_)",
        full.names = TRUE, recursive = TRUE)
      temp_size <- if (length(temp_files) > 0) {
        s <- sum(file.size(temp_files), na.rm = TRUE) / 1024^2
        unlink(temp_files, recursive = TRUE); s
      } else 0

      plot_top3_raw(NULL); plot_top3_norm(NULL)
      plot_distance_binned(NULL); plot_distance_scatter(NULL)
      gc()

      showNotification(
        sprintf("✓ Cleared: %.2f MB freed", cache_size + temp_size),
        type = "message", duration = 5
      )
    })

    # ── Main processing pipeline ──────────────────────────────────────────────
    observeEvent(input$run_processing, {
      mz_ref      <- selected_mz()
      sample_name <- current_sample()

      if (is.null(mz_ref) || is.null(sample_name)) {
        showNotification("Please configure all parameters first",
                         type = "error", duration = NULL)
        return()
      }

      # Exact-match dedup check
      exact_match <- mongo_meta$find(jsonlite::toJSON(list(
        sample_name    = sample_name,
        stage_type     = "binned_dataframe",
        resolution     = as.numeric(input$resolution),
        snr            = as.numeric(input$snr),
        tolerance      = as.numeric(input$tolerance),
        reference_name = mz_ref$name
      ), auto_unbox = TRUE))

      if (nrow(exact_match) > 0) {
        showNotification(
          "This exact processing already exists. No action needed.",
          type = "warning", duration = 10
        )
        return()
      }

      # Reset plots
      plot_top3_raw(NULL); plot_top3_norm(NULL)
      plot_distance_binned(NULL); plot_distance_scatter(NULL)

      shinyjs::disable("run_processing")
      on.exit(shinyjs::enable("run_processing"), add = TRUE)

      progress <- Progress$new(session, min = 0, max = 100)
      progress$set(message = "Starting processing pipeline...", value = 0)
      on.exit(progress$close(), add = TRUE)

      processing_log("")
      cleanup_cardinal_temp()

      tryCatch({
        add_log("=== PROCESSING STARTED ===")
        add_log(sprintf("Sample: %s", sample_name))
        add_log(sprintf("Res=%d ppm | SNR=%.1f | Tol=%.2f | Ref=%s",
                        input$resolution, input$snr, input$tolerance, mz_ref$name))

        # Throw-away working dir — deleted on handler exit
        work_dir <- tempfile("msi_run_")
        dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(
          tryCatch(unlink(work_dir, recursive = TRUE), error = function(e) NULL),
          add = TRUE
        )
        current_cache_dir(work_dir)
        current_sample_name(sample_name)
        add_log(sprintf("Work dir: %s", work_dir))

        # ── STEP 1: Raw files ───────────────────────────────────────────────
        progress$set(value = 10, message = "Handling raw data...")

        if (input$data_source == "Upload new files") {
          req(input$msi_files)
          files     <- input$msi_files
          imzml_idx <- grepl("\\.imzML$", files$name, ignore.case = TRUE)
          ibd_idx   <- grepl("\\.ibd$",   files$name, ignore.case = TRUE)

          if (!any(imzml_idx) || !any(ibd_idx))
            stop("Both imzML and ibd files are required")

          existing_raw <- mongo_meta$find(jsonlite::toJSON(list(
            sample_name = sample_name, stage_type = "raw_files"
          ), auto_unbox = TRUE))

          if (nrow(existing_raw) > 0) {
            add_log("⚠ Raw files already in database — skipping upload")
          } else {
            add_log("Uploading raw files to MongoDB...")
            save_raw_pair_to_mongo(
              sample_name = sample_name,
              imzml_path  = files$datapath[imzml_idx][1],
              ibd_path    = files$datapath[ibd_idx][1],
              db_name     = "MSI_database"
            )
            add_log("✓ Raw files saved")
          }
        }

        # ── STEP 2: Load raw from MongoDB ───────────────────────────────────
        progress$set(value = 25, message = "Loading MSI object...")
        add_log("Downloading raw files from MongoDB...")

        msi_data <- load_raw_object_from_mongo(
          sample_name = sample_name,
          workdir     = work_dir,
          db_name     = "MSI_database",
          resolution  = as.numeric(input$resolution)
        )
        add_log(sprintf("✓ MSI loaded: %d pixels × %d m/z values",
                        ncol(msi_data), nrow(msi_data)))

        # ── STEP 3: Mean → peakPick → align ─────────────────────────────────
        progress$set(value = 45, message = "Mean spectrum + peak picking + alignment...")
        add_log("Computing mean spectrum...")
        control_mean <- Cardinal::summarizeFeatures(msi_data, "mean")

        add_log(sprintf("Peak picking (SNR=%.1f) + aligning (tol=%.2f)...",
                        input$snr, input$tolerance))
        processing_run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))

        control_MSI_ref <- control_mean |>
          Cardinal::peakPick(SNR = input$snr) |>
          Cardinal::peakAlign(ref      = mz_ref$mz,
                              tolerance = input$tolerance,
                              units    = "mz") |>
          Cardinal::subsetFeatures() |>
          Cardinal::process()
        add_log(sprintf("✓ Reference aligned: %d m/z bins", nrow(control_MSI_ref)))

        # ── STEP 4: Bin full dataset ─────────────────────────────────────────
        progress$set(value = 70, message = "Binning full dataset...")
        add_log("Binning MSI data...")

        msi_data_binned <- Cardinal::bin(
          msi_data,
          ref       = Cardinal::mz(control_MSI_ref),
          tolerance = input$tolerance,
          units     = "mz",
          BPPARAM   = BiocParallel::bpparam()
        ) |> Cardinal::process()
        add_log("✓ Data binned")

        # ── STEP 5: TIC normalization + plots ────────────────────────────────
        progress$set(value = 82, message = "Generating plots...")
        add_log("Normalizing (TIC) and generating plots...")

        spec_mat     <- as.matrix(Cardinal::spectra(msi_data_binned))  # features × pixels
        tic          <- colSums(spec_mat, na.rm = TRUE)
        tic[!is.finite(tic) | tic <= 0] <- NA_real_
        spec_mat_tic <- sweep(spec_mat, 2, tic, "/")

        mz_vals <- Cardinal::mz(msi_data_binned)

        var_raw  <- apply(spec_mat,     1, var, na.rm = TRUE)
        var_norm <- apply(spec_mat_tic, 1, var, na.rm = TRUE)

        top3_mz      <- mz_vals[order(var_raw,  decreasing = TRUE)[1:3]]
        norm_top3_mz <- mz_vals[order(var_norm, decreasing = TRUE)[1:3]]

        coords_df <- Cardinal::coord(msi_data_binned)

        # Extract image data NOW while .ibd file still exists in work_dir
        # Build pixel × mz matrices for the top 3 m/z only — tiny memory footprint
        top3_idx      <- match(top3_mz,      mz_vals)
        norm_top3_idx <- match(norm_top3_mz, mz_vals)

        make_image_df <- function(idx, coords, mat) {
          data.frame(
            x     = coords$x,
            y     = coords$y,
            mz1   = mat[idx[1], ],
            mz2   = mat[idx[2], ],
            mz3   = mat[idx[3], ]
          )
        }

        img_df_raw  <- make_image_df(top3_idx,      coords_df, spec_mat)
        img_df_norm <- make_image_df(norm_top3_idx, coords_df, spec_mat_tic)

        # Labels for legend
        raw_labels  <- paste0("mz=", round(top3_mz,      2))
        norm_labels <- paste0("mz=", round(norm_top3_mz, 2))

        make_overlay_plot <- function(df, lbl, title) {
          scale01 <- function(v) {
            v[!is.finite(v)] <- 0
            lo <- min(v); hi <- max(v)
            if (hi == lo) return(rep(0, length(v)))
            (v - lo) / (hi - lo)
          }
          r <- scale01(df$mz1)
          g <- scale01(df$mz2)
          b <- scale01(df$mz3)
          pixel_cols <- rgb(r, g, b,
                            alpha        = pmax(r, g, b) * 0.9 + 0.1,
                            maxColorValue = 1)
          list(x = df$x, y = df$y, cols = pixel_cols, lbl = lbl, title = title)
        }

        plot_top3_raw(make_overlay_plot(img_df_raw,  raw_labels,  "Top 3 m/z (raw variance)"))
        plot_top3_norm(make_overlay_plot(img_df_norm, norm_labels, "Top 3 m/z (TIC-normalized variance)"))

        # ── STEP 6: Spatial distance plots ───────────────────────────────────
        add_log("Calculating spatial vs intensity distances...")

        coords_df       <- Cardinal::coord(msi_data_binned)
        norm_msi_matrix <- cbind(x = coords_df$x, y = coords_df$y, t(spec_mat_tic))
        valid_rows      <- complete.cases(norm_msi_matrix)
        norm_msi_matrix <- norm_msi_matrix[valid_rows, , drop = FALSE]
        add_log(sprintf("Using %d/%d valid pixels for distance calc",
                        sum(valid_rows), length(valid_rows)))

        n_pairs <- 10000L
        n       <- nrow(norm_msi_matrix)
        pairs   <- data.frame(i = sample(n, n_pairs, replace = TRUE),
                              j = sample(n, n_pairs, replace = TRUE))
        pairs   <- subset(pairs, i != j)
        pairs   <- unique(data.frame(i = pmin(pairs$i, pairs$j),
                                     j = pmax(pairs$i, pairs$j)))
        if (nrow(pairs) > n_pairs) pairs <- pairs[seq_len(n_pairs), ]

        xy    <- norm_msi_matrix[, c("x", "y"), drop = FALSE]
        intens <- norm_msi_matrix[, -c(1, 2), drop = FALSE]

        space_dist <- sqrt(rowSums(
          (xy[pairs$i, , drop = FALSE] - xy[pairs$j, , drop = FALSE])^2
        ))

        cosine_dist <- function(a, b) {
          na <- sqrt(sum(a^2, na.rm = TRUE))
          nb <- sqrt(sum(b^2, na.rm = TRUE))
          if (!is.finite(na) || !is.finite(nb) || na == 0 || nb == 0) return(NA_real_)
          1 - sum(a * b, na.rm = TRUE) / (na * nb)
        }
        intens_dist <- mapply(function(i, j) cosine_dist(intens[i, ], intens[j, ]),
                              pairs$i, pairs$j)

        df_dist <- data.frame(space_distance = space_dist,
                              intensity_distance = intens_dist)
        
        # Drop NA pairs before plotting
        df_dist <- df_dist[is.finite(df_dist$space_distance) &
                           is.finite(df_dist$intensity_distance), ]

        add_log(sprintf("Distance pairs for scatter: %d", nrow(df_dist)))

        df_binned <- df_dist |>
          dplyr::mutate(bin = cut(space_distance, breaks = 50L)) |>
          dplyr::group_by(bin) |>
          dplyr::summarise(
            space_mid  = mean(space_distance,    na.rm = TRUE),
            int_median = median(intensity_distance, na.rm = TRUE),
            int_q25    = quantile(intensity_distance, 0.25, na.rm = TRUE),
            int_q75    = quantile(intensity_distance, 0.75, na.rm = TRUE),
            .groups = "drop"
          )

        plot_distance_binned(
          ggplot(df_binned, aes(x = space_mid, y = int_median)) +
            geom_line() +
            geom_ribbon(aes(ymin = int_q25, ymax = int_q75), alpha = 0.2) +
            theme_bw() +
            labs(x = "Euclidean pixel distance",
                 y = "Cosine distance (median ± IQR)",
                 title = "Spatial vs Intensity Distance (binned)")
        )

        plot_distance_scatter(
          ggplot(df_dist, aes(x = space_distance, y = intensity_distance)) +
            geom_point(alpha = 0.2, size = 1) +
            theme_bw() +
            labs(x = "Euclidean pixel distance",
                 y = "Cosine distance",
                 title = "Spatial vs Intensity Distance (10k pairs)")
        )
        add_log("✓ Plots generated")

        # ── STEP 7: Feature matrix → MongoDB ────────────────────────────────
        progress$set(value = 92, message = "Building feature matrix...")
        add_log("Building feature matrix...")

        msi_matrix  <- t(spec_mat)   # reuse already-materialized matrix
        mz_names    <- paste0("mz_", mz_vals)   # reuse already-fetched mz_vals
        coords2     <- as.data.frame(coords_df)  # reuse already-fetched coords
        pixel_names <- rep(Cardinal::runNames(msi_data_binned), nrow(msi_matrix))

        full_df <- data.frame(
          runNames = pixel_names,
          x        = coords2$x,
          y        = coords2$y,
          msi_matrix,
          check.names = FALSE
        )
        colnames(full_df) <- c("runNames", "x", "y", mz_names)

        save_stage_to_mongo(
          full_df, processing_run_id, "binned_dataframe",
          sample_name = sample_name,
          params = list(
            snr            = as.numeric(input$snr),
            tolerance      = as.numeric(input$tolerance),
            reference_name = mz_ref$name,
            resolution     = as.numeric(input$resolution),
            num_features   = length(mz_names),
            num_pixels     = nrow(full_df)
          ),
          db_name = "MSI_database"
        )

        add_log(sprintf("✓ Saved: %d pixels × %d features",
                        nrow(full_df), length(mz_names)))

        progress$set(value = 100, message = "Complete!")
        add_log("=== PROCESSING COMPLETE ===")
        add_log(sprintf("Run ID: %s", processing_run_id))

        output$pipeline_status <- renderUI({
          div(class = "alert alert-success",
            h4("✅ Processing Complete"),
            p(sprintf("Sample: %s",       sample_name)),
            p(sprintf("Run ID: %s",       processing_run_id)),
            p(sprintf("Resolution: %d ppm", input$resolution)),
            p(sprintf("SNR: %.1f",         input$snr)),
            p(sprintf("Tolerance: %.2f",   input$tolerance)),
            p(sprintf("Reference: %s",     mz_ref$name)),
            p(sprintf("Features: %d m/z bins", length(mz_names))),
            p(sprintf("Pixels: %d",        nrow(full_df)))
          )
        })

        showNotification(
          sprintf("✅ Processing complete! %d features | Run: %s",
                  length(mz_names), processing_run_id),
          type = "message", duration = 10
        )

      }, error = function(e) {
        add_log(sprintf("❌ ERROR: %s", e$message))
        showNotification(paste("Processing error:", e$message),
                         type = "error", duration = NULL)
      })
    })

    # ── Render outputs ────────────────────────────────────────────────────────
    output$processing_log <- renderText({ processing_log() })

    output$cache_status <- renderText({
      cache_dir <- current_cache_dir()
      if (is.null(cache_dir) || !dir.exists(cache_dir)) return("No active cache")
      files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
      if (length(files) == 0) return(sprintf("Work dir: %s\n(empty)", cache_dir))
      sprintf("Work dir: %s\nFiles: %d | Size: %.2f MB\nSample: %s",
              cache_dir, length(files),
              sum(file.size(files)) / 1024^2,
              current_sample_name() %||% "None")
    })

    output$top3_raw_plot <- renderPlot({
      req(plot_top3_raw())
      p <- plot_top3_raw()
      par(bg = "black", col.axis = "white", col.lab = "white",
          col.main = "white", fg = "white", mar = c(3, 3, 2, 1))
      plot(p$x, p$y, col = p$cols, pch = 15, cex = 0.5,
           xlab = "x", ylab = "y", main = p$title, asp = 1, axes = FALSE)
      axis(1, col = "white", col.ticks = "white", col.axis = "white")
      axis(2, col = "white", col.ticks = "white", col.axis = "white")
      legend("topright", legend = p$lbl, col = c("red", "green", "blue"),
             pch = 15, pt.cex = 1.5, text.col = "white",
             bg = adjustcolor("black", alpha.f = 0.6), box.col = "white")
    })

    output$top3_norm_plot <- renderPlot({
      req(plot_top3_norm())
      p <- plot_top3_norm()
      par(bg = "black", col.axis = "white", col.lab = "white",
          col.main = "white", fg = "white", mar = c(3, 3, 2, 1))
      plot(p$x, p$y, col = p$cols, pch = 15, cex = 0.5,
           xlab = "x", ylab = "y", main = p$title, asp = 1, axes = FALSE)
      axis(1, col = "white", col.ticks = "white", col.axis = "white")
      axis(2, col = "white", col.ticks = "white", col.axis = "white")
      legend("topright", legend = p$lbl, col = c("red", "green", "blue"),
             pch = 15, pt.cex = 1.5, text.col = "white",
             bg = adjustcolor("black", alpha.f = 0.6), box.col = "white")
    })

    output$distance_binned_plot <- renderPlot({
      req(plot_distance_binned())
      plot_distance_binned()
    })

    output$distance_scatter_plot <- renderPlot({
      req(plot_distance_scatter())
      plot_distance_scatter()
    })
  })
}