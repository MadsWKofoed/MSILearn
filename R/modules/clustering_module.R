# R/modules/clustering_module.R
#
# Clustering tab — provenance-aware, with original plotting/annotation logic.
#
# Provenance chain enforced:
#   study → sample → pipeline_id (binned_dataframe artifact)
#     → clustering params → clustering artifact
#     → annotation set → annotation
#
# Plotting logic: raster-based PNG overlay (from old version).
# Lasso selection: drawclosedpath + SVG path parsing (from old version).
# Annotation state: annotated_data() separate from clustered_data() (from old version).
# DB layer: fully new-schema provenance API.

library(shiny)
library(plotly)
library(sp)

MY_PALETTE <- c(
  "red","blue","orange","lightgreen","mediumpurple","brown","pink","cyan",
  "magenta","yellow","darkred","darkblue","darkgreen","darkorange","darkviolet",
  "gold","gray20","gray50","deepskyblue","springgreen","navy","maroon","olive",
  "turquoise","orchid","salmon","khaki","steelblue","seagreen","tan"
)

make_raster_png <- function(df, fill_var, colors) {
  df$x <- df$x - min(df$x) + 1
  df$y <- df$y - min(df$y) + 1
  width <- max(df$x)
  height <- max(df$y)

  mat <- matrix(NA_character_, nrow = height, ncol = width)
  mat[cbind(height - df$y + 1, df$x)] <- as.character(df[[fill_var]])

  col_img <- matrix(colors[mat], nrow = height, ncol = width)
  rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
  rgb_array <- array(NA_real_, dim = c(height, width, 4))
  rgb_array[, , 1] <- matrix(rgb_vals["red", ], nrow = height, ncol = width)
  rgb_array[, , 2] <- matrix(rgb_vals["green", ], nrow = height, ncol = width)
  rgb_array[, , 3] <- matrix(rgb_vals["blue", ], nrow = height, ncol = width)
  rgb_array[, , 4] <- matrix(rgb_vals["alpha", ], nrow = height, ncol = width)

  na_pixels <- is.na(mat)
  rgb_array[, , 4][na_pixels] <- 0

  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgb_array, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}

clustering_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Clustering",
    sidebarLayout(
      sidebarPanel(
        width = 2,

        tags$h5("Study & Sample", style = "font-weight:bold; margin-bottom:4px;"),
        selectInput(ns("study_select"), "Study", choices = c("— select —" = ""), width = "100%"),
        actionButton(ns("refresh_studies"), "↺ Refresh", class = "btn-xs"),
        tags$hr(style = "margin:6px 0;"),
        selectInput(ns("sample_select"), "Sample", choices = c("— select study first —" = ""), width = "100%"),
        tags$hr(),

        tags$h5("Processing Artifact", style = "font-weight:bold; margin-bottom:4px;"),
        selectInput(ns("pipeline_select"), "Pipeline", choices = c("— select sample first —" = ""), width = "100%"),
        uiOutput(ns("pipeline_params_ui")),
        actionButton(ns("load_dataset_btn"), "Load Dataset", class = "btn-primary btn-sm", width = "100%"),
        tags$hr(),

        fileInput(ns("histology_img"), "Histology image (optional)", accept = c("image/png", "image/jpeg", "image/jpg")),
        actionButton(ns("clear_histology"), "Clear histology", class = "btn-sm btn-warning"),
        tags$hr(),

        tags$h5("NDPI", style = "font-weight:bold; margin-bottom:4px;"),
        fileInput(ns("ndpi_file"), "NDPI slide", accept = c(".ndpi")),
        numericInput(ns("ndpi_workers"), "NDPI workers", value = 6, min = 1, max = 10, step = 1),
        actionButton(ns("start_reg_mode"), "Start landmark mode", class = "btn-sm"),
        actionButton(ns("stop_reg_mode"), "Stop landmark mode", class = "btn-sm"),
        actionButton(ns("fit_registration"), "Fit NDPI→MSI", class = "btn-sm btn-primary"),
        actionButton(ns("draw_ndpi_polygon"), "Draw NDPI polygon", class = "btn-sm"),
        actionButton(ns("reset_registration"), "Reset registration", class = "btn-sm btn-warning"),
        verbatimTextOutput(ns("registration_status")),
        tags$hr(),

        tags$h4("Clustering Configuration"),
        selectInput(ns("method"), "Clustering method", choices = c("K-means", "VSClust", "MSIClust")),
        selectInput(ns("normalize"), "Normalisation", choices = c("None" = "none", "TIC" = "tic", "Median" = "median", "RMS" = "rms")),
        numericInput(ns("clusters"), "Number of clusters", value = 3, min = 2, max = 30),
        uiOutput(ns("method_params_ui")),
        actionButton(ns("run_clustering"), "Run Clustering"),
        tags$hr(),

        tags$h5("Annotation Set", style = "font-weight:bold; margin-bottom:4px;"),
        radioButtons(ns("ann_set_mode"), NULL, choices = c("Use existing" = "existing", "Create new" = "new"), selected = "existing"),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'existing'", ns("ann_set_mode")),
          selectInput(ns("ann_set_select"), "Annotation set:", choices = c("— select study first —" = ""), width = "100%")
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'new'", ns("ann_set_mode")),
          textInput(ns("ann_set_name"), "Name:", placeholder = "e.g. Tumour vs Stroma"),
          textInput(ns("ann_set_labels"), "Labels (comma-separated):", placeholder = "Tumour, Stroma, Background"),
          actionButton(ns("create_ann_set_btn"), "Create annotation set", class = "btn-sm btn-success"),
          uiOutput(ns("ann_set_create_status"))
        ),
        tags$hr(),

        selectInput(ns("orientation"), "Orientation adjustment", choices = c("Default", "Flip X", "Flip Y", "Flip Both")),
        textInput(ns("class_label"), "Assign Class", value = "Class1"),
        actionButton(ns("assign_class"), "Assign to Selection"),
        actionButton(ns("assign_all"), "Assign ALL unassigned"),
        tags$hr(),
        actionButton(ns("commit_db"), "Commit to MongoDB", class = "btn-danger btn-sm", width = "100%"),
        uiOutput(ns("commit_status"))
      ),

      mainPanel(
        width = 10,
        tags$head(
          tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/openseadragon/5.0.1/openseadragon.min.js"),
          tags$script(src = "ndpi_viewer_sync.js"),
          tags$script(HTML(
            sprintf(
              "
              Shiny.addCustomMessageHandler('%s', function(msg){
                if(window.ndpiSyncViewer){
                  window.ndpiSyncViewer.init({
                    containerId: msg.containerId,
                    dziUrl: msg.dziUrl,
                    inputPrefix: msg.inputPrefix
                  });
                }
              });
              Shiny.addCustomMessageHandler('%s', function(msg){
                if(window.ndpiSyncViewer){
                  window.ndpiSyncViewer.setRegistrationMode(!!msg.enabled);
                }
              });
              Shiny.addCustomMessageHandler('%s', function(msg){
                if(window.ndpiSyncViewer){
                  window.ndpiSyncViewer.startPolygon();
                }
              });
              ",
              ns("ndpiLoadSlide"),
              ns("ndpiSetRegistrationMode"),
              ns("ndpiStartPolygon")
            )
          ))
        ),
        tags$div(
          tags$h5("NDPI Viewer"),
          tags$div(id = ns("ndpi_viewer"), style = "height:420px; border:1px solid #ccc; margin-bottom:10px; background:#111;")
        ),
        uiOutput(ns("cluster_layout")),
        textOutput(ns("status_text"))
      )
    )
  )
}

clustering_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_study_id    <- reactiveVal(NULL)
    active_sample_id   <- reactiveVal(NULL)
    active_pipeline_id <- reactiveVal(NULL)
    active_artifact_id <- reactiveVal(NULL)
    active_ann_set_id  <- reactiveVal(NULL)

    processed_data            <- reactiveVal(NULL)
    clustered_data            <- reactiveVal(NULL)
    annotated_data            <- reactiveVal(NULL)
    original_clustered        <- reactiveVal(NULL)
    histology_image           <- reactiveVal(NULL)
    vsclust_membership_data   <- reactiveVal(NULL)
    current_method            <- reactiveVal(NULL)

    class_colors <- reactiveVal(c())
    next_color_i <- reactiveVal(1L)

    sel_shape <- reactiveVal(NULL)

    pixel_class_state <- reactiveVal(NULL)

    reg_mode <- reactiveVal(FALSE)
    registration_state <- reactiveVal(list(
      ndpi_points = data.frame(x = numeric(), y = numeric()),
      msi_points = data.frame(x = numeric(), y = numeric()),
      fit = NULL,
      valid = FALSE,
      rms = NA_real_,
      orientation_at_fit = NULL,
      ndpi_slide_name = NULL
    ))

    ndpi_runtime <- new.env(parent = emptyenv())
    ndpi_runtime$proc <- proc
    ndpi_runtime$port <- port
    ndpi_runtime$output_dir <- out_dir

    ensure_class_colors <- function(class_vec) {
      labs <- sort(unique(class_vec))
      labs <- labs[!is.na(labs) & labs != "Unassigned"]
      cols <- class_colors()
      idx <- next_color_i()

      for (lab in labs) {
        if (!(lab %in% names(cols))) {
          cols[lab] <- MY_PALETTE[((idx - 1) %% length(MY_PALETTE)) + 1]
          idx <- idx + 1L
        }
      }

      class_colors(cols)
      next_color_i(idx)
    }

    sync_annotated_from_state <- function() {
      base_df <- original_clustered()
      show_df <- clustered_data()
      cls <- pixel_class_state()
      if (is.null(base_df) || is.null(show_df) || is.null(cls)) return(invisible(NULL))
      if (length(cls) != nrow(base_df) || nrow(show_df) != nrow(base_df)) return(invisible(NULL))
      show_df$Class <- cls
      annotated_data(show_df)
      ensure_class_colors(cls)
      invisible(NULL)
    }

    reset_class_state <- function(n) {
      pixel_class_state(rep("Unassigned", n))
      sync_annotated_from_state()
    }

    find_python_bin <- function() {
      py <- Sys.which("python")
      if (!nzchar(py)) py <- Sys.which("python3")
      if (!nzchar(py)) stop("Python executable not found in PATH.")
      py
    }

    find_free_port <- function() {
      httpuv::randomPort()
    }

    read_status_json <- function(port) {
      txt <- tryCatch(
        paste(readLines(sprintf("http://127.0.0.1:%d/status", as.integer(port)), warn = FALSE), collapse = "\n"),
        error = function(e) ""
      )
      if (!nzchar(txt)) return(NULL)
      tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
    }

    stop_ndpi_server <- function() {
      p <- ndpi_runtime$proc
      if (!is.null(p) && p$is_alive()) {
        try(p$kill(), silent = TRUE)
      }
      ndpi_runtime$proc <- NULL
      ndpi_runtime$port <- NULL
      ndpi_runtime$output_dir <- NULL
    }

    locate_preprocess_script <- function() {
      cand <- c("preprocess_server.py", "Version4/preprocess_server.py")
      hit <- cand[file.exists(cand)][1]
      if (is.na(hit) || is.null(hit)) stop("preprocess_server.py not found in project root.")
      normalizePath(hit, mustWork = TRUE)
    }

    invalidate_registration_fit <- function() {
      st <- registration_state()
      st$fit <- NULL
      st$valid <- FALSE
      st$rms <- NA_real_
      registration_state(st)
    }

    to_original_polygon <- function(poly_xy, orientation, base_df) {
      xr <- range(base_df$x, na.rm = TRUE)
      yr <- range(base_df$y, na.rm = TRUE)
      oriented_to_original_xy(
        xy = poly_xy,
        orientation = orientation,
        x_min = xr[1], x_max = xr[2], y_min = yr[1], y_max = yr[2]
      )
    }

    refresh_ann_sets <- function(study_id) {
      tryCatch({
        sets_df <- list_annotation_sets(study_id)
        if (nrow(sets_df) == 0 || !("_id" %in% names(sets_df))) {
          updateSelectInput(session, "ann_set_select", choices = c("No annotation sets found" = ""))
        } else {
          choices <- setNames(sets_df[["_id"]], sets_df$name)
          updateSelectInput(session, "ann_set_select", choices = c("— select —" = "", choices))
        }
      }, error = function(e) {
        showNotification(paste("Error loading annotation sets:", e$message), type = "error")
      })
    }

    load_studies <- function() {
      tryCatch({
        df <- get_studies()
        has_ids <- !is.null(df) && nrow(df) > 0 && "_id" %in% names(df)
        if (!has_ids) {
          updateSelectInput(session, "study_select", choices = c("— no studies found —" = ""))
          return()
        }
        choices <- setNames(df[["_id"]], df$name)
        updateSelectInput(session, "study_select", choices = c("— select —" = "", choices))
      }, error = function(e) {
        showNotification(paste("Error loading studies:", e$message), type = "error")
      })
    }

    observeEvent(input$refresh_studies, load_studies(), ignoreInit = FALSE)
    session$onFlushed(function() load_studies(), once = TRUE)

    observeEvent(input$study_select, {
      sid <- input$study_select
      if (!nzchar(sid)) {
        active_study_id(NULL)
        updateSelectInput(session, "sample_select", choices = c("— select study first —" = ""))
        return()
      }
      active_study_id(sid)
      tryCatch({
        df <- get_samples(sid)
        if (is.null(df) || nrow(df) == 0 || !("_id" %in% names(df))) {
          updateSelectInput(session, "sample_select", choices = c("— no samples —" = ""))
        } else {
          choices <- setNames(df[["_id"]], df$sample_name)
          updateSelectInput(session, "sample_select", choices = c("— select —" = "", choices))
        }
        refresh_ann_sets(sid)
      }, error = function(e) {
        showNotification(paste("Error loading samples:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$sample_select, {
      samp <- input$sample_select
      if (!nzchar(samp)) {
        active_sample_id(NULL)
        updateSelectInput(session, "pipeline_select", choices = c("— select sample first —" = ""))
        return()
      }
      active_sample_id(samp)
      tryCatch({
        pids <- list_available_pipeline_ids(samp, "binned_dataframe")
        if (length(pids) == 0) {
          updateSelectInput(session, "pipeline_select", choices = c("— no processed artifacts —" = ""))
        } else {
          labels <- vapply(pids, function(pid) {
            tryCatch({
              meta <- get_pipeline(pid)
              paste0(meta$name[1], " (", substr(pid, 1, 8), "…)")
            }, error = function(e) substr(pid, 1, 16))
          }, character(1))
          updateSelectInput(session, "pipeline_select", choices = c("— select —" = "", setNames(pids, labels)))
        }
      }, error = function(e) {
        showNotification(paste("Error listing pipelines:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    output$pipeline_params_ui <- renderUI({
      pid <- input$pipeline_select
      if (!nzchar(pid %||% "")) return(NULL)
      tryCatch({
        meta <- get_pipeline(pid)
        params <- extract_params(meta$params)
        tags$div(
          style = "font-family:monospace; font-size:11px; color:#555; word-break:break-all;",
          tags$b("Params: "),
          paste(names(params), unlist(params), sep = "=", collapse = ", ")
        )
      }, error = function(e) NULL)
    })

    observeEvent(input$pipeline_select, {
      pid <- input$pipeline_select
      active_pipeline_id(if (nzchar(pid %||% "")) pid else NULL)
    })

    observeEvent(input$load_dataset_btn, {
      samp <- active_sample_id()
      pid <- active_pipeline_id()

      if (is.null(samp) || !nzchar(samp)) {
        showNotification("Select a sample first.", type = "warning")
        return()
      }
      if (is.null(pid) || !nzchar(pid)) {
        showNotification("Select a processing pipeline first.", type = "warning")
        return()
      }

      shinyjs::disable("load_dataset_btn")
      on.exit(shinyjs::enable("load_dataset_btn"), add = TRUE)

      progress <- shiny::Progress$new(session, min = 0, max = 100)
      progress$set(message = "Loading dataset...", value = 0)
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        art_meta <- query_artifacts(sample_id = samp, stage_type = "binned_dataframe", pipeline_id = pid)
        if (nrow(art_meta) == 0) stop("No artifact found for this sample + pipeline.")
        active_artifact_id(art_meta[["_id"]][1])

        progress$set(value = 40, message = "Downloading from GridFS...")
        df <- load_artifact_by_pipeline(samp, "binned_dataframe", pid)

        progress$set(value = 90, message = "Finalising...")
        processed_data(df)
        clustered_data(NULL)
        annotated_data(NULL)
        original_clustered(NULL)
        pixel_class_state(NULL)
        class_colors(c())
        next_color_i(1L)

        invalidate_registration_fit()

        progress$set(value = 100)
        output$status_text <- renderText(
          paste0("Loaded: ", nrow(df), " pixels × ", sum(grepl("^mz_", names(df))), " features")
        )
        showNotification(
          sprintf("Dataset loaded: %d pixels × %d features.", nrow(df), sum(grepl("^mz_", names(df)))),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Load failed:", e$message), type = "error")
      })
    })

    observeEvent(input$histology_img, {
      req(input$histology_img)
      tryCatch({
        img_path <- input$histology_img$datapath
        img_data <- readBin(img_path, "raw", file.info(img_path)$size)
        ext <- tolower(tools::file_ext(input$histology_img$name))
        mime <- switch(ext, "png" = "image/png", "jpg" = "image/jpeg", "jpeg" = "image/jpeg", "image/png")
        img_uri <- paste0("data:", mime, ";base64,", base64enc::base64encode(img_data))
        histology_image(img_uri)
        showNotification("Histology image loaded.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error loading image:", e$message), type = "error")
      })
    })

    observeEvent(input$clear_histology, {
      histology_image(NULL)
      showNotification("Histology image cleared.", type = "message", duration = 2)
    })

    observeEvent(input$method, {
      if (input$method == "MSIClust" && input$normalize == "none") {
        updateSelectInput(session, "normalize", selected = "tic")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$normalize, {
      if (input$method == "MSIClust" && input$normalize == "none") {
        updateSelectInput(session, "normalize", selected = "tic")
        showNotification("MSIClust requires normalization. Switched to TIC.", type = "warning", duration = 4)
      }
    }, ignoreInit = TRUE)

    output$method_params_ui <- renderUI({
      req(input$method)
      if (input$method == "K-means") {
        helpText("K-means partitions data into k distinct clusters.")
      } else if (input$method == "VSClust") {
        tagList(
          numericInput(ns("Sds"), "Fuzziness (Sds)", value = 1.3, min = 0.5, max = 3, step = 0.01),
          numericInput(ns("minMem"), "Min membership", value = 0.5, min = 0.1, max = 1, step = 0.01),
          helpText("VSClust: fuzzy clustering with membership scores.")
        )
      } else {
        tagList(
          numericInput(ns("cor_radius"), "Correlation radius (px)", value = 1, min = 1, max = 5, step = 1),
          numericInput(ns("cor_scale"), "Correlation scale factor", value = 25, min = 1, max = 100, step = 1),
          numericInput(ns("minMem"), "Min membership", value = 0.5, min = 0.1, max = 1, step = 0.01),
          tags$div(
            class = "alert alert-warning", style = "padding:6px; font-size:12px;",
            tags$b("MSIClust requires normalization."),
            " 'None' is not supported — TIC normalization will be applied automatically."
          ),
          helpText("MSIClust: spatial correlation sets per-pixel fuzzifiers.")
        )
      }
    })

    observeEvent(input$run_clustering, {
      df <- processed_data()
      req(df)

      shinyjs::disable("run_clustering")
      on.exit(shinyjs::enable("run_clustering"), add = TRUE)

      progress <- shiny::Progress$new(session, min = 0, max = 100)
      progress$set(message = "Running clustering...", value = 0)
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        progress$set(value = 30, message = paste0("Running ", input$method, " with k=", input$clusters, "..."))
        clustered <- switch(
          input$method,
          "K-means" = {
            current_method("K-means")
            vsclust_membership_data(NULL)
            run_kmeans(df, k = input$clusters, normalize_method = input$normalize)
          },
          "VSClust" = {
            current_method("VSClust")
            result <- run_vsclust(df, k = input$clusters, normalize_method = input$normalize, Sds = input$Sds %||% 1.3, minMem = input$minMem %||% 0.5)
            vsclust_membership_data(result)
            result
          },
          "MSIClust" = {
            current_method("MSIClust")
            result <- run_msiclust(
              df,
              k = input$clusters,
              normalize_method = input$normalize,
              cor_radius = input$cor_radius %||% 1,
              cor_scale = input$cor_scale %||% 25,
              cor_cores = max(parallel::detectCores() - 1, 1),
              minMem = input$minMem %||% 0.5
            )
            vsclust_membership_data(result)
            result
          },
          stop("Unknown clustering method")
        )

        progress$set(value = 90, message = "Finalising...")
        original_clustered(clustered)
        clustered_data(clustered)
        annotated_data(NULL)
        class_colors(c())
        next_color_i(1L)

        reset_class_state(nrow(clustered))
        invalidate_registration_fit()

        progress$set(value = 100)
        norm_text <- if (input$normalize == "none") "no normalisation" else paste("with", input$normalize, "normalisation")
        output$status_text <- renderText(
          paste0("Clustering complete: ", input$method, " (", norm_text, ") — ", input$clusters, " clusters")
        )
        showNotification(paste0("Clustering complete: ", input$clusters, " clusters identified."), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Clustering error:", e$message), type = "error", duration = NULL)
      })
    })

    observeEvent(input$minMem, {
      req(current_method() %in% c("VSClust", "MSIClust"))
      req(vsclust_membership_data())
      req(clustered_data())

      tryCatch({
        df_updated <- apply_minmem_threshold(vsclust_membership_data(), input$minMem)
        original_clustered(df_updated)

        orientation <- input$orientation %||% "Default"
        if (orientation == "Default") {
          clustered_data(df_updated)
        } else {
          df_adj <- df_updated
          if (orientation %in% c("Flip X", "Flip Both")) df_adj$x <- max(df_updated$x) - df_adj$x + min(df_updated$x)
          if (orientation %in% c("Flip Y", "Flip Both")) df_adj$y <- max(df_updated$y) - df_adj$y + min(df_updated$y)
          clustered_data(df_adj)
        }

        cls <- pixel_class_state()
        if (is.null(cls) || length(cls) != nrow(df_updated)) {
          pixel_class_state(rep("Unassigned", nrow(df_updated)))
        }
        sync_annotated_from_state()
        invalidate_registration_fit()

        showNotification(paste0("Cluster assignments updated (minMem=", input$minMem, ")."), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("minMem update error:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observe({
      req(input$orientation)
      base_df <- original_clustered()
      req(base_df)

      if (input$orientation == "Default") {
        clustered_data(base_df)
      } else {
        df_adj <- base_df
        if (input$orientation %in% c("Flip X", "Flip Both")) df_adj$x <- max(base_df$x) - df_adj$x + min(base_df$x)
        if (input$orientation %in% c("Flip Y", "Flip Both")) df_adj$y <- max(base_df$y) - df_adj$y + min(base_df$y)
        clustered_data(df_adj)
      }
      sync_annotated_from_state()

      st <- registration_state()
      if (isTRUE(st$valid) && !identical(st$orientation_at_fit, input$orientation)) {
        invalidate_registration_fit()
        showNotification("Orientation changed. Registration invalidated; refit required.", type = "warning")
      }
    }) |> bindEvent(input$orientation)

    observeEvent(input$ann_set_select, {
      v <- input$ann_set_select
      if (!is.null(v) && nzchar(v)) active_ann_set_id(v) else active_ann_set_id(NULL)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$ann_set_mode, {
      if (input$ann_set_mode == "existing") {
        sid <- active_study_id()
        if (!is.null(sid) && nzchar(sid)) refresh_ann_sets(sid)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$create_ann_set_btn, {
      sid <- active_study_id()
      if (is.null(sid)) {
        showNotification("Select a study first.", type = "warning")
        return()
      }

      nm <- trimws(input$ann_set_name %||% "")
      if (!nzchar(nm)) {
        showNotification("Enter a name.", type = "warning")
        return()
      }

      labels <- trimws(strsplit(trimws(input$ann_set_labels %||% ""), ",")[[1]])
      labels <- labels[nzchar(labels)]
      if (length(labels) == 0) {
        showNotification("Enter at least one class label.", type = "warning")
        return()
      }

      tryCatch({
        ann_set_id <- upsert_annotation_set(study_id = sid, name = nm, label_schema = labels)
        active_ann_set_id(ann_set_id)
        refresh_ann_sets(sid)
        output$ann_set_create_status <- renderUI({
          tags$div(class = "alert alert-success", style = "padding:4px; font-size:12px", paste0("✓ Created: ", nm))
        })
        showNotification(paste0("Annotation set created: ", ann_set_id), type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Error creating annotation set:", e$message), type = "error")
      })
    })

    observeEvent(event_data("plotly_relayout", source = ns("cluster_src")), {
      ev <- event_data("plotly_relayout", source = ns("cluster_src"))
      req(ev)

      if (any(grepl("shapes\\[\\d+\\]\\.path$", names(ev)))) {
        path_key <- grep("shapes\\[\\d+\\]\\.path$", names(ev), value = TRUE)[1]
        path <- ev[[path_key]]
        if (!is.null(path) && nchar(path) > 0) {
          coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
          xy <- do.call(rbind, strsplit(coords, ","))
          sel_shape(list(type = "polygon", x = as.numeric(xy[, 1]), y = as.numeric(xy[, 2])))
          showNotification(paste0("✓ Polygon captured (", nrow(xy), " points)"), type = "message", duration = 2)
        }
        return()
      }

      if ("shapes" %in% names(ev)) {
        shapes_data <- ev$shapes
        if (is.data.frame(shapes_data) && nrow(shapes_data) > 0) {
          last_row <- shapes_data[nrow(shapes_data), ]
          if (!is.null(last_row$path) && nchar(last_row$path) > 0) {
            path <- last_row$path
            coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
            xy <- do.call(rbind, strsplit(coords, ","))
            sel_shape(list(type = "polygon", x = as.numeric(xy[, 1]), y = as.numeric(xy[, 2])))
            showNotification(paste0("✓ Polygon captured (", nrow(xy), " points)"), type = "message", duration = 2)
          }
        }
      }
    })

    observeEvent(input$assign_class, {
      shape <- sel_shape()
      if (is.null(shape)) {
        showNotification("No selection drawn.", type = "warning", duration = 4)
        return()
      }
      if (!identical(shape$type, "polygon")) {
        showNotification("Only polygon selection supported.", type = "error")
        return()
      }

      base_df <- original_clustered()
      req(base_df)

      cls <- pixel_class_state()
      if (is.null(cls) || length(cls) != nrow(base_df)) cls <- rep("Unassigned", nrow(base_df))

      poly_disp <- cbind(shape$x, shape$y)
      poly_orig <- to_original_polygon(poly_disp, input$orientation %||% "Default", base_df)

      res <- assign_polygon_to_pixel_classes(
        base_df = base_df,
        poly_xy_original = poly_orig,
        class_label = input$class_label,
        class_vec = cls
      )

      if (res$n_updated == 0) {
        showNotification("No pixels in selection.", type = "warning")
        return()
      }

      pixel_class_state(res$class_vec)
      sync_annotated_from_state()

      try(plotlyProxy("cluster_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = list())), silent = TRUE)
      sel_shape(NULL)

      showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, res$n_updated), type = "message", duration = 3)
    })

    observeEvent(input$assign_all, {
      base_df <- original_clustered()
      req(base_df)

      cls <- pixel_class_state()
      if (is.null(cls) || length(cls) != nrow(base_df)) cls <- rep("Unassigned", nrow(base_df))

      n_unassigned <- sum(cls == "Unassigned")
      if (n_unassigned == 0) {
        showNotification("No unassigned pixels.", type = "warning")
        return()
      }

      cls[cls == "Unassigned"] <- input$class_label
      pixel_class_state(cls)
      sync_annotated_from_state()

      showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, n_unassigned), type = "message", duration = 3)
    })

    observeEvent(input$start_reg_mode, {
      reg_mode(TRUE)
      session$sendCustomMessage(ns("ndpiSetRegistrationMode"), list(enabled = TRUE))
      showNotification("Landmark mode enabled.", type = "message")
    })

    observeEvent(input$stop_reg_mode, {
      reg_mode(FALSE)
      session$sendCustomMessage(ns("ndpiSetRegistrationMode"), list(enabled = FALSE))
      showNotification("Landmark mode disabled.", type = "message")
    })

    observeEvent(input$reset_registration, {
      registration_state(list(
        ndpi_points = data.frame(x = numeric(), y = numeric()),
        msi_points = data.frame(x = numeric(), y = numeric()),
        fit = NULL,
        valid = FALSE,
        rms = NA_real_,
        orientation_at_fit = NULL,
        ndpi_slide_name = registration_state()$ndpi_slide_name %||% NULL
      ))
      showNotification("Registration reset.", type = "message")
    })

    observeEvent(input$draw_ndpi_polygon, {
      st <- registration_state()
      if (!isTRUE(st$valid) || is.null(st$fit)) {
        showNotification("Fit registration first.", type = "warning")
        return()
      }
      if (!identical(st$orientation_at_fit, input$orientation %||% "Default")) {
        showNotification("Orientation changed since fit. Refit registration.", type = "warning")
        return()
      }
      session$sendCustomMessage(ns("ndpiStartPolygon"), list())
    })

    observeEvent(input$ndpi_file, {
      req(input$ndpi_file$datapath)

      stop_ndpi_server()

      out_dir <- tempfile("ndpi_tiles_")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      py <- find_python_bin()
      script <- locate_preprocess_script()
      port <- find_free_port()

      args <- c(
        script,
        "--slide", normalizePath(input$ndpi_file$datapath, mustWork = TRUE),
        "--output-dir", normalizePath(out_dir, mustWork = TRUE),
        "--host", "127.0.0.1",
        "--port", as.character(port),
        "--workers", as.character(input$ndpi_workers %||% 6)
      )

      proc <- processx::process$new(
        command = py,
        args = args,
        stdout = "|",
        stderr = "|",
        cleanup = TRUE
      )

      ndpi_proc(proc)
      ndpi_port(port)
      ndpi_output_dir(out_dir)

      t0 <- Sys.time()
      ready <- FALSE
      repeat {
        st <- read_status_json(port)
        if (!is.null(st) && isTRUE(st$ready)) {
          ready <- TRUE
          break
        }
        if (!proc$is_alive()) break
        if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 240) break
        Sys.sleep(0.5)
      }

      if (!ready) {
        err_tail <- tryCatch(paste(proc$read_error_lines(), collapse = "\n"), error = function(e) "")
        showNotification(paste("NDPI preprocessing failed or timed out.", err_tail), type = "error", duration = 10)
        return()
      }

      st_reg <- registration_state()
      st_reg$ndpi_slide_name <- input$ndpi_file$name
      registration_state(st_reg)

      session$sendCustomMessage(ns("ndpiLoadSlide"), list(
        containerId = ns("ndpi_viewer"),
        dziUrl = sprintf("http://127.0.0.1:%d/slide.dzi?ts=%d", port, as.integer(Sys.time())),
        inputPrefix = session$ns("")
      ))

      showNotification("NDPI ready.", type = "message")
    })

    observeEvent(input$ndpi_landmark_click, {
      req(reg_mode())
      x <- as.numeric(input$ndpi_landmark_click$x %||% NA_real_)
      y <- as.numeric(input$ndpi_landmark_click$y %||% NA_real_)
      if (!is.finite(x) || !is.finite(y)) return()

      st <- registration_state()
      st$ndpi_points <- rbind(st$ndpi_points, data.frame(x = x, y = y))
      registration_state(st)
    })

    observeEvent(event_data("plotly_click", source = ns("cluster_src")), {
      req(reg_mode())
      ev <- event_data("plotly_click", source = ns("cluster_src"))
      req(!is.null(ev$x), !is.null(ev$y))

      st <- registration_state()
      st$msi_points <- rbind(st$msi_points, data.frame(x = as.numeric(ev$x), y = as.numeric(ev$y)))
      registration_state(st)
    })

    observeEvent(input$fit_registration, {
      st <- registration_state()
      n_ndpi <- nrow(st$ndpi_points)
      n_msi <- nrow(st$msi_points)

      if (n_ndpi < 3 || n_msi < 3) {
        showNotification("Need at least 3 NDPI and 3 MSI landmarks.", type = "warning")
        return()
      }

      n <- min(n_ndpi, n_msi)
      if (n_ndpi != n_msi) {
        showNotification("Landmark counts differ. Using first matched pairs by order.", type = "warning")
      }

      ndpi_xy <- as.matrix(st$ndpi_points[seq_len(n), c("x", "y"), drop = FALSE])
      msi_xy <- as.matrix(st$msi_points[seq_len(n), c("x", "y"), drop = FALSE])

      fit <- fit_affine_ndpi_to_msi(ndpi_xy, msi_xy)
      if (!isTRUE(fit$valid)) {
        st$fit <- NULL
        st$valid <- FALSE
        st$rms <- NA_real_
        registration_state(st)
        showNotification(fit$reason %||% "Registration fit failed.", type = "error")
        return()
      }

      st$fit <- fit
      st$valid <- TRUE
      st$rms <- fit$rms
      st$orientation_at_fit <- input$orientation %||% "Default"
      registration_state(st)

      showNotification(sprintf("Registration valid. RMS = %.4f", fit$rms), type = "message")
    })

    observeEvent(input$ndpi_polygon_finished, {
      st <- registration_state()
      base_df <- original_clustered()
      req(base_df)

      if (!isTRUE(st$valid) || is.null(st$fit)) {
        showNotification("Registration is not valid.", type = "warning")
        return()
      }

      if (!identical(st$orientation_at_fit, input$orientation %||% "Default")) {
        showNotification("Orientation changed since fit. Refit registration.", type = "warning")
        return()
      }

      pts_list <- input$ndpi_polygon_finished$points
      if (is.null(pts_list) || length(pts_list) < 3) {
        showNotification("Polygon needs at least 3 points.", type = "warning")
        return()
      }

      pts <- do.call(
        rbind,
        lapply(pts_list, function(p) c(as.numeric(p$x), as.numeric(p$y)))
      )
      colnames(pts) <- c("x", "y")

      msi_oriented <- apply_affine_xy(pts, st$fit$A, st$fit$b)
      poly_orig <- to_original_polygon(msi_oriented, st$orientation_at_fit, base_df)

      cls <- pixel_class_state()
      if (is.null(cls) || length(cls) != nrow(base_df)) cls <- rep("Unassigned", nrow(base_df))

      res <- assign_polygon_to_pixel_classes(
        base_df = base_df,
        poly_xy_original = poly_orig,
        class_label = input$class_label %||% "Class1",
        class_vec = cls
      )

      pixel_class_state(res$class_vec)
      sync_annotated_from_state()

      showNotification(sprintf("NDPI polygon assigned '%s' to %d MSI pixels.", input$class_label, res$n_updated), type = "message")
    })

    output$registration_status <- renderText({
      st <- registration_state()
      paste0(
        "NDPI points: ", nrow(st$ndpi_points), "\n",
        "MSI points: ", nrow(st$msi_points), "\n",
        "Valid: ", isTRUE(st$valid), "\n",
        "RMS: ", ifelse(is.finite(st$rms), format(round(st$rms, 4), nsmall = 4), "NA"), "\n",
        "Orientation at fit: ", st$orientation_at_fit %||% "NA", "\n",
        "Slide: ", st$ndpi_slide_name %||% "NA"
      )
    })

    output$cluster_plot <- renderPlotly({
      df <- clustered_data()
      req(df)

      df$cluster <- as.character(df$cluster)
      present <- unique(df$cluster)
      present <- c(if ("No_cluster" %in% present) "No_cluster", sort(setdiff(present, "No_cluster")))
      valid <- sort(setdiff(present, "No_cluster"))
      n_valid <- length(valid)

      cols_base <- RColorBrewer::brewer.pal(max(n_valid, 3), "Set3")[seq_len(n_valid)]
      names(cols_base) <- valid
      all_colors <- c("No_cluster" = "#D9D9D9", cols_base)

      img_uri <- make_raster_png(df, "cluster", all_colors)

      x_min <- min(df$x)
      x_max <- max(df$x)
      y_min <- min(df$y)
      y_max <- max(df$y)

      x_range <- c(x_min, x_max)
      y_range <- c(y_min, y_max)
      img_x <- x_min
      img_y <- y_max
      img_sizex <- x_max - x_min
      img_sizey <- y_max - y_min

      orientation <- input$orientation %||% "Default"
      if (orientation %in% c("Flip X", "Flip Both")) {
        x_range <- rev(x_range)
        img_x <- x_max
        img_sizex <- -(x_max - x_min)
      }
      if (orientation %in% c("Flip Y", "Flip Both")) {
        y_range <- rev(y_range)
        img_y <- y_min
      }

      p <- plot_ly(source = ns("cluster_src")) |>
        add_trace(
          x = df$x, y = df$y,
          type = "scattergl",
          mode = "markers",
          marker = list(size = 7, color = "rgba(0,0,0,0)"),
          hoverinfo = "skip",
          showlegend = FALSE,
          inherit = FALSE
        ) |>
        layout(
          images = list(list(
            source = img_uri, xref = "x", yref = "y",
            x = img_x, y = img_y,
            sizex = img_sizex, sizey = img_sizey,
            sizing = "stretch", layer = "below"
          )),
          dragmode = "drawclosedpath",
          newshape = list(line = list(color = "black", width = 1), fillcolor = "rgba(0,0,0,0.05)"),
          title = "MSI Clustering Result",
          xaxis = list(range = x_range, title = "x"),
          yaxis = list(range = y_range, title = "y", scaleanchor = "x", scaleratio = 1),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top")
        ) |>
        config(
          displaylogo = FALSE,
          modeBarButtonsToAdd = list("drawclosedpath", "eraseshape"),
          modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines", "toImage", "select2d", "lasso2d")
        )

      for (cls in present) {
        p <- p |>
          add_trace(
            x = x_min - 1000, y = y_min - 1000,
            type = "scatter",
            mode = "markers",
            marker = list(size = 10, color = all_colors[[cls]]),
            name = if (cls == "No_cluster") "No cluster" else paste("Cluster", cls),
            showlegend = TRUE,
            hoverinfo = "skip",
            inherit = FALSE
          )
      }
      p <- event_register(p, "plotly_click")
      p <- event_register(p, "plotly_relayout")
      p
    })

    output$class_plot <- renderPlotly({
      df <- annotated_data() %||% clustered_data()
      req(df)

      if (!"Class" %in% names(df)) {
        df$Class <- "Unassigned"
      } else {
        df$Class[is.na(df$Class)] <- "Unassigned"
      }
      df$Class <- as.character(df$Class)

      cols_used <- class_colors()
      cols_used <- cols_used[names(cols_used) != "Unassigned"]

      present <- unique(df$Class)
      present <- c(if ("Unassigned" %in% present) "Unassigned", sort(setdiff(present, "Unassigned")))

      all_colors <- c("Unassigned" = "#B8BFFC")
      for (cls in present) {
        if (cls != "Unassigned") all_colors[cls] <- cols_used[[cls]]
      }

      img_uri <- make_raster_png(df, "Class", all_colors)

      x_min <- min(df$x)
      x_max <- max(df$x)
      y_min <- min(df$y)
      y_max <- max(df$y)

      x_range <- c(x_min, x_max)
      y_range <- c(y_min, y_max)
      img_x <- x_min
      img_y <- y_max
      img_sizex <- x_max - x_min
      img_sizey <- y_max - y_min

      orientation <- input$orientation %||% "Default"
      if (orientation %in% c("Flip X", "Flip Both")) {
        x_range <- rev(x_range)
        img_x <- x_max
        img_sizex <- -(x_max - x_min)
      }
      if (orientation %in% c("Flip Y", "Flip Both")) {
        y_range <- rev(y_range)
        img_y <- y_min
      }

      p <- plot_ly() |>
        layout(
          images = list(list(
            source = img_uri, xref = "x", yref = "y",
            x = img_x, y = img_y,
            sizex = img_sizex, sizey = img_sizey,
            sizing = "stretch", layer = "below"
          )),
          title = "Class Assignment",
          xaxis = list(range = x_range, title = "x"),
          yaxis = list(range = y_range, title = "y", scaleanchor = "x", scaleratio = 1),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top")
        ) |>
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines", "toImage", "select2d", "lasso2d")
        )

      for (cls in present) {
        p <- p |>
          add_trace(
            x = x_min - 1000, y = y_min - 1000,
            type = "scatter",
            mode = "markers",
            marker = list(size = 10, color = all_colors[[cls]]),
            name = cls,
            showlegend = TRUE,
            hoverinfo = "skip"
          )
      }

      p
    })

    output$histology_plot <- renderPlot({
      img_uri <- histology_image()
      df <- clustered_data()
      req(img_uri, df)

      x_min <- min(df$x)
      x_max <- max(df$x)
      y_min <- min(df$y)
      y_max <- max(df$y)

      img_data <- sub("^data:image/[a-z]+;base64,", "", img_uri)
      img_raw <- base64enc::base64decode(img_data)
      tmp_img <- tempfile(fileext = ".png")
      writeBin(img_raw, tmp_img)

      img <- tryCatch(
        if (grepl("data:image/png", img_uri)) png::readPNG(tmp_img) else jpeg::readJPEG(tmp_img),
        error = function(e) NULL
      )
      if (is.null(img)) {
        plot.new()
        text(0.5, 0.5, "Error loading image", cex = 1.5)
        return()
      }

      par(mar = c(4, 4, 3, 1))
      plot(NULL, xlim = c(x_min, x_max), ylim = c(y_min, y_max), xlab = "x", ylab = "y", main = "Histology", asp = 1)
      graphics::rasterImage(img, x_min, y_min, x_max, y_max)
    })

    output$cluster_layout <- renderUI({
      req(clustered_data())
      has_hist <- !is.null(histology_image())
      if (has_hist) {
        tagList(
          fluidRow(
            column(6, plotlyOutput(ns("cluster_plot"), height = "600px")),
            column(6, plotOutput(ns("histology_plot"), height = "600px"))
          ),
          fluidRow(
            column(6, plotlyOutput(ns("class_plot"), height = "600px"))
          )
        )
      } else {
        fluidRow(
          column(6, plotlyOutput(ns("cluster_plot"), height = "600px")),
          column(6, plotlyOutput(ns("class_plot"), height = "600px"))
        )
      }
    })

    observeEvent(input$commit_db, {
      study_id <- active_study_id()
      sample_id <- active_sample_id()
      artifact_id <- active_artifact_id()
      ann_set_id <- active_ann_set_id()
      pid <- active_pipeline_id()
      base_df <- original_clustered()
      cls <- pixel_class_state()

      if (is.null(study_id)) {
        showNotification("No study selected.", type = "error")
        return()
      }
      if (is.null(sample_id)) {
        showNotification("No sample selected.", type = "error")
        return()
      }
      if (is.null(pid)) {
        showNotification("No pipeline selected.", type = "error")
        return()
      }
      if (is.null(base_df)) {
        showNotification("Run clustering first.", type = "error")
        return()
      }
      if (is.null(ann_set_id)) {
        showNotification("Select or create an annotation set.", type = "error")
        return()
      }
      if (is.null(cls) || length(cls) != nrow(base_df)) {
        showNotification("Annotation state is not initialized.", type = "error")
        return()
      }

      df_to_save <- base_df
      df_to_save$Class <- as.character(cls)
      annotation_df <- df_to_save[df_to_save$Class != "Unassigned", c("x", "y", "Class"), drop = FALSE]

      if (nrow(annotation_df) == 0) {
        showNotification("No pixels assigned a class yet.", type = "warning")
        return()
      }

      shinyjs::disable("commit_db")
      on.exit(shinyjs::enable("commit_db"), add = TRUE)

      progress <- shiny::Progress$new(session, min = 0, max = 100)
      progress$set(message = "Committing...", value = 0)
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        progress$set(value = 20, message = "Registering clustering pipeline...")

        method <- current_method() %||% input$method
        cluster_params <- list(
          method = method,
          k = input$clusters,
          normalize = input$normalize,
          input_pipeline_id = pid
        )
        if (method %in% c("VSClust", "MSIClust")) {
          cluster_params$minMem <- input$minMem
          if (method == "VSClust") cluster_params$Sds <- input$Sds
          if (method == "MSIClust") cluster_params$cor_radius <- input$cor_radius
        }

        cluster_pid <- upsert_pipeline(
          type = "clustering",
          name = paste0(method, "_k", input$clusters),
          params = cluster_params,
          code_version = "dev"
        )

        progress$set(value = 50, message = "Saving clustering artifact...")

        art_id <- save_clustering_artifact(
          clustered_df = df_to_save,
          study_id = study_id,
          sample_id = sample_id,
          input_artifact_id = artifact_id %||% "",
          cluster_pipeline_id = cluster_pid
        )

        progress$set(value = 75, message = "Saving annotations...")

        ann_id <- upsert_annotation(
          annotation_df = annotation_df,
          sample_id = sample_id,
          annotation_set_id = ann_set_id
        )

        progress$set(value = 90, message = "Writing metadata...")

        tryCatch({
          .insert(.con("clustering_metadata", DB_NAME, MONGO_URL), list(
            assignment_id = art_id,
            study_id = study_id,
            sample_id = sample_id,
            pipeline_id = pid,
            cluster_pipeline_id = cluster_pid,
            clustering_method = method,
            k = input$clusters,
            normalize = input$normalize,
            annotation_set_id = ann_set_id,
            annotation_id = ann_id,
            n_annotated = nrow(annotation_df),
            created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          ))
        }, error = function(e) NULL)

        st <- registration_state()
        if (isTRUE(st$valid) && !is.null(st$fit)) {
          tryCatch({
            .insert(.con("ndpi_registrations", DB_NAME, MONGO_URL), list(
              sample_id = sample_id,
              pipeline_id = pid,
              ndpi_slide_name = st$ndpi_slide_name %||% NA_character_,
              orientation = st$orientation_at_fit %||% NA_character_,
              rms = st$rms,
              ndpi_landmarks = st$ndpi_points,
              msi_landmarks = st$msi_points,
              affine_A = unname(as.vector(st$fit$A)),
              affine_b = unname(st$fit$b),
              created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            ))
          }, error = function(e) NULL)
        }

        progress$set(value = 100)

        output$commit_status <- renderUI(
          tags$div(
            style = "color:green; margin-top:6px;",
            tags$b("✓ Committed"), tags$br(),
            tags$small("Artifact: ", substr(art_id, 1, 12), "…"), tags$br(),
            tags$small("Annotation: ", substr(ann_id, 1, 12), "…")
          )
        )
        showNotification("Committed to database.", type = "message")
      }, error = function(e) {
        output$commit_status <- renderUI(tags$span(style = "color:red", "Error: ", e$message))
        showNotification(paste("Commit failed:", e$message), type = "error", duration = NULL)
      })
    })

    session$onSessionEnded(function() {
      stop_ndpi_server()
    })
  })
}