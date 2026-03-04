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

# ─────────────────────────────────────────────────────────────────────────────
# COLOUR PALETTE  (old version: 30-colour named palette)
# ─────────────────────────────────────────────────────────────────────────────

MY_PALETTE <- c(
  "red","blue","orange","lightgreen","mediumpurple","brown","pink","cyan",
  "magenta","yellow","darkred","darkblue","darkgreen","darkorange","darkviolet",
  "gold","gray20","gray50","deepskyblue","springgreen","navy","maroon","olive",
  "turquoise","orchid","salmon","khaki","steelblue","seagreen","tan"
)

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: pixel matrix → base64 PNG  (old version logic, preserved exactly)
# ─────────────────────────────────────────────────────────────────────────────

make_raster_png <- function(df, fill_var, colors) {
  df$x <- df$x - min(df$x) + 1
  df$y <- df$y - min(df$y) + 1
  width  <- max(df$x)
  height <- max(df$y)

  mat <- matrix(NA_character_, nrow = height, ncol = width)
  mat[cbind(height - df$y + 1, df$x)] <- as.character(df[[fill_var]])

  col_img  <- matrix(colors[mat], nrow = height, ncol = width)
  rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
  rgb_array <- array(NA_real_, dim = c(height, width, 4))
  rgb_array[,,1] <- matrix(rgb_vals["red",   ], nrow = height, ncol = width)
  rgb_array[,,2] <- matrix(rgb_vals["green", ], nrow = height, ncol = width)
  rgb_array[,,3] <- matrix(rgb_vals["blue",  ], nrow = height, ncol = width)
  rgb_array[,,4] <- matrix(rgb_vals["alpha", ], nrow = height, ncol = width)

  na_pixels <- is.na(mat)
  rgb_array[,,4][na_pixels] <- 0

  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgb_array, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

clustering_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel("Clustering",
    sidebarLayout(

      sidebarPanel(width = 2,

        # ── 1. Study & Sample ──────────────────────────────────────────────
        tags$h5("Study & Sample", style = "font-weight:bold; margin-bottom:4px;"),
        selectInput(ns("study_select"), "Study",
                    choices = c("— select —" = ""), width = "100%"),
        actionButton(ns("refresh_studies"), "↺ Refresh", class = "btn-xs"),
        tags$hr(style = "margin:6px 0;"),
        selectInput(ns("sample_select"), "Sample",
                    choices = c("— select study first —" = ""), width = "100%"),
        tags$hr(),

        # ── 2. Processing artifact ─────────────────────────────────────────
        tags$h5("Processing Artifact", style = "font-weight:bold; margin-bottom:4px;"),
        selectInput(ns("pipeline_select"), "Pipeline",
                    choices = c("— select sample first —" = ""), width = "100%"),
        uiOutput(ns("pipeline_params_ui")),
        actionButton(ns("load_dataset_btn"), "Load Dataset",
                     class = "btn-primary btn-sm", width = "100%"),
        tags$hr(),

        # ── 3. Histology ───────────────────────────────────────────────────
        fileInput(ns("histology_img"), "Histology image (optional)",
                  accept = c("image/png","image/jpeg","image/jpg")),
        actionButton(ns("clear_histology"), "Clear histology",
                     class = "btn-sm btn-warning"),
        tags$hr(),

        # ── 4. Clustering config ───────────────────────────────────────────
        tags$h4("Clustering Configuration"),
        selectInput(ns("method"), "Clustering method",
                    choices = c("K-means","VSClust","MSIClust")),
        selectInput(ns("normalize"), "Normalisation",
                    choices = c("None"="none","TIC"="tic",
                                "Median"="median","RMS"="rms")),
        numericInput(ns("clusters"), "Number of clusters", value = 3, min = 2, max = 30),
        uiOutput(ns("method_params_ui")),
        actionButton(ns("run_clustering"), "Run Clustering"),
        tags$hr(),

        # ── 5. Annotation set ──────────────────────────────────────────────
        tags$h5("Annotation Set", style = "font-weight:bold; margin-bottom:4px;"),
        radioButtons(ns("ann_set_mode"), NULL,
                    choices = c("Use existing" = "existing", "Create new" = "new"),
                    selected = "existing"),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'existing'", ns("ann_set_mode")),
          selectInput(ns("ann_set_select"), "Annotation set:",
                      choices = c("— select study first —" = ""), width = "100%")
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'new'", ns("ann_set_mode")),
          textInput(ns("ann_set_name"), "Name:", placeholder = "e.g. Tumour vs Stroma"),
          textInput(ns("ann_set_labels"), "Labels (comma-separated):",
                    placeholder = "Tumour, Stroma, Background"),
          actionButton(ns("create_ann_set_btn"), "Create annotation set",
                      class = "btn-sm btn-success"),
          uiOutput(ns("ann_set_create_status"))
        ),
        tags$hr(),

        # ── 6. Class assignment ────────────────────────────────────────────
        selectInput(ns("orientation"), "Orientation adjustment",
                    choices = c("Default","Flip X","Flip Y","Flip Both")),
        textInput(ns("class_label"), "Assign Class", value = "Class1"),
        actionButton(ns("assign_class"), "Assign to Selection"),
        actionButton(ns("assign_all"),   "Assign ALL unassigned"),
        tags$hr(),
        actionButton(ns("commit_db"), "Commit to MongoDB",
                     class = "btn-danger btn-sm", width = "100%"),
        uiOutput(ns("commit_status"))
      ),

      mainPanel(width = 10,
        uiOutput(ns("cluster_layout")),
        textOutput(ns("status_text"))
      )
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

clustering_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ────────────────────────────────────────────────────────
    active_study_id    <- reactiveVal(NULL)
    active_sample_id   <- reactiveVal(NULL)
    active_pipeline_id <- reactiveVal(NULL)
    active_artifact_id <- reactiveVal(NULL)
    active_ann_set_id  <- reactiveVal(NULL)

    processed_data     <- reactiveVal(NULL)   # loaded feature matrix
    clustered_data     <- reactiveVal(NULL)   # after run_clustering
    annotated_data     <- reactiveVal(NULL)   # after class assignment (old pattern)
    original_clustered <- reactiveVal(NULL)   # unflipped snapshot
    histology_image    <- reactiveVal(NULL)   # base64 URI string (old pattern)
    vsclust_membership_data <- reactiveVal(NULL)
    current_method     <- reactiveVal(NULL)

    # Colour book-keeping (old pattern: named character vector)
    class_colors       <- reactiveVal(c())
    next_color_i       <- reactiveVal(1L)

    # Shape captured from drawclosedpath (old pattern)
    sel_shape          <- reactiveVal(NULL)

    # ── Study list ────────────────────────────────────────────────────────────

    load_studies <- function() {
      tryCatch({
        df      <- get_studies()
        has_ids <- !is.null(df) && nrow(df) > 0 && "_id" %in% names(df)
        if (!has_ids) {
          updateSelectInput(session, "study_select",
                            choices = c("\u2014 no studies found \u2014" = ""))
          return()
        }
        choices <- setNames(df[["_id"]], df$name)
        updateSelectInput(session, "study_select",
                          choices = c("\u2014 select \u2014" = "", choices))
      }, error = function(e) {
        showNotification(paste("Error loading studies:", e$message), type = "error")
      })
    }

    observeEvent(input$refresh_studies, load_studies(), ignoreInit = FALSE)
    session$onFlushed(function() load_studies(), once = TRUE)

    # ── Study → sample cascade ────────────────────────────────────────────────

    observeEvent(input$study_select, {
      sid <- input$study_select
      if (!nzchar(sid)) {
        active_study_id(NULL)
        updateSelectInput(session, "sample_select",
                          choices = c("— select study first —" = ""))
        return()
      }
      active_study_id(sid)
      tryCatch({
        df <- get_samples(sid)
        if (is.null(df) || nrow(df) == 0 || !("_id" %in% names(df))) {
          updateSelectInput(session, "sample_select",
                            choices = c("— no samples —" = ""))
        } else {
          choices <- setNames(df[["_id"]], df$sample_name)
          updateSelectInput(session, "sample_select",
                            choices = c("— select —" = "", choices))
        }
        refresh_ann_sets(sid)
      }, error = function(e) {
        showNotification(paste("Error loading samples:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Sample → pipeline cascade ─────────────────────────────────────────────

    observeEvent(input$sample_select, {
      samp <- input$sample_select
      if (!nzchar(samp)) {
        active_sample_id(NULL)
        updateSelectInput(session, "pipeline_select",
                          choices = c("— select sample first —" = ""))
        return()
      }
      active_sample_id(samp)
      tryCatch({
        pids <- list_available_pipeline_ids(samp, "binned_dataframe")
        if (length(pids) == 0) {
          updateSelectInput(session, "pipeline_select",
                            choices = c("— no processed artifacts —" = ""))
        } else {
          choices <- vapply(pids, function(pid) {
            tryCatch({
              meta <- get_pipeline(pid)
              paste0(meta$name[1], " (", substr(pid, 1, 8), "\u2026)")
            }, error = function(e) substr(pid, 1, 16))
          }, character(1))
          updateSelectInput(session, "pipeline_select",
                            choices = c("— select —" = "", setNames(pids, choices)))
        }
      }, error = function(e) {
        showNotification(paste("Error listing pipelines:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Pipeline params preview ───────────────────────────────────────────────

    output$pipeline_params_ui <- renderUI({
      pid <- input$pipeline_select
      if (!nzchar(pid %||% "")) return(NULL)
      tryCatch({
        meta   <- get_pipeline(pid)
        params <- meta$params[[1]]
        tags$div(style = "font-family:monospace; font-size:11px; color:#555; word-break:break-all;",
          tags$b("Params: "),
          paste(names(params), unlist(params), sep = "=", collapse = ", ")
        )
      }, error = function(e) NULL)
    })

    observeEvent(input$pipeline_select, {
      pid <- input$pipeline_select
      active_pipeline_id(if (nzchar(pid %||% "")) pid else NULL)
    })

    # ── Load dataset ──────────────────────────────────────────────────────────

    observeEvent(input$load_dataset_btn, {
      samp <- active_sample_id()
      pid  <- active_pipeline_id()
      if (is.null(samp) || !nzchar(samp)) {
        showNotification("Select a sample first.", type = "warning"); return()
      }
      if (is.null(pid) || !nzchar(pid)) {
        showNotification("Select a processing pipeline first.", type = "warning"); return()
      }

      shinyjs::disable("load_dataset_btn")
      on.exit(shinyjs::enable("load_dataset_btn"))

      progress <- shiny::Progress$new(session, min = 0, max = 100)
      progress$set(message = "Loading dataset...", value = 0)
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        art_meta <- query_artifacts(sample_id  = samp,
                                    stage_type  = "binned_dataframe",
                                    pipeline_id = pid)
        if (nrow(art_meta) == 0) stop("No artifact found for this sample + pipeline.")
        active_artifact_id(art_meta[["_id"]][1])

        progress$set(value = 40, message = "Downloading from GridFS...")
        df <- load_artifact_by_pipeline(samp, "binned_dataframe", pid)

        progress$set(value = 90, message = "Finalising...")
        processed_data(df)
        clustered_data(NULL)
        annotated_data(NULL)
        original_clustered(NULL)
        class_colors(c())
        next_color_i(1L)

        progress$set(value = 100)
        output$status_text <- renderText(
          paste0("Loaded: ", nrow(df), " pixels \u00d7 ",
                 sum(grepl("^mz_", names(df))), " features")
        )
        showNotification(
          sprintf("Dataset loaded: %d pixels \u00d7 %d features.",
                  nrow(df), sum(grepl("^mz_", names(df)))),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Load failed:", e$message), type = "error")
      })
    })

    # ── Histology (old pattern: base64 URI string) ────────────────────────────

    observeEvent(input$histology_img, {
      req(input$histology_img)
      tryCatch({
        img_path <- input$histology_img$datapath
        img_data <- readBin(img_path, "raw", file.info(img_path)$size)
        ext  <- tolower(tools::file_ext(input$histology_img$name))
        mime <- switch(ext, "png"="image/png", "jpg"="image/jpeg",
                       "jpeg"="image/jpeg", "image/png")
        img_uri <- paste0("data:", mime, ";base64,",
                          base64enc::base64encode(img_data))
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

    # ── Dynamic method params UI (old version) ────────────────────────────────

    output$method_params_ui <- renderUI({
        req(input$method)
        if (input$method == "K-means") {
          helpText("K-means partitions data into k distinct clusters.")
        } else if (input$method == "VSClust") {
          tagList(
            numericInput(ns("Sds"),    "Fuzziness (Sds)",
                        value = 1.3, min = 0.5, max = 3, step = 0.01),
            numericInput(ns("minMem"), "Min membership",
                        value = 0.5, min = 0.1, max = 1, step = 0.01),
            helpText("VSClust: fuzzy clustering with membership scores.")
          )
        } else if (input$method == "MSIClust") {
          tagList(
            numericInput(ns("cor_radius"), "Correlation radius (px)",
                        value = 1, min = 1, max = 5, step = 1),
            numericInput(ns("cor_scale"),  "Correlation scale factor",
                        value = 25, min = 1, max = 100, step = 1),
            numericInput(ns("minMem"),     "Min membership",
                        value = 0.5, min = 0.1, max = 1, step = 0.01),
            tags$div(class = "alert alert-warning", style = "padding:6px; font-size:12px;",
              tags$b("MSIClust requires normalization."),
              " 'None' is not supported — TIC normalization will be applied automatically."
            ),
            helpText("MSIClust: spatial correlation sets per-pixel fuzzifiers.")
          )
        }
      })
      
      # When MSIClust is selected, force normalize to TIC if currently "none"
      observeEvent(input$method, {
        if (input$method == "MSIClust" && input$normalize == "none") {
          updateSelectInput(session, "normalize", selected = "tic")
        }
      }, ignoreInit = TRUE)
      
      # Prevent user from selecting "none" while MSIClust is active
      observeEvent(input$normalize, {
        if (input$method == "MSIClust" && input$normalize == "none") {
          updateSelectInput(session, "normalize", selected = "tic")
          showNotification(
            "MSIClust requires normalization. Switched to TIC.",
            type = "warning", duration = 4
          )
        }
      }, ignoreInit = TRUE)

    # ── Run clustering (old logic, new reactiveVals) ───────────────────────────

    observeEvent(input$run_clustering, {
      df <- processed_data()
      req(df)

      shinyjs::disable("run_clustering")
      on.exit(shinyjs::enable("run_clustering"))

      progress <- shiny::Progress$new(session, min = 0, max = 100)
      progress$set(message = "Running clustering...", value = 0)
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        progress$set(value = 30,
                     message = paste0("Running ", input$method,
                                      " with k=", input$clusters, "..."))
        clustered <- switch(input$method,
          "K-means" = {
            current_method("K-means")
            vsclust_membership_data(NULL)
            run_kmeans(df, k = input$clusters,
                       normalize_method = input$normalize)
          },
          "VSClust" = {
            current_method("VSClust")
            result <- run_vsclust(df, k = input$clusters,
                                  normalize_method = input$normalize,
                                  Sds    = input$Sds    %||% 1.3,
                                  minMem = input$minMem %||% 0.5)
            vsclust_membership_data(result)
            result
          },
          "MSIClust" = {
            current_method("MSIClust")
            result <- run_msiclust(df, k = input$clusters,
                                   normalize_method = input$normalize,
                                   cor_radius = input$cor_radius %||% 1,
                                   cor_scale  = input$cor_scale  %||% 25,
                                   cor_cores  = max(parallel::detectCores() - 1, 1),
                                   minMem     = input$minMem %||% 0.5)
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

        progress$set(value = 100)
        norm_text <- if (input$normalize == "none") "no normalisation"
                     else paste("with", input$normalize, "normalisation")
        output$status_text <- renderText(
          paste0("Clustering complete: ", input$method, " (", norm_text,
                 ") — ", input$clusters, " clusters")
        )
        showNotification(
          paste0("Clustering complete: ", input$clusters, " clusters identified."),
          type = "message", duration = 5
        )
      }, error = function(e) {
        showNotification(paste("Clustering error:", e$message),
                         type = "error", duration = NULL)
      })
    })

    # ── MinMem live update (old logic) ────────────────────────────────────────

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
          if (orientation %in% c("Flip X", "Flip Both"))
            df_adj$x <- max(df_updated$x) - df_adj$x + min(df_updated$x)
          if (orientation %in% c("Flip Y", "Flip Both"))
            df_adj$y <- max(df_updated$y) - df_adj$y + min(df_updated$y)
          clustered_data(df_adj)
        }
        annotated_data(NULL)
        class_colors(c())
        next_color_i(1L)
        showNotification(
          paste0("Cluster assignments updated (minMem=", input$minMem, ")."),
          type = "message", duration = 3
        )
      }, error = function(e) {
        showNotification(paste("minMem update error:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Orientation flip (old logic: isolate + reset annotations) ─────────────

    observe({
      req(input$orientation)
      base_df <- original_clustered()
      req(base_df)

      if (input$orientation == "Default") {
        isolate({
          clustered_data(base_df)
          annotated_data(NULL)
          class_colors(c())
          next_color_i(1L)
        })
        return()
      }
      df_adj <- base_df
      if (input$orientation %in% c("Flip X", "Flip Both"))
        df_adj$x <- max(base_df$x) - df_adj$x + min(base_df$x)
      if (input$orientation %in% c("Flip Y", "Flip Both"))
        df_adj$y <- max(base_df$y) - df_adj$y + min(base_df$y)
      isolate({
        clustered_data(df_adj)
        annotated_data(NULL)
        class_colors(c())
        next_color_i(1L)
      })
    }) |> bindEvent(input$orientation)

    # ── Annotation set helpers ────────────────────────────────────────────────

    refresh_ann_sets <- function(study_id) {
      message("[refresh_ann_sets] called with study_id='", study_id, "'")
      message("[refresh_ann_sets] nchar=", nchar(study_id),
              " trimmed='", trimws(study_id), "'")
      q <- sprintf('{"study_id": "%s"}', study_id)
      message("[refresh_ann_sets] query=", q)

      tryCatch({
        sets_df <- list_annotation_sets(study_id)
        message("[refresh_ann_sets] nrow=", nrow(sets_df),
                " cols=", paste(names(sets_df), collapse = ","))

        # ── THIS BLOCK WAS MISSING ──────────────────────────────────────────
        if (nrow(sets_df) == 0 || !("_id" %in% names(sets_df))) {
          updateSelectInput(session, "ann_set_select",
                            choices = c("No annotation sets found" = ""))
        } else {
          choices <- setNames(sets_df[["_id"]], sets_df$name)
          updateSelectInput(session, "ann_set_select",
                            choices = c("— select —" = "", choices))
        }
        # ───────────────────────────────────────────────────────────────────

      }, error = function(e) {
        message("[refresh_ann_sets] ERROR: ", e$message)
        showNotification(paste("Error loading annotation sets:", e$message), type = "error")
      })
    }

    observeEvent(input$ann_set_select, {
      v <- input$ann_set_select
      if (!is.null(v) && nzchar(v)) {
        active_ann_set_id(v)
      } else {
        active_ann_set_id(NULL)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$ann_set_mode, {
      if (input$ann_set_mode == "existing") {
        sid <- active_study_id()
        if (!is.null(sid) && nzchar(sid)) {
          refresh_ann_sets(sid)
        }
      }
    }, ignoreInit = TRUE)

    observeEvent(input$create_ann_set_btn, {
      sid <- active_study_id()
      if (is.null(sid)) {
        showNotification("Select a study first.", type = "warning"); return()
      }
      nm <- trimws(input$ann_set_name %||% "")
      if (!nzchar(nm)) {
        showNotification("Enter a name.", type = "warning"); return()
      }
      labels <- trimws(strsplit(trimws(input$ann_set_labels %||% ""), ",")[[1]])
      labels <- labels[nzchar(labels)]
      if (length(labels) == 0) {
        showNotification("Enter at least one class label.", type = "warning"); return()
      }
      tryCatch({
        ann_set_id <- upsert_annotation_set(
          study_id     = sid,
          name         = nm,
          label_schema = labels
        )
        active_ann_set_id(ann_set_id)
        refresh_ann_sets(sid)
        output$ann_set_create_status <- renderUI({
          tags$div(class = "alert alert-success", style = "padding:4px; font-size:12px",
                  paste0("✓ Created: ", nm))
        })
        showNotification(paste0("Annotation set created: ", ann_set_id),
                        type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Error creating annotation set:", e$message), type = "error")
      })
    })

    # ── Capture drawclosedpath selection (old SVG path parser, exact) ─────────

    observeEvent(event_data("plotly_relayout", source = ns("cluster_src")), {
      ev <- event_data("plotly_relayout", source = ns("cluster_src"))
      req(ev)

      # Method 1: direct path key after edit
      if (any(grepl("shapes\\[\\d+\\]\\.path$", names(ev)))) {
        path_key <- grep("shapes\\[\\d+\\]\\.path$", names(ev), value = TRUE)[1]
        path <- ev[[path_key]]
        if (!is.null(path) && nchar(path) > 0) {
          coords <- regmatches(path,
            gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
          xy <- do.call(rbind, strsplit(coords, ","))
          sel_shape(list(type = "polygon",
                         x = as.numeric(xy[,1]),
                         y = as.numeric(xy[,2])))
          showNotification(
            paste0("\u2713 Polygon captured (", nrow(xy), " points)"),
            type = "message", duration = 2
          )
        }
        return()
      }

      # Method 2: shapes array on first draw
      if ("shapes" %in% names(ev)) {
        shapes_data <- ev$shapes
        if (is.data.frame(shapes_data) && nrow(shapes_data) > 0) {
          last_row <- shapes_data[nrow(shapes_data), ]
          if (!is.null(last_row$path) && nchar(last_row$path) > 0) {
            path <- last_row$path
            coords <- regmatches(path,
              gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
            xy <- do.call(rbind, strsplit(coords, ","))
            sel_shape(list(type = "polygon",
                           x = as.numeric(xy[,1]),
                           y = as.numeric(xy[,2])))
            showNotification(
              paste0("\u2713 Polygon captured (", nrow(xy), " points)"),
              type = "message", duration = 2
            )
          }
        }
      }
    })

    # ── Assign to selection (old logic, exact) ────────────────────────────────

    observeEvent(input$assign_class, {
      shape <- sel_shape()
      if (is.null(shape)) {
        showNotification("No selection drawn.", type = "warning", duration = 4)
        return()
      }
      df <- annotated_data() %||% clustered_data()
      req(df)

      if (!"Class" %in% names(df)) df$Class <- "Unassigned" else
        df$Class[is.na(df$Class)] <- "Unassigned"
      df$Class <- as.character(df$Class)

      if (!identical(shape$type, "polygon")) {
        showNotification("Only polygon selection supported.", type = "error"); return()
      }

      poly_x <- shape$x
      poly_y <- shape$y

      # Inverse-transform polygon coordinates to match original coord space
      base_df     <- original_clustered()
      req(base_df)
      orientation <- input$orientation %||% "Default"
      if (orientation %in% c("Flip X", "Flip Both"))
        poly_x <- max(base_df$x) + min(base_df$x) - poly_x
      if (orientation %in% c("Flip Y", "Flip Both"))
        poly_y <- max(base_df$y) + min(base_df$y) - poly_y

      inside <- sp::point.in.polygon(df$x, df$y, poly_x, poly_y) > 0
      n_sel  <- sum(inside)
      if (n_sel == 0) {
        showNotification("No pixels in selection.", type = "warning"); return()
      }

      lab <- input$class_label
      df$Class[inside] <- lab
      annotated_data(df)

      if (lab != "Unassigned") {
        cols <- class_colors()
        if (!(lab %in% names(cols))) {
          i <- next_color_i()
          cols[lab] <- MY_PALETTE[((i - 1) %% length(MY_PALETTE)) + 1]
          next_color_i(i + 1L)
          class_colors(cols)
        }
      }

      # Clear drawn shapes from plot
      try(plotlyProxy("cluster_plot", session) |>
            plotlyProxyInvoke("relayout", list(shapes = list())), silent = TRUE)
      sel_shape(NULL)

      showNotification(sprintf("Assigned '%s' to %d pixels.", lab, n_sel),
                       type = "message", duration = 3)
    })

    # ── Assign all unassigned (old logic, exact) ──────────────────────────────

    observeEvent(input$assign_all, {
      df <- annotated_data() %||% clustered_data()
      req(df)

      if (!"Class" %in% names(df)) df$Class <- "Unassigned" else
        df$Class[is.na(df$Class)] <- "Unassigned"
      df$Class <- as.character(df$Class)

      n_unassigned <- sum(df$Class == "Unassigned")
      if (n_unassigned == 0) {
        showNotification("No unassigned pixels.", type = "warning"); return()
      }

      lab  <- input$class_label
      df$Class[df$Class == "Unassigned"] <- lab
      annotated_data(df)

      if (lab != "Unassigned") {
        cols <- class_colors()
        if (!(lab %in% names(cols))) {
          i <- next_color_i()
          cols[lab] <- MY_PALETTE[((i - 1) %% length(MY_PALETTE)) + 1]
          next_color_i(i + 1L)
          class_colors(cols)
        }
      }

      showNotification(sprintf("Assigned '%s' to %d pixels.", lab, n_unassigned),
                       type = "message", duration = 3)
    })

    # ── Cluster plot (old raster logic + drawclosedpath) ──────────────────────

    output$cluster_plot <- renderPlotly({
      df <- clustered_data()
      req(df)

      df$cluster <- as.character(df$cluster)
      present    <- unique(df$cluster)
      present    <- c(if ("No_cluster" %in% present) "No_cluster",
                      sort(setdiff(present, "No_cluster")))
      valid      <- sort(setdiff(present, "No_cluster"))
      n_valid    <- length(valid)

      cols_base        <- RColorBrewer::brewer.pal(max(n_valid, 3), "Set3")[seq_len(n_valid)]
      names(cols_base) <- valid
      all_colors       <- c("No_cluster" = "#D9D9D9", cols_base)

      img_uri <- make_raster_png(df, "cluster", all_colors)

      x_min <- min(df$x); x_max <- max(df$x)
      y_min <- min(df$y); y_max <- max(df$y)
      x_range   <- c(x_min, x_max)
      y_range   <- c(y_min, y_max)
      img_x     <- x_min;  img_y     <- y_max
      img_sizex <- x_max - x_min
      img_sizey <- y_max - y_min

      orientation <- input$orientation %||% "Default"
      if (orientation %in% c("Flip X", "Flip Both")) {
        x_range   <- rev(x_range); img_x     <- x_max
        img_sizex <- -(x_max - x_min)
      }
      if (orientation %in% c("Flip Y", "Flip Both")) {
        y_range   <- rev(y_range); img_y     <- y_min
      }

      p <- plot_ly(source = ns("cluster_src")) |>
        add_trace(x = NULL, y = NULL, type = "scatter", mode = "markers") |>
        layout(
          images = list(list(
            source = img_uri, xref = "x", yref = "y",
            x = img_x, y = img_y,
            sizex = img_sizex, sizey = img_sizey,
            sizing = "stretch", layer = "below"
          )),
          dragmode  = "drawclosedpath",
          newshape  = list(line      = list(color = "black", width = 1),
                           fillcolor = "rgba(0,0,0,0.05)"),
          title     = "MSI Clustering Result",
          xaxis     = list(range = x_range, title = "x"),
          yaxis     = list(range = y_range, title = "y",
                           scaleanchor = "x", scaleratio = 1),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = -0.15, yanchor = "top")
        ) |>
        config(
          displaylogo = FALSE,
          modeBarButtonsToAdd    = list("drawclosedpath", "eraseshape"),
          modeBarButtonsToRemove = c("hoverClosestCartesian",
                                     "hoverCompareCartesian",
                                     "toggleSpikelines","toImage",
                                     "select2d","lasso2d")
        )

      for (cls in present) {
        p <- p |> add_trace(
          x = x_min - 1000, y = y_min - 1000,
          type = "scatter", mode = "markers",
          marker = list(size = 10, color = all_colors[[cls]]),
          name = if (cls == "No_cluster") "No cluster" else paste("Cluster", cls),
          showlegend = TRUE, hoverinfo = "skip", inherit = FALSE
        )
      }
      p
    })

    # ── Class plot (old raster logic, exact) ──────────────────────────────────

    output$class_plot <- renderPlotly({
      df <- annotated_data() %||% clustered_data()
      req(df)

      if (!"Class" %in% names(df)) df$Class <- "Unassigned" else
        df$Class[is.na(df$Class)] <- "Unassigned"
      df$Class <- as.character(df$Class)

      cols_used <- class_colors()
      cols_used <- cols_used[names(cols_used) != "Unassigned"]

      present <- unique(df$Class)
      present <- c(if ("Unassigned" %in% present) "Unassigned",
                   sort(setdiff(present, "Unassigned")))

      all_colors <- c("Unassigned" = "#B8BFFC")
      for (cls in present) {
        if (cls != "Unassigned") all_colors[cls] <- cols_used[[cls]]
      }

      img_uri <- make_raster_png(df, "Class", all_colors)

      x_min <- min(df$x); x_max <- max(df$x)
      y_min <- min(df$y); y_max <- max(df$y)
      x_range   <- c(x_min, x_max)
      y_range   <- c(y_min, y_max)
      img_x     <- x_min;  img_y     <- y_max
      img_sizex <- x_max - x_min
      img_sizey <- y_max - y_min

      orientation <- input$orientation %||% "Default"
      if (orientation %in% c("Flip X", "Flip Both")) {
        x_range   <- rev(x_range); img_x     <- x_max
        img_sizex <- -(x_max - x_min)
      }
      if (orientation %in% c("Flip Y", "Flip Both")) {
        y_range   <- rev(y_range); img_y     <- y_min
      }

      p <- plot_ly() |>
        layout(
          images = list(list(
            source = img_uri, xref = "x", yref = "y",
            x = img_x, y = img_y,
            sizex = img_sizex, sizey = img_sizey,
            sizing = "stretch", layer = "below"
          )),
          title  = "Class Assignment",
          xaxis  = list(range = x_range, title = "x"),
          yaxis  = list(range = y_range, title = "y",
                        scaleanchor = "x", scaleratio = 1),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = -0.15, yanchor = "top")
        ) |>
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("hoverClosestCartesian",
                                     "hoverCompareCartesian",
                                     "toggleSpikelines","toImage",
                                     "select2d","lasso2d")
        )

      for (i in seq_along(present)) {
        cls <- present[i]
        p   <- p |> add_trace(
          x = x_min - 1000, y = y_min - 1000,
          type = "scatter", mode = "markers",
          marker = list(size = 10, color = all_colors[[cls]]),
          name = cls, showlegend = TRUE, hoverinfo = "skip"
        )
      }
      p
    })

    # ── Histology plot (old decode logic, exact) ──────────────────────────────

    output$histology_plot <- renderPlot({
      img_uri <- histology_image()
      df      <- clustered_data()
      req(img_uri, df)

      x_min <- min(df$x); x_max <- max(df$x)
      y_min <- min(df$y); y_max <- max(df$y)

      # Decode base64 URI → raw → temp file → raster
      img_data <- sub("^data:image/[a-z]+;base64,", "", img_uri)
      img_raw  <- base64enc::base64decode(img_data)
      tmp_img  <- tempfile(fileext = ".png")
      writeBin(img_raw, tmp_img)

      img <- tryCatch(
        if (grepl("data:image/png", img_uri)) png::readPNG(tmp_img)
        else                                   jpeg::readJPEG(tmp_img),
        error = function(e) NULL
      )
      if (is.null(img)) {
        plot.new(); text(0.5, 0.5, "Error loading image", cex = 1.5); return()
      }

      par(mar = c(4, 4, 3, 1))
      plot(NULL, xlim = c(x_min, x_max), ylim = c(y_min, y_max),
           xlab = "x", ylab = "y", main = "Histology", asp = 1)
      graphics::rasterImage(img, x_min, y_min, x_max, y_max)
    })

    # ── Layout ────────────────────────────────────────────────────────────────

    output$cluster_layout <- renderUI({
      req(clustered_data())
      has_hist <- !is.null(histology_image())
      if (has_hist) {
        tagList(
          fluidRow(
            column(6, plotlyOutput(ns("cluster_plot"),  height = "600px")),
            column(6, plotOutput( ns("histology_plot"), height = "600px"))
          ),
          fluidRow(
            column(6, plotlyOutput(ns("class_plot"), height = "600px"))
          )
        )
      } else {
        fluidRow(
          column(6, plotlyOutput(ns("cluster_plot"), height = "600px")),
          column(6, plotlyOutput(ns("class_plot"),   height = "600px"))
        )
      }
    })

    # ── Commit to MongoDB (new provenance schema + old coordinate logic) ───────

    observeEvent(input$commit_db, {
      study_id    <- active_study_id()
      sample_id   <- active_sample_id()
      artifact_id <- active_artifact_id()
      ann_set_id  <- active_ann_set_id()
      pid         <- active_pipeline_id()

      # Use annotated_data if available, fall back to clustered_data
      df <- annotated_data() %||% clustered_data()

      if (is.null(study_id))  { showNotification("No study selected.",    type = "error"); return() }
      if (is.null(sample_id)) { showNotification("No sample selected.",   type = "error"); return() }
      if (is.null(pid))       { showNotification("No pipeline selected.", type = "error"); return() }
      if (is.null(df))        { showNotification("Run clustering first.", type = "error"); return() }
      if (is.null(ann_set_id)) {
        showNotification("Select or create an annotation set.", type = "error"); return()
      }

      # Ensure Class column
      if (!"Class" %in% names(df)) df$Class <- "Unassigned" else
        df$Class[is.na(df$Class)] <- "Unassigned"
      df$Class <- as.character(df$Class)

      # OLD LOGIC: save with ORIGINAL coordinates by transferring Class column
      # to base_df — preserves coordinate integrity regardless of orientation.
      base_df <- original_clustered()
      if (is.null(base_df)) {
        showNotification("No original clustering data.", type = "error"); return()
      }
      if (nrow(base_df) != nrow(df)) {
        showNotification("Row mismatch between original and transformed data.",
                         type = "error"); return()
      }
      df_to_save <- base_df
      df_to_save$Class <- df$Class   # transfer class labels to original coords

      annotation_df <- df_to_save[df_to_save$Class != "Unassigned",
                                   c("x","y","Class"), drop = FALSE]
      if (nrow(annotation_df) == 0) {
        showNotification("No pixels assigned a class yet.", type = "warning"); return()
      }

      shinyjs::disable("commit_db")
      on.exit(shinyjs::enable("commit_db"))
      progress <- shiny::Progress$new(session, min = 0, max = 100)
      progress$set(message = "Committing...", value = 0)
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        progress$set(value = 20, message = "Registering clustering pipeline...")

        # 1) Deterministic clustering pipeline
        method <- current_method() %||% input$method
        cluster_params <- list(
          method            = method,
          k                 = input$clusters,
          normalize         = input$normalize,
          input_pipeline_id = pid
        )
        if (method %in% c("VSClust", "MSIClust")) {
          cluster_params$minMem <- input$minMem
          if (method == "VSClust")  cluster_params$Sds        <- input$Sds
          if (method == "MSIClust") cluster_params$cor_radius <- input$cor_radius
        }
        cluster_pid <- upsert_pipeline(
          type         = "clustering",
          name         = paste0(method, "_k", input$clusters),
          params       = cluster_params,
          code_version = "dev"
        )

        progress$set(value = 50, message = "Saving clustering artifact...")

        # 2) Save clustering result (upsert — allows re-commit)
        art_id <- save_clustering_artifact(
          clustered_df        = df_to_save,
          study_id            = study_id,
          sample_id           = sample_id,
          input_artifact_id   = artifact_id %||% "",
          cluster_pipeline_id = cluster_pid
        )

        progress$set(value = 75, message = "Saving annotations...")

        # 3) Save/replace annotations
        ann_id <- upsert_annotation(
          annotation_df     = annotation_df,
          sample_id         = sample_id,
          annotation_set_id = ann_set_id
        )

        progress$set(value = 90, message = "Writing legacy metadata...")

        # 4) Legacy clustering_metadata insert (new-schema fields included)
        tryCatch({
          .insert(.con("clustering_metadata", DB_NAME, MONGO_URL), list(
            assignment_id       = art_id,
            study_id            = study_id,
            sample_id           = sample_id,
            pipeline_id         = pid,
            cluster_pipeline_id = cluster_pid,
            clustering_method   = method,
            k                   = input$clusters,
            normalize           = input$normalize,
            annotation_set_id   = ann_set_id,
            annotation_id       = ann_id,
            n_annotated         = nrow(annotation_df),
            created_at          = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          ))
        }, error = function(e)
          message("Legacy clustering_metadata insert failed (non-fatal): ", e$message)
        )

        progress$set(value = 100)
        output$commit_status <- renderUI(
          tags$div(style = "color:green; margin-top:6px;",
            tags$b("\u2713 Committed"), tags$br(),
            tags$small("Artifact: ",   substr(art_id,  1, 12), "\u2026"), tags$br(),
            tags$small("Annotation: ", substr(ann_id,  1, 12), "\u2026")
          )
        )
        showNotification("Committed to database.", type = "message")

      }, error = function(e) {
        output$commit_status <- renderUI(
          tags$span(style = "color:red", "Error: ", e$message)
        )
        showNotification(paste("Commit failed:", e$message),
                         type = "error", duration = NULL)
      })
    })

  }) # end moduleServer
}