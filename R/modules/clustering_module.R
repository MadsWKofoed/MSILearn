# R/modules/clustering_module.R
# Clustering tab — provenance-aware rewrite.
#
# Provenance chain enforced:
#   study → sample → pipeline_id (binned_dataframe artifact)
#     → clustering params → clustering artifact
#     → annotation set → annotation
#
# Rules:
#   • No "most recent" fallback anywhere.
#   • All artifact loading requires explicit (sample_id, stage_type, pipeline_id).
#   • Annotation sets are version-controlled; each annotation is keyed by
#     (sample_id, annotation_set_id).
#   • Legacy insert into "clustering_metadata" kept for backwards compat.

library(shiny)
library(plotly)
library(sp)

# ─────────────────────────────────────────────────────────────────────────────
# COLOUR PALETTE
# ─────────────────────────────────────────────────────────────────────────────

CLASS_COLORS <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3",
  "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62",
  "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494"
)

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: rasterise a ggplot / base-R plot to a plotly PNG layer
# ─────────────────────────────────────────────────────────────────────────────

make_raster_png <- function(plot_obj, width = 800, height = 600) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = width, height = height)
  if (inherits(plot_obj, "gg")) {
    print(plot_obj)
  } else {
    plot_obj
  }
  grDevices::dev.off()
  base64enc::base64encode(tmp)
}


# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

clustering_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel("Clustering",
  fluidPage(
    tags$head(tags$style(HTML("
      .section-header { font-weight: bold; margin-top: 8px; margin-bottom: 4px; }
      .pipeline-badge { font-family: monospace; font-size: 11px;
                        word-break: break-all; color: #555; }
    "))),

    fluidRow(

      # ── LEFT SIDEBAR ──────────────────────────────────────────────────────
      column(3,

        # ── 1. Study & Sample ───────────────────────────────────────────────
        wellPanel(
          tags$div(class = "section-header", "Study & Sample"),

          selectInput(ns("study_select"), "Study",
                      choices = c("— select —" = ""),
                      width = "100%"),

          actionButton(ns("refresh_studies"), "↺ Refresh", class = "btn-xs"),
          tags$hr(style = "margin: 6px 0"),

          selectInput(ns("sample_select"), "Sample",
                      choices = c("— select study first —" = ""),
                      width = "100%")
        ),

        # ── 2. Processing artifact (pipeline) ───────────────────────────────
        wellPanel(
          tags$div(class = "section-header", "Processing Artifact"),

          tags$p("Select the processing pipeline used to generate the feature matrix.",
                 style = "font-size:12px; color:#666;"),

          selectInput(ns("pipeline_select"), "Pipeline ID",
                      choices = c("— select sample first —" = ""),
                      width = "100%"),

          uiOutput(ns("pipeline_params_ui")),

          actionButton(ns("load_dataset_btn"), "Load Dataset",
                       class = "btn-primary btn-sm", width = "100%")
        ),

        # ── 3. Histology ─────────────────────────────────────────────────────
        wellPanel(
          tags$div(class = "section-header", "Histology Image (optional)"),
          fileInput(ns("histology_upload"), NULL,
                    accept = c("image/png", "image/jpeg"),
                    buttonLabel = "Upload image")
        ),

        # ── 4. Clustering config ─────────────────────────────────────────────
        wellPanel(
          tags$div(class = "section-header", "Clustering Configuration"),

          selectInput(ns("normalize"), "Normalisation",
                      choices = c("None" = "none", "TIC" = "tic",
                                  "Median" = "median", "RMS" = "rms")),

          radioButtons(ns("method"), "Method",
                       choices = c("K-means" = "kmeans",
                                   "VSClust"  = "vsclust",
                                   "MSIClust" = "msiclust"),
                       selected = "kmeans"),

          sliderInput(ns("clusters"), "Number of clusters (k)",
                      min = 2, max = 15, value = 5, step = 1),

          # VSClust / MSIClust params (shown conditionally)
          conditionalPanel(
            condition = sprintf("input['%s'] != 'kmeans'", ns("method")),
            sliderInput(ns("Sds"), "Sds (noise sensitivity)",
                        min = 0.1, max = 5.0, value = 1.3, step = 0.1),
            sliderInput(ns("minMem"), "Min membership",
                        min = 0.0, max = 1.0, value = 0.5, step = 0.05)
          ),

          # MSIClust extra
          conditionalPanel(
            condition = sprintf("input['%s'] == 'msiclust'", ns("method")),
            sliderInput(ns("cor_radius"), "Correlation radius",
                        min = 1, max = 5, value = 1, step = 1)
          ),

          actionButton(ns("run_clustering_btn"), "Run Clustering",
                       class = "btn-success btn-sm", width = "100%")
        ),

        # ── 5. Annotation set ────────────────────────────────────────────────
        wellPanel(
          tags$div(class = "section-header", "Annotation Set"),

          radioButtons(ns("ann_set_mode"), NULL,
                       choices = c("Select existing"  = "existing",
                                   "Create new"       = "new"),
                       selected = "existing"),

          # existing branch
          conditionalPanel(
            condition = sprintf("input['%s'] == 'existing'", ns("ann_set_mode")),
            selectInput(ns("ann_set_select"), "Annotation set",
                        choices = c("— none —" = ""),
                        width = "100%")
          ),

          # new branch
          conditionalPanel(
            condition = sprintf("input['%s'] == 'new'", ns("ann_set_mode")),
            textInput(ns("ann_set_name"), "Name", placeholder = "e.g. tumour_vs_stroma"),
            textInput(ns("ann_set_labels"), "Classes (comma-separated)",
                      placeholder = "Tumour, Stroma, Necrosis"),
            actionButton(ns("create_ann_set_btn"), "Create annotation set",
                         class = "btn-info btn-sm", width = "100%"),
            uiOutput(ns("ann_set_create_status"))
          )
        ),

        # ── 6. Class assignment ───────────────────────────────────────────────
        wellPanel(
          tags$div(class = "section-header", "Assign Class to Selection"),

          uiOutput(ns("class_buttons_ui")),

          tags$hr(style = "margin: 6px 0"),

          selectInput(ns("orientation"), "Orientation",
                      choices = c("Original"    = "original",
                                  "Flip X"      = "flip_x",
                                  "Flip Y"      = "flip_y",
                                  "Flip Both"   = "flip_both")),

          actionButton(ns("assign_all_btn"), "Assign to ALL pixels",
                       class = "btn-warning btn-xs"),

          tags$hr(style = "margin: 6px 0"),

          actionButton(ns("commit_btn"), "Commit to Database",
                       class = "btn-danger btn-sm", width = "100%"),
          uiOutput(ns("commit_status"))
        )
      ), # end column(3)

      # ── CENTRE: plots ─────────────────────────────────────────────────────
      column(9,
        uiOutput(ns("cluster_layout"))
      )
    ) # end fluidRow
  )  # end fluidPage
  )  # end tabPanel
}


# ───────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

clustering_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ───────────────────────────────────────────────────────

    active_study_id       <- reactiveVal(NULL)
    active_sample_id      <- reactiveVal(NULL)
    active_pipeline_id    <- reactiveVal(NULL)
    active_artifact_id    <- reactiveVal(NULL)   # the processing artifact _id
    active_ann_set_id     <- reactiveVal(NULL)

    processed_data        <- reactiveVal(NULL)   # loaded feature matrix (data.frame)
    clustered_data        <- reactiveVal(NULL)   # after run_clustering
    original_clustered    <- reactiveVal(NULL)   # unflipped copy
    histology_image       <- reactiveVal(NULL)
    vsclust_membership    <- reactiveVal(NULL)   # membership matrix for VSClust/MSIClust
    current_method        <- reactiveVal("kmeans")

    # Colour book-keeping
    class_colors          <- reactiveVal(list())
    next_color_i          <- reactiveVal(1L)

    # Shape from plotly_relayout (lasso / rectangle)
    sel_shape             <- reactiveVal(NULL)


    # ── Study list ──────────────────────────────────────────────────────────

    load_studies <- function() {
      tryCatch({
        df <- get_studies()
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

    # Auto-load on module start using a one-shot invalidation
    session$onFlushed(function() load_studies(), once = TRUE)

    # ── Study → sample cascade ───────────────────────────────────────────────

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
        if (is.null(df) || nrow(df) == 0) {
          updateSelectInput(session, "sample_select",
                            choices = c("— no samples in this study —" = ""))
        } else {
          choices <- setNames(df[["_id"]], df$sample_name)
          updateSelectInput(session, "sample_select",
                            choices = c("— select —" = "", choices))
        }
        # refresh annotation sets for this study
        refresh_ann_sets(sid)
      }, error = function(e) {
        showNotification(paste("Error loading samples:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Sample → pipeline list cascade ──────────────────────────────────────

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
          # Enrich each pipeline_id with human-readable label
          choices <- vapply(pids, function(pid) {
            tryCatch({
              meta <- get_pipeline(pid)
              paste0(meta$name[1], " (", substr(pid, 1, 8), "…)")
            }, error = function(e) substr(pid, 1, 16))
          }, character(1))
          choices <- setNames(pids, choices)
          updateSelectInput(session, "pipeline_select",
                            choices = c("— select —" = "", choices))
        }
      }, error = function(e) {
        showNotification(paste("Error listing pipelines:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # ── Pipeline params preview ──────────────────────────────────────────────

    output$pipeline_params_ui <- renderUI({
      pid <- input$pipeline_select
      if (!nzchar(pid %||% "")) return(NULL)
      tryCatch({
        meta   <- get_pipeline(pid)
        params <- meta$params[[1]]
        tags$div(
          class = "pipeline-badge",
          tags$b("Params: "),
          paste(names(params), unlist(params), sep = "=", collapse = ", ")
        )
      }, error = function(e) NULL)
    })

    observeEvent(input$pipeline_select, {
      pid <- input$pipeline_select
      active_pipeline_id(if (nzchar(pid %||% "")) pid else NULL)
    })

    # ── Load dataset button ──────────────────────────────────────────────────

    observeEvent(input$load_dataset_btn, {
      samp <- active_sample_id()
      pid  <- active_pipeline_id()

      if (is.null(samp) || !nzchar(samp)) {
        showNotification("Select a sample first.", type = "warning"); return()
      }
      if (is.null(pid) || !nzchar(pid)) {
        showNotification("Select a processing pipeline first.", type = "warning"); return()
      }

      withProgress(message = "Loading feature matrix …", {
        tryCatch({
          # Resolve artifact metadata to get artifact _id for provenance
          art_meta <- query_artifacts(sample_id  = samp,
                                      stage_type  = "binned_dataframe",
                                      pipeline_id = pid)
          if (nrow(art_meta) == 0) stop("No artifact found for this sample + pipeline.")

          active_artifact_id(art_meta[["_id"]][1])

          df <- load_artifact_by_pipeline(samp, "binned_dataframe", pid)
          processed_data(df)
          clustered_data(NULL)
          original_clustered(NULL)
          vsclust_membership(NULL)

          showNotification(
            sprintf("Loaded %d pixels × %d features.",
                    nrow(df),
                    length(grep("^mz_", names(df)))),
            type = "message"
          )
        }, error = function(e) {
          showNotification(paste("Load failed:", e$message), type = "error")
        })
      })
    })

    # ── Histology upload ─────────────────────────────────────────────────────

    observeEvent(input$histology_upload, {
      req(input$histology_upload)
      img <- tryCatch(
        png::readPNG(input$histology_upload$datapath),
        error = function(e)
          jpeg::readJPEG(input$histology_upload$datapath)
      )
      histology_image(img)
    })

    # ── Run clustering ───────────────────────────────────────────────────────

    observeEvent(input$run_clustering_btn, {
      df <- processed_data()
      if (is.null(df)) {
        showNotification("Load a dataset first.", type = "warning"); return()
      }

      method <- input$method
      current_method(method)
      k      <- input$clusters
      norm   <- input$normalize

      withProgress(message = paste("Running", method, "…"), {
        tryCatch({
          result <- switch(method,
            kmeans = run_kmeans(df, k = k, normalize_method = norm),
            vsclust = {
              res <- run_vsclust(df, k = k, normalize_method = norm,
                                 Sds = input$Sds, minMem = input$minMem)
              mem_cols <- grep("^membership_", names(res), value = TRUE)
              vsclust_membership(as.matrix(res[, mem_cols, drop = FALSE]))
              res
            },
            msiclust = {
              res <- run_msiclust(df, k = k, normalize_method = norm,
                                  Sds = input$Sds, minMem = input$minMem,
                                  cor_radius = input$cor_radius)
              mem_cols <- grep("^membership_", names(res), value = TRUE)
              vsclust_membership(as.matrix(res[, mem_cols, drop = FALSE]))
              res
            }
          )
          result$Class <- NA_character_
          clustered_data(result)
          original_clustered(result)

          # Reset colour assignments
          class_colors(list())
          next_color_i(1L)

          showNotification(
            sprintf("Clustering done. %d pixels, %d unique clusters.",
                    nrow(result),
                    length(unique(result$cluster[result$cluster != "No_cluster"]))),
            type = "message"
          )
        }, error = function(e) {
          showNotification(paste("Clustering error:", e$message), type = "error")
        })
      })
    })

    # ── MinMem live update (VSClust / MSIClust) ───────────────────────────────

    observeEvent(input$minMem, {
      df <- clustered_data()
      if (is.null(df)) return()
      if (!("raw_cluster" %in% names(df))) return()
      clustered_data(apply_minmem_threshold(df, input$minMem))
    }, ignoreInit = TRUE)

    # ── Orientation flip ──────────────────────────────────────────────────────

    observeEvent(input$orientation, {
      orig <- original_clustered()
      if (is.null(orig)) return()
      df <- orig
      x_range <- range(df$x, na.rm = TRUE)
      y_range <- range(df$y, na.rm = TRUE)
      df <- switch(input$orientation,
        flip_x    = { df$x <- x_range[2] - df$x + x_range[1]; df },
        flip_y    = { df$y <- y_range[2] - df$y + y_range[1]; df },
        flip_both = { df$x <- x_range[2] - df$x + x_range[1]
                      df$y <- y_range[2] - df$y + y_range[1]; df },
        df   # original
      )
      clustered_data(df)
    }, ignoreInit = TRUE)

    # ── Annotation set helpers ────────────────────────────────────────────────

    refresh_ann_sets <- function(study_id) {
      tryCatch({
        df <- list_annotation_sets(study_id)
        if (is.null(df) || nrow(df) == 0) {
          choices <- c("— none —" = "")
        } else {
          choices <- setNames(df[["_id"]], df$name)
        }
        updateSelectInput(session, "ann_set_select",
                          choices = c("— select —" = "", choices))
      }, error = function(e) NULL)
    }

    observeEvent(input$ann_set_select, {
      v <- input$ann_set_select
      active_ann_set_id(if (nzchar(v %||% "")) v else NULL)
    })

    # Create new annotation set
    observeEvent(input$create_ann_set_btn, {
      sid <- active_study_id()
      if (is.null(sid)) {
        showNotification("Select a study first.", type = "warning"); return()
      }
      nm <- trimws(input$ann_set_name %||% "")
      if (!nzchar(nm)) {
        showNotification("Enter a name for the annotation set.", type = "warning"); return()
      }
      labels_raw <- trimws(input$ann_set_labels %||% "")
      if (!nzchar(labels_raw)) {
        showNotification("Enter at least one class label.", type = "warning"); return()
      }
      labels <- trimws(strsplit(labels_raw, ",")[[1]])
      labels <- labels[nzchar(labels)]

      tryCatch({
        ann_id <- upsert_annotation_set(sid, nm, labels)
        active_ann_set_id(ann_id)
        refresh_ann_sets(sid)
        # switch UI to existing mode and select the new set
        updateRadioButtons(session, "ann_set_mode", selected = "existing")
        updateSelectInput(session, "ann_set_select", selected = ann_id)

        output$ann_set_create_status <- renderUI(
          tags$span(style = "color:green", "✓ Created: ", nm)
        )
      }, error = function(e) {
        output$ann_set_create_status <- renderUI(
          tags$span(style = "color:red", "Error: ", e$message)
        )
      })
    })

    # ── Class button UI (one button per unique cluster) ───────────────────────

    output$class_buttons_ui <- renderUI({
      df <- clustered_data()
      if (is.null(df)) return(tags$p("Run clustering first.", style = "color:#999;"))

      clusters <- sort(unique(df$cluster[df$cluster != "No_cluster"]))
      if (length(clusters) == 0) return(NULL)

      colors <- class_colors()

      lapply(clusters, function(cl) {
        col <- colors[[as.character(cl)]] %||% "#cccccc"
        fluidRow(
          column(2, tags$div(style = sprintf(
            "width:16px; height:16px; background:%s; margin-top:6px;", col))),
          column(10,
            actionButton(
              ns(paste0("assign_", cl)),
              label = paste("Cluster", cl),
              class = "btn-xs btn-default",
              style = sprintf("border-left: 4px solid %s; margin-bottom:3px;", col)
            )
          )
        )
      })
    })

    # Assign class via polygon selection — one observer per cluster button
    observe({
      df <- clustered_data()
      if (is.null(df)) return()

      clusters <- sort(unique(df$cluster[df$cluster != "No_cluster"]))

      lapply(clusters, function(cl) {
        local({
          lcl <- cl
          btn_id <- paste0("assign_", lcl)
          observeEvent(input[[btn_id]], {
            shape <- sel_shape()
            df    <- clustered_data()
            if (is.null(shape) || is.null(df)) {
              showNotification("Draw a shape on the cluster plot first.", type = "warning")
              return()
            }

            # Colour assignment
            colors <- class_colors()
            key    <- as.character(lcl)
            if (is.null(colors[[key]])) {
              i          <- next_color_i()
              colors[[key]] <- CLASS_COLORS[((i - 1) %% length(CLASS_COLORS)) + 1]
              next_color_i(i + 1L)
              class_colors(colors)
            }

            # Point-in-polygon
            xs <- shape$x
            ys <- shape$y
            if (length(xs) < 3) {
              # Bounding box from rectangle selection
              in_sel <- df$x >= min(xs) & df$x <= max(xs) &
                        df$y >= min(ys) & df$y <= max(ys)
            } else {
              in_poly <- sp::point.in.polygon(df$x, df$y, xs, ys)
              in_sel  <- in_poly > 0
            }

            in_cluster <- df$cluster == as.character(lcl)
            df$Class[in_sel & in_cluster] <- paste("Class", lcl)
            clustered_data(df)
          }, ignoreInit = TRUE)
        })
      })
    })

    # Assign all pixels of a cluster to a class (no selection needed)
    observeEvent(input$assign_all_btn, {
      df <- clustered_data()
      if (is.null(df)) {
        showNotification("Run clustering first.", type = "warning"); return()
      }

      clusters <- sort(unique(df$cluster[df$cluster != "No_cluster"]))
      colors   <- class_colors()
      i        <- next_color_i()

      for (cl in clusters) {
        key <- as.character(cl)
        if (is.null(colors[[key]])) {
          colors[[key]] <- CLASS_COLORS[((i - 1) %% length(CLASS_COLORS)) + 1]
          i <- i + 1L
        }
        df$Class[df$cluster == as.character(cl)] <- paste("Class", cl)
      }
      next_color_i(i)
      class_colors(colors)
      clustered_data(df)
    })

    # ── Commit to database ────────────────────────────────────────────────────

    observeEvent(input$commit_btn, {
      study_id    <- active_study_id()
      sample_id   <- active_sample_id()
      artifact_id <- active_artifact_id()
      ann_set_id  <- active_ann_set_id()
      pid         <- active_pipeline_id()
      df          <- clustered_data()

      # ── Validate prerequisites
      if (is.null(study_id))  { showNotification("No study selected.", type = "error"); return() }
      if (is.null(sample_id)) { showNotification("No sample selected.", type = "error"); return() }
      if (is.null(pid))       { showNotification("No processing pipeline selected.", type = "error"); return() }
      if (is.null(df))        { showNotification("Run clustering first.", type = "error"); return() }
      if (is.null(ann_set_id)) {
        showNotification("Select or create an annotation set first.", type = "error"); return()
      }

      # Check that at least some pixels are annotated
      annotation_df <- df[!is.na(df$Class), c("x", "y", "Class"), drop = FALSE]
      if (nrow(annotation_df) == 0) {
        showNotification("No pixels have been assigned a class yet.", type = "warning"); return()
      }

      withProgress(message = "Committing to database …", {
        tryCatch({

          # 1) Register clustering pipeline (deterministic ID)
          method <- current_method()
          cluster_params <- list(
            method             = method,
            k                  = input$clusters,
            normalize          = input$normalize,
            input_pipeline_id  = pid   # CRITICAL: provenance link to processing artifact
          )
          if (method %in% c("vsclust", "msiclust")) {
            cluster_params$Sds    <- input$Sds
            cluster_params$minMem <- input$minMem
          }
          if (method == "msiclust") {
            cluster_params$cor_radius <- input$cor_radius
          }

          cluster_pid <- upsert_pipeline(
            type         = "clustering",
            name         = paste0(method, "_k", input$clusters),
            params       = cluster_params,
            code_version = "dev"
          )

          # 2) Save clustering result as artifact
          art_id <- save_clustering_artifact(
            clustered_df       = df,
            study_id           = study_id,
            sample_id          = sample_id,
            input_artifact_id  = artifact_id %||% "",
            cluster_pipeline_id = cluster_pid
          )

          # 3) Save annotations
          ann_id <- save_annotation(
            annotation_df    = annotation_df,
            sample_id        = sample_id,
            annotation_set_id = ann_set_id
          )

          # 4) Legacy compat — insert into clustering_metadata
          tryCatch({
            mongo_cluster_meta <- mongolite::mongo(
              collection = "clustering_metadata",
              db         = DB_NAME,
              url        = MONGO_URL
            )
            mongo_cluster_meta$insert(list(
              assignment_id      = art_id,
              study_id           = study_id,
              sample_id          = sample_id,
              pipeline_id        = pid,
              cluster_pipeline_id = cluster_pid,
              clustering_method  = method,
              k                  = input$clusters,
              normalize          = input$normalize,
              annotation_set_id  = ann_set_id,
              annotation_id      = ann_id,
              n_annotated        = nrow(annotation_df),
              created_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            ))
          }, error = function(e) {
            message("Legacy clustering_metadata insert failed (non-fatal): ", e$message)
          })

          output$commit_status <- renderUI(
            tags$div(style = "color:green; margin-top:6px;",
              tags$b("✓ Committed"), tags$br(),
              tags$small("Artifact: ", substr(art_id, 1, 12), "…"), tags$br(),
              tags$small("Annotation: ", substr(ann_id, 1, 12), "…")
            )
          )
          showNotification("Committed to database.", type = "message")

        }, error = function(e) {
          output$commit_status <- renderUI(
            tags$span(style = "color:red", "Error: ", e$message)
          )
          showNotification(paste("Commit failed:", e$message), type = "error")
        })
      })
    })

    # ── Plots layout ──────────────────────────────────────────────────────────

    output$cluster_layout <- renderUI({
      df <- clustered_data()
      if (is.null(df)) {
        return(tags$div(
          style = "padding:40px; text-align:center; color:#aaa;",
          tags$h4("Load a dataset and run clustering to see results.")
        ))
      }

      has_hist <- !is.null(histology_image())
      plot_rows <- list(
        fluidRow(
          column(6, plotlyOutput(ns("cluster_plot"),  height = "420px")),
          column(6, plotlyOutput(ns("class_plot"),    height = "420px"))
        )
      )
      if (has_hist) {
        plot_rows <- c(plot_rows, list(
          fluidRow(column(12, plotOutput(ns("histology_plot"), height = "320px")))
        ))
      }
      do.call(tagList, plot_rows)
    })

    # ── Cluster plot (plotly, lasso/rectangle selection) ─────────────────────

    output$cluster_plot <- renderPlotly({
      df <- clustered_data()
      req(df)

      colors <- class_colors()

      # Build colour map: cluster → colour
      clusters <- unique(df$cluster)
      pal <- setNames(
        CLASS_COLORS[seq_along(clusters) %% length(CLASS_COLORS) + 1],
        clusters
      )
      pal["No_cluster"] <- "#e0e0e0"
      cols <- pal[df$cluster]

      p <- plot_ly(
        x = df$x, y = df$y, type = "scatter", mode = "markers",
        marker = list(color = cols, size = 4, opacity = 0.85),
        text   = paste("Cluster:", df$cluster),
        hoverinfo = "text",
        source = ns("cluster_plot")
      ) |>
        layout(
          title  = "Cluster map",
          xaxis  = list(title = "x", scaleanchor = "y"),
          yaxis  = list(title = "y"),
          dragmode = "lasso"
        ) |>
        event_register("plotly_relayout") |>
        event_register("plotly_selected")

      p
    })

    # Capture lasso / rectangle selection coordinates
    observeEvent(event_data("plotly_selected", source = ns("cluster_plot")), {
      ev <- event_data("plotly_selected", source = ns("cluster_plot"))
      if (!is.null(ev) && nrow(ev) > 0) {
        sel_shape(list(x = ev$x, y = ev$y))
      }
    })

    # ── Class plot ────────────────────────────────────────────────────────────

    output$class_plot <- renderPlotly({
      df <- clustered_data()
      req(df)

      annotated <- df[!is.na(df$Class), ]
      unanno    <- df[is.na(df$Class),  ]

      colors <- class_colors()
      cls    <- unique(annotated$Class)
      pal    <- setNames(
        CLASS_COLORS[seq_along(cls) %% length(CLASS_COLORS) + 1],
        cls
      )
      col_vec <- ifelse(is.na(df$Class), "#e0e0e0", pal[df$Class])

      plot_ly(
        x = df$x, y = df$y, type = "scatter", mode = "markers",
        marker = list(color = col_vec, size = 4, opacity = 0.85),
        text = paste("Class:", ifelse(is.na(df$Class), "—", df$Class)),
        hoverinfo = "text"
      ) |>
        layout(
          title = "Class map",
          xaxis = list(title = "x", scaleanchor = "y"),
          yaxis = list(title = "y")
        )
    })

    # ── Histology overlay ─────────────────────────────────────────────────────

    output$histology_plot <- renderPlot({
      img <- histology_image()
      df  <- clustered_data()
      req(img, df)

      x_range <- range(df$x, na.rm = TRUE)
      y_range <- range(df$y, na.rm = TRUE)

      graphics::plot(
        1, type = "n",
        xlim = x_range, ylim = y_range,
        xlab = "x", ylab = "y",
        main = "Histology overlay"
      )
      graphics::rasterImage(img,
                            x_range[1], y_range[1],
                            x_range[2], y_range[2],
                            interpolate = TRUE)
    })

  }) # end moduleServer
}
