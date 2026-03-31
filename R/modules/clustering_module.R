# R/modules/clustering_module.R

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

# r
parse_plotly_path_to_shape <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  coords <- regmatches(path, gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path))[[1]]
  if (length(coords) < 3) return(NULL)
  xy <- do.call(rbind, strsplit(coords, ","))
  list(type = "polygon", x = as.numeric(xy[, 1]), y = as.numeric(xy[, 2]))
}

extract_polygon_from_relayout <- function(ev) {
  if (is.null(ev)) return(NULL)
  nms <- names(ev)

  # format: shapes[0].path
  path_keys <- grep("^shapes\\[[0-9]+\\]\\.path$", nms, value = TRUE)
  if (length(path_keys) > 0) {
    path <- ev[[tail(path_keys, 1)]]
    shp <- parse_plotly_path_to_shape(path)
    if (!is.null(shp)) return(shp)
  }

  # format: shapes as data.frame/list
  if ("shapes" %in% nms) {
    sh <- ev$shapes

    if (is.data.frame(sh) && "path" %in% names(sh) && nrow(sh) > 0) {
      path <- sh$path[nrow(sh)]
      shp <- parse_plotly_path_to_shape(path)
      if (!is.null(shp)) return(shp)
    }

    if (is.list(sh) && length(sh) > 0) {
      for (i in rev(seq_along(sh))) {
        if (!is.null(sh[[i]]$path)) {
          shp <- parse_plotly_path_to_shape(sh[[i]]$path)
          if (!is.null(shp)) return(shp)
        }
      }
    }
  }

  NULL
}


clustering_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Clustering",
    sidebarLayout(
      sidebarPanel(
        width = 2,

        tags$style(HTML("
          .step-box {
            border: 1px solid #ddd;
            border-radius: 8px;
            margin-bottom: 10px;
            background: #fff;
            overflow: hidden;
          }
          .step-head {
            padding: 10px 12px;
            font-weight: 600;
            background: #f7f7f9;
            cursor: pointer;
            border-bottom: 1px solid #eee;
          }
          .step-body {
            padding: 10px 12px;
          }
          .btn-blockish {
            width: 100%;
            margin-bottom: 6px;
          }
        ")),

        tags$div(
          class = "step-box",
          tags$div(
            class = "step-head",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("step_data")),
            "1. Data & setup"
          ),
          tags$div(
            id = ns("step_data"),
            class = "step-body collapse in",

            tags$h5("Study & Sample", style = "font-weight:bold; margin-bottom:4px;"),
            selectInput(ns("study_select"), "Study", choices = c("— select —" = ""), width = "100%"),
            actionButton(ns("refresh_studies"), "↺ Refresh", class = "btn-xs"),
            tags$hr(style = "margin:6px 0;"),

            selectInput(ns("sample_select"), "Sample", choices = c("— select study first —" = ""), width = "100%"),

            tags$h5("Workflow", style = "font-weight:bold; margin-bottom:4px; margin-top:12px;"),
            selectInput(
              ns("annotation_mode"), "Mode",
              choices = c("MSI only" = "msi_only", "MSI + NDPI" = "msi_ndpi"),
              selected = "msi_only"
            ),

            tags$h5("Processing Artifact", style = "font-weight:bold; margin-bottom:4px; margin-top:12px;"),
            selectInput(ns("pipeline_select"), "Pipeline", choices = c("— select sample first —" = ""), width = "100%"),
            uiOutput(ns("pipeline_params_ui")),
            actionButton(ns("load_dataset_btn"), "Load dataset", class = "btn-primary btn-sm btn-blockish"),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'msi_ndpi'", ns("annotation_mode")),
              tags$hr(),
              tags$h5("NDPI setup", style = "font-weight:bold; margin-bottom:4px;"),
              fileInput(ns("ndpi_file"), "NDPI slide", accept = c(".ndpi")),
              numericInput(ns("ndpi_workers"), "NDPI workers", value = 8, min = 1, max = 10, step = 1)
            )
          )
        ),

        tags$div(
          class = "step-box",
          tags$div(
            class = "step-head",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("step_cluster")),
            "2. Run clustering"
          ),
          tags$div(
            id = ns("step_cluster"),
            class = "step-body collapse",

            selectInput(ns("method"), "Clustering method", choices = c("K-means", "VSClust", "MSIClust")),
            selectInput(ns("normalize"), "Normalisation", choices = c("None" = "none", "TIC" = "tic", "Median" = "median", "RMS" = "rms")),
            numericInput(ns("clusters"), "Number of clusters", value = 3, min = 3, max = 30),
            uiOutput(ns("method_params_ui")),
            actionButton(ns("run_clustering"), "Run clustering", class = "btn-primary btn-sm btn-blockish")
          )
        ),

        tags$div(
          class = "step-box",
          tags$div(
            class = "step-head",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("step_align")),
            "3. Alignment & orientation"
          ),
          tags$div(
            id = ns("step_align"),
            class = "step-body collapse",

            selectInput(ns("orientation"), "Orientation adjustment", choices = c("Default", "Flip X", "Flip Y", "Flip Both")),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'msi_ndpi'", ns("annotation_mode")),
              tags$hr(),
              tags$h5("NDPI alignment", style = "font-weight:bold; margin-bottom:4px;"),
              textInput(ns("reg_region_id"), "Region ID", value = "R1", placeholder = "e.g. R1"),
              actionButton(ns("save_msi_reg_polygon"), "Save current MSI polygon", class = "btn-sm btn-blockish"),
              actionButton(ns("draw_ndpi_reg_polygon"), "Draw NDPI region polygon", class = "btn-sm btn-blockish"),
              actionButton(ns("fit_registration"), "Fit NDPI → MSI", class = "btn-sm btn-primary btn-blockish"),
              actionButton(ns("reset_registration"), "Reset registration", class = "btn-sm btn-warning btn-blockish"),
              verbatimTextOutput(ns("registration_status"))
            )
          )
        ),

          tags$div(
            class = "step-head",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("step_annot")),
            onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("step_annot_clicked")),
            "4. Annotation"
          ),
          tags$div(
            id = ns("step_annot"),
            class = "step-body collapse",

            tags$h5("Annotation set", style = "font-weight:bold; margin-bottom:4px;"),
            radioButtons(ns("ann_set_mode"), NULL, choices = c("Use existing" = "existing", "Create new" = "new"), selected = "existing"),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'existing'", ns("ann_set_mode")),
              selectInput(ns("ann_set_select"), "Annotation set", choices = c("— select study first —" = ""), width = "100%")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'new'", ns("ann_set_mode")),
              textInput(ns("ann_set_name"), "Name", placeholder = "e.g. Tumour vs Stroma"),
              textInput(ns("ann_set_labels"), "Labels (comma-separated)", placeholder = "Tumour, Stroma, Background"),
              actionButton(ns("create_ann_set_btn"), "Create annotation set", class = "btn-sm btn-success btn-blockish"),
              uiOutput(ns("ann_set_create_status"))
            ),

            selectInput(ns("class_label"), "Assign class", choices = c(), width = "100%"),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'msi_ndpi'", ns("annotation_mode")),
              actionButton(ns("draw_ndpi_polygon"), "Draw NDPI annotation polygon", class = "btn-sm btn-blockish")
            ),

            actionButton(ns("assign_class"), "Assign to selection", class = "btn-sm btn-blockish"),
            actionButton(ns("assign_all"), "Assign all unassigned", class = "btn-sm btn-blockish"),

            tags$hr(),
            actionButton(ns("commit_db"), "Commit to MongoDB", class = "btn-danger btn-sm btn-blockish"),
            uiOutput(ns("commit_status"))
          )
        )
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
                  window.ndpiSyncViewer.startPolygon();
                }
              });
              ",
              ns("ndpiLoadSlide"),
              ns("ndpiStartPolygon")
            )
          ))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'msi_ndpi'", ns("annotation_mode")),
          tags$div(
            tags$h5("NDPI Viewer"),
            tags$div(
              id = ns("ndpi_viewer"),
              style = "width:min(95vw,900px);height:min(55vh,520px);aspect-ratio:1/1;border:1px solid #ccc;margin-bottom:10px;background:#111;"
            )
          )
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

    processed_data          <- reactiveVal(NULL)
    clustered_data          <- reactiveVal(NULL)
    annotated_data          <- reactiveVal(NULL)
    original_clustered      <- reactiveVal(NULL)
    vsclust_membership_data <- reactiveVal(NULL)
    current_method          <- reactiveVal(NULL)

    class_colors <- reactiveVal(c())
    next_color_i <- reactiveVal(1L)
    sel_shape <- reactiveVal(NULL)

    get_latest_msi_polygon <- function() {
      shp <- extract_polygon_from_relayout(
        event_data("plotly_relayout", source = ns("cluster_src"))
      )
      if (!is.null(shp)) return(shp)

      shp <- extract_polygon_from_relayout(input$cluster_plot_relayout)
      if (!is.null(shp)) return(shp)

      sel_shape()
    }

    pixel_class_state <- reactiveVal(NULL)

    # registration by polygon pairs only
    ndpi_draw_mode <- reactiveVal(NULL) # "registration" or "annotation"
    registration_state <- reactiveVal(list(
      ndpi_reg_polys = list(),
      msi_reg_polys = list(),
      fit = NULL,
      fit_by_region = list(),
      valid = FALSE,
      rms = NA_real_,
      rms_by_region = numeric(0),
      orientation_at_fit = NULL,
      ndpi_slide_name = NULL,
      matched_region_ids = character(0),
      n_anchor_pairs = 0L
    ))

    ndpi_runtime <- new.env(parent = emptyenv())
    ndpi_runtime$proc <- NULL
    ndpi_runtime$port <- NULL
    ndpi_runtime$output_dir <- NULL

    normalize_region_id <- function(x) {
      y <- trimws(as.character(x %||% ""))
      if (!nzchar(y)) return(NA_character_)
      y
    }

    get_ann_set_labels <- function(ann_set_id) {
      if (is.null(ann_set_id) || !nzchar(ann_set_id)) return(character(0))
      doc <- tryCatch(
        .con("annotation_sets", DB_NAME, MONGO_URL)$find(
          sprintf('{"_id":"%s"}', ann_set_id),
          fields = '{"label_schema":1}'
        ),
        error = function(e) data.frame()
      )
      if (nrow(doc) == 0 || !("label_schema" %in% names(doc))) return(character(0))
      lbl <- unlist(doc$label_schema[[1]])
      lbl <- trimws(as.character(lbl))
      lbl[nzchar(lbl)]
    }

    update_class_choices <- function() {
      labs <- get_ann_set_labels(active_ann_set_id())
      if (length(labs) == 0) {
        updateSelectInput(session, "class_label", choices = c(), selected = character(0))
      } else {
        updateSelectInput(session, "class_label", choices = labs, selected = labs[1])
      }
    }

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

    clear_cluster_shapes <- function() {
      try(
        plotlyProxy("cluster_plot", session) |>
          plotlyProxyInvoke("relayout", list(shapes = list())),
        silent = TRUE
      )
      sel_shape(NULL)
    }

    open_sidebar_step <- function(step = c("data", "cluster", "align", "annot")) {
      step <- match.arg(step)

      ids <- c(
        data = ns("step_data"),
        cluster = ns("step_cluster"),
        align = ns("step_align"),
        annot = ns("step_annot")
      )

      target <- ids[[step]]

      js <- sprintf(
        "$('#%s').collapse('hide');
        $('#%s').collapse('hide');
        $('#%s').collapse('hide');
        $('#%s').collapse('hide');
        $('#%s').collapse('show');",
        ids[["data"]], ids[["cluster"]], ids[["align"]], ids[["annot"]], target
      )

      shinyjs::runjs(js)
    }

    find_python_bin <- function() {
      py <- Sys.which("python")
      if (!nzchar(py)) py <- Sys.which("python3")
      if (!nzchar(py)) stop("Python executable not found in PATH.")
      py
    }

    find_free_port <- function() httpuv::randomPort()

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
      st$fit_by_region <- list()
      st$valid <- FALSE
      st$rms <- NA_real_
      st$rms_by_region <- numeric(0)
      st$matched_region_ids <- character(0)
      st$n_anchor_pairs <- 0L
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

    close_polygon_xy <- function(poly_xy) {
      poly_xy <- as.matrix(poly_xy)
      storage.mode(poly_xy) <- "double"

      if (ncol(poly_xy) != 2) stop("Polygon must be an n x 2 matrix.")
      if (nrow(poly_xy) < 3) stop("Polygon must have at least 3 points.")

      if (!all(poly_xy[1, ] == poly_xy[nrow(poly_xy), ])) {
        poly_xy <- rbind(poly_xy, poly_xy[1, , drop = FALSE])
      }
      poly_xy
    }

    get_pixels_in_polygon <- function(base_df, poly_xy_original) {
      poly_xy_original <- close_polygon_xy(poly_xy_original)

      inside <- sp::point.in.polygon(
        point.x = base_df$x,
        point.y = base_df$y,
        pol.x = poly_xy_original[, 1],
        pol.y = poly_xy_original[, 2]
      ) > 0

      base_df[inside, c("x", "y"), drop = FALSE]
    }

    bbox_from_xy <- function(xy) {
      xy <- as.matrix(xy)
      storage.mode(xy) <- "double"

      if (nrow(xy) < 1 || ncol(xy) != 2) return(NULL)

      list(
        xmin = min(xy[, 1], na.rm = TRUE),
        xmax = max(xy[, 1], na.rm = TRUE),
        ymin = min(xy[, 2], na.rm = TRUE),
        ymax = max(xy[, 2], na.rm = TRUE)
      )
    }

    bbox_to_corner_matrix_msi <- function(bb) {
      out <- rbind(
        c(bb$xmin, bb$ymin),  # bottom-left
        c(bb$xmax, bb$ymin),  # bottom-right
        c(bb$xmin, bb$ymax),  # top-left
        c(bb$xmax, bb$ymax)   # top-right
      )
      colnames(out) <- c("x", "y")
      storage.mode(out) <- "double"
      out
    }

    bbox_to_corner_matrix_ndpi <- function(bb) {
      out <- rbind(
        c(bb$xmin, bb$ymax),  # bottom-left in MSI terms
        c(bb$xmax, bb$ymax),  # bottom-right in MSI terms
        c(bb$xmin, bb$ymin),  # top-left in MSI terms
        c(bb$xmax, bb$ymin)   # top-right in MSI terms
      )
      colnames(out) <- c("x", "y")
      storage.mode(out) <- "double"
      out
    }

    build_bbox_pairs_from_regions <- function(st, base_df) {
      ids <- intersect(names(st$ndpi_reg_polys), names(st$msi_reg_polys))
      if (length(ids) == 0) {
        return(list(
          ndpi = NULL,
          msi = NULL,
          ids = character(0),
          n = 0L,
          region_pixel_counts = integer(0)
        ))
      }

      ndpi_pts_list <- list()
      msi_pts_list  <- list()
      used_ids <- character(0)
      region_pixel_counts <- integer(0)

      for (id in ids) {
        ndpi_poly <- st$ndpi_reg_polys[[id]]
        msi_poly  <- st$msi_reg_polys[[id]]

        msi_pixels <- get_pixels_in_polygon(base_df, msi_poly)
        if (nrow(msi_pixels) < 3) next

        ndpi_bb <- bbox_from_xy(ndpi_poly)
        msi_bb  <- bbox_from_xy(msi_pixels[, c("x", "y"), drop = FALSE])

        if (is.null(ndpi_bb) || is.null(msi_bb)) next

        ndpi_pts_list[[id]] <- bbox_to_corner_matrix_ndpi(ndpi_bb)
        msi_pts_list[[id]]  <- bbox_to_corner_matrix_msi(msi_bb)

        used_ids <- c(used_ids, id)
        region_pixel_counts[id] <- nrow(msi_pixels)
      }

      if (length(used_ids) == 0) {
        return(list(
          ndpi = NULL,
          msi = NULL,
          ids = character(0),
          n = 0L,
          region_pixel_counts = integer(0)
        ))
      }

      ndpi_pts <- do.call(rbind, ndpi_pts_list)
      msi_pts  <- do.call(rbind, msi_pts_list)

      colnames(ndpi_pts) <- c("x", "y")
      colnames(msi_pts)  <- c("x", "y")

      list(
        ndpi = ndpi_pts,
        msi = msi_pts,
        ids = used_ids,
        n = nrow(ndpi_pts),
        region_pixel_counts = region_pixel_counts
      )
    }

    build_bbox_pairs_for_region <- function(st, base_df, rid) {
      ndpi_poly <- st$ndpi_reg_polys[[rid]]
      msi_poly  <- st$msi_reg_polys[[rid]]

      if (is.null(ndpi_poly) || is.null(msi_poly)) {
        return(list(ndpi = NULL, msi = NULL, id = rid, n = 0L))
      }

      msi_pixels <- get_pixels_in_polygon(base_df, msi_poly)
      if (nrow(msi_pixels) < 3) {
        return(list(ndpi = NULL, msi = NULL, id = rid, n = 0L))
      }

      ndpi_bb <- bbox_from_xy(ndpi_poly)
      msi_bb  <- bbox_from_xy(msi_pixels[, c("x", "y"), drop = FALSE])

      if (is.null(ndpi_bb) || is.null(msi_bb)) {
        return(list(ndpi = NULL, msi = NULL, id = rid, n = 0L))
      }

      ndpi_pts <- bbox_to_corner_matrix_ndpi(ndpi_bb)
      msi_pts  <- bbox_to_corner_matrix_msi(msi_bb)

      list(
        ndpi = ndpi_pts,
        msi = msi_pts,
        id = rid,
        n = nrow(ndpi_pts)
      )
    }

    choose_nearest_region_id <- function(annotation_pts, ndpi_reg_polys, fitted_ids = NULL) {
      ids <- names(ndpi_reg_polys)

      if (!is.null(fitted_ids)) {
        ids <- intersect(ids, fitted_ids)
      }

      if (length(ids) == 0) return(NA_character_)

      ann_ctr <- colMeans(annotation_pts)

      d <- vapply(ids, function(id) {
        poly <- ndpi_reg_polys[[id]]
        if (is.null(poly) || nrow(poly) < 1) return(Inf)
        ctr <- colMeans(as.matrix(poly))
        sqrt(sum((ann_ctr - ctr)^2))
      }, numeric(1))

      ids[which.min(d)]
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

    observeEvent(input$step_annot_clicked, {
      clear_cluster_shapes()
    }, ignoreInit = TRUE)

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
        if (identical(input$annotation_mode, "msi_only")) {
          open_sidebar_step("cluster")
        }
      }, error = function(e) {
        showNotification(paste("Load failed:", e$message), type = "error")
      })
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
        clear_cluster_shapes()

        if (identical(input$annotation_mode, "msi_ndpi")) {
          open_sidebar_step("align")
        } else {
          open_sidebar_step("annot")
        }
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

    }) |> bindEvent(input$orientation)

    observeEvent(input$ann_set_select, {
      v <- input$ann_set_select
      if (!is.null(v) && nzchar(v)) active_ann_set_id(v) else active_ann_set_id(NULL)
      update_class_choices()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$ann_set_mode, {
      if (input$ann_set_mode == "existing") {
        sid <- active_study_id()
        active_ann_set_id(NULL)
        update_class_choices()
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
        update_class_choices()
        output$ann_set_create_status <- renderUI({
          tags$div(class = "alert alert-success", style = "padding:4px; font-size:12px", paste0("✓ Created: ", nm))
        })
        showNotification(paste0("Annotation set created: ", ann_set_id), type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Error creating annotation set:", e$message), type = "error")
      })
    })

    # MSI drawclosedpath capture
    observeEvent(event_data("plotly_relayout", source = ns("cluster_src")), {
      shp <- extract_polygon_from_relayout(event_data("plotly_relayout", source = ns("cluster_src")))
      if (!is.null(shp)) {
        sel_shape(shp)
      }
    })

    # save MSI registration polygon under region id
    observeEvent(input$save_msi_reg_polygon, {
      req(input$annotation_mode == "msi_ndpi")
      rid <- normalize_region_id(input$reg_region_id)
      if (is.na(rid)) {
        showNotification("Enter a Region ID.", type = "warning")
        return()
      }

      shape <- get_latest_msi_polygon()
      if (!is.null(shape)) sel_shape(shape)
      if (is.null(shape) || !identical(shape$type, "polygon")) {
        showNotification("Draw a polygon on MSI cluster plot first.", type = "warning")
        return()
      }

      base_df <- original_clustered()
      req(base_df)

      poly_xy_disp <- cbind(shape$x, shape$y)

    st <- registration_state()
    st$msi_reg_polys[[rid]] <- poly_xy_disp
    st$fit <- NULL
    st$fit_by_region <- list()
    st$valid <- FALSE
    st$rms <- NA_real_
    st$rms_by_region <- numeric(0)
    registration_state(st)

      showNotification(sprintf("Saved MSI registration polygon for %s", rid), type = "message")
      clear_cluster_shapes()
    })

    # trigger NDPI registration polygon drawing
    observeEvent(input$draw_ndpi_reg_polygon, {
      req(input$annotation_mode == "msi_ndpi")
      rid <- normalize_region_id(input$reg_region_id)
      if (is.na(rid)) {
        showNotification("Enter a Region ID.", type = "warning")
        return()
      }
      ndpi_draw_mode("registration")
      session$sendCustomMessage(ns("ndpiStartPolygon"), list())
    })

    # trigger NDPI annotation polygon drawing
    observeEvent(input$draw_ndpi_polygon, {
      req(input$annotation_mode == "msi_ndpi")
      st <- registration_state()

      if (!isTRUE(st$valid) || length(st$fit_by_region) == 0) {
        showNotification("Fit registration first.", type = "warning")
        return()
      }

      clear_cluster_shapes()
      ndpi_draw_mode("annotation")
      session$sendCustomMessage(ns("ndpiStartPolygon"), list())
    })

    observeEvent(input$reset_registration, {
      st <- registration_state()
      st$ndpi_reg_polys <- list()
      st$msi_reg_polys <- list()
      st$fit <- NULL
      st$fit_by_region <- list()
      st$valid <- FALSE
      st$rms <- NA_real_
      st$rms_by_region <- numeric(0)
      st$orientation_at_fit <- NULL
      st$matched_region_ids <- character(0)
      st$n_anchor_pairs <- 0L
      registration_state(st)
      showNotification("Registration reset.", type = "message")
    })

    observeEvent(input$fit_registration, {
      req(input$annotation_mode == "msi_ndpi")

      st <- registration_state()
      base_df <- clustered_data()
      req(base_df)

      ids <- intersect(names(st$ndpi_reg_polys), names(st$msi_reg_polys))
      if (length(ids) == 0) {
        showNotification("Need at least one matched region pair.", type = "warning")
        return()
      }

      fit_by_region <- list()
      rms_by_region <- numeric(0)
      used_ids <- character(0)
      total_pairs <- 0L

      for (rid in ids) {
        pairs <- build_bbox_pairs_for_region(st, base_df, rid)
        if (pairs$n < 4) next

        fit <- fit_affine_ndpi_to_msi(pairs$ndpi, pairs$msi)
        if (!isTRUE(fit$valid)) next

        fit_by_region[[rid]] <- fit
        rms_by_region[rid] <- fit$rms
        used_ids <- c(used_ids, rid)
        total_pairs <- total_pairs + pairs$n
      }

      if (length(used_ids) == 0) {
        st$fit <- NULL
        st$fit_by_region <- list()
        st$valid <- FALSE
        st$rms <- NA_real_
        st$rms_by_region <- numeric(0)
        st$matched_region_ids <- character(0)
        st$n_anchor_pairs <- 0L
        registration_state(st)
        showNotification("Registration fit failed for all region pairs.", type = "error")
        return()
      }

      st$fit <- NULL
      st$fit_by_region <- fit_by_region
      st$valid <- TRUE
      st$rms <- min(rms_by_region)
      st$rms_by_region <- rms_by_region
      st$orientation_at_fit <- input$orientation %||% "Default"
      st$matched_region_ids <- used_ids
      st$n_anchor_pairs <- total_pairs
      registration_state(st)

      showNotification(
        sprintf("Registration valid for %d region(s). Best RMS = %.4f",
                length(used_ids), min(rms_by_region)),
        type = "message"
      )
      clear_cluster_shapes()
      open_sidebar_step("annot")
    })

    observeEvent(input$ndpi_file, {
      req(input$annotation_mode == "msi_ndpi")
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

      ndpi_runtime$proc <- proc
      ndpi_runtime$port <- port
      ndpi_runtime$output_dir <- out_dir

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
      if (!is.null(processed_data())) {
        open_sidebar_step("cluster")
      }
    })

    # NDPI polygon finished: registration OR annotation mode
    observeEvent(input$ndpi_polygon_finished, {
      req(input$annotation_mode == "msi_ndpi")

      pts_list <- input$ndpi_polygon_finished$points
      if (is.null(pts_list) || length(pts_list) < 3) {
        showNotification("Polygon needs at least 3 points.", type = "warning")
        return()
      }

      pts <- do.call(rbind, lapply(pts_list, function(p) c(as.numeric(p$x), as.numeric(p$y))))
      colnames(pts) <- c("x", "y")

      mode <- ndpi_draw_mode()

      if (identical(mode, "registration")) {
        rid <- normalize_region_id(input$reg_region_id)
        if (is.na(rid)) {
          showNotification("Enter a Region ID before drawing NDPI registration polygon.", type = "warning")
          ndpi_draw_mode(NULL)
          return()
        }

        st <- registration_state()
        st$ndpi_reg_polys[[rid]] <- pts
        st$fit <- NULL
        st$fit_by_region <- list()
        st$valid <- FALSE
        st$rms <- NA_real_
        st$rms_by_region <- numeric(0)
        registration_state(st)

        ndpi_draw_mode(NULL)
        showNotification(sprintf("Saved NDPI registration polygon for %s", rid), type = "message")
        return()
      }

      if (identical(mode, "annotation")) {
        st <- registration_state()
        base_df <- original_clustered()
        req(base_df)

        if (!isTRUE(st$valid) || length(st$fit_by_region) == 0) {
          showNotification("Registration is not valid.", type = "warning")
          ndpi_draw_mode(NULL)
          return()
        }

        if (!identical(st$orientation_at_fit, input$orientation %||% "Default")) {
          showNotification("Orientation changed since fit. Refit registration.", type = "warning")
          ndpi_draw_mode(NULL)
          return()
        }

        if (is.null(input$class_label) || !nzchar(input$class_label)) {
          showNotification("Select a class from the annotation set.", type = "warning")
          ndpi_draw_mode(NULL)
          return()
        }

        fitted_ids <- names(st$fit_by_region)

        rid_use <- choose_nearest_region_id(
          annotation_pts = pts,
          ndpi_reg_polys = st$ndpi_reg_polys,
          fitted_ids = fitted_ids
        )

        fit_use <- st$fit_by_region[[rid_use]]
        showNotification(
          sprintf("Using fitted region %s", rid_use),
          type = "message"
        )

        if (is.null(rid_use) || is.na(rid_use) || is.null(fit_use)) {
          showNotification("Could not determine nearest fitted region.", type = "warning")
          ndpi_draw_mode(NULL)
          return()
        }

        poly_disp <- apply_affine_xy(pts, fit_use$A, fit_use$b)

        poly_orig <- to_original_polygon(
          poly_disp,
          input$orientation %||% "Default",
          base_df
        )

        cls <- pixel_class_state()
        if (is.null(cls) || length(cls) != nrow(base_df)) cls <- rep("Unassigned", nrow(base_df))

        res <- assign_polygon_to_pixel_classes(
          base_df = base_df,
          poly_xy_original = poly_orig,
          class_label = input$class_label,
          class_vec = cls
        )

        pixel_class_state(res$class_vec)
        sync_annotated_from_state()
        ndpi_draw_mode(NULL)
        showNotification(
          sprintf("NDPI polygon assigned '%s' to %d MSI pixels using region %s.",
                  input$class_label, res$n_updated, rid_use),
          type = "message"
        )
      }
    })

    output$registration_status <- renderText({
      st <- registration_state()
      ndpi_ids <- names(st$ndpi_reg_polys)
      msi_ids <- names(st$msi_reg_polys)
      matched <- intersect(ndpi_ids, msi_ids)

      paste0(
        "NDPI registration polygons: ", length(ndpi_ids), "\n",
        "MSI registration polygons: ", length(msi_ids), "\n",
        "Matched Region IDs: ", ifelse(length(matched) > 0, paste(matched, collapse = ", "), "None"), "\n",
        "BBox corner pairs used: ", st$n_anchor_pairs, "\n",
        "Valid: ", isTRUE(st$valid), "\n",
        "Best RMS: ", ifelse(is.finite(st$rms), format(round(st$rms, 4), nsmall = 4), "NA"), "\n",
        "RMS by region: ", ifelse(length(st$rms_by_region) > 0,
                                  paste(names(st$rms_by_region), sprintf("%.4f", st$rms_by_region), collapse = ", "),
                                  "NA"), "\n",
        "Orientation at fit: ", st$orientation_at_fit %||% "NA", "\n",
        "Slide: ", st$ndpi_slide_name %||% "NA"
      )
    })

    observeEvent(input$assign_class, {
      shape <- get_latest_msi_polygon()
      if (!is.null(shape)) sel_shape(shape)
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

      if (is.null(input$class_label) || !nzchar(input$class_label)) {
        showNotification("Select a class from the annotation set.", type = "warning")
        return()
      }

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

      if (is.null(input$class_label) || !nzchar(input$class_label)) {
        showNotification("Select a class from the annotation set.", type = "warning")
        return()
      }

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


    output$cluster_layout <- renderUI({
      req(clustered_data())
      fluidRow(
        column(6, plotlyOutput(ns("cluster_plot"), height = "600px")),
        column(6, plotlyOutput(ns("class_plot"), height = "600px"))
      )
    })

    observeEvent(input$commit_db, {
      study_id <- active_study_id()
      sample_id <- active_sample_id()
      artifact_id <- active_artifact_id()
      ann_set_id <- active_ann_set_id()
      pid <- active_pipeline_id()
      base_df <- original_clustered()
      cls <- pixel_class_state()

      if (is.null(study_id)) return(showNotification("No study selected.", type = "error"))
      if (is.null(sample_id)) return(showNotification("No sample selected.", type = "error"))
      if (is.null(pid)) return(showNotification("No pipeline selected.", type = "error"))
      if (is.null(base_df)) return(showNotification("Run clustering first.", type = "error"))
      if (is.null(ann_set_id)) return(showNotification("Select or create an annotation set.", type = "error"))
      if (is.null(cls) || length(cls) != nrow(base_df)) return(showNotification("Annotation state is not initialized.", type = "error"))

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
              matched_region_ids = st$matched_region_ids,
              n_anchor_pairs = st$n_anchor_pairs,
              ndpi_reg_polys = st$ndpi_reg_polys,
              msi_reg_polys = st$msi_reg_polys,
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