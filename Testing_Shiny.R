# file: app.R
# ────────────────────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
library(plotly)
library(RColorBrewer)
library(mongolite)
library(uuid)
library(jsonlite)

source("Shiny_Clustering.R")

options(shiny.maxRequestSize = 500*1024^2)
options(shiny.launch.browser = TRUE)

# Reusable Mongo connection (optional, not required to run clustering)
msi_con <- mongo(collection = "msi_data", db = "msi_project", url = "mongodb://localhost")



# --- Helpers (drop-in) ---------------------------------------------------------
sanitize_colnames <- function(nms) {
  nms <- gsub("\\.", "_", nms, perl = TRUE)                  # Mongo forbids '.'
  nms <- ifelse(grepl("^\\$", nms), paste0("dollar_", sub("^\\$", "", nms)), nms)  # forbids leading $
  nms
}

normalize_for_mongo <- function(df) {
  # keep only JSON-safe atomic scalars; make names Mongo-safe
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  
  # factors -> character
  is_fac <- vapply(df, is.factor, logical(1))
  if (any(is_fac)) df[is_fac] <- lapply(df[is_fac], as.character)
  
  # list/complex/raw -> character scalars
  is_list <- vapply(df, is.list, logical(1))
  if (any(is_list)) {
    df[is_list] <- lapply(df[is_list], function(col)
      vapply(col, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
    )
  }
  
  # numeric: replace NaN/Inf (JSON/BSON don’t allow them)
  is_num <- vapply(df, is.numeric, logical(1))
  if (any(is_num)) {
    df[is_num] <- lapply(df[is_num], function(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })
  }
  
  # logical NA is fine; character NA will be serialized as null below
  rownames(df) <- NULL
  names(df) <- sanitize_colnames(names(df))
  df
}

stream_import_to_mongo <- function(mongo_con, df) {
  # write NDJSON and import; fastest path for huge frames
  tmp <- tempfile(fileext = ".json")
  out <- file(tmp, open = "wt")
  on.exit({ try(close(out), silent = TRUE); unlink(tmp) }, add = TRUE)
  jsonlite::stream_out(df, con = out, pagesize = 1000, verbose = FALSE)
  close(out)
  mongo_con$import(tmp)  # path string; mongolite detects NDJSON
}

insert_json_batches <- function(mongo_con, df, batch_size = 1000) {
  # fully control JSON; each element = one JSON document
  n <- nrow(df)
  if (!n) return(invisible(NULL))
  starts <- seq(1, n, by = batch_size)
  for (s in starts) {
    e <- min(s + batch_size - 1, n)
    rows <- s:e
    docs <- vapply(rows, function(i) {
      # one object per row
      jsonlite::toJSON(as.list(df[i, , drop = FALSE]),
                       auto_unbox = TRUE, na = "null", null = "null",
                       POSIXt = "ISO8601", digits = NA)
    }, character(1))
    mongo_con$insert(docs)  # vector of JSON documents
  }
  invisible(NULL)
}


ui <- navbarPage(
  title = "MSI Clustering & Prediction",
  tabPanel("Welcome",
           h3("Welcome to the MSI Clustering App"),
           p("Upload imzML + ibd files, perform clustering, and compare to histology.")
  ),
  tabPanel("Clustering",
           sidebarLayout(
             sidebarPanel(
               fileInput("msi_files", "Upload imzML + ibd files",
                         multiple = TRUE, accept = c(".imzML", ".ibd")),
               fileInput("histology", "Optional: Upload histology image",
                         accept = c(".png", ".jpg", ".jpeg", ".tif")),
               numericInput("clusters", "Number of clusters:", value = 3, min = 2, max = 30),
               selectInput("method", "Clustering method:", choices = c("K-means", "Hierarchical")),
               actionButton("run", "Run Clustering"),
               selectInput("orientation", "Orientation adjustment:",
                           choices = c("Default", "Swap axes", "Flip X", "Flip Y", "Flip Both")),
               textInput("class_label", "Assign Class:", value = "Class1"),
               actionButton("assign_class", "Assign to Selection"),
               actionButton("assign_all", "Assign ALL unassigned"),
               tags$hr(),
               actionButton("commit_db", "Commit to MongoDB"),
               width = 2
             ),
             mainPanel(
               uiOutput("cluster_layout"), width = 10
             )
           )
  ),
  tabPanel("Prediction",
           h3("Prediction page"),
           p("This is where tissue classification or other predictions could be implemented.")
  )
)

server <- function(input, output, session) {
  
  uploaded_paths <- reactiveVal(NULL)
  
  observeEvent(input$msi_files, {
    req(input$msi_files)
    # Match by extension on the *original names*; keep both paths
    imzml_idx <- grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)
    ibd_idx   <- grepl("\\.ibd$",   input$msi_files$name, ignore.case = TRUE)
    
    validate(
      need(sum(imzml_idx) == 1, "Please upload one .imzML file"),
      need(sum(ibd_idx)   == 1, "Please upload one .ibd file")
    )
    
    uploaded_paths(list(
      imzml = input$msi_files$datapath[imzml_idx],
      ibd   = input$msi_files$datapath[ibd_idx],
      imzml_name = input$msi_files$name[imzml_idx],
      ibd_name   = input$msi_files$name[ibd_idx]
    ))
  })
  
  # Processing + clustering only on click, with visible errors.
  clustered_data <- eventReactive(input$run, {
    paths <- uploaded_paths()
    req(paths)
    
    withProgress(message = "Running preprocessing & clustering…", value = 0, {
      incProgress(0.05, detail = "Preparing inputs")
      out <- tryCatch({
        df <- process_msi_files(
          imzml_path = paths$imzml,
          ibd_path   = paths$ibd,
          ref_mz_path = "ref_mz.csv"  # optional; handled inside
        )
        incProgress(0.5, detail = "Clustering")
        if (input$method == "K-means") {
          run_kmeans(df, k = input$clusters)
        } else {
          run_hclust(df, k = input$clusters)
        }
      }, error = function(e) {
        showNotification(
          paste("Clustering failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
        return(NULL)
      })
      incProgress(1)
      out
    })
  })
  
  # Histology zoom ranges
  histology_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$histology_brush, {
    histology_ranges$x <- c(input$histology_brush$xmin, input$histology_brush$xmax)
    histology_ranges$y <- c(input$histology_brush$ymin, input$histology_brush$ymax)
  })
  observeEvent(input$reset_zoom, {
    histology_ranges$x <- NULL; histology_ranges$y <- NULL
  })
  
  # Class assignment storage & palette
  annotated_data <- reactiveVal(NULL)
  observeEvent(input$run, { annotated_data(NULL); class_colors(c("Unassigned" = "grey80")); next_color_i(1) })
  
  my_palette <- c(
    "red","blue","green","orange","purple","brown","pink","cyan","magenta","yellow",
    "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
    "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
    "khaki","steelblue","seagreen","tan"
  )
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  
  observeEvent(input$assign_all, {
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    n_unassigned <- sum(is.na(df$Class))
    if (n_unassigned > 0) {
      df$Class[is.na(df$Class)] <- input$class_label
      annotated_data(df)
      cols <- class_colors(); lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, n_unassigned),
                       type = "message", duration = 3)
    } else {
      showNotification("No unassigned pixels left.", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$assign_class, {
    sel <- event_data("plotly_selected", source = "cluster"); req(sel); req("key" %in% names(sel))
    idx <- sort(unique(as.integer(sel$key)))
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class[idx] <- input$class_label
    annotated_data(df)
    cols <- class_colors(); lab <- input$class_label
    if (!(lab %in% names(cols))) {
      i <- next_color_i(); cols[lab] <- my_palette[i]; class_colors(cols)
      next_color_i(if (i == length(my_palette)) 1 else i + 1)
    }
    showNotification(sprintf("Assigned '%s' to %d pixels.", input$class_label, length(idx)),
                     type = "message", duration = 3)
  })
  
  plot_ranges <- reactive({
    df <- clustered_data(); req(df)
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") { tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") { df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") { df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") { df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot }
    list(x = range(df$x_plot, na.rm = TRUE), y = range(df$y_plot, na.rm = TRUE))
  })
  
  output$cluster_layout <- renderUI({
    req(clustered_data())
    if (is.null(input$histology)) {
      fluidRow(
        column(6, plotlyOutput("cluster_plot", height = "600px")),
        column(6, plotlyOutput("class_plot", height = "600px"))
      )
    } else {
      fluidRow(
        column(6, plotOutput("histology_plot", height = "600px",
                             brush = brushOpts(id = "histology_brush", resetOnNew = TRUE)),
               actionButton("reset_zoom", "Reset Zoom")),
        column(6, plotlyOutput("cluster_plot", height = "600px"),
               plotlyOutput("class_plot", height = "600px"))
      )
    }
  })
  
  output$histology_plot <- renderPlot({
    req(input$histology)
    img <- png::readPNG(input$histology$datapath)
    dims <- dim(img)
    x_range <- histology_ranges$x %||% c(0, dims[2])
    y_range <- histology_ranges$y %||% c(0, dims[1])
    ggplot() +
      annotation_raster(img, xmin = 0, xmax = dims[2], ymin = 0, ymax = dims[1]) +
      coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE) +
      theme_void()
  })
  
  output$cluster_plot <- renderPlotly({
    df <- clustered_data(); req(df)
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") { tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") { df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") { df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") { df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot }
    df$row_id <- seq_len(nrow(df))
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = factor(cluster))) +
      geom_tile(width = 1, height = 1) + coord_equal() + theme_minimal() +
      guides(fill = "none") + labs(title = "MSI Clustering Result")
    fig <- ggplotly(g, tooltip = NULL, dynamicTicks = FALSE, source = "cluster")
    fig <- event_register(fig, "plotly_selected")
    fig %>%
      add_markers(
        data = df, x = ~x_plot, y = ~y_plot,
        key = ~row_id, opacity = 0.01, hoverinfo = "skip",
        marker = list(symbol = "square", size = 8),
        showlegend = FALSE, inherit = FALSE
      ) %>%
      layout(
        dragmode = NULL,
        xaxis = list(range = plot_ranges()$x),
        yaxis = list(range = plot_ranges()$y, scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("hoverClosestCartesian","hoverCompareCartesian",
                                        "toggleSpikelines","toImage"))
  })
  
  output$class_plot <- renderPlotly({
    df <- annotated_data() %||% clustered_data(); req(df)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") { tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") { df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") { df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") { df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot }
    present <- unique(df$Class_plot)
    cols <- class_colors(); cols_used <- cols[present]; names(cols_used) <- present
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = Class_plot)) +
      geom_tile(width = 1, height = 1) +
      scale_fill_manual(values = cols_used, drop = FALSE) +
      coord_equal() + theme_minimal() +
      labs(fill = "Class", title = "User Annotation Result")
    ggplotly(g, tooltip = "fill") %>%
      layout(
        dragmode = "zoom",
        xaxis = list(range = plot_ranges()$x),
        yaxis = list(range = plot_ranges()$y, scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("select2d","lasso2d","hoverClosestCartesian",
                                        "hoverCompareCartesian","toggleSpikelines","toImage"))
  })
  
  # Commit to MongoDB (unchanged logic)
  observeEvent(input$commit_db, {
    df <- annotated_data() %||% clustered_data()
    req(df)
    
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class <- as.character(df$Class)
    
    paths <- uploaded_paths()
    assignment_id <- UUIDgenerate()
    committed_at  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
    
    df$row_id         <- seq_len(nrow(df))
    df$assignment_id  <- assignment_id
    df$committed_at   <- committed_at
    df$method         <- input$method
    df$k              <- as.integer(input$clusters)
    df$orientation    <- input$orientation
    df$imzml_file     <- if (!is.null(paths)) paths$imzml_name else NA_character_
    df$ibd_file       <- if (!is.null(paths)) paths$ibd_name   else NA_character_
    df$histology_file <- if (!is.null(input$histology)) basename(input$histology$name) else NA_character_
    
    tryCatch({
      withProgress(message = "Committing to MongoDB…", value = 0, {
        safe_df <- normalize_for_mongo(df)
        incProgress(0.3, detail = "Preparing records")
        
        # First try: fast NDJSON import
        tryCatch({
          stream_import_to_mongo(msi_con, safe_df)
        }, error = function(e) {
          # Fallback: JSON-string batches (avoids any ambiguous R->BSON conversion)
          insert_json_batches(msi_con, safe_df, batch_size = 1000)
        })
        
        incProgress(1, detail = sprintf("assignment_id=%s", assignment_id))
      })
      
      ins_count <- msi_con$count(list(assignment_id = assignment_id))
      showNotification(sprintf("Committed %d rows (assignment_id=%s).", ins_count, assignment_id),
                       type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste0("MongoDB commit failed: ", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })
}

shinyApp(ui = ui, server = server)

