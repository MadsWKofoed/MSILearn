# app.R
# ────────────────────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
library(plotly)
library(RColorBrewer)
library(mongolite)
library(uuid)       # for assignment_id

# Source clustering helpers
source("Shiny_Clustering.R")

options(shiny.maxRequestSize = 500*1024^2)
options(shiny.launch.browser = TRUE)

# Global Mongo connection (reuse across sessions/process where possible)
msi_con <- mongo(
  collection = "msi_data",
  db = "msi_project",
  url = "mongodb://localhost"
)

# ── Helpers (server-agnostic) ───────────────────────────────────────────────────
sanitize_colnames <- function(nms) {
  # Why: MongoDB does not allow dots in keys or keys starting with '$'
  nms <- gsub("\\.", "_", nms, perl = TRUE)
  nms <- ifelse(grepl("^\\$", nms), paste0("dollar_", sub("^\\$", "", nms)), nms)
  nms
}

insert_in_batches <- function(con, df, batch_size = 5000, progress_cb = NULL) {
  n <- nrow(df)
  if (n == 0) return(invisible(NULL))
  starts <- seq(1, n, by = batch_size)
  for (i in seq_along(starts)) {
    s <- starts[i]
    e <- min(s + batch_size - 1, n)
    # Why: protect large writes, show incremental progress
    con$insert(df[s:e, , drop = FALSE])
    if (!is.null(progress_cb)) progress_cb(i / length(starts))
  }
  invisible(NULL)
}

# ── UI ──────────────────────────────────────────────────────────────────────────
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
                         multiple = TRUE,
                         accept = c(".imzML", ".ibd")),
               
               fileInput("histology", "Optional: Upload histology image",
                         accept = c(".png", ".jpg", ".jpeg", ".tif")),
               
               numericInput("clusters", "Number of clusters:", 
                            value = 3, min = 2, max = 30),
               
               selectInput("method", "Clustering method:",
                           choices = c("K-means", "Hierarchical")),
               
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
               uiOutput("cluster_layout"),
               width = 10
             )
           )
  ),
  
  tabPanel("Prediction",
           h3("Prediction page"),
           p("This is where tissue classification or other predictions could be implemented.")
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # just cache uploaded paths, no processing yet
  uploaded_paths <- reactiveVal(NULL)
  
  observeEvent(input$msi_files, {
    req(input$msi_files)
    imzml_path <- input$msi_files$datapath[grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)]
    ibd_path   <- input$msi_files$datapath[grepl("\\.ibd$", input$msi_files$name, ignore.case = TRUE)]
    
    validate(
      need(length(imzml_path) == 1, "Please upload one .imzML file"),
      need(length(ibd_path) == 1, "Please upload one .ibd file")
    )
    
    uploaded_paths(list(
      imzml = imzml_path,
      ibd   = ibd_path,
      imzml_name = basename(input$msi_files$name[grepl("\\.imzML$", input$msi_files$name, ignore.case = TRUE)]),
      ibd_name   = basename(input$msi_files$name[grepl("\\.ibd$", input$msi_files$name, ignore.case = TRUE)])
    ))
  })
  
  # processing + clustering happens only on button click
  clustered_data <- eventReactive(input$run, {
    paths <- uploaded_paths()
    req(paths)
    
    message("Preprocessing + clustering now running...")
    df <- process_msi_files(paths$imzml, paths$ibd, ref_mz_path = "ref_mz.csv")
    
    if (input$method == "K-means") {
      run_kmeans(df, k = input$clusters)
    } else {
      run_hclust(df, k = input$clusters)
    }
  })
  
  # --- Histology zoom ranges ---
  histology_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$histology_brush, {
    histology_ranges$x <- c(input$histology_brush$xmin, input$histology_brush$xmax)
    histology_ranges$y <- c(input$histology_brush$ymin, input$histology_brush$ymax)
  })
  
  observeEvent(input$reset_zoom, {
    histology_ranges$x <- NULL
    histology_ranges$y <- NULL
  })
  
  # --- Class assignment storage ---
  annotated_data <- reactiveVal(NULL)
  
  observeEvent(input$run, { 
    annotated_data(NULL)
    class_colors(c("Unassigned" = "grey80"))
    next_color_i(1)
  })
  
  # --- Fixed palette + pointer ---
  my_palette <- c(
    "red","blue","green","orange","purple","brown","pink","cyan","magenta","yellow",
    "darkred","darkblue","darkgreen","darkorange","darkviolet","gold","gray20","gray50",
    "deepskyblue","springgreen","navy","maroon","olive","turquoise","orchid","salmon",
    "khaki","steelblue","seagreen","tan"
  )
  
  class_colors <- reactiveVal(c("Unassigned" = "grey80"))
  next_color_i <- reactiveVal(1)
  
  # --- Assign ALL unassigned to a class ---
  observeEvent(input$assign_all, {
    df <- annotated_data() %||% clustered_data()
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    
    n_unassigned <- sum(is.na(df$Class))
    if (n_unassigned > 0) {
      df$Class[is.na(df$Class)] <- input$class_label
      annotated_data(df)
      
      cols <- class_colors()
      lab <- input$class_label
      if (!(lab %in% names(cols))) {
        i <- next_color_i()
        cols[lab] <- my_palette[i]
        class_colors(cols)
        next_color_i(if (i == length(my_palette)) 1 else i + 1)
      }
      
      showNotification(
        sprintf("Assigned '%s' to %d previously unassigned pixels.", 
                input$class_label, n_unassigned),
        type = "message", duration = 3
      )
    } else {
      showNotification("No unassigned pixels left.", type = "warning", duration = 3)
    }
  })
  
  # --- Assign selected pixels to a class + assign color sequentially ---
  observeEvent(input$assign_class, {
    sel <- event_data("plotly_selected", source = "cluster")
    req(sel)
    req("key" %in% names(sel))
    
    idx <- sort(unique(as.integer(sel$key)))
    
    df <- annotated_data() %||% clustered_data()
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class[idx] <- input$class_label
    annotated_data(df)
    
    cols <- class_colors()
    lab <- input$class_label
    if (!(lab %in% names(cols))) {
      i <- next_color_i()
      cols[lab] <- my_palette[i]
      class_colors(cols)
      next_color_i(if (i == length(my_palette)) 1 else i + 1)
    }
    
    showNotification(
      sprintf("Assigned '%s' to %d pixels.", input$class_label, length(idx)),
      type = "message", duration = 3
    )
  })
  
  plot_ranges <- reactive({
    df <- clustered_data()
    req(df)
    
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") {
      tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") {
      df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") {
      df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") {
      df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot
    }
    
    list(
      x = range(df$x_plot, na.rm = TRUE),
      y = range(df$y_plot, na.rm = TRUE)
    )
  })
  
  # --- Clustering plot ---
  output$cluster_plot <- renderPlotly({
    df <- clustered_data()
    
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") {
      tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") {
      df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") {
      df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") {
      df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot
    }
    
    df$row_id <- seq_len(nrow(df))
    
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = factor(cluster))) +
      geom_tile(width = 1, height = 1) +
      coord_equal() +
      theme_minimal() +
      guides(fill = "none") +
      labs(title = "MSI Clustering Result")
    
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
        dragmode = input$dragmode,
        xaxis = list(range = plot_ranges()$x),
        yaxis = list(range = plot_ranges()$y, scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("hoverClosestCartesian",
                                   "hoverCompareCartesian",
                                   "toggleSpikelines", "toImage")
      )
  })
  
  # --- Class plot ---
  output$class_plot <- renderPlotly({
    df <- annotated_data() %||% clustered_data()
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class_plot <- ifelse(is.na(df$Class), "Unassigned", df$Class)
    
    df$x_plot <- df$x; df$y_plot <- df$y
    if (input$orientation == "Swap axes") {
      tmp <- df$x_plot; df$x_plot <- df$y_plot; df$y_plot <- tmp
    } else if (input$orientation == "Flip X") {
      df$x_plot <- -df$x_plot
    } else if (input$orientation == "Flip Y") {
      df$y_plot <- -df$y_plot
    } else if (input$orientation == "Flip Both") {
      df$x_plot <- -df$x_plot; df$y_plot <- -df$y_plot
    }
    
    present <- unique(df$Class_plot)
    cols <- class_colors()
    cols_used <- cols[present]; names(cols_used) <- present
    
    g <- ggplot(df, aes(x = x_plot, y = y_plot, fill = Class_plot)) +
      geom_tile(width = 1, height = 1) +
      scale_fill_manual(values = cols_used, drop = FALSE) +
      coord_equal() +
      theme_minimal() +
      labs(fill = "Class", title = "User Annotation Result")
    
    ggplotly(g, tooltip = "fill") %>%
      layout(
        dragmode = "zoom",
        xaxis = list(range = plot_ranges()$x),
        yaxis = list(range = plot_ranges()$y, scaleanchor = "x", scaleratio = 1)
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d",
                                   "hoverClosestCartesian",
                                   "hoverCompareCartesian",
                                   "toggleSpikelines", "toImage")
      )
  })
  
  # ── COMMIT TO MONGODB ─────────────────────────────────────────────────────────
  observeEvent(input$commit_db, {
    df <- annotated_data() %||% clustered_data()
    req(df)
    
    # Ensure Class column exists and is character (NA -> null)
    if (!"Class" %in% names(df)) df$Class <- NA_character_
    df$Class <- as.character(df$Class)
    
    # File metadata
    paths <- uploaded_paths()
    imzml_name <- if (!is.null(paths)) paths$imzml_name else NA_character_
    ibd_name   <- if (!is.null(paths)) paths$ibd_name   else NA_character_
    histo_name <- if (!is.null(input$histology)) basename(input$histology$name) else NA_character_
    
    # Assignment + context metadata
    assignment_id <- UUIDgenerate()
    committed_at  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")  # ISO-ish
    
    df$row_id      <- seq_len(nrow(df))
    df$assignment_id <- assignment_id
    df$committed_at  <- committed_at
    df$method        <- input$method
    df$k             <- as.integer(input$clusters)
    df$orientation   <- input$orientation
    df$imzml_file    <- imzml_name
    df$ibd_file      <- ibd_name
    df$histology_file<- histo_name
    
    # Sanitize column names for Mongo
    names(df) <- sanitize_colnames(names(df))
    
    # Write with progress + basic error handling
    tryCatch({
      withProgress(message = "Committing to MongoDB…", value = 0, {
        insert_in_batches(
          con = msi_con,
          df  = df,
          batch_size = 5000,
          progress_cb = function(p) incProgress(p - get("._last_p", envir = .GlobalEnv, inherits = FALSE, ifnotfound = 0),
                                                detail = sprintf("assignment_id=%s", assignment_id))
        )
      })
      
      # Count rows inserted for this assignment
      ins_count <- msi_con$count(query = list(assignment_id = assignment_id))
      showNotification(
        sprintf("Success: committed %d pixel rows (assignment_id=%s).", ins_count, assignment_id),
        type = "message", duration = 6
      )
    }, error = function(e) {
      showNotification(
        paste0("MongoDB commit failed: ", conditionMessage(e)),
        type = "error", duration = 8
      )
    })
  })
}

shinyApp(ui = ui, server = server)
