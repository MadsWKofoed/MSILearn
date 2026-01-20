library(shiny)
library(plotly)
library(sp)

# ------------------------------------------------------------
# 0) Sørg for plot-koordinater (matcher dit MSI PNG grid)
# ------------------------------------------------------------
if (!("x_plot" %in% names(msi_df))) {
  msi_df$x_plot <- msi_df$x - min(msi_df$x, na.rm = TRUE) + 1L
}
if (!("y_plot" %in% names(msi_df))) {
  msi_df$y_plot <- msi_df$y - min(msi_df$y, na.rm = TRUE) + 1L
}

# MSI + H&E URIs
msi_uri <- uri  # din MSI dataURI
# he_uri skal allerede eksistere fra dit script

# Axes (plot coords)
x_range <- c(1, W)
y_range <- c(1, H)

# ------------------------------------------------------------
# 1) Shiny UI
# ------------------------------------------------------------
ui <- fluidPage(
  fluidRow(
    column(8, plotlyOutput("p", height = "750px")),
    column(4,
           h4("ROI punkter"),
           checkboxInput("restrict_to_he", "Restrict ROI to H&E area", value = TRUE),
           verbatimTextOutput("n_inside"),
           tableOutput("preview"),
           downloadButton("download_roi", "Download ROI (CSV)"),
           tags$hr(),
           actionButton("clear_roi", "Clear ROI")
    )
  )
)

# ------------------------------------------------------------
# 2) Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Holder den seneste polygon
  sel_shape <- reactiveVal(NULL)
  
  # Holder de pixels der ligger i ROI
  roi_df <- reactiveVal(NULL)
  
  # --- Plot: overlay (MSI baggrund + H&E ovenpå) ---
  output$p <- renderPlotly({
    plot_ly(source = "overlay") %>%
      add_trace(type="scatter", mode="markers",
                x = numeric(0), y = numeric(0),
                hoverinfo="skip", showlegend=FALSE) %>%
      layout(
        images = list(
          # MSI nederst
          list(
            source = msi_uri,
            xref="x", yref="y",
            x = 1, y = H,
            sizex = W, sizey = H,
            sizing="stretch",
            layer="below"
          ),
          # H&E ovenpå (du tegner visuelt på denne)
          list(
            source = he_uri,
            xref="x", yref="y",
            x = he_params$x, y = he_params$y,
            sizex = he_params$sizex, sizey = he_params$sizey,
            sizing="stretch",
            layer="above"
          )
        ),
        dragmode = "drawclosedpath",
        newshape = list(
          line = list(color = "black", width = 1),
          fillcolor = "rgba(0,0,0,0.05)"
        ),
        xaxis = list(range = x_range, title = "x"),
        yaxis = list(range = y_range, title = "y", scaleanchor="x", scaleratio=1),
        showlegend = FALSE
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToAdd = list("drawclosedpath", "eraseshape")
      )
  })
  
  # --- Capture drawn polygon (samme stil som du brugte før) ---
  observeEvent(event_data("plotly_relayout", source = "overlay"), {
    ev <- event_data("plotly_relayout", source = "overlay")
    req(ev)
    
    # Method 1: shapes[i].path keys
    path_keys <- grep("shapes\\[\\d+\\]\\.path$", names(ev), value = TRUE)
    if (length(path_keys) > 0) {
      path_key <- tail(path_keys, 1)  # seneste shape
      path <- ev[[path_key]]
      
      if (!is.null(path) && nchar(path) > 0) {
        coords <- regmatches(
          path,
          gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path)
        )[[1]]
        
        if (length(coords) > 2) {
          xy <- do.call(rbind, strsplit(coords, ","))
          sel_shape(list(
            type = "polygon",
            x = as.numeric(xy[,1]),
            y = as.numeric(xy[,2])
          ))
          showNotification(sprintf("✓ Polygon captured (%d points)", nrow(xy)),
                           type="message", duration=2)
        }
      }
      return()
    }
    
    # Method 2: shapes data.frame (første draw event)
    if ("shapes" %in% names(ev)) {
      shapes_data <- ev$shapes
      if (is.data.frame(shapes_data) && nrow(shapes_data) > 0) {
        last_row <- shapes_data[nrow(shapes_data), ]
        path <- last_row$path
        
        if (!is.null(path) && nchar(path) > 0) {
          coords <- regmatches(
            path,
            gregexpr("[-+]?[0-9]*\\.?[0-9]+,[-+]?[0-9]*\\.?[0-9]+", path)
          )[[1]]
          
          if (length(coords) > 2) {
            xy <- do.call(rbind, strsplit(coords, ","))
            sel_shape(list(
              type = "polygon",
              x = as.numeric(xy[,1]),
              y = as.numeric(xy[,2])
            ))
            showNotification(sprintf("✓ Polygon captured (%d points)", nrow(xy)),
                             type="message", duration=2)
          }
        }
      }
      return()
    }
  })
  
  # --- Når vi har en polygon: find MSI-pixels indenfor ---
  observeEvent(sel_shape(), {
    shape <- sel_shape()
    req(shape)
    req(identical(shape$type, "polygon"))
    
    poly_x <- shape$x
    poly_y <- shape$y
    
    inside <- sp::point.in.polygon(
      point.x = msi_df$x_plot,
      point.y = msi_df$y_plot,
      pol.x = poly_x,
      pol.y = poly_y
    ) > 0
    
    sel <- msi_df[inside, c("x","y","x_plot","y_plot"), drop = FALSE]
    
    # (Valgfrit) Restrict ROI til H&E rektangel (i plot coords)
    if (isTRUE(input$restrict_to_he) && nrow(sel) > 0) {
      in_he_bbox <- sel$x_plot >= he_params$x &
        sel$x_plot <= (he_params$x + he_params$sizex) &
        sel$y_plot <= he_params$y &
        sel$y_plot >= (he_params$y - he_params$sizey)
      
      sel <- sel[in_he_bbox, , drop = FALSE]
    }
    
    roi_df(sel)
    
    showNotification(sprintf("ROI pixels: %d", nrow(sel)),
                     type = if (nrow(sel) == 0) "warning" else "message",
                     duration = 3)
  })
  
  # --- Clear ROI button ---
  observeEvent(input$clear_roi, {
    sel_shape(NULL)
    roi_df(NULL)
    
    # Fjern shapes i plotly
    try({
      plotlyProxy("p", session) %>%
        plotlyProxyInvoke("relayout", list(shapes = list()))
    }, silent = TRUE)
  })
  
  # --- Outputs ---
  output$n_inside <- renderText({
    df <- roi_df()
    if (is.null(df)) return("Tegn en ROI med drawclosedpath.")
    paste("Antal punkter i ROI:", nrow(df))
  })
  
  output$preview <- renderTable({
    df <- roi_df()
    if (is.null(df)) return(NULL)
    head(df, 25)
  })
  
  output$download_roi <- downloadHandler(
    filename = function() paste0("roi_points_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- roi_df()
      if (is.null(df)) df <- data.frame()
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
