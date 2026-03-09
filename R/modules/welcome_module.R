# R/modules/welcome_module.R

welcome_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Welcome",
    
    div(class = "welcome-container",
    
      h1("MSI Clustering & Prediction Platform"),
      
      p(class="lead",
        "An interactive workflow for analysing Mass Spectrometry Imaging (MSI) data."
      ),
      
      br(),
      
      # WORKFLOW CARDS
      fluidRow(
        
        column(3,
          div(class="welcome-card",
              h4("1. Processing"),
              icon("cogs", "fa-2x"),
              p("Convert raw MSI data into feature matrices."),
              actionButton(ns("show_processing"), "Learn more")
          )
        ),
        
        column(3,
          div(class="welcome-card",
              h4("2. Clustering"),
              icon("project-diagram", "fa-2x"),
              p("Explore spatial clusters and assign labels."),
              actionButton(ns("show_clustering"), "Learn more")
          )
        ),
        
        column(3,
          div(class="welcome-card",
              h4("3. Training"),
              icon("brain", "fa-2x"),
              p("Train machine learning models."),
              actionButton(ns("show_training"), "Learn more")
          )
        ),
        
        column(3,
          div(class="welcome-card",
              h4("4. Prediction"),
              icon("chart-line", "fa-2x"),
              p("Predict tissue classes in new data."),
              actionButton(ns("show_prediction"), "Learn more")
          )
        )
        
      ),
      
      br(),
      br(),
      
      h3("Data Objects in the Platform"),
      
      tags$img(
        src="workflow_diagram.png",
        style="width:100%; max-width:800px;"
      ),
      
      br(),
      
      p(
        "All analysis steps store structured objects in the database to ensure reproducibility."
      ),
      
      br(),
      
      uiOutput(ns("details"))
      
    )
  )
}


welcome_module_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    output$details <- renderUI({
      
      if(input$show_processing > 0){
        return(
          div(
            h4("Processing"),
            p("Raw imzML data is converted into feature matrices using configurable pipelines."),
            p("Output: Artifact (binned_dataframe)")
          )
        )
      }
      
      if(input$show_clustering > 0){
        return(
          div(
            h4("Clustering"),
            p("Spatial clustering identifies tissue structures."),
            p("Clusters can be annotated using biological labels.")
          )
        )
      }
      
      if(input$show_training > 0){
        return(
          div(
            h4("Training"),
            p("Frozen datasets combine features and annotations."),
            p("Machine learning models are trained and evaluated.")
          )
        )
      }
      
      if(input$show_prediction > 0){
        return(
          div(
            h4("Prediction"),
            p("Trained models can classify new MSI samples.")
          )
        )
      }
      
    })
    
  })
}