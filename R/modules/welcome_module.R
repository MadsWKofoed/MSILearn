# R/modules/welcome_module.R


welcome_module_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Welcome",
    app_page_shell(
      app_page_hero(
        "MSI Clustering & Prediction Platform",
        "An interactive workflow for analysing Mass Spectrometry Imaging (MSI) data across processing, clustering, training, prediction, and database management."
      ),
      div(class = "welcome-container",
        fluidRow(
          column(
            3,
            div(
              class = "welcome-card",
              h4("1. Processing"),
              icon("cogs", "fa-2x"),
              p("Convert raw MSI data into feature matrices."),
              actionButton(ns("show_processing"), "Learn more")
            )
          ),
          column(
            3,
            div(
              class = "welcome-card",
              h4("2. Clustering"),
              icon("project-diagram", "fa-2x"),
              p("Explore spatial clusters and assign labels."),
              actionButton(ns("show_clustering"), "Learn more")
            )
          ),
          column(
            3,
            div(
              class = "welcome-card",
              h4("3. Training"),
              icon("brain", "fa-2x"),
              p("Train machine learning models."),
              actionButton(ns("show_training"), "Learn more")
            )
          ),
          column(
            3,
            div(
              class = "welcome-card",
              h4("4. Prediction"),
              icon("chart-line", "fa-2x"),
              p("Predict tissue classes in new data."),
              actionButton(ns("show_prediction"), "Learn more")
            )
          )
        ),
        br(),
        uiOutput(ns("details")),
        br(),
        h3("Data Objects in the Platform"),
        p(
          class = "lead-small",
          "Click an object in the workflow below to see what it represents and how it is used."
        ),
        div(
          class = "object-flow-wrap",
          div(
            class = "object-flow-row",
            actionButton(ns("obj_study"), "Study", class = "flow-object-btn"),
            div(class = "flow-arrow", HTML("&rarr;")),
            actionButton(ns("obj_sample"), "Sample", class = "flow-object-btn"),
            div(class = "flow-arrow", HTML("&rarr;")),
            actionButton(ns("obj_pipeline"), "Pipeline", class = "flow-object-btn"),
            div(class = "flow-arrow", HTML("&rarr;")),
            actionButton(ns("obj_artifact"), "Artifact", class = "flow-object-btn")
          ),
          div(class = "flow-break"),
          div(
            class = "object-flow-row",
            actionButton(ns("obj_annset"), "Annotation Set", class = "flow-object-btn"),
            div(class = "flow-arrow", HTML("&rarr;")),
            actionButton(ns("obj_annotation"), "Annotation", class = "flow-object-btn"),
            div(class = "flow-arrow", HTML("&rarr;")),
            actionButton(ns("obj_dataset"), "Dataset", class = "flow-object-btn"),
            div(class = "flow-arrow", HTML("&rarr;")),
            actionButton(ns("obj_modelrun"), "Model Run", class = "flow-object-btn")
          )
        ),
        br(),
        uiOutput(ns("object_details")),
        br(),
        div(
          class = "welcome-detail-box soft-box",
          h4("Suggested workflow"),
          p(
            strong("Processing"), " creates feature representations from raw MSI data. ",
            strong("Clustering"), " helps identify spatial structure and assign labels. ",
            strong("Training"), " uses reproducible datasets to fit machine learning models. ",
            strong("Prediction"), " applies trained models to new data."
          )
        )
      )
    )
  )
}


welcome_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    active_panel  <- reactiveVal("intro")
    active_object <- reactiveVal("none")

    observeEvent(input$show_processing, active_panel("processing"))
    observeEvent(input$show_clustering, active_panel("clustering"))
    observeEvent(input$show_training,   active_panel("training"))
    observeEvent(input$show_prediction, active_panel("prediction"))

    observeEvent(input$obj_study,      active_object("study"))
    observeEvent(input$obj_sample,     active_object("sample"))
    observeEvent(input$obj_pipeline,   active_object("pipeline"))
    observeEvent(input$obj_artifact,   active_object("artifact"))
    observeEvent(input$obj_annset,     active_object("annset"))
    observeEvent(input$obj_annotation, active_object("annotation"))
    observeEvent(input$obj_dataset,    active_object("dataset"))
    observeEvent(input$obj_modelrun,   active_object("modelrun"))

    output$details <- renderUI({
      panel <- active_panel()

      if (panel == "processing") {
        return(
          div(
            class = "welcome-detail-box",
            h4("Processing"),
            p("Raw imzML and ibd files are converted into pixel-level feature matrices using a configurable processing pipeline."),
            p(strong("Main stored objects: "), "Study, Sample, Pipeline, and Artifact."),
            p(strong("Main output: "), "A binned_dataframe artifact used for clustering and downstream model training.")
          )
        )
      }

      if (panel == "clustering") {
        return(
          div(
            class = "welcome-detail-box",
            h4("Clustering"),
            p("Processed MSI features are clustered to identify spatial patterns and tissue structure."),
            p("Users can inspect results and assign biologically meaningful labels."),
            p(strong("Main stored objects: "), "Clustering pipeline, clustering artifact, annotation set, and annotations.")
          )
        )
      }

      if (panel == "training") {
        return(
          div(
            class = "welcome-detail-box",
            h4("Training"),
            p("Frozen datasets combine selected samples, processing outputs, annotation labels, and split settings into a reproducible training input."),
            p("Machine learning models are trained and stored together with hyperparameters and evaluation metrics."),
            p(strong("Main stored objects: "), "Dataset and Model Run.")
          )
        )
      }

      if (panel == "prediction") {
        return(
          div(
            class = "welcome-detail-box",
            h4("Prediction"),
            p("Trained models can be applied to new MSI data to generate predicted labels."),
            p("This module is intended for inference on unseen samples and comparison of prediction results.")
          )
        )
      }

      div(
        class = "welcome-detail-box",
        h4("Get started"),
        p("Click one of the workflow cards above to learn more about each module."),
        tags$ol(
          tags$li("Process raw MSI data"),
          tags$li("Cluster pixels and assign labels"),
          tags$li("Create a reproducible dataset"),
          tags$li("Train and compare models"),
          tags$li("Apply trained models to new data")
        )
      )
    })

    output$object_details <- renderUI({
      obj <- active_object()

      if (obj == "study") {
        return(
          div(class = "welcome-detail-box",
            h4("Study"),
            p("A study is the top-level organisational unit in the platform."),
            p("It groups related MSI samples that belong to the same project, experiment, or cohort."),
            p(strong("Used in: "), "Processing, Clustering, and Training.")
          )
        )
      }

      if (obj == "sample") {
        return(
          div(class = "welcome-detail-box",
            h4("Sample"),
            p("A sample represents one individual MSI experiment or tissue section within a study."),
            p("Samples are the units that are processed, clustered, annotated, and later included in datasets."),
            p(strong("Used in: "), "Processing, Clustering, and Training.")
          )
        )
      }

      if (obj == "pipeline") {
        return(
          div(class = "welcome-detail-box",
            h4("Pipeline"),
            p("A pipeline stores the parameter configuration used for a specific analysis step."),
            p("It describes how processing or clustering was performed, but it does not contain the output data itself."),
            p(strong("Think of it as: "), "the method or recipe.")
          )
        )
      }

      if (obj == "artifact") {
        return(
          div(class = "welcome-detail-box",
            h4("Artifact"),
            p("An artifact is the output generated by a pipeline."),
            p("For example, a processing pipeline can generate a binned feature matrix stored as a binned_dataframe artifact."),
            p(strong("Think of it as: "), "the result produced by the method.")
          )
        )
      }

      if (obj == "annset") {
        return(
          div(class = "welcome-detail-box",
            h4("Annotation Set"),
            p("An annotation set defines the list of possible labels that can be assigned."),
            p("Examples could be labels such as Healthy, LowGrade, HighGrade, or Squamous."),
            p(strong("Think of it as: "), "the vocabulary of labels.")
          )
        )
      }

      if (obj == "annotation") {
        return(
          div(class = "welcome-detail-box",
            h4("Annotation"),
            p("An annotation is the actual assignment of a label to specific pixels or regions within a sample."),
            p("Annotations are used as ground truth labels for supervised learning."),
            p(strong("Think of it as: "), "the concrete label assignment.")
          )
        )
      }

      if (obj == "dataset") {
        return(
          div(class = "welcome-detail-box",
            h4("Dataset"),
            p("A dataset is a frozen snapshot used for machine learning."),
            p("It combines selected samples, processed data, annotations, and split settings into one reproducible training input."),
            p(strong("Used in: "), "Training.")
          )
        )
      }

      if (obj == "modelrun") {
        return(
          div(class = "welcome-detail-box",
            h4("Model Run"),
            p("A model run is one concrete machine learning experiment."),
            p("It stores the trained model, the selected hyperparameters, and the evaluation metrics."),
            p(strong("Used in: "), "Training and later prediction workflows.")
          )
        )
      }

      div(
        class = "welcome-detail-box",
        h4("Explore the data model"),
        p("Follow the workflow above from left to right."),
        p("Click any object to see what it means and how it fits into the platform.")
      )
    })
  })
}
