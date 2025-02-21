sidebarLayout(
  sidebarPanel(
    tooltip(
      bs_icon("info-circle"),
      "Data imported should be in CSV format. Upload all three omics as three separate files"
    ),
    fileInput('file1', 'Omics file 1',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    fileInput('file2', 'Omics file 2',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    fileInput('file3', 'Omics file 3',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tooltip(
      bs_icon("info-circle"),
      "Metadata should be imported only if you want to split the train-test data based on replicates.
      It must contain at least two pieces of information: (1) an ID column that matches the omics data,
      and (2) replicate information specific to your experimental conditions"
    ),
    fileInput('file4', 'Meta data file',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tags$hr(),
    radioButtons("choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    tags$hr(),
    tooltip(
      bs_icon("info-circle"),
      "Choose 'Replicate' only if you want to split the train-test data based on replicates"
    ),
    radioButtons("choosereplicate", 
                 "Choose replicate:",
                 c("Random", "Replicate")),
    tags$hr(),
    tooltip(
      bs_icon("info-circle"),
      "Choose cross-validation (CV) for robust model generalization, whereas leave-one-out cross-validation (LOOCV) is more suitable for small datasets."
    ),
    radioButtons("choosecv", 
                 "Choose cross-validation:",
                 c("CV", "LOOCV")),
    tags$hr(),
    tooltip(
      bs_icon("info-circle"),
      "Select the target variable you want to predict"
    ),
    uiOutput("target_data"),
    
    
    h4("Prediction"),
    tooltip(
      bs_icon("info-circle"),
      "Recommended max depth: 3 to 6"
    ),
    sliderInput("max_depth", "Max Depth:", value = 2, min = 1, max = 10, step = 1),
    tooltip(
      bs_icon("info-circle"),
      "Set nrounds as long as the model continues improving"
    ),
    sliderInput("nrounds", "Number of Rounds:", value = 10, min = 1, max = 100, step = 1),
    tooltip(
      bs_icon("info-circle"),
      "Depends on the size of your dataset"
    ),
    sliderInput("proportion", "Training Size:", value = 0.8, min = 0.5, max = 1, step = 0.05),
    actionButton("optimize_btn", "Optimize Hyperparameters", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Adjust hyperparameters using the sliders, then click 'Optimize Hyperparameters'.
      Once optimization is complete, click 'Run' to generate results."
    ),
    actionButton("xgb_btn", "Run Model", icon = icon("refresh")),
    tags$hr(),
    
    h4("Interaction"),
    tooltip(
      bs_icon("info-circle"),
      "Provide your list of features to check for interactions"
    ),
    fileInput('file5', 'List of features',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tooltip(
      bs_icon("info-circle"),
      "Choose 'Random' if you are providing unknown features, and
      'Monotonic' if you are providing known features"
    ),
    radioButtons("constraints", 
                 "Choose constraints:",
                 c("Random", "Monotonic")),
    actionButton("cons_btn", "Constrain", icon = icon("refresh")),
  ),
  mainPanel(
    fluidRow(
      column(width = 12, tags$hr()), # Add gap between plots
      column(
        width = 6,  # Full width on small screens (mobile)
        sm = 6,  # 50% width on medium screens (tablets) and above
        shinycssloaders::withSpinner(plotlyOutput("distributionPlot"))
      ),
      column(
        width = 6,  # Full width on small screens (mobile)
        sm = 6,  # 50% width on medium screens (tablets) and above
        shinycssloaders::withSpinner(plotlyOutput("com_plot"))
      ),
      column(width = 12, tags$hr()), # Add gap between plots
      column(
        width = 6,  # Full width on small screens (mobile)
        sm = 6,  # 50% width on medium screens (tablets) and above
        DT::dataTableOutput("feature_importance")
      ),
      column(
        width = 6,  # Full width on small screens (mobile)
        sm = 6,  # 50% width on medium screens (tablets) and above
        shinycssloaders::withSpinner(plotlyOutput("shap_plot"))
      ),
      column(
        width = 6,  # Full width on small screens (mobile)
        sm = 6,  # 50% width on medium screens (tablets) and above
        shinycssloaders::withSpinner(plotlyOutput("boruta_plot"))
      ),
      column(
        width = 6,  # Full width on small screens (mobile)
        sm = 6,  # 50% width on medium screens (tablets) and above
        shinycssloaders::withSpinner(plotlyOutput("interaction_plot"))
      )
    ),
    uiOutput("download")
  )
)

