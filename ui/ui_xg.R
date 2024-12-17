sidebarLayout(
  sidebarPanel(
    tooltip(
      bs_icon("info-circle"),
      "Data imported should contain feature and target variable file"
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
      "Meta Data imported should match the sample names from the above file"
    ),
    fileInput('file4', 'Meta data file',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tags$hr(),
    radioButtons("choosereplicate", 
                 "Choose replicate:",
                 c("Random", "Replicate")),
    tags$hr(),
    radioButtons("choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    tags$hr(),
    uiOutput("target_data"),
    
    
    h4("Prediction"),
    tooltip(
      bs_icon("info-circle"),
      "Recomended 2 to 3 max_depth"
    ),
    sliderInput("max_depth", "Max Depth:", value = 2, min = 1, max = 6, step = 1),
    tooltip(
      bs_icon("info-circle"),
      "Set nrounds as long as model is improving"
    ),
    sliderInput("nrounds", "Number of Rounds:", value = 10, min = 1, max = 100, step = 1),
    tooltip(
      bs_icon("info-circle"),
      "All depends on the size of your data"
    ),
    sliderInput("proportion", "Training Size:", value = 0.8, min = 0.5, max = 1, step = 0.05),
    actionButton("optimize_btn", "Optimize Hyperparameters", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Move the slider to change hyperparameters and click the Optimize Hyperparameters bottom"
    ),
    actionButton("xgb_btn", "Run Model", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Run button"
    ),
    tags$hr(),
    
    h4("Interaction"),
    tooltip(
      bs_icon("info-circle"),
      "Provide your list of features"
    ),
    fileInput('file5', 'List of features',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tooltip(
      bs_icon("info-circle"),
      "Provide your list of features"
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
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("distributionPlot"))
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("com_plot"))
      ),
      column(width = 12, tags$hr()), # Add gap between plots
      column(
        width = 6,
        DT::dataTableOutput("feature_importance")
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("shap_plot"))
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("boruta_plot"))
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("interaction_plot"))
      )
    ),
    uiOutput("download")
  )
)
         
