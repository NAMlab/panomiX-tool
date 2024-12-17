sidebarLayout(
  sidebarPanel(
    h4("Dataset"),
    fileInput('mb_file1', 'Omics file 1',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    fileInput('mb_file2', 'Omics file 2',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    fileInput('mb_file3', 'Omics file 3',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tooltip(
      bs_icon("info-circle"),
      "Data imported should contain feature and target variable in one file"
    ),
    tags$hr(),
    radioButtons("mb_choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    br(), # Add a line break for spacing
    h4("Checking collinearity"),
    actionButton("check_btn", "Check", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Check for collinearity in three omics"
    ),
    br(), # Add a line break for spacing
    br(), # Add a line break for spacing
    h4("Tuning Model"),
    
    sliderInput("component", "Latent Component:", value = 5, min = 1, max = 10, step = 1),
    actionButton("tune_btn", "Tune Model", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Set latent Component as per your analysis"
    ),
    
    br(), # Add a line break for spacing
    sliderInput("mb_component", "Multi-block Latent Component:", value = 1, min = 1, max = 10, step = 1),
    
    actionButton("run_btn", "Run Model", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Move the slider to change latent variable and click the tune bottom"
    )
  ),
  mainPanel(
    fluidRow(
      column(width = 12, tags$hr()), # Add gap between plots
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("linearityPlot"))
      ), #plotOutput
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("linePlot"))
      )
    ),
    fluidRow(
      column(width = 12, tags$hr()), # Add gap between plots
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("cor_plot"))
      ),
      conditionalPanel(
        condition = "output.error_notification",
        uiOutput("error_notification")
      )
    ),
    fluidRow(
      column(width = 12, tags$hr()), # Add gap between plots
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("positive_plot"))
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(plotlyOutput("negative_plot"))
      )
    )
  )
)
         