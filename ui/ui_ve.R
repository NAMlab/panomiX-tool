sidebarLayout(
  sidebarPanel(
    h4("Dataset"),
    fileInput('dh_file1', 'Omics file 1',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    fileInput('dh_file2', 'Omics file 2',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    fileInput('dh_file3', 'Omics file 3',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tooltip(
      bs_icon("info-circle"),
      "Data imported should contain feature and target variable in one file"
    ),
    tags$hr(),
    radioButtons("dh_choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    tags$hr(),
    fileInput('meta_file', 'Meta data file',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    
    actionButton("run", "PCA", icon = icon("refresh")),
    actionButton("run_anova", "ANOVA", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Click the PCA button to run PCA analysis"
    )
  ),
  
  mainPanel(
    conditionalPanel(
      condition = "input.run > 0",
      fluidRow(
        column(width = 12, tags$hr()), # Add gap between plot
        column(
          width = 5,
          shinycssloaders::withSpinner(plotlyOutput("pca_plot"))
        )
      ),
      fluidRow(
        column(width = 12, tags$hr()), # Add gap between plot
        column(
          width = 9,
          shinycssloaders::withSpinner(plotlyOutput("pca1_plot"))
        )
      )
    ),
    conditionalPanel(
      condition = "input.run_anova > 0",
      column(width = 12,
             h3("Two way anova result"),
             DTOutput("anova_table")
      )
    ),
    uiOutput("dddownload")
  )
)
