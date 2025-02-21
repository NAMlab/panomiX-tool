sidebarLayout(
  sidebarPanel(
    h4("Dataset"),
    tooltip(
      bs_icon("info-circle"),
      "Data imported should be in CSV format. Upload all three omics as three separate files"
    ),
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
    tags$hr(),
    tooltip(
      bs_icon("info-circle"),
      "Metadata should be imported in CSV format and must contain at least two pieces of information:
      (1) an ID column that matches the omics data, and (2) if there are two experimental conditions,
      specify them as 'condition1' and 'condition2'"
    ),
    fileInput('meta_file', 'Metadata file',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tags$hr(),
    radioButtons("dh_choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    
    actionButton("run", "PCA", icon = icon("refresh")),
    actionButton("run_anova", "ANOVA", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Click the 'PCA' button to run the PCA analysis and the 'ANOVA' button to run the ANOVA analysis"
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
    uiOutput("pc_download")
  )
)
