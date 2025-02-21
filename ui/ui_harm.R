sidebarLayout(
  sidebarPanel(
    tooltip(
      bs_icon("info-circle"),
      "Data imported should be in CSV format, with samples in the columns and molecules or traits in the rows"
    ),
    fileInput('nor_file1', 'Omics file',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    selectInput("type", "Recommended data formate", choices = c("Table", "Abundance_Plot")),
    tags$hr(),
    radioButtons("nor_choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    actionButton("tra_btn", "Transcriptomic normalization", icon = icon("refresh")),
    actionButton("ftir_btn", "FTIR normalization", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "Click the bottom for normalization"
    )
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.type == 'Table'",
      column(width = 12,
             h3("Recommended data formate for the panomiX tool"),
             DTOutput("table")
      )
    ),
    conditionalPanel(
      condition = "input.type == 'Abundance_Plot'",
      plotOutput("original_boxplot")
    ),
    # Conditional panel for FTIR Raw Plot
    conditionalPanel(
      condition = "input.ftir_btn > 0",
      plotlyOutput("ftir_raw_plot")
    ),
    
    # Conditional panel for FTIR Smoothed Plot
    conditionalPanel(
      condition = "input.ftir_btn > 0",
      plotlyOutput("ftir_smooth_plot")
    ),
    uiOutput("norm_download")
  )
)
