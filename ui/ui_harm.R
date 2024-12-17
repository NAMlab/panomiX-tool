sidebarLayout(
  sidebarPanel(
    fileInput('nor_file1', 'Omics file',
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    tooltip(
      bs_icon("info-circle"),
      "Data imported should contain feature in one file"
    ),
    selectInput("type", "Recommended data formate", choices = c("Table", "Abundance_Plot")),
    tags$hr(),
    radioButtons("nor_choosedataset", 
                 "Choose dataset:",
                 c("Example-data", "Upload-data")),
    actionButton("tra_btn", "Transcriptomic normalization", icon = icon("refresh")),
    actionButton("pro_btn", "Proteomic normalization", icon = icon("refresh")),
    actionButton("met_btn", "Metabolomic normalization", icon = icon("refresh")),
    actionButton("impute_btn", "Impute data", icon = icon("refresh")),
    tooltip(
      bs_icon("info-circle"),
      "click the tune bottom"
    )
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.type == 'Table'",
      column(width = 12,
             h3("Recommended data formate for the omiX tool"),
             DTOutput("table")
      )
    ),
    conditionalPanel(
      condition = "input.type == 'Abundance_Plot'",
      plotOutput("original_boxplot")
    ),
    uiOutput("norm_download")
  )
)

