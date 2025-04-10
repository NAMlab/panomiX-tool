sidebarLayout(
  sidebarPanel(
    h4(strong("Welcome to panomiX")),
    p("panomiX provides various analysis tools such as:"),
    tags$ul(
      tags$li("Data Harmonisation"),
      tags$li("Variance Estimation"),
      tags$li("Multi-omics Prediction"),
      tags$li("Omics Correlation")
    ),
    p("To get started, explore the available tools or upload your own dataset."),
    actionButton(
      inputId = "go_harmonise",
      label = tagList(bs_icon("play-circle"), "Get started"),
      class = "btn btn-primary btn-lg"
    ),
    width = 3
  ),
  
  mainPanel(
    fluidRow(
      column(
        width = 12,
        # 🔹 Welcome message
        h4(strong("panomiX: Investigating Mechanisms Of Trait Emergence Through Multi-Omics Data Integration")),
        p("The ",
          strong("panomiX toolbox"),
          " is a web-based platform developed using R Shiny for interactive and reproducible multi-omics data analysis."
        ),
        p("It offers a comprehensive set of tools for seamless integration and analysis across various omics layers, including:"),
        tags$ul(
          tags$li("Genomics"),
          tags$li("Transcriptomics"),
          tags$li("Proteomics"),
          tags$li("Metabolomics"),
          tags$li("FTIR"),
          tags$li("Phenomics")
        ),
        
        # 🔹 A short additional paragraph
        p("panomiX is designed to simplify the complexity of systems biology data integration, interpretation and interaction with a user-friendly interface."),
        
        # 🔹 Action Button
        div(
          style = "text-align: left; margin-top: 20px; padding-left: 10px;",
          actionButton(
            inputId = "go_info",
            label = tagList(bs_icon("play-circle"), "Learn more"),
            class = "btn btn-primary btn-lg"
          )
        )
      )
    ),
    width = 9
  )
)
