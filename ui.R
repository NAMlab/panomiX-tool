library(shiny)
library(DT)
library(bslib)
library(bsicons)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(markdown)
library(mixOmics)
library(tidyr)
options(shiny.maxRequestSize = 30*1024^2)

shinyUI(navbarPage(
  theme = bs_theme(
    preset = "shiny",
    "primary" = "#0675DD"
  ),
  lang = "en",
  title = tags$span(
    tags$a(
      href = "https://github.com/NAMlab",
    tags$img(
      src = "logo.png",
      width = "240px",
      height = "auto",
      class = "me-3",
      style = "margin-left: -10px;"
    )),
    tags$a("", style = "font-size: 1px;
           font-weight: bold; text-decoration: none;"),
  ),
  nav_item(
    tags$a(
      tags$span(
        bsicons::bs_icon("code-slash"), "Source code"
      ),
      href = "https://github.com/NAMlab"
    )
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  
  # UI modules 
  tabPanel("Data Harmonisation", source('ui/ui_harm.R', local=T)[1]),
  tabPanel("Variance Estimation", source('ui/ui_ve.R', local=T)[1]),
  tabPanel("Multi-omics Prediction", source('ui/ui_xg.R', local=T)[1]),
  tabPanel("Omics Correlation", source('ui/ui_mb.R', local=T)[1]),
  
  tabPanel("Information", 
           includeMarkdown("info.Rmd"))
))
