library(shiny)
library(caret)
library(xgboost)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(mixOmics)
library(tibble)
library(ggrepel)
library(irlba)
library(RColorBrewer)
library(Boruta)
library(viridis) #new
library(svglite) #new
library(gridExtra) #new
library(baseline) #new
library(signal) #new
library(reshape2) #new
library(dplyr) #new

shinyServer(function(input, output, session) { 
  # Server modules 
  source('server/xg_tool8.R', local = TRUE)
  source('server/mb_tool1.R', local = TRUE)
  source('server/ve_tool2.R', local = TRUE)
  source('server/harm_tool1.R', local = TRUE)
  
  # Action for the "Learn more" button (go_harmonise)
  observeEvent(input$go_harmonise, {
    # Switch to the "Information" tab
    updateTabsetPanel(session, inputId = "navbar", selected = "Data Harmonisation")
  })
  
  # Action for the "Explore More Features" button (go_info)
  observeEvent(input$go_info, {
    # Switch to the "Information" tab as well
    updateTabsetPanel(session, inputId = "navbar", selected = "Information")
  })
  
  # Other server-side logic here (e.g., data processing, UI updates)
})
