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
library(viridis)
library(dplyr) #new

shinyServer(function(input, output, session) {

  # Server modules 
  source('server/xg_tool5.R', local = TRUE)
  source('server/mb_tool.R', local = TRUE)
  source('server/ve_tool1.R', local = TRUE)
  source('server/harm_tool.R', local = TRUE)
  
})
