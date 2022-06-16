library("tidyverse")
library("shiny")
library("shinyjs")
source("pietro_system_tab.R")
source("kafelki.R")

ui <- fluidPage(
  
  theme = "https://stackpath.bootstrapcdn.com/bootswatch/3.4.1/morph/bootstrap.min.css",
  
  navbarPage("Geo App", id = "all_tabs", 
             
             tabPanel("Home"),
             
             pietro_system_tab,
             
             kafelki
             
             )
  
  )