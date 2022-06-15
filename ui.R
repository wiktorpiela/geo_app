library("tidyverse")
library("shiny")
source("pietro_system_tab.R")

ui <- fluidPage(
  
  navbarPage("Geo App",
             
             pietro_system_tab,
             
             tabPanel("tab2")

             )
  
)