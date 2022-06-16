library("tidyverse")
library("shiny")
source("zadania_otwarte.R")
source("kafelki.R")

ui <- fluidPage(
  
  navbarPage("Geo App", id = "all_tabs", 
             
             tabPanel("Home",
                      
                      fluidRow(
                        
                        column(6, actionButton("goto_zad_otw", "Zadania otwarte")),
                        
                        column(6 , actionButton("goto_kafelki", "Kalefki"))
                        
                        )
                      ),
             
             zadania_otwarte,
             
             kafelki
             
             )
  )