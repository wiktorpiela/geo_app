library("tidyverse")
library("shiny")
library("shinyWidgets")
source("zadania_otwarte.R")
source("kafelki.R")

ui <- fluidPage(
  
  
  tags$head(
    
    tags$style(HTML("

               body {
               background-color: #9fc9dd;
               color: white;
               }
               
               .navbar {
    background-color: #437d96;
    -webkit-transition: padding .3s;
    -moz-transition: padding .3s;
    transition: padding .3s;
    border: none;
               }
               
               .navbar .navbar-nav {
    float: right;
               }

.navbar-default .navbar-nav>li>a {
  	font-family: 'Open Sans', sans-serif;
  	font-size: 15px;
    color: #000;
    text-transform: uppercase;
    background: #FFF;
    position: relative;
    display: block;
    padding: 10px 15px;
}

.navbar-default .navbar-brand {
    font-size: 45px;
    color: #e3edf2;
}                         ")),
  ),
  
  navbarPage("Geo App", id = "all_tabs",
             
             tabPanel("Home",
                      
                      setBackgroundImage(
                        src = "tatry3.jpg"
                      ),
                      
                      tags$head(tags$script(src="image.js")),

                      br(),
                      br(),
                      br(),
                      
                      fluidRow(
                        
                        column(3),
                        
                        column(3, actionButton("goto_zad_otw", "Zadania otwarte",
                                             style = "height: 150px;width: 300px;border-radius: 8px;
                                             text-align: left;
                                             box-shadow:0 1px 2px #5e5d5b;
                                             background: #098ebb;
                                             color: #e3edf2;
                                             font-family: Arial Black;
                                             font-size: 20px;
                                             border: none;
                                             padding-bottom:100px")),
                        
                        column(3 , actionButton("goto_kafelki", "Kalefki",
                                                style = "height: 150px;width: 300px;border-radius: 8px;
                                                text-align: left;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #fdc23a;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding-bottom:100px")),
                        
                        column(3)
                        
                        ),
                      
                      br(),
                      br(),
                      br(),
                      
                      fluidRow(
                        
                        column(3),
                        
                        column(3, actionButton("goto_3", "Baton 1",
                                               style = "height: 150px;width: 300px;border-radius: 8px;
                                               text-align: left;
                                               box-shadow:0 1px 2px #5e5d5b;
                                               background: #e96449;
                                               color: #e3edf2;
                                               font-family: Arial Black;
                                               font-size: 20px;
                                               border: none;
                                               padding-bottom:100px")),
                        
                        column(3 , actionButton("goto_4", "Baton 2",
                                                style = "height: 150px;width: 300px;border-radius: 8px;
                                                text-align: left;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #818286;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding-bottom:100px")),
                        
                        column(3)
                        )
                      ),
             
             zadania_otwarte,
             
             kafelki
             
             )
  )