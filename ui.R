library("tidyverse")
library("shinyjs")
library("shiny")
library("shinyWidgets")
source("zadania_otwarte.R")
source("kafelki.R")

ui <- fluidPage(
  
  tags$head(
    
    tags$style(HTML("

               body {
               background-color: #9fc9dd;
               color: #333333;
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
  	font-family: 'Arial Black', sans-serif;
  	font-size: 15px;
    color: #000;
    text-transform: uppercase;
    background: #FFF;
    position: relative;
    display: block;
    padding: 10px 15px;
}

.navbar-default .navbar-brand {
    font-size: 40px;
    color: #e3edf2;
}

body {
  font-family: 'Source Sans Pro', sans-serif;
  margin-bottom : 150px;
}

")),
  ),
  
  navbarPage("Geo App",
             
             id = "all_tabs",
             
             tabPanel("Home",
                      
                      HTML("<h1><center>WELCOME TO <b>GEO APP</b></center></h1>"),
                      
                      setBackgroundImage(
                        src = "tatry3.jpg"
                      ),

                      tags$head(tags$script(src="image.js")),

                      br(),br(),br(),
                      
                      fluidRow(
                        
                        column(3, align = "center"),
                        
                        column(3, align = "center",
                               
                               actionButton("goto_zad_otw", "Zadania otwarte",
                                             style = "height: 150px;width: 300px;border-radius: 8px;
                                             text-align: left;
                                             box-shadow:0 1px 2px #5e5d5b;
                                             background: #098ebb;
                                             color: #e3edf2;
                                             font-family: Arial Black;
                                             font-size: 20px;
                                             border: none;
                                             padding-bottom:100px;
                                            animation: glowing 1300ms infinite;",
                                            icon = icon("book"))),
                        
                        column(3 , align = "center",
                               
                               actionButton("goto_kafelki", "Kalefki",
                                                style = "height: 150px;width: 300px;border-radius: 8px;
                                                text-align: left;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #fdc23a;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding-bottom:100px",
                                            icon = icon("boxes"))),
                        
                        column(3, align = "center")
                        
                        ),
                      
                      br(),br(),br(),
                      
                      fluidRow(
                        
                        column(3, align = "center"),
                        
                        column(3, align = "center",
                               
                               actionButton("goto_3", "Baton 1",
                                               style = "height: 150px;width: 300px;border-radius: 8px;
                                               text-align: left;
                                               box-shadow:0 1px 2px #5e5d5b;
                                               background: #e96449;
                                               color: #e3edf2;
                                               font-family: Arial Black;
                                               font-size: 20px;
                                               border: none;
                                               padding-bottom:100px")),
                        
                        column(3, align = "center",
                               
                               actionButton("goto_4", "Baton 2",
                                                style = "height: 150px;width: 300px;border-radius: 8px;
                                                text-align: left;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #818286;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding-bottom:100px")),
                        
                        column(3, align = "center")
                        
                        ),
                      
                      br(),br(),br(),
                      
                      wellPanel(
                        
                        HTML("<h1><b>some txt</b></h1>"),
                        HTML("<h4><b>geo app</b> some xtx
                               .</h4>"),
                        
                        style = "background: #a0acd6;
                        color: #333333;
                        font-family: Arial Black"
                        
                        
                      ),
                      
                      
                      br(),br(),br(),
                      
                      tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2022 Copyright:
                           <a href='https://github.com/wiktorpiela'> my GitHub</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"))
                      
                      ),
             
             zadania_otwarte,
             
             kafelki
             
             )
  )

