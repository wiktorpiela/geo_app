library("tidyverse")
library("shinyjs")
library("shiny")
library("shinyWidgets")
library("shinyBS")
source("zadania_otwarte.R")
source("kafelki.R")
source("zadania_abcd_tab.R")
source("test_mod.R")
source("prekambr.R")

js_code <- "shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}"

ui <- fluidPage(
  
  tags$head(
    
    tags$style(HTML("

               body {
               background-color: #9fc9dd;
               color: #333333;
               }
               
               .navbar {
    background-color: #a2cee4;
    -webkit-transition: padding .3s;
    -moz-transition: padding .3s;
    transition: padding .3s;
    border: none;
    opacity: 0.9;
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
    color: #F94143;
    font-family: 'Arial Black', sans-serif;
}

body {
  font-family: 'Source Sans Pro', sans-serif;
  margin-bottom : 150px;
}

")),
  ),
  
  navbarPage("wucziAPPing",
             
             id = "all_tabs",
             
             tabPanel(icon("home"),
                      
                      #HTML("<h3><center>SPRAWDZ SWOJA WIEDZE <b>ZE STRATYGRAFII</b></center></h3>"),
                      
                      setBackgroundImage(
                        src = "foto1.jpg"
                      ),

                      tags$head(tags$script(src="image.js")),

                      br(),br(),br(),
                      
                      fluidRow(
                        
                        column(4, align = "right",
                               
                               actionButton("goto_zad_otw", "Pytania otwarte",
                                             style = "height: 150px;
                                             width: 300px;
                                             border-radius: 30px;
                                             text-align: center;
                                             vertical-align:middle;
                                             box-shadow:0 1px 2px #5e5d5b;
                                             background: #72c4a6 ;
                                             color: #e3edf2;
                                             font-family: Arial Black;
                                             font-size: 20px;
                                             border: none;
                                            padding: 50px 0",
                                            icon = icon("book"))),
                        
                        column(4, align = "center",
                               
                               actionButton("goto_kafelki", "Kafelki",
                                                style = "height: 150px;
                                                width: 300px;
                                                border-radius: 30px;
                                                text-align: center;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #fdc23a;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding: 60px 0;",
                                            icon = icon("cube"))),
                        
                        column(4, align = "left",
                               
                               actionButton("goto_prekambr","Prekambr",
                                            
                                            style = "height: 150px;
                                                width: 300px;
                                                border-radius: 30px;
                                                text-align: center;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #7ab2ff;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding: 60px 0;",
                                            icon=icon("mountain")))
                        
                        ),
                      
                      br(),br(),br(),
                      
                      fluidRow(
                        
                        column(4, align = "right",
                               
                               actionButton("goto_quiz1", "Pytania zamknięte",
                                               style = "height: 150px;
                                               width: 300px;
                                               border-radius: 30px;
                                               text-align: center;
                                               box-shadow:0 1px 2px #5e5d5b;
                                               background: #bc4b80;
                                               color: #e3edf2;
                                               font-family: Arial Black;
                                               font-size: 20px;
                                               border: none;
                                               white-space:normal;
                                            padding: 50px 0;",
                                            icon = icon("spell-check"))),
                        
                        column(4, align = "center",
                               
                               actionButton("goto_quiz2", "Test",
                                                style = "height: 150px;
                                                width: 300px;
                                                border-radius: 30px;
                                                text-align: center;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #818286;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding: 60px 0;",
                                            icon=icon("time",lib="glyphicon"))),
                        
                        column(4, align = "left",
                               
                               useShinyjs(),
                               extendShinyjs(text = js_code, functions = 'browseURL'),
                               
                               actionButton("goto_ics_web","ICS",
                                            
                                            style = "height: 150px;
                                                width: 300px;
                                                border-radius: 30px;
                                                text-align: center;
                                                box-shadow:0 1px 2px #5e5d5b;
                                                background: #ff7e7c;
                                                color: #e3edf2;
                                                font-family: Arial Black;
                                                font-size: 20px;
                                                border: none;
                                                padding: 60px 0;",
                                            icon = icon("globe",lib="glyphicon")))
                        
                        ),
                      
                      br(),br(),br(),
                      
                      # wellPanel(
                      #   
                      #   HTML("<h4><b>Darmowa aplikacja do nauki tabeli stratygraficznej dla studentow i pasjonatow geologii.<br><br> Nie zawiera polskich znakow. </b>"),
                      #   
                      #   style = "background: #cfd4b1;
                      #   color: #333333;
                      #   font-family: Arial Black"
                      #   
                      #   
                      # ),
                      
                      wellPanel(
                        
                        
                      tags$footer(
                        
                        wellPanel(
                          
                          HTML("<h4><b>Darmowa aplikacja do nauki tabeli stratygraficznej dla studentów i pasjonatów geologii.<br><br></b>"),
                          
                          HTML(paste(icon("photo"),"Pogranicze Pienińskiego Pasa Skałkowego i płaszczowiny magurskiej")),
                          
                          style = "background: #cfd4b1;
                          opacity: 0.8;
                          border: none;
                          color: #333333;
                          font-family: Arial Black"
                          
                          ),
                          
                          HTML("
                          <!-- Footer -->
                          <footer class='page-footer font-large indigo'>
                          <!-- Copyright -->
                          <div class='footer-copyright text-center py-3'>© Wiktor Piela & Weronika Pratkowiecka 2022:
                          <a href='https://github.com/wiktorpiela'> visit my GitHub</a>
                          </div>
                          <!-- Copyright -->
                          
                          </footer>
                               <!-- Footer -->")),
                             
                             style = "background: #cfd4b1;
                                      opacity: 0.8;
                                      color: #333333;
                                      border:none;
                                      height:200px")
                      
                      ),
             
             zadania_otwarte,
             
             kafelki,
             
             zadania_abcd_jedn,
             
             test,
             
             prekambr
             
             )
  )

