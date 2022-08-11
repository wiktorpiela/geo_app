library("shiny")
library("sortable")


kafelki <- tabPanel("kafelki",
                    
                    wellPanel(
                      h6("Poziom",style="text-align: center"),
                      br(),
                      tags$div(
                      materialSwitch("switch_button",
                                     label = icon("arrow-down"),
                                     inline = TRUE,
                                     status = "primary"),
                      tags$span(icon("arrow-up"))),
                      
                      style="background-color: white;
                      width: 130px;
                      font-size:11px;
                      border-radius:10px;
                      box-shadow:0 1px 2px #5e5d5b;
                      color: #33518e;
                      border:none;
                      font-family: Arial Black;"
                      ),
                    
                    tabsetPanel(id="kafelki_inside_tabset",
                                
                                type = "hidden",
                      
                      tabPanel("A",
                               
                               sidebarLayout(
                                 
                                 sidebarPanel(
                                   
                                   selectInput("typ_pyt_zam",
                                               "Wybierz typ pytania:",
                                               
                                               c("Uszereguj piętra systemu" = "pie_sys",
                                                 "Uszereguj systemy eratemu" = "sys_era",
                                                 "Uszereguj oddziały systemu" = "odd_sys",
                                                 "Uszereguj piętra oddziału" = "pie_odd")),
                                   
                                   actionButton("zamkniete_los", "Losuj pytanie",
                                                
                                   style="background-color: #33518e;
                                   width: 250px;
                                   font-size:15px;
                                   border-radius:10px;
                                   box-shadow:0 1px 2px #5e5d5b;
                                   color: #FFFFFF;
                                   border:none;
                                   font-family: Arial Black"),
                                   
                                   style = "background-color: white;
                                   font-size:15px;
                                   border-radius:0px;
                                   box-shadow:0 1px 2px #5e5d5b"
                                   
                                 ),
                                 
                                 mainPanel(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                       
                                       column(3, 
                                              
                                              actionButton("zam_check", "Sprawdź",
                                              
                                              style="background-color: #33518e;
                                              font-size:15px;
                                              border-radius:10px;
                                              box-shadow:0 1px 2px #5e5d5b;
                                              color: #FFFFFF;
                                              border:none;
                                              font-family: Arial Black")
                                              
                                              ),
                                       
                                       column(7,
                                              
                                              actionButton("poprawna_odp_kafelki", "Pokaż odpowiedź",
                                              
                                              style="background-color: #33518e;
                                              width: 250px;
                                              font-size:15px;
                                              border-radius:10px;
                                              box-shadow:0 1px 2px #5e5d5b;
                                              color: #FFFFFF;
                                              border:none;
                                              font-family: Arial Black"),
                                              
                                              bsModal("window3",
                                                      title="Poprawna kolejność",
                                                      trigger="poprawna_odp_kafelki",
                                                      textOutput("spr"),
                                                      tags$head(tags$style("#window3 .modal-footer{display:none}"))
                                                      
                                                      )
                                              ),
                                       
                                       column(2,uiOutput("zamkniete_image"))
                                       
                                       ), br(),
                                     
                                     htmlOutput("klocki_tab",
                                                style = "background: white;
                                                color: black;
                                                width:550px;
                                                font-size:15px;
                                                border-radius:0px;
                                                border:none;
                                                font-family: Arial Black"),
                                     
                                     style = "background-color: white;
                            width:600px;
                            font-size:15px;
                            border-radius:0px;
                            box-shadow:0 1px 2px #5e5d5b"
                                     
                                   )
                                 )
                               )  
                               
                               ),
                      
                      
                      tabPanel("B",
                               
                               sidebarLayout(
                                 
                                 sidebarPanel(
                                   
                                   fluidRow(
                                     
                                     column(2,actionButton("los_system", "Losuj system",
                                     
                                     style="background-color: #33518e;
                                     font-size:15px;
                                     border-radius:10px;
                                     box-shadow:0 1px 2px #5e5d5b;
                                     color: #FFFFFF;
                                     border:none;
                                     font-family: Arial Black")
                                     
                                     ),
                                     
                                     column(2, actionButton("check_bucket", "Potwierdź",
                                     
                                     style="background-color: #33518e;
                                     font-size:15px;
                                     border-radius:10px;
                                     box-shadow:0 1px 2px #5e5d5b;
                                     color: #FFFFFF;
                                     border:none;
                                     font-family: Arial Black")
                                     
                                     ),
                                     
                                     column(3, actionButton("zobacz_odp_kafelki", "Pokaż odpowiedź",
                                                            
                                                            style="background-color: #33518e;
                                                            font-size:15px;
                                                            border-radius:10px;
                                                            box-shadow:0 1px 2px #5e5d5b;
                                                            color: #FFFFFF;
                                                            border:none;
                                                            font-family: Arial Black"),
                                            
                                            bsModal("window4",
                                                    title = "Poprawna kolejność",
                                                    trigger = "zobacz_odp_kafelki",
                                                    tabsetPanel(
                                                      tabPanel("Oddziały",tableOutput("odp_kafelki_window")),
                                                      tabPanel("Piętra",tableOutput("odp_kafelki_window2"))
                                                      ),
                                                    tags$head(tags$style("#window4 .modal-footer{display:none}"))
                                                    )
                                            
                                            ),
                                     
                                     column(2, uiOutput("bucket_icon")),
                                     
                                     
                                     column(1),
                                     
                                   ),
                                   
                                   htmlOutput("bucket_list",
                                              style = "background: white;
                                                color: black;
                                                width:1500px;
                                                font-size:15px;
                                                border-radius:0px;
                                                border:none;
                                              font-family: Arial Black"),
                                   
                                   style = "background-color: white;
                                   width:1550px;
                                   font-size:15px;
                                   border-radius:0px;
                                   box-shadow:0 1px 2px #5e5d5b"
                                   
                                   ),
                                 
                                 mainPanel()
                                 
                                 ),
                      
                               # textOutput("czek3"),
                               # textOutput("czek4"),
                               # textOutput("czek5"),
                               # textOutput("czek6"),
                               # verbatimTextOutput("res2")
                               
                               )
                      )
                    )
