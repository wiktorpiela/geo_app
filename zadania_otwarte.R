library("shiny")

zadania_otwarte <- tabPanel("Pytania otwarte",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput("typ_pyt_otwartego",
                                            
                                            "Wybierz typ pytania otwartego:",
                                            
                                            c("Wypisz piętra systemu" = "pie_sys",
                                              "Wypisz systemy eratemu" = "sys_era",
                                              "Wypisz oddziały systemu" = "odd_sys",
                                              "Wypisz piętra oddziału" = "pie_odd")),
                                
                                actionButton("otwarte_los", "Losuj pytanie",
                                
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
                                    
                                    column(11,
                                           
                                           textOutput("polecenie"),
                                           
                                           br(),
                                           
                                           textInput("otwarte_input",label=NULL),
                                           
                                           #textOutput("odpowiedz"),
                                           
                                           fluidRow(
                                             
                                             column(2, actionButton("otwarte_sprawdz", "Potwierdź",
                                                                    style="background-color: #33518e;
                                                           font-size:15px;
                                                           border-radius:10px;
                                                           box-shadow:0 1px 2px #5e5d5b;
                                                           color: #FFFFFF;
                                                           border:none;
                                                           font-family: Arial Black")),
                                             
                                             column(2, actionButton("otwarte_wyczysc","Wyczyść",
                                                                    style="background-color: #33518e;
                                                           font-size:15px;
                                                           border-radius:10px;
                                                           box-shadow:0 1px 2px #5e5d5b;
                                                           color: #FFFFFF;
                                                           border:none;
                                                           font-family: Arial Black")),
                                             
                                             column(4, actionButton("otwarte_pokaz","Pokaż odpowiedź",
                                                                    style="background-color: #33518e;
                                                                    width: 200px;
                                                           font-size:15px;
                                                           border-radius:10px;
                                                           box-shadow:0 1px 2px #5e5d5b;
                                                           color: #FFFFFF;
                                                           border:none;
                                                           font-family: Arial Black")),
                                             
                                             column(4,actionButton("next_quest_otwarte","Następne pytanie",
                                                                   style="background-color: #33518e;
                                                                   width: 200px;
                                                           font-size:15px;
                                                           border-radius:10px;
                                                           box-shadow:0 1px 2px #5e5d5b;
                                                           color: #FFFFFF;
                                                           border:none;
                                                           font-family: Arial Black"))
                                             )
                                           ),
                                    
                                    column(1,
                                           
                                           uiOutput("otwarte_image")
                                           
                                           )
                                    ),
                                    
                                    style = "background-color: white;
                                    width: 750px;
                                    font-size:15px;
                                    border-radius:0px;
                                  box-shadow:0 1px 2px #5e5d5b"
                                  
                                  )
                                )
                              ),
                            
                            bsModal("window",
                                    title = "Poprawna kolejność (od najstarszych do najmłodszych)",
                                    trigger = "otwarte_pokaz",
                                    textOutput("otwarte_odpowiedz"),
                                    tags$head(tags$style("#window .modal-footer{ display:none}"))
                                    
                                    )
                            
                            )
                            