library("shiny")

zadania_abcd_jedn <- tabPanel("Pytania zamknięte",
                              
                              shinyjs::useShinyjs(),
                              
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  selectInput("typ_zad_zam_jedn",
                                              
                                              "Wybierz typ pytania",
                                              
                                              c("Wybierz piętro systemu"="pie_sys",
                                                "Wybierz system eratemu"="sys_era",
                                                "Wybierz oddział systemu"="oddz_sys",
                                                "Wybierz piętro oddziału"="pie_oddz")),
                                  
                                  actionButton("losuj_quiz1_id", "Losuj pytanie",
                                  
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
                                      
                                      column(11, uiOutput("quiz1_ui")),
                                      
                                      column(1, uiOutput("icon_result"))
                                      
                                      ),
                                    
                                    fluidRow(
                                      
                                      column(4,
                                             
                                             actionButton("check_quiz1_id", "Potwierdź",
                                                          
                                                          style="background-color: #33518e;
                                                          width: 175px;
                                                          font-size:15px;
                                                          border-radius:10px;
                                                          box-shadow:0 1px 2px #5e5d5b;
                                                          color: #FFFFFF;
                                                          border:none;
                                                          font-family: Arial Black")
                                             
                                             ),
                                      
                                      column(4,actionButton("check_answer", "Pokaż odpowiedź",
                                                            
                                                            style="background-color: #33518e;
                                                            font-size:15px;
                                                            border-radius:10px;
                                                            box-shadow:0 1px 2px #5e5d5b;
                                                            color: #FFFFFF;
                                                            border:none;
                                                            font-family: Arial Black"),
                                             
                                             bsModal("window2",
                                                     trigger = "check_answer",
                                                     title = "Poprawna odpowiedź",
                                                     textOutput("zamk_jedn_poprawna"),
                                                     tags$head(tags$style("#window2 .modal-footer{ display:none}"))
                                                     )
                                             
                                             ),
                                      
                                      column(4, actionButton("next_question_test1", "Następne pytanie",
                                                             
                                                             style="background-color: #33518e;
                                                            font-size:15px;
                                                            border-radius:10px;
                                                            box-shadow:0 1px 2px #5e5d5b;
                                                            color: #FFFFFF;
                                                            border:none;
                                                            font-family: Arial Black")
                                             )
                                      
                                      ),
                                      
                                      style = "background-color: white;
                                      width: 650px;
                                      font-size:15px;
                                      border-radius:0px;
                                      box-shadow:0 1px 2px #5e5d5b
                                    "
                                    
                                    )
                                  )
                                ),
                              textOutput("txt1"),
                              textOutput("txt2")
                              )