library("shiny")

zadania_otwarte <- tabPanel("Zadania otwarte",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput("typ_pyt_otwartego",
                                            "Wybierz typ pytania otwartego:",
                                            c("Wypisz pietra systemu" = "pie_sys",
                                              "Wypisz systemy ery" = "sys_era",
                                              "Wypisz oddzialy systemu" = "odd_sys",
                                              "Wypisz pietra oddzialu" = "pie_odd"))
                                
                                ),
                              
                              mainPanel(
                                
                                textOutput("polecenie"),
                                actionButton("otwarte_los", "Losuj"),
                                br(),
                                textInput("otwarte_input",label=NULL),
                                textOutput("odpowiedz"),
                                actionButton("otwarte_sprawdz", "Sprawdz"),
                                actionButton("otwarte_wyczysc","Wyczysc"),
                                actionButton("otwarte_pokaz","Pokaz/ukryj odpowiedzi"),
                                textOutput("otwarte_wynik"),
                                uiOutput("otwarte_image"),
                                tableOutput("otwarte_odpowiedz"),
                                
                                
                                tableOutput("probatable")
                              
                                
                                
                                )
                              )
                            )