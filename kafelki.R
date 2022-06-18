library("shiny")
library("tidyverse")
library("sortable")


kafelki <- tabPanel("kafelki",
                    
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        selectInput("typ_pyt_zam",
                                    "Wybierz typ pytania otwartego:",
                                    c("Wypisz pietra systemu" = "pie_sys",
                                      "Wypisz systemy ery" = "sys_era",
                                      "Wypisz oddzialy systemu" = "odd_sys",
                                      "Wypisz pietra oddzialu" = "pie_odd"))
                        
                      ),
                      
                      mainPanel(
                        
                        actionButton("zamkniete_los", "Losuj"),
                        textOutput("polecenie_zamkniete"),
                        htmlOutput("klocki_tab",
                                   style = "{
                                   background: #098ebb;
                                             color: #e3edf2;
                                   }"),
                        textOutput("spr")
                        
                        
                      )
                    
                    )
                    
                  )




labels <- list(
  "one",
  "two",
  "three",
  "five" 
  )





ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      tags$h2("Default, multi-drag and swapping behaviour"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Default",
          tags$b("Exercise"),
          #rank_list_basic,
          tags$b("Result"),
          verbatimTextOutput("results_basic")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$results_basic <- renderPrint({
    input$rank_list_basic # This matches the input_id of the rank list
    
    rank_list_basic <- rank_list(
      text = "Drag the items in any desired order",
      labels = labels,
      input_id = "rank_list_basic"
    )
    
  })

}

shinyApp(ui, server)
