library(shiny)
library(tidyverse)


quest <- read_csv("data/testUI_data.csv") %>% drop_na()

logic_mask <- tibble(
  
  col1 = c(TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE),
  col2 = c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
  col3 = c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
  col4 = c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE)
)

ui <- fluidPage(
  
  actionButton("losuj","Losuj"),
  
  uiOutput("batonik"),
  
  textOutput("los"),
  
  actionButton("check", "Sprawdz"),
  
  verbatimTextOutput("log_vec"),
  
  textOutput("selected"),
  
  textOutput("true_indx"),
  
  textOutput("result")
  
)

server <- function(input,output,session){
  
  true_indx <- reactiveVal(NULL)
  
  random <- reactiveVal(NULL)
  
  observeEvent(input$losuj,{
    
    new_num <- sample(c(1:nrow(quest)),1)
    
    random(new_num)
    
    new_indx <- which(as.logical(logic_mask[random(),]),TRUE)
    
    true_indx(new_indx)
    
  })
  
  output$los <- renderText(random())
  
  
  output$batonik <- renderUI({
    
    req(random())
    
    radioButtons("radio",
                 label = paste(quest[random(),1],random()),
                 # choices = as.character(quest[random(),2:5]),
                 choiceNames = as.character(quest[random(),2:5]),
                 choiceValues = c(1,2,3,4),
                 selected = "")
    
  })
  
  output$log_vec <- renderPrint(as.logical(logic_mask[random(),]))
  
  output$selected <- renderText(input$radio)
  
  output$true_indx <- renderPrint(true_indx())
  
  observeEvent(input$check, {
    
    if(true_indx() == input$radio){
      
      output$result <- renderText("poprawna odpowiedk")
      
    } else {
      
      output$result <- renderText("zla odpowiedz")
      
    }
    
    
  })
  
}

shinyApp(ui,server)




as.character(which(as.logical(logic_mask[1,]),TRUE))


