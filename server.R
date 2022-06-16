library("tidyverse")
library("shiny")
source("func.R")
source("pietro_system_tab.R")

server <- function(input, output, session){
  
  if(1==1) {
    
    hideTab(inputId = "all_tabs", 
            target = list("Pietro system"))
    
    }
  
# pietro system tab -------------------------------------------------------
  
  los_sys <- reactiveVal(sys[1])
  
  observeEvent(input$ps_los, {
    
    new_los_sys <- sample(sys,1)
    los_sys(new_los_sys)
    
    updateTextInput(session,"ps_txt_in1",value="")
    
    output$ps_result <- renderText({})
    
    output$ps_image <- renderUI({})
    
    output$ps_asnwer_tab <- renderTable({})
    
  })
  
  output$ps_txt_out1 <- renderText(paste("Wymien chronologicznie pietra", los_sys()))
  
  correct_value <- reactive({

    pietro_system_data %>%
      filter(str_detect(quest, los_sys())==TRUE) %>% 
      select(ans) %>% 
      pull() 

  })
  
  answer_object <- reactive({
    
    tibble(answers = unlist(strsplit(correct_value(),";"))) %>% 
      mutate(n = row_number())
    
    })
  
  output$tab <- renderText(correct_value())
  
 
  observeEvent(input$ps_show, {
    
    if(input$ps_show%%2==0){
      
      output$ps_asnwer_tab <- renderTable({})
      
    } else {
      
      output$ps_asnwer_tab <- renderTable(answer_object())
      
    }
    
 })
  
  observeEvent(input$ps_clear, {
    
    updateTextInput(session,"ps_txt_in1", value="")
    
  })
  
   observeEvent(input$ps_check,{
    
    req(input$ps_txt_in1)
    
    if(ps_clear_input(input$ps_txt_in1) == ps_clear_input(correct_value())){
      
      output$ps_result <- renderText("OK!")
      output$ps_image <- renderUI({
        
        tags$img(src = "ok_icon.png",
                 height = 80,
                 width = 80)
        
      })
      message("OK")
      
    } else {
      
      output$ps_result <- renderText("Fail!")
      output$ps_image <- renderUI({
        
        tags$img(src = "fail_icon.jpg",
                 height = 80,
                 width = 80)
          
      })
      message("fail")
    }
    
  })
  

  
  
  
  
  
   
   
   
   
   
   
}


