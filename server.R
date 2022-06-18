library("tidyverse")
library("shiny")
source("func.R")
source("zadania_otwarte.R")

data_otwarte <- read_rds("data/prepared_data.rds")

server <- function(input, output, session){
  
# home --------------------------------------------------------------------
  
  observeEvent(input$goto_zad_otw, {
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected="Zadania otwarte")
    
  })
  
  observeEvent(input$goto_kafelki, {
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected="kafelki")
    
  })

# zadania otwarte -------------------------------------------------------
  
  
  observeEvent(input$typ_pyt_otwartego, {
    
    updateTextInput(session,"otwarte_input", value="")
    
    output$otwarte_odpowiedz <- renderTable({})
    
    output$otwarte_image <- renderUI({})
    
    output$otwarte_wynik <- renderText({})
    
    new_random_key <- sample(otwarte_zadania_klucz(),1)
    
    random_key(new_random_key)
  
  })

  # dane z zaleznosci od selecta
  
  otwarte_zadania_dane <- reactive({
    
    
    switch(input$typ_pyt_otwartego,
           "pie_sys" = prepare_pie_sys_data(data_otwarte),
           "sys_era" = prepare_sys_era_data(data_otwarte),
           "odd_sys" = prepare_odd_sys_data(data_otwarte),
           "pie_odd" = prepare_pie_odd_data(data_otwarte))
  })
  
  otwarte_zadania_klucz <- reactive({
    
    otwarte_zadania_dane() %>% 
      pull(1)
    
    })
  
  # pulpit zadania otwarte
  
  observeEvent(input$typ_pyt_otwartego, {

    if(input$typ_pyt_otwartego=="pie_sys"){

      output$polecenie <- renderText(paste("Wypisz pietra systemu:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

    } else if(input$typ_pyt_otwartego=="sys_era") {

      output$polecenie <- renderText(paste("Wypisz systemy ery:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

      } else if(input$typ_pyt_otwartego=="odd_sys"){

      output$polecenie <- renderText(paste("Wypisz oddzialy systemu:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

    } else {

      output$polecenie <- renderText(paste("Wypisz pietra oddzialu:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

    }

  })
  
  random_key <- reactiveVal(NULL)
  
  pula <<- character() 

  observeEvent(input$otwarte_los, {
    
    new_random_key <- sample(otwarte_zadania_klucz()[!otwarte_zadania_klucz() %in% pula & !otwarte_zadania_klucz() %in% random_key()],1)
    random_key(new_random_key)
    
    pula <<- append(pula, new_random_key)
    
    if(length(pula)==3 & input$typ_pyt_otwartego=="sys_era"){
      
      pula <<- character()
      
    } else if(length(pula)==12 & input$typ_pyt_otwartego=="pie_sys" | input$typ_pyt_otwartego=="odd_sys"){
      
      pula <<- character()
      
    } else if(length(pula)==34 & input$typ_pyt_otwartego=="pie_odd"){
      
      pula <<- character()
      
    }
    
    updateTextInput(session,"otwarte_input",value="")
    
    output$otwarte_wynik <- renderText({})
    
    output$otwarte_image <- renderUI({})
    
    output$otwarte_odpowiedz <- renderTable({})
       
     })
  
  
  output$odpowiedz <- renderText(correct_value())
  
  correct_value <- reactive({
    
    req(random_key())

    otwarte_zadania_dane() %>%
       filter(str_detect(klucz, random_key())==TRUE) %>%
       select(wartosc) %>%
       pull()

   })
  
    observeEvent(input$otwarte_sprawdz,{

     req(input$otwarte_input)

     if(ps_clear_input(input$otwarte_input) == ps_clear_input(correct_value())){

       output$otwarte_wynik <- renderText("OK!")
       output$otwarte_image <- renderUI({

         tags$img(src = "ok_icon.png",
                  height = 80,
                  width = 80)

       })
       message("OK")

     } else {

       output$otwarte_wynik <- renderText("Fail!")
       output$otwarte_image <- renderUI({

         tags$img(src = "fail_icon.jpg",
                  height = 80,
                  width = 80)

       })
       message("fail")
     }

   })
    
    
    observeEvent(input$otwarte_wyczysc, {
      
      updateTextInput(session,"otwarte_input", value="")

     })
    
    
     observeEvent(input$otwarte_pokaz, {

       if(input$otwarte_pokaz%%2==0){

         output$otwarte_odpowiedz <- renderTable({})

       } else {

         output$otwarte_odpowiedz <- renderTable(answer_object())

       }

    })
     
      answer_object <- reactive({
        
        req(correct_value())

        tibble(answers = unlist(strsplit(correct_value(),";"))) %>%
          mutate(n = row_number()) %>% 
          arrange(desc(n))

        })
      
      
      
      # output$probatable <- renderTable(otwarte_zadania_dane())
      
   
# zadania zamkniete -------------------------------------------------------
      
  zamkniete_zadania_dane <- reactive({
    
    switch(input$typ_pyt_zam,
           "pie_sys" = prepare_pie_sys_data(data_otwarte),
           "sys_era" = prepare_sys_era_data(data_otwarte),
           "odd_sys" = prepare_odd_sys_data(data_otwarte),
           "pie_odd" = prepare_pie_odd_data(data_otwarte))
    
    })
      
  zamkniete_klucz <- reactive({
    
    pull(zamkniete_zadania_dane(),1)
    
  })
  
  zamk_los_klucz <- reactiveVal(NULL)
  
  pula_zamkniete <<- character()
  
  observeEvent(input$zamkniete_los, {
    
    nowy_los <- sample(zamkniete_klucz()[!zamkniete_klucz() %in% pula_zamkniete & zamkniete_klucz()!=zamk_los_klucz()],1)
    zamk_los_klucz(nowy_los)
    
    pula_zamkniete <<- append(pula_zamkniete, nowy_los) 
    
    if(length(pula_zamkniete)==3 & input$typ_pyt_zam=="sys_era"){
      
      pula_zamkniete <<- character()
      
    } else if(length(pula_zamkniete)==12 & input$typ_pyt_zam=="pie_sys" | input$typ_pyt_zam=="odd_sys"){
      
      pula_zamkniete <<- character()
      
    } else if(length(pula_zamkniete)==34 & input$typ_pyt_zam=="pie_odd"){
      
      pula_zamkniete <<- character()
      
    }
    
  })
  
  observeEvent(input$typ_pyt_zam, {
    
    if(input$typ_pyt_zam=="pie_sys"){
      
      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)
      
      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz()) 
      
      output$polecenie_zamkniete <- renderText(paste("Wypisz pietra systemu:",zamk_los_klucz()))
      
    } else if(input$typ_pyt_zam=="sys_era"){
      
      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)
      
      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz()) 
      
      output$polecenie_zamkniete <- renderText(paste("Wypisz systemy ery:",zamk_los_klucz()))
      
    } else if(input$typ_pyt_zam=="odd_sys"){
      
      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)
      
      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz()) 
      
      output$polecenie_zamkniete <- renderText(paste("Wypisz oddzialy systemu:",zamk_los_klucz()))
      
    } else {
      
      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)
      
      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz()) 
      
      output$polecenie_zamkniete <- renderText(paste("Wypisz pietra oddzialu:",zamk_los_klucz()))
      
    }
    
  })
  
  klocki <- reactive({
    
    sample(zamkniete_zadania_dane() %>% 
      filter(klucz==zamk_los_klucz()) %>% 
      pull(2) %>% 
      str_split(";") %>% 
      unlist())
    
    })
  
  output$klocki_tab <- renderUI({
    
    rank_list(
      
        text = "Drag the items in any desired order",
        labels = klocki(),
        input_id = "rank_list_basic"
      )
    })
  

  output$spr <- renderText(klocki())
  

  

}