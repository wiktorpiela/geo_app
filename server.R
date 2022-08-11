library("tidyverse")
library("shiny")
source("func.R")
source("zadania_otwarte.R")
source("kafelki.R")

data_otwarte <- read_rds("data/prepared_data.rds")

server <- function(input, output, session){
  
# home navi --------------------------------------------------------------------
  
  observeEvent(input$goto_zad_otw, {
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected="Pytania otwarte")
    
  })
  
  observeEvent(input$goto_kafelki, {
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected="kafelki")
    
  })
  
  observeEvent(input$goto_quiz1, {
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected="Pytania zamknięte")
    
  })
  
  observeEvent(input$goto_quiz2, {
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected="Test")
    
  })
  
  observeEvent(input$goto_prekambr,{
    
    updateNavbarPage(session,
                     "all_tabs",
                     selected = "Prekambr")
    
  })
  
  # navi kafelki tab
  
  observeEvent(input$switch_button,{
    
    if(input$switch_button==FALSE){
      
      updateTabsetPanel(session,
                        "kafelki_inside_tabset",
                        selected = "A")
      
    } else {
        
      updateTabsetPanel(session,
                        "kafelki_inside_tabset",
                        selected = "B")
      
      }

  })
  
  observeEvent(input$goto_ics_web,{
    
    js$browseURL("https://stratigraphy.org/chart")
    
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

      output$polecenie <- renderText(paste("Wypisz piętra systemu:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

    } else if(input$typ_pyt_otwartego=="sys_era") {

      output$polecenie <- renderText(paste("Wypisz systemy eratemu:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

      } else if(input$typ_pyt_otwartego=="odd_sys"){

      output$polecenie <- renderText(paste("Wypisz oddziały systemu:",random_key()))
      
      pula <<- character() 
      
      pula <<- append(pula, random_key())

    } else {

      output$polecenie <- renderText(paste("Wypisz piętra oddziału:",random_key()))
      
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
  
  observeEvent(input$next_quest_otwarte, {
    
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
                  height = 40,
                  width = 40)

       })
       message("OK")

     } else {

       output$otwarte_wynik <- renderText("Fail!")
       output$otwarte_image <- renderUI({

         tags$img(src = "fail_icon.jpg",
                  height = 40,
                  width = 40)

       })
       message("fail")
     }

   })
    
    
    observeEvent(input$otwarte_wyczysc, {
      
      updateTextInput(session,"otwarte_input", value="")

     })
    
    
    #  observeEvent(input$otwarte_pokaz, {
    # 
    #    if(input$otwarte_pokaz%%2==0){
    # 
    #      output$otwarte_odpowiedz <- renderTable({})
    # 
    #    } else {
    # 
    #      output$otwarte_odpowiedz <- renderTable(answer_object())
    # 
    #    }
    # 
    # })
    
    observeEvent(input$otwarte_pokaz,{
      
      output$otwarte_odpowiedz <- renderText(answer_object())
      
      })
     
      answer_object <- reactive({
        
        req(correct_value())

        tibble(answers = unlist(strsplit(correct_value(),";"))) %>%
          mutate(n = row_number()) %>% 
          arrange(desc(n)) %>% pull(answers) %>% rev()

        })
      
# zadania zamkniete cz 1 -------------------------------------------------------

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
    
    output$zamkniete_image <- renderUI({})

    nowy_los <- sample(zamkniete_klucz()[!zamkniete_klucz() %in% pula_zamkniete & zamkniete_klucz()!=zamk_los_klucz()],1)
    zamk_los_klucz(nowy_los)

    pula_zamkniete <<- append(pula_zamkniete, nowy_los)

    if(length(pula_zamkniete)==3 & input$typ_pyt_zam=="sys_era"){

      pula_zamkniete <<- character()

    } else if(length(pula_zamkniete)==12 & input$typ_pyt_zam=="pie_sys" | input$typ_pyt_zam=="odd_sys"){

      pula_zamkniete <<- character()

    } else if(length(pula_zamkniete)==32 & input$typ_pyt_zam=="pie_odd"){

      pula_zamkniete <<- character()

    }

  })

  observeEvent(input$typ_pyt_zam, {
    
    output$zamkniete_image <- renderUI({})
    
    if(input$typ_pyt_zam=="pie_sys"){

      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)

      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz())

    } else if(input$typ_pyt_zam=="sys_era"){

      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)

      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz())

    } else if(input$typ_pyt_zam=="odd_sys"){

      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)

      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz())

    } else {

      nowy_los <- sample(zamkniete_klucz(),1)
      zamk_los_klucz(nowy_los)

      pula_zamkniete <<- character()
      pula_zamkniete <<- append(pula_zamkniete,zamk_los_klucz())

    }

  })
  
  # sprawdzenie czy klocki są poprawnie ulozone

  klocki <- reactive({
    
    sample(klocki_odp())

    })
  
  klocki_odp <- reactive({
    
    zamkniete_zadania_dane() %>%
      filter(klucz==zamk_los_klucz()) %>%
      pull(2) %>%
      str_split(";") %>%
      unlist() %>% 
      rev()
    
    })
  
  output$klocki_tab<- renderUI({
    
    if(input$typ_pyt_zam=="pie_sys"){
      
      rank_list(
        text = paste("Uszereguj piętra systemu:",zamk_los_klucz()),
        labels = klocki(),
        input_id = "rank_list_basic"
      )
      
    } else if(input$typ_pyt_zam=="sys_era"){
      
      rank_list(
        text = paste("Uszereguj systemy eratemu:",zamk_los_klucz()),
        labels = klocki(),
        input_id = "rank_list_basic"
        
      )
      
    } else if(input$typ_pyt_zam=="odd_sys"){
      
      rank_list(
        text = paste("Uszereguj oddziały systemu:",zamk_los_klucz()),
        labels = klocki(),
        input_id = "rank_list_basic"
      )
      
    } else {
      
      rank_list(
        text = paste("Uszereguj piętra oddziału:",zamk_los_klucz()),
        labels = klocki(),
        input_id = "rank_list_basic"
      )
      
    }

    })
  
  output$spr <- renderText(klocki_odp())
  
  output$res <- renderPrint(input$rank_list_basic)
  
  observeEvent(input$zam_check, {
    
    n <- 0
    
    for(x in seq_along(input$rank_list_basic)){
      
      if(input$rank_list_basic[x]==klocki_odp()[x]){
        
        n <- n + 1 
        
      } else next

    }
    
    if(length(input$rank_list_basic)==n){
      
      output$zamkniete_image <- renderUI({
        
        tags$img(src = "ok_icon.png",
                 height = 35,
                 width = 35)
      })
      } else {
            
          output$zamkniete_image <- renderUI({

            tags$img(src = "fail_icon.jpg",
                     height = 35,
                     width = 35)
          })
          }
    })
    
# zadania zamkniete cz 2 --------------------------------------------------
  
  observeEvent(input$kafelki_inside_tabset,{
    
    output$bucket_icon <- renderUI({})
    
    new_random_system <- sample(pull(prepare_brick_quest_data(data_otwarte)[1])[!pull(prepare_brick_quest_data(data_otwarte)[1])%in%pula_zamkniete2],1)
    random_system(new_random_system)
    
    pula_zamkniete2 <<- append(pula_zamkniete2,new_random_system)
    
    if(length(pula_zamkniete2)==12){
      
      pula_zamkniete2 <<- character()
      
    }
    
  })

  random_system <- reactiveVal(NULL)
  
  pula_zamkniete2 <<- character()

  observeEvent(input$los_system, {
    
    output$bucket_icon <- renderUI({})
    
    new_random_system <- sample(pull(prepare_brick_quest_data(data_otwarte)[1])[!pull(prepare_brick_quest_data(data_otwarte)[1])%in%pula_zamkniete2],1)
    random_system(new_random_system)
    
    pula_zamkniete2 <<- append(pula_zamkniete2,new_random_system)
    
    if(length(pula_zamkniete2)==12){
      
      pula_zamkniete2 <<- character()
      
    }
    
    })
  
  oddzialy <- reactive({
    
    req(random_system())

    data_otwarte %>%
      filter(SYSTEM==random_system()) %>%
      select(ODDZIAL) %>%
      distinct() %>%
      pull()

    })

  pietra <- reactive({
    
    req(random_system())
    
    x <- data_otwarte %>%
      filter(SYSTEM==random_system()) %>%
      select(PIETRO) %>%
      distinct() %>%
      mutate(PIETRO = ifelse(str_detect(PIETRO,";")==TRUE, str_split(PIETRO,";"), PIETRO)) %>%
      unnest(cols = PIETRO) %>%
      pull()
    
    if(x[1] == "grenlandian" & 
       x[2] == "nortgripian"&
       x[3] == "megalanian"){
      
      append(rev(x[1:3]),x[4:length(x)])
      
    } else x

  })
  
  brick_labels <- reactive({
    
    req(oddzialy())
    
    # switch(length(oddzialy()),
    #        2 = {c(sample(pietra()), sample(oddzialy()),rep("__",2))},
    #        3 = {c(sample(pietra()), sample(oddzialy()),rep("__",3))},
    #        4 = {c(sample(pietra()), sample(oddzialy()),rep("__",4))})
    
    if(length(oddzialy())==2){

      c(sample(pietra()), sample(oddzialy()),rep("____________",2))

    } else if(length(oddzialy())==3){

      c(sample(pietra()), sample(oddzialy()),rep("____________",4))

    } else {

      c(sample(pietra()), sample(oddzialy()),rep("____________",6))

    }

    })

  output$bucket_list <- renderUI({

    bucket_list(
        header = "",
        group_name = "bucket_list",
        orientation = "horizontal",

        add_rank_list(
          text = paste("System: ",random_system()),
          labels = sample(brick_labels()),
          input_id = "rozsypanka"),

        add_rank_list(
          text = "Uszereguj chronologicznie oddziały",
          labels = NULL,
          input_id = "oddzialy"),

        add_rank_list(
          text = "Uszereguj chronologicznie piętra",
          labels = NULL,
          input_id = "pietra")
        )
      })
  
  output$odp_kafelki_window <- renderTable({
    
    data.frame("oddzialy" = as.vector(rbind(oddzialy(),"____________"))[-length(as.vector(rbind(oddzialy(),"____________")))])
    
    })
  
  output$odp_kafelki_window2 <- renderTable({
    
    str2 <- switch(random_system(),
                   "czwartorzed" = c("megalanian","nortgripian","grenlandian","____________","tarant","ion","kalabr","gelas"),
                   "neogen" = c("piacent","zanki","____________","messyn","torton","serrawal","lang","burdygal","akwitan"),
                   "paleogen" = c("szat","rupel","____________","priabon","barton","lutet","iprez","____________","tanet","zeland","dan"),
                   "kreda" = c("mastrycht", "kampan","santon","koniak","turon","cenoman","____________","alb","apt","barrem","hoteryw","walanzyn","berrias"),
                   "jura" = c("tyton","kimeryd","oksford","____________","kelowej","baton","bajos","aalen","____________","toark","pliensbach","synemur","hettang"),
                   "trias" = c("retyk","noryk","karnik","____________","ladyn","anizyk","____________","olenek","ind"),
                   "perm" = c("czangsing","wucziaping","____________","kapitan","word","road","____________","kungur","artynsk","sakmar","assel"),
                   "karbon" = c("gzel","kasim","moskow","baszkir","____________","serpuchow","wizen","turnej"),
                   "dewon" = c("famen","fran","____________","zywet","eifel","____________","ems","prag","lochkow"),
                   "sylur" = c("","____________","ludford","gorst","____________","homer","szejnwud","____________","telicz","aeron","ruddan"),
                   "ordowik" = c("hirnant","kat","sandb","____________","darriwil","daping","____________","flo","tremadok"),
                   "kambr" = c("pietro 10","dziangszan","paib","____________","guzang","drum","pietro 5","____________","pietro 4,","pietro 3","____________","pietro 2","fortun")
    )
    
    data.frame("pietra"=str2)
    
  })
  
  output$czek3 <- renderText(random_system())
  
  output$czek4 <- renderText(oddzialy())
  
  output$czek5 <- renderText(as.vector(paste0(pietra(),",")))
  
  output$czek6 <- renderText(brick_labels())
  
  output$res2 <- renderPrint(input$bucket_list)
  
  observeEvent(input$check_bucket, {
    
    # standard spr poprawnosci oddzialu
    
    check_oddzial <- as.vector(rbind(oddzialy(),"____________")) 
    
    # sprawdzanie poprawnosci pietra
    
    check_pietra <- switch(random_system(),
                           "czwartorzed" = c("megalanian","nortgripian","grenlandian","____________","tarant","ion","kalabr","gelas"),
                           "neogen" = c("piacent","zanki","____________","messyn","torton","serrawal","lang","burdygal","akwitan"),
                           "paleogen" = c("szat","rupel","____________","priabon","barton","lutet","iprez","____________","tanet","zeland","dan"),
                           "kreda" = c("mastrycht", "kampan","santon","koniak","turon","cenoman","____________","alb","apt","barrem","hoteryw","walanzyn","berrias"),
                           "jura" = c("tyton","kimeryd","oksford","____________","kelowej","baton","bajos","aalen","____________","toark","pliensbach","synemur","hettang"),
                           "trias" = c("retyk","noryk","karnik","____________","ladyn","anizyk","____________","olenek","ind"),
                           "perm" = c("czangsing","wucziaping","____________","kapitan","word","road","____________","kungur","artynsk","sakmar","assel"),
                           "karbon" = c("gzel","kasim","moskow","baszkir","____________","serpuchow","wizen","turnej"),
                           "dewon" = c("famen","fran","____________","zywet","eifel","____________","ems","prag","lochkow"),
                           "sylur" = c("","____________","ludford","gorst","____________","homer","szejnwud","____________","telicz","aeron","ruddan"),
                           "ordowik" = c("hirnant","kat","sandb","____________","darriwil","daping","____________","flo","tremadok"),
                           "kambr" = c("pietro 10","dziangszan","paib","____________","guzang","drum","pietro 5","____________","pietro 4,","pietro 3","____________","pietro 2","fortun")
                           )
    
    if(identical(input$oddzialy,check_oddzial[-length(check_oddzial)]) & identical(input$pietra,check_pietra)){ 

     output$bucket_icon <- renderUI({

       tags$img(src = "ok_icon.png",
                height = 35,
                width = 35)
       })

    } else {

      output$bucket_icon <- renderUI({

        tags$img(src = "fail_icon.jpg",
                 height = 35,
                 width = 35)
        })
      }


  })

# zadania zamkniete jednokrotnego wyboru ----------------------------------
  
  zadania_zamk_quiz1_data <- reactive({
    
    switch(input$typ_zad_zam_jedn,
           "pie_sys"= read_rds("data/abc_jedn_pie_sys.rds"),
           "sys_era"= read_rds("data/abc_jedn_sys_era.rds"),
           "oddz_sys"= read_rds("data/abc_jedn_oddz_sys.rds"),
           "pie_oddz"= read_rds("data/abc_jedn_pie_oddz.rds"))
    
  })
  
  zadania_zamk_quiz1_logic_mask <- reactive({
    
    switch(input$typ_zad_zam_jedn,
           "pie_sys"= read_rds("data/abc_jedn_pie_sys_logic_mask_pie_sys.rds"),
           "sys_era"= read_rds("data/abc_jedn_pie_sys_logic_mask_sys_era.rds"),
           "oddz_sys"= read_rds("data/abc_jedn_pie_sys_logic_mask_oddz_sys.rds"),
           "pie_oddz"= read_rds("data/abc_jedn_pie_sys_logic_mask_pie_oddz.rds"))
    
  })
  
  shinyjs::disable("check_answer")
  
  true_indx <- reactiveVal(NULL)

  random <- reactiveVal(NULL)

  observeEvent(input$losuj_quiz1_id,{
    
    shinyjs::enable("quiz1")
    
    shinyjs::disable("check_answer")
    
    output$result <- renderText({})
    
    output$icon_result <- renderUI({})

    new_num <- sample(c(1:nrow(zadania_zamk_quiz1_data())),1)

    random(new_num)

    new_indx <- which(as.logical(zadania_zamk_quiz1_logic_mask()[random(),]),TRUE)

    true_indx(new_indx)

  })
  
  observeEvent(input$next_question_test1, {
    
    shinyjs::enable("quiz1")
    
    shinyjs::disable("check_answer")
    
    output$result <- renderText({})
    
    output$icon_result <- renderUI({})
    
    new_num <- sample(c(1:nrow(zadania_zamk_quiz1_data())),1)
    
    random(new_num)
    
    new_indx <- which(as.logical(zadania_zamk_quiz1_logic_mask()[random(),]),TRUE)
    
    true_indx(new_indx)
    
    })
  
  observeEvent(input$typ_zad_zam_jedn,{
    
    shinyjs::enable("quiz1")
    
    output$result <- renderText({})
    
    output$icon_result <- renderUI({})
    
    new_num <- sample(c(1:nrow(zadania_zamk_quiz1_data())),1)
    
    random(new_num)
    
    new_indx <- which(as.logical(zadania_zamk_quiz1_logic_mask()[random(),]),TRUE)
    
    true_indx(new_indx)
    
  })
  

  output$quiz1_ui <- renderUI({

    req(random())
    
    if(input$typ_zad_zam_jedn != "sys_era"){
      
      radioGroupButtons("quiz1",
                   label = paste(zadania_zamk_quiz1_data()[random(),1],zadania_zamk_quiz1_data()[random(),2]),
                   choiceNames = as.character(zadania_zamk_quiz1_data()[random(),3:7]),
                   choiceValues = c(1,2,3,4,5),
                   selected = "",
                   individual = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-circle", 
                                  style = "color: steelblue"),
                     no = tags$i(class = "fa fa-circle-o", 
                                 style = "color: steelblue")))
      
    } else {
        
      radioGroupButtons("quiz1",
                   label = paste(mutate(zadania_zamk_quiz1_data()[random(),1],pytanie = replace(pytanie,pytanie=="Wybierz system ery:","Wybierz system eratemu:")),zadania_zamk_quiz1_data()[random(),2]),
                   choiceNames = as.character(zadania_zamk_quiz1_data()[random(),3:6]),
                   choiceValues = c(1,2,3,4),
                   selected = "",
                   individual = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-circle", 
                                  style = "color: steelblue"),
                     no = tags$i(class = "fa fa-circle-o", 
                                 style = "color: steelblue")))
      
    }
    
  })
  
  observeEvent(input$check_quiz1_id, {
    
    req(input$quiz1)
    
    shinyjs::disable("quiz1")
    
    shinyjs::enable("check_answer")
    
    if(true_indx() == input$quiz1){
      
      output$result <- renderUI({
        
        HTML(paste("Poprawna","odpowiedz!",sep ="<br/>"))
        
        })
      
      output$icon_result <- renderUI({
        
        tags$img(src = "ok_icon.png",
                 height = 30,
                 width = 30)
        
      })
      
    } else {
      
      output$result <- renderText("Niepoprawna odpowiedz!")
      
      output$icon_result <- renderUI({
        
        tags$img(src = "fail_icon.jpg",
                 height = 30,
                 width = 30)
        
      })
      
    }
    
  })
  
  observeEvent(input$check_answer, {
    
    output$zamk_jedn_poprawna <- renderText({
      
      zadania_zamk_quiz1_data()[random(),-c(1,2)] %>% select(true_indx()) %>% pull()
      
      })
    
    })
  
}
