library("tidyverse")


# funkcje do zadan otwartych ----------------------------------------------

ps_clear_input <- function(text){
  
  text <- tolower(str_replace_all(text,"[^[:alnum:]]",""))
  
  return(text)
  
}

prepare_pie_sys_data <- function(data){
  
  data <- data %>% 
    group_by(SYSTEM) %>% 
    mutate(PIETRO = rev(PIETRO)) %>% 
    group_by(SYSTEM) %>% 
    summarise(PIETRO = paste0(PIETRO, collapse=";"))
  
  colnames(data) <- c("klucz", "wartosc")
  
  return(data)
}

prepare_sys_era_data <- function(data){
  
  data <- data %>% 
    group_by(ERA) %>% 
    mutate(SYSTEM = rev(SYSTEM)) %>% 
    select(ERA,SYSTEM) %>% 
    distinct() %>% 
    group_by(ERA) %>% 
    summarise(SYSTEM = paste0(SYSTEM, collapse=";"))
  
  colnames(data) <- c("klucz", "wartosc")

  return(data)
}


prepare_odd_sys_data <- function(data){
  
  data <- data %>% 
    group_by(SYSTEM) %>% 
    mutate(ODDZIAL = rev(ODDZIAL)) %>% 
    select(SYSTEM, ODDZIAL) %>% 
    distinct() %>% 
    ungroup() %>% 
    mutate(ODDZIAL = case_when(ODDZIAL == "gorna" | ODDZIAL == "gorny" |
                                 ODDZIAL == "dolny" | ODDZIAL == "dolna" |
                                 ODDZIAL == "srodkowy" | ODDZIAL == "srodkowa" ~ paste(SYSTEM,ODDZIAL),
                               TRUE ~ ODDZIAL)) %>%
    group_by(SYSTEM) %>% 
    summarise(paste0(ODDZIAL, collapse=";"))
  
  colnames(data) <- c("klucz", "wartosc")

  return(data)
}

prepare_pie_odd_data <- function(data){
  
  data <- data %>% 
    mutate(ODDZIAL = case_when(ODDZIAL == "gorna" | ODDZIAL == "gorny" |
                                 ODDZIAL == "dolny" | ODDZIAL == "dolna" |
                                 ODDZIAL == "srodkowy" | ODDZIAL == "srodkowa" ~ paste(SYSTEM,ODDZIAL),
                               TRUE ~ ODDZIAL)) %>% 
    select(ODDZIAL, PIETRO) %>% 
    group_by(ODDZIAL) %>% 
    mutate(PIETRO = rev(PIETRO)) %>% 
    summarise(paste0(PIETRO,collapse = ";")) 
  
  colnames(data) <- c("klucz", "wartosc")

  return(data)
}

# prepare_pie_sys_data(data_otwarte)
# prepare_sys_era_data(data_otwarte)
# prepare_odd_sys_data(data_otwarte)
# prepare_pie_odd_data(data_otwarte)



# funkcje do zadan zamknietych --------------------------------------------

unlist(strsplit(pull(prepare_pie_sys_data(data_otwarte)[1,2]),";"))


