library("tidyverse")

ps_clear_input <- function(text){
  
  text <- tolower(str_replace_all(text,"[^[:alnum:]]",""))
  
  return(text)
  
}