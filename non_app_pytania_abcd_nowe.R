library("tidyverse")
source("func.R")

data_otwarte <- read_rds("data/prepared_data.rds")

pytania_abcd <- separate_rows(data_otwarte[1,],PIETRO) %>% 
  bind_rows(data_otwarte[-1,]) %>% 
  mutate(ODDZIAL = case_when(ODDZIAL == "górna" | ODDZIAL == "górny" |
                               ODDZIAL == "dolny" | ODDZIAL == "dolna" |
                               ODDZIAL == "środkowy" | ODDZIAL == "środkowa" ~ paste(SYSTEM,ODDZIAL),
                             TRUE ~ ODDZIAL))


# wskaz pietro systemu ----------------------------------------------------
pietr_sys <- pytania_abcd[,c(2,4)]

pietr_sys <- pietr_sys[rep(seq_len(nrow(pietr_sys)), each=10),]

pietr_sys <- pietr_sys %>% 
  mutate(pytanie = rep("Wybierz piętro systemu:",nrow(pietr_sys)), .before=SYSTEM) %>% 
  rename(true_ = "PIETRO") %>% 
  filter(true_!="")

random_answer_df <- tibble(
  col1 = rep(NA,nrow(pietr_sys)),
  col2 = rep(NA,nrow(pietr_sys)),
  col3 = rep(NA,nrow(pietr_sys)),
  col4 = rep(NA,nrow(pietr_sys)),
  col5 = rep(NA,nrow(pietr_sys))
  )

random_answers <- list()

pula <- unique(c(pytania_abcd$PIETRO,pytania_abcd$ODDZIAL))
pula <- pula[pula!=""]

for(x in 1:nrow(pietr_sys)){
  
  switch(
    
    pietr_sys$SYSTEM[x],
    
    "czwartorzęd" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="czwartorzęd"),true_))],4),
                                                         sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="czwartorzęd"),true_))],1))),
    
    "neogen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="neogen"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="neogen"),true_))],1))),
    
    "paleogen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="paleogen"),true_))],4),
                                                      sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="paleogen"),true_))],1))),
    
    "kreda" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="kreda"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="kreda"),true_))],1))),
    
    "jura" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="jura"),true_))],4),
                                                  sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="jura"),true_))],1))),
    
    "trias" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="trias"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="trias"),true_))],1))),
    
    "perm" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="perm"),true_))],4),
                                                  sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="perm"),true_))],1))),
    
    "karbon" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="karbon"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="karbon"),true_))],1))),
    
    "dewon" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="dewon"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="dewon"),true_))],1))),
    
    "sylur" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="sylur"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="sylur"),true_))],1))),
    
    "ordowik" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="ordowik"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="ordowik"),true_))],1))),
    
    "kambr" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_sys, SYSTEM=="kambr"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(pietr_sys, SYSTEM=="kambr"),true_))],1)))
  )
  
}

for(row in 1:nrow(random_answer_df)){
  
  for(col in 1:ncol(random_answer_df)){
    
    random_answer_df[row,col] <- random_answers[[row]][col]
    
  }
}

pietr_sys <- bind_cols(pietr_sys[,-3],random_answer_df)


popr <- distinct(pytania_abcd[,c(2,4)]) %>% 
  filter(PIETRO!="") %>% 
  group_by(SYSTEM) %>% 
  summarise(PIETRO = paste0(PIETRO, collapse = ","))

lista_poprawnych <- list()

for(x in 1:nrow(popr)){
  
  lista_poprawnych[[x]] <- extract_char_vec(popr[x,2])
  
}

poprawne <- character()

for(x in 1:nrow(pietr_sys)){
  
  switch(pietr_sys$SYSTEM[x],
         
         "czwartorzęd" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[1]]),
         
         "dewon" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[2]]),
         
         "jura" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[3]]),
         
         "kambr" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[4]]),
         
         "karbon" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[5]]),
         
         "kreda" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[6]]),
         
         "neogen" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[7]]),
         
         "ordowik" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[8]]),
         
         "paleogen" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[9]]),
         
         "perm" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[10]]),
         
         "sylur" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[11]]),
         
         "trias" = poprawne[x] <- intersect(extract_char_vec(pietr_sys[x,3:7]),lista_poprawnych[[12]])
    )
}

pietr_sys %>% 
  mutate(poprawna = poprawne) %>% 
  select(SYSTEM,poprawna) %>% 
  distinct() %>% 
  group_by(SYSTEM) %>% 
  summarise(pop = paste0(poprawna,collapse = ","))








