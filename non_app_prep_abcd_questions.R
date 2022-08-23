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

#przypisanie poprawnych odpowiedz
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

pietr_sys <- mutate(pietr_sys, poprawna = poprawne) 

# write_rds(pietr_sys,"data/abcd_pietr_sys.rds")


# wskaz system eratemu ----------------------------------------------------
sys_era <- distinct(pytania_abcd[,c(1,2)])

sys_era <- sys_era[rep(seq_len(nrow(sys_era)), each=20),]

sys_era <- sys_era %>% 
  mutate(pytanie = rep("Wybierz system eratemu:",nrow(sys_era)), .before=ERA) %>% 
  rename(true_ = "SYSTEM") %>% 
  filter(true_!="")

random_answer_df <- tibble(
  col1 = rep(NA,nrow(sys_era)),
  col2 = rep(NA,nrow(sys_era)),
  col3 = rep(NA,nrow(sys_era)),
  col4 = rep(NA,nrow(sys_era)),
  col5 = rep(NA,nrow(sys_era))
)

random_answers <- list()

pula <- unique(c(pytania_abcd$SYSTEM,pytania_abcd$PIETRO,pytania_abcd$ODDZIAL))
pula <- pula[pula!=""]

for(x in 1:nrow(sys_era)){
  
  switch(
    
    sys_era$ERA[x],
    
    "kenozoik" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(sys_era, ERA=="kenozoik"),true_))],4),
                                                         sample(pula[pula%in%unique(pull(filter(sys_era, ERA=="kenozoik"),true_))],1))),
    
    "mezozoik" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(sys_era, ERA=="mezozoik"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(sys_era, ERA=="mezozoik"),true_))],1))),
    
    "paleozoik" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(sys_era, ERA=="paleozoik"),true_))],4),
                                                      sample(pula[pula%in%unique(pull(filter(sys_era, ERA=="paleozoik"),true_))],1)))
  )
  
}

for(row in 1:nrow(random_answer_df)){
  
  for(col in 1:ncol(random_answer_df)){
    
    random_answer_df[row,col] <- random_answers[[row]][col]
    
  }
}

sys_era <- bind_cols(sys_era[,-3],random_answer_df)

popr <- distinct(pytania_abcd[,c(1,2)]) %>% 
  group_by(ERA) %>% 
  summarise(SYSTEM = paste0(SYSTEM, collapse = ","))

lista_poprawnych <- list()

for(x in 1:nrow(popr)){
  
  lista_poprawnych[[x]] <- extract_char_vec(popr[x,2])
  
}

poprawne <- character()

for(x in 1:nrow(sys_era)){
  
  switch(sys_era$ERA[x],
         
         "kenozoik" = poprawne[x] <- intersect(extract_char_vec(sys_era[x,3:7]),lista_poprawnych[[1]]),
         
         "mezozoik" = poprawne[x] <- intersect(extract_char_vec(sys_era[x,3:7]),lista_poprawnych[[2]]),
         
         "paleozoik" = poprawne[x] <- intersect(extract_char_vec(sys_era[x,3:7]),lista_poprawnych[[3]])
  )
}

sys_era <- mutate(sys_era, poprawna = poprawne) 

# write_rds(sys_era,"data/abcd_sys_era.rds")

# wskaz oddzial systemu ---------------------------------------------------
oddz_sys <- distinct(pytania_abcd[,c(2,3)])

oddz_sys <- oddz_sys[rep(seq_len(nrow(oddz_sys)), each=20),]

oddz_sys <- oddz_sys %>% 
  mutate(pytanie = rep("Wybierz oddział systemu:",nrow(oddz_sys)), .before=SYSTEM) %>% 
  rename(true_ = "ODDZIAL") %>% 
  filter(true_!="")

random_answer_df <- tibble(
  col1 = rep(NA,nrow(oddz_sys)),
  col2 = rep(NA,nrow(oddz_sys)),
  col3 = rep(NA,nrow(oddz_sys)),
  col4 = rep(NA,nrow(oddz_sys)),
  col5 = rep(NA,nrow(oddz_sys))
)

random_answers <- list()

pula <- unique(c(pytania_abcd$SYSTEM,pytania_abcd$PIETRO,pytania_abcd$ODDZIAL))
pula <- pula[pula!=""]

for(x in 1:nrow(oddz_sys)){
  
  switch(
    
    oddz_sys$SYSTEM[x],
    
    "czwartorzęd" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="czwartorzęd"),true_))],4),
                                                         sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="czwartorzęd"),true_))],1))),
    
    "neogen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="neogen"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="neogen"),true_))],1))),
    
    "paleogen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="paleogen"),true_))],4),
                                                      sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="paleogen"),true_))],1))),
    
    "kreda" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="kreda"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="kreda"),true_))],1))),
    
    "jura" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="jura"),true_))],4),
                                                  sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="jura"),true_))],1))),
    
    "trias" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="trias"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="trias"),true_))],1))),
    
    "perm" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="perm"),true_))],4),
                                                  sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="perm"),true_))],1))),
    
    "karbon" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="karbon"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="karbon"),true_))],1))),
    
    "dewon" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="dewon"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="dewon"),true_))],1))),
    
    "sylur" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="sylur"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="sylur"),true_))],1))),
    
    "ordowik" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="ordowik"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="ordowik"),true_))],1))),
    
    "kambr" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(oddz_sys, SYSTEM=="kambr"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(oddz_sys, SYSTEM=="kambr"),true_))],1)))
  )
  
}

for(row in 1:nrow(random_answer_df)){
  
  for(col in 1:ncol(random_answer_df)){
    
    random_answer_df[row,col] <- random_answers[[row]][col]
    
  }
}

oddz_sys <- bind_cols(oddz_sys[,-3],random_answer_df)

#przypisanie poprawnych odpowiedzi
popr <- distinct(pytania_abcd[,c(2,3)]) %>% 
  group_by(SYSTEM) %>% 
  summarise(ODDZIAL = paste0(ODDZIAL, collapse = ","))

lista_poprawnych <- list()

for(x in 1:nrow(popr)){
  
  lista_poprawnych[[x]] <- extract_char_vec(popr[x,2])
  
}

poprawne <- character()

for(x in 1:nrow(oddz_sys)){
  
  switch(oddz_sys$SYSTEM[x],
         
         "czwartorzęd" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[1]]),
         
         "dewon" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[2]]),
         
         "jura" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[3]]),
         
         "kambr" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[4]]),
         
         "karbon" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[5]]),
         
         "kreda" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[6]]),
         
         "neogen" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[7]]),
         
         "ordowik" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[8]]),
         
         "paleogen" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[9]]),
         
         "perm" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[10]]),
         
         "sylur" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[11]]),
         
         "trias" = poprawne[x] <- intersect(extract_char_vec(oddz_sys[x,3:7]),lista_poprawnych[[12]])
  )
}

oddz_sys <- mutate(oddz_sys, poprawna = poprawne)

# write_rds(oddz_sys,"data/abcd_oddz_sys.rds")


# wskaz pietro oddzialu ---------------------------------------------------
pietr_oddz <- pytania_abcd[,c(3,4)]

pietr_oddz <- pietr_oddz[rep(seq_len(nrow(pietr_oddz)), each=10),]

pietr_oddz <- pietr_oddz %>% 
  mutate(pytanie = rep("Wybierz piętro oddziału:",nrow(pietr_oddz)), .before=ODDZIAL) %>% 
  rename(true_ = "PIETRO") %>% 
  filter(true_!="")

random_answer_df <- tibble(
  col1 = rep(NA,nrow(pietr_oddz)),
  col2 = rep(NA,nrow(pietr_oddz)),
  col3 = rep(NA,nrow(pietr_oddz)),
  col4 = rep(NA,nrow(pietr_oddz)),
  col5 = rep(NA,nrow(pietr_oddz))
)

random_answers <- list()

pula <- unique(c(pytania_abcd$PIETRO,pytania_abcd$ODDZIAL))
pula <- pula[pula!=""]

for(x in 1:nrow(pietr_oddz)){
  
  switch(
    
    pietr_oddz$ODDZIAL[x],
    
    "holocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="holocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="holocen"),true_))],1))),
    
    "plejstocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="plejstocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="plejstocen"),true_))],1))),
    
    "pliocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="pliocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="pliocen"),true_))],1))),
    
    "miocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="miocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="miocen"),true_))],1))),
    
    "oligocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="oligocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="oligocen"),true_))],1))),
    
    "eocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="eocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="eocen"),true_))],1))),
    
    "paleocen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="paleocen"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="paleocen"),true_))],1))),
    
    "kreda górna" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="kreda górna"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="kreda górna"),true_))],1))),
    
    "kreda dolna" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="kreda dolna"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="kreda dolna"),true_))],1))),
    
    "jura górna" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="jura górna"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="jura górna"),true_))],1))),
    
    "jura środkowa" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="jura środkowa"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="jura środkowa"),true_))],1))),
    
    "jura dolna" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="jura dolna"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="jura dolna"),true_))],1))),
    
    "trias górny" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="trias górny"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="trias górny"),true_))],1))),
    
    "trias środkowy" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="trias środkowy"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="trias środkowy"),true_))],1))),
    
    "trias dolny" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="trias dolny"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="trias dolny"),true_))],1))),
    
    "loping" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="loping"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="loping"),true_))],1))),
    
    "gwadelup" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="gwadelup"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="gwadelup"),true_))],1))),
    
    "cisural" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="cisural"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="cisural"),true_))],1))),
    
    "pensylwan" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="pensylwan"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="pensylwan"),true_))],1))),
    
    "mississipi" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="mississipi"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="mississipi"),true_))],1))),
    
    "dewon górny" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="dewon górny"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="dewon górny"),true_))],1))),
    
    "dewon środkowy" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="dewon środkowy"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="dewon środkowy"),true_))],1))),
    
    "dewon dolny" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="dewon dolny"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="dewon dolny"),true_))],1))),
    
    "ludlow" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ludlow"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ludlow"),true_))],1))),
    
    "wenlok" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="wenlok"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="wenlok"),true_))],1))),
    
    "landower" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="landower"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="landower"),true_))],1))),
    
    "ordowik górny" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ordowik górny"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ordowik górny"),true_))],1))),
    
    "ordowik środkowy" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ordowik środkowy"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ordowik środkowy"),true_))],1))),
    
    "ordowik dolny" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ordowik dolny"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="ordowik dolny"),true_))],1))),
    
    "furong" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="furong"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="furong"),true_))],1))),
    
    "epoka 3" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="epoka 3"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="epoka 3"),true_))],1))),
    
    "epoka 2" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="epoka 2"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="epoka 2"),true_))],1))),
    
    "terenew" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="terenew"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(pietr_oddz, ODDZIAL=="terenew"),true_))],1)))
    )
  
}

for(row in 1:nrow(random_answer_df)){
  
  for(col in 1:ncol(random_answer_df)){
    
    random_answer_df[row,col] <- random_answers[[row]][col]
    
  }
}

pietr_oddz <- bind_cols(pietr_oddz[,-3],random_answer_df)

#przypisanie poprawnych odpowiedzi
popr <- distinct(pytania_abcd[,c(3,4)]) %>% 
  filter(PIETRO!="") %>% 
  group_by(ODDZIAL) %>% 
  summarise(PIETRO = paste0(PIETRO, collapse = ",")) %>% 
  arrange(ODDZIAL)

lista_poprawnych <- list()

for(x in 1:nrow(popr)){
  
  lista_poprawnych[[x]] <- extract_char_vec(popr[x,2])
  
}

poprawne <- character()

for(x in 1:nrow(pietr_oddz)){
  
  switch(pietr_oddz$ODDZIAL[x],
         
         "cisural" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[1]]),
         
         "dewon dolny" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[2]]),
         
         "dewon górny" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[3]]),
         
         "dewon środkowy" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[4]]),
         
         "eocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[5]]),
         
         "epoka 2" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[6]]),
         
         "epoka 3" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[7]]),
         
         "furong" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[8]]),
         
         "gwadelup" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[9]]),
         
         "holocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[10]]),
         
         "jura dolna" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[11]]),
         
         "jura górna" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[12]]),
         
         "jura środkowa" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[13]]),
         
         "kreda dolna" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[14]]),
         
         "kreda górna" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[15]]),
         
         "landower" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[16]]),
         
         "loping" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[17]]),
         
         "ludlow" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[18]]),
         
         "miocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[19]]),
         
         "mississipi" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[20]]),
         
         "oligocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[21]]),
         
         "ordowik dolny" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[22]]),
         
         "ordowik górny" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[23]]),
         
         "ordowik środkowy" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[24]]),
         
         "paleocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[25]]),
         
         "pensylwan" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[26]]),
         
         "plejstocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[27]]),
         
         "pliocen" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[28]]),
         
         "terenew" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[29]]),
         
         "trias dolny" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[30]]),
         
         "trias górny" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[31]]),
         
         "trias środkowy" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[32]]),
         
         "wenlok" = poprawne[x] <- intersect(extract_char_vec(pietr_oddz[x,3:7]),lista_poprawnych[[33]])
         
         )
}

pietr_oddz <- mutate(pietr_oddz, poprawna = poprawne) 

# write_rds(pietr_oddz,"data/abcd_pietr_oddz.rds")
