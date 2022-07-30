library("tidyverse")
source("func.R")

data_otwarte <- read_rds("data/prepared_data.rds")

pytania_abcd <- separate_rows(data_otwarte[1,],PIETRO) %>% 
  bind_rows(data_otwarte[-1,]) %>% 
  mutate(ODDZIAL = case_when(ODDZIAL == "gorna" | ODDZIAL == "gorny" |
                               ODDZIAL == "dolny" | ODDZIAL == "dolna" |
                               ODDZIAL == "srodkowy" | ODDZIAL == "srodkowa" ~ paste(SYSTEM,ODDZIAL),
                             TRUE ~ ODDZIAL))


# pytanie o pietro do systemu ----------------------------------------------------
# pula oddzial + pietro
sp <- distinct(select(pytania_abcd, c(2,4)))

type1 <- sp[rep(seq_len(nrow(sp)), each=10),]

type1 <- type1 %>% 
  mutate(pytanie = rep("Wybierz pietro systemu:",nrow(type1)), .before=SYSTEM) %>% 
  rename(true_ = "PIETRO") 

random_answer_df <- data.frame(
  
  col1 = rep(NA,nrow(type1)),
  col2 = rep(NA,nrow(type1)),
  col3 = rep(NA,nrow(type1)),
  col4 = rep(NA,nrow(type1)),
  col5 = rep(NA,nrow(type1))
  
)

random_answers <- list()

pula <- unique(c(pytania_abcd$PIETRO,pytania_abcd$ODDZIAL))
pula <- pula[pula!=""]

for(x in 1:nrow(type1)){
  
  switch(
    
    type1$SYSTEM[x],
    
    "czwartorzed" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="czwartorzed"),true_))],4),
                                                         sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="czwartorzed"),true_))],1))),
    
    "neogen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="neogen"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="neogen"),true_))],1))),
    
    "paleogen" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="paleogen"),true_))],4),
                                                      sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="paleogen"),true_))],1))),
    
    "kreda" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="kreda"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="kreda"),true_))],1))),
    
    "jura" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="jura"),true_))],4),
                                                  sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="jura"),true_))],1))),
    
    "trias" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="trias"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="trias"),true_))],1))),
    
    "perm" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="perm"),true_))],4),
                                                  sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="perm"),true_))],1))),
    
    "karbon" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="karbon"),true_))],4),
                                                    sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="karbon"),true_))],1))),
    
    "dewon" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="dewon"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="dewon"),true_))],1))),
    
    "sylur" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="sylur"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="sylur"),true_))],1))),
    
    "ordowik" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="ordowik"),true_))],4),
                                                     sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="ordowik"),true_))],1))),
    
    "kambr" = random_answers[[x]] <- sample(append(sample(pula[!pula%in%unique(pull(filter(type1, SYSTEM=="kambr"),true_))],4),
                                                   sample(pula[pula%in%unique(pull(filter(type1, SYSTEM=="kambr"),true_))],1)))
    )
  
}

for(row in 1:nrow(random_answer_df)){
  
  for(col in 1:ncol(random_answer_df)){
    
    random_answer_df[row,col] <- random_answers[[row]][col]
    
  }
  
}

type1_ready <- bind_cols(type1[,-3],random_answer_df)

# sprawdzenie ktora odpowiedz jest poprawna - przypisuje poprawny ideks
index_poprawny <- numeric()

for(row in 1:nrow(type1_ready)){
  
  for(col in 3:ncol(type1_ready)){
    
    if(pull(type1_ready[row,2])=="czwartorzed" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="czwartorzed"),true_))==TRUE) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="neogen" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="neogen"),true_))) index_poprawny <- append(index_poprawny,col)
      
    if(pull(type1_ready[row,2])=="paleogen" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="paleogen"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="kreda" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="kreda"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="jura" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="jura"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="trias" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="trias"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="perm" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="perm"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="karbon" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="karbon"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="dewon" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="dewon"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="sylur" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="sylur"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="ordowik" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="ordowik"),true_))) index_poprawny <- append(index_poprawny,col)
    
    if(pull(type1_ready[row,2])=="kambr" & pull(type1_ready[row,col])%in%unique(pull(filter(type1, SYSTEM=="kambr"),true_))) index_poprawny <- append(index_poprawny,col)
    
    else next
    
  }
}

logical_mask_type1 <- tibble(
  
  col1 = rep(FALSE,nrow(type1_ready)),
  col2 = rep(FALSE,nrow(type1_ready)),
  col3 = rep(FALSE,nrow(type1_ready)),
  col4 = rep(FALSE,nrow(type1_ready)),
  col5 = rep(FALSE,nrow(type1_ready)),
  index_poprawny = index_poprawny-2
  
)


for(row in 1:nrow(logical_mask_type1)){

  if(logical_mask_type1[row,6]==5) logical_mask_type1$col5[row] <- TRUE
  
  if(logical_mask_type1[row,6]==4) logical_mask_type1$col4[row] <- TRUE
  
  if(logical_mask_type1[row,6]==3) logical_mask_type1$col3[row] <- TRUE
  
  if(logical_mask_type1[row,6]==2) logical_mask_type1$col2[row] <- TRUE
  
  if(logical_mask_type1[row,6]==1) logical_mask_type1$col1[row] <- TRUE
  
}

logical_mask_type1 <- select(logical_mask_type1,-index_poprawny)


#gotowe ramki system-pietro
type1_ready
logical_mask_type1


# pytanie o system do ery -------------------------------------------------
sys_era <- distinct(pytania_abcd[,c(1,2)])

type2 <- sys_era[rep(seq_len(nrow(sys_era)), each=10),]

type2 <- type2 %>% 
  mutate(pytanie = rep("Wybierz system ery:",nrow(type2)), .before=ERA) %>% 
  rename(true_ = "SYSTEM")

random_answer_df2 <- tibble(
  
  col1 = rep(NA,nrow(type2)),
  col2 = rep(NA,nrow(type2)),
  col3 = rep(NA,nrow(type2)),
  col4 = rep(NA,nrow(type2))
  
  )

random_answers2 <- list()

pula2 <- unique(c(pytania_abcd$SYSTEM,pytania_abcd$ODDZIAL))

for(x in 1:nrow(type2)){
  
  switch(
    
    type2$ERA[x],
    
    "kenozoik" = random_answers2[[x]] <- sample(append(sample(pula2[!pula2%in%unique(pull(filter(type2,ERA=="kenozoik"),true_))],3),
                                                sample(pula2[pula2%in%unique(pull(filter(type2,ERA=="kenozoik"),true_))],1))),
    
    "mezozoik" = random_answers2[[x]] <- sample(append(sample(pula2[!pula2%in%unique(pull(filter(type2,ERA=="mezozoik"),true_))],3),
                                                sample(pula2[pula2%in%unique(pull(filter(type2,ERA=="mezozoik"),true_))],1))),
    
    "paleozoik" = random_answers2[[x]] <- sample(append(sample(pula2[!pula2%in%unique(pull(filter(type2,ERA=="paleozoik"),true_))],3),
                                                sample(pula2[pula2%in%unique(pull(filter(type2,ERA=="paleozoik"),true_))],1)))
    )
}


for(row in 1:nrow(random_answer_df2)){
  
  for(col in 1:ncol(random_answer_df2)){
    
    random_answer_df2[row,col] <- random_answers2[[row]][col]
    
  }
}

type2_ready <- bind_cols(type2[,-3],random_answer_df2)

# sprawdzenie ktora odpowiedz jest poprawna - przypisuje poprawny ideks
index_poprawny <- numeric()

for(row in 1:nrow(type2_ready)){
  
  for(col in 3:ncol(type2_ready)){
    
    if(pull(type2_ready[row,2])=="kenozoik" & pull(type2_ready[row,col])%in%unique(pull(filter(type2, ERA=="kenozoik"),true_))){
      
      index_poprawny <- append(index_poprawny, col)
      
    } else if(pull(type2_ready[row,2])=="mezozoik" & pull(type2_ready[row,col])%in%unique(pull(filter(type2, ERA=="mezozoik"),true_))){
      
      index_poprawny <- append(index_poprawny, col)
      
    } else if(pull(type2_ready[row,2])=="paleozoik" & pull(type2_ready[row,col])%in%unique(pull(filter(type2, ERA=="paleozoik"),true_))){
      
      index_poprawny <- append(index_poprawny, col)
      
    }
    
  }
}


logical_mask_type2 <- tibble(
  
  col1 = rep(FALSE,nrow(type2_ready)),
  col2 = rep(FALSE,nrow(type2_ready)),
  col3 = rep(FALSE,nrow(type2_ready)),
  col4 = rep(FALSE,nrow(type2_ready)),
  index_poprawny = index_poprawny-2
  
)


for(row in 1:nrow(logical_mask_type2)){
  
  if(logical_mask_type2[row,5]==4) logical_mask_type2$col4[row] <- TRUE
  
  if(logical_mask_type2[row,5]==3) logical_mask_type2$col3[row] <- TRUE
  
  if(logical_mask_type2[row,5]==2) logical_mask_type2$col2[row] <- TRUE
  
  if(logical_mask_type2[row,5]==1) logical_mask_type2$col1[row] <- TRUE
  
}

logical_mask_type2 <- select(logical_mask_type2,-index_poprawny)


type2_ready
logical_mask_type2

# oddzial systemu ---------------------------------------------------------

odz_sys <- distinct(pytania_abcd[,c(2,3)])

type3 <- odz_sys[rep(seq_len(nrow(odz_sys)), each=20),]

type3 <- type3 %>% 
  mutate(pytanie = rep("Wybierz oddzial systemu:",nrow(type3)), .before=SYSTEM) %>% 
  rename(true_ = "ODDZIAL")

random_answer_df3 <- tibble(
  
  col1 = rep(NA,nrow(type3)),
  col2 = rep(NA,nrow(type3)),
  col3 = rep(NA,nrow(type3)),
  col4 = rep(NA,nrow(type3)),
  col5 = rep(NA,nrow(type3))
)

random_answers3 <- list()

pula3 <- unique(c(pytania_abcd$PIETRO,pytania_abcd$ODDZIAL))
pula3 <- pula3[pula3!=""]

for(x in 1:nrow(type3)){
  
  switch(
    
    type3$SYSTEM[x],
    
    "czwartorzed" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="czwartorzed"),true_))],4),
                                                       sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="czwartorzed"),true_))],1))),
    
    "neogen" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="neogen"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="neogen"),true_))],1))),
    
    "paleogen" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="paleogen"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="paleogen"),true_))],1))),
    
    "kreda" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="kreda"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="kreda"),true_))],1))),
    
    "jura" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="jura"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="jura"),true_))],1))),
    
    "trias" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="trias"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="trias"),true_))],1))),
    
    "perm" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="perm"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="perm"),true_))],1))),
    
    "karbon" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="karbon"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="karbon"),true_))],1))),
    
    "dewon" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="dewon"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="dewon"),true_))],1))),
    
    "sylur" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="sylur"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="sylur"),true_))],1))),
    
    "ordowik" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="ordowik"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="ordowik"),true_))],1))),
    
    "kambr" = random_answers3[[x]] <- sample(append(sample(pula3[!pula3%in%unique(pull(filter(type3,SYSTEM=="kambr"),true_))],4),
                                                          sample(pula3[pula3%in%unique(pull(filter(type3,SYSTEM=="kambr"),true_))],1)))
    
  )
}

for(row in 1:nrow(random_answer_df3)){
  
  for(col in 1:ncol(random_answer_df3)){
    
    random_answer_df3[row,col] <- random_answers3[[row]][col]
    
  }
}

type3_ready <- bind_cols(type3[,-3],random_answer_df3)

# sprawdzenie ktora odpowiedz jest poprawna - przypisuje poprawny ideks
index_poprawny <- numeric()

for(row in 1:nrow(type3_ready)){
  
  for(col in 3:ncol(type3_ready)){
    
    if(pull(type3_ready[row,2])=="czwartorzed" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="czwartorzed"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="neogen" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="neogen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="paleogen" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="paleogen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="kreda" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="kreda"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="jura" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="jura"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="trias" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="trias"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="perm" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="perm"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="karbon" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="karbon"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="dewon" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="dewon"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="sylur" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="sylur"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="ordowik" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="ordowik"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type3_ready[row,2])=="kambr" & pull(type3_ready[row,col])%in%unique(pull(filter(type3, SYSTEM=="kambr"),true_))) index_poprawny <- append(index_poprawny, col)

  }
}

logical_mask_type3 <- tibble(
  
  col1 = rep(FALSE,nrow(type3_ready)),
  col2 = rep(FALSE,nrow(type3_ready)),
  col3 = rep(FALSE,nrow(type3_ready)),
  col4 = rep(FALSE,nrow(type3_ready)),
  col5 = rep(FALSE,nrow(type3_ready)),
  index_poprawny = index_poprawny-2
  
)


for(row in 1:nrow(logical_mask_type3)){
  
  if(logical_mask_type3[row,6]==5) logical_mask_type3$col5[row] <- TRUE
  
  if(logical_mask_type3[row,6]==4) logical_mask_type3$col4[row] <- TRUE
  
  if(logical_mask_type3[row,6]==3) logical_mask_type3$col3[row] <- TRUE
  
  if(logical_mask_type3[row,6]==2) logical_mask_type3$col2[row] <- TRUE
  
  if(logical_mask_type3[row,6]==1) logical_mask_type3$col1[row] <- TRUE
  
}

logical_mask_type3 <- select(logical_mask_type3,-index_poprawny)


type3_ready
logical_mask_type3


# pietro oddzialu ---------------------------------------------------------

pie_oddz <- distinct(pytania_abcd[,c(3,4)])

type4 <- pie_oddz[rep(seq_len(nrow(pie_oddz)), each=10),]

type4 <- type4 %>% 
  mutate(pytanie = rep("Wybierz pietro oddzialu:",nrow(type4)), .before=ODDZIAL) %>% 
  rename(true_ = "PIETRO") %>% 
  mutate(true_ = ifelse(ODDZIAL=="przidoli","nie istnieje",true_))

random_answer_df4 <- tibble(
  
  col1 = rep(NA,nrow(type4)),
  col2 = rep(NA,nrow(type4)),
  col3 = rep(NA,nrow(type4)),
  col4 = rep(NA,nrow(type4)),
  col5 = rep(NA,nrow(type4))
)

random_answers4 <- list()

pula4 <- unique(c(pytania_abcd$PIETRO,pytania_abcd$SYSTEM,pytania_abcd$ERA))
pula4[pula4==""] <- "nie istnieje"

oddzialy <- unique(type4$ODDZIAL)

for(x in 1:nrow(type4)){

  switch(

    type4$ODDZIAL[x],

    "holocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="holocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="holocen"),true_))],1))),
    
    "plejstocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="plejstocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="plejstocen"),true_))],1))),
    
    "pliocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="pliocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="pliocen"),true_))],1))),
    
    "miocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="miocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="miocen"),true_))],1))),
    
    "oligocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="oligocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="oligocen"),true_))],1))),
    
    "eocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="eocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="eocen"),true_))],1))),
    
    "paleocen" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="paleocen"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="paleocen"),true_))],1))),
    
    "kreda gorna" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="kreda gorna"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="kreda gorna"),true_))],1))),
    
    "kreda dolna" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="kreda dolna"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="kreda dolna"),true_))],1))),
    
    "jura gorna" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="jura gorna"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="jura gorna"),true_))],1))),
    
    "jura srodkowa" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="jura srodkowa"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="jura srodkowa"),true_))],1))),
    
    "jura dolna" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="jura dolna"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="jura dolna"),true_))],1))),
    
    "trias gorny" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="trias gorny"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="trias gorny"),true_))],1))),
    
    "trias srodkowy" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="trias srodkowy"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="trias srodkowy"),true_))],1))),
    
    "trias dolny" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="trias dolny"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="trias dolny"),true_))],1))),
    
    "loping" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="loping"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="loping"),true_))],1))),
    
    "gwadelup" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="gwadelup"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="gwadelup"),true_))],1))),
    
    "cisural" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="cisural"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="cisural"),true_))],1))),
    
    "pensylwan" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="pensylwan"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="pensylwan"),true_))],1))),
    
    "mississipi" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="mississipi"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="mississipi"),true_))],1))),
    
    "dewon gorny" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="dewon gorny"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="dewon gorny"),true_))],1))),
    
    "dewon srodkowy" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="dewon srodkowy"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="dewon srodkowy"),true_))],1))),
    
    "dewon dolny" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="dewon dolny"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="dewon dolny"),true_))],1))),
    
    "przidoli" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="przidoli"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="przidoli"),true_))],1))),
    
    "ludlow" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="ludlow"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="ludlow"),true_))],1))),
    
    "wenlok" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="wenlok"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="wenlok"),true_))],1))),
    
    "landower" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="landower"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="landower"),true_))],1))),
    
    "ordowik gorny" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="ordowik gorny"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="ordowik gorny"),true_))],1))),
    
    "ordowik srodkowy" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="ordowik srodkowy"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="ordowik srodkowy"),true_))],1))),
    
    "ordowik dolny" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="ordowik dolny"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="ordowik dolny"),true_))],1))),
    
    "furong" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="furong"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="furong"),true_))],1))),
    
    "epoka 3" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="epoka 3"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="epoka 3"),true_))],1))),
    
    "epoka 2" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="epoka 2"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="epoka 2"),true_))],1))),
    
    "terenew" = random_answers4[[x]] <- sample(append(sample(pula4[!pula4%in%unique(pull(filter(type4,ODDZIAL=="terenew"),true_))],4),sample(pula4[pula4%in%unique(pull(filter(type4,ODDZIAL=="terenew"),true_))],1)))
    
  )
}

for(row in 1:nrow(random_answer_df4)){
  
  for(col in 1:ncol(random_answer_df4)){
    
    random_answer_df4[row,col] <- random_answers4[[row]][col]
    
  }
}

type4_ready <- bind_cols(type4[,-3],random_answer_df4)

# sprawdzenie ktora odpowiedz jest poprawna - przypisuje poprawny ideks
index_poprawny <- numeric()

for(row in 1:nrow(type4_ready)){
  
  for(col in 3:ncol(type4_ready)){
    
    if(pull(type4_ready[row,2])=="holocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="holocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="plejstocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="plejstocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="pliocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="pliocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="miocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="miocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="oligocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="oligocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="eocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="eocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="paleocen" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="paleocen"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="kreda gorna" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="kreda gorna"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="kreda dolna" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="kreda dolna"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="jura gorna" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="jura gorna"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="jura srodkowa" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="jura srodkowa"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="jura dolna" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="jura dolna"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="trias gorny" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="trias gorny"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="trias srodkowy" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="trias srodkowy"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="trias dolny" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="trias dolny"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="loping" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="loping"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="gwadelup" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="gwadelup"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="cisural" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="cisural"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="pensylwan" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="pensylwan"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="mississipi" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="mississipi"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="dewon gorny" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="dewon gorny"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="dewon srodkowy" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="dewon srodkowy"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="dewon dolny" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="dewon dolny"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="przidoli" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="przidoli"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="ludlow" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="ludlow"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="wenlok" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="wenlok"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="landower" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="landower"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="ordowik gorny" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="ordowik gorny"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="ordowik srodkowy" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="ordowik srodkowy"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="ordowik dolny" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="ordowik dolny"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="furong" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="furong"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="epoka 3" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="epoka 3"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="epoka 2" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="epoka 2"),true_))) index_poprawny <- append(index_poprawny, col)
    
    if(pull(type4_ready[row,2])=="terenew" & pull(type4_ready[row,col])%in%unique(pull(filter(type4, ODDZIAL=="terenew"),true_))) index_poprawny <- append(index_poprawny, col)

  }
}


logical_mask_type4 <- tibble(
  
  col1 = rep(FALSE,nrow(type4_ready)),
  col2 = rep(FALSE,nrow(type4_ready)),
  col3 = rep(FALSE,nrow(type4_ready)),
  col4 = rep(FALSE,nrow(type4_ready)),
  col5 = rep(FALSE,nrow(type4_ready)),
  index_poprawny = index_poprawny-2
  
)


for(row in 1:nrow(logical_mask_type4)){
  
  if(logical_mask_type4[row,6]==5) logical_mask_type4$col5[row] <- TRUE
  
  if(logical_mask_type4[row,6]==4) logical_mask_type4$col4[row] <- TRUE
  
  if(logical_mask_type4[row,6]==3) logical_mask_type4$col3[row] <- TRUE
  
  if(logical_mask_type4[row,6]==2) logical_mask_type4$col2[row] <- TRUE
  
  if(logical_mask_type4[row,6]==1) logical_mask_type4$col1[row] <- TRUE
  
}

logical_mask_type4 <- select(logical_mask_type4,-index_poprawny)


type4_ready
logical_mask_type4
    

# export ------------------------------------------------------------------

write_rds(type1_ready,"data/abc_jedn_pie_sys.rds")
write_rds(logical_mask_type1,"data/abc_jedn_pie_sys_logic_mask_pie_sys.rds")

write_rds(type2_ready,"data/abc_jedn_sys_era.rds")
write_rds(logical_mask_type2,"data/abc_jedn_pie_sys_logic_mask_sys_era.rds")

write_rds(type3_ready,"data/abc_jedn_oddz_sys.rds")
write_rds(logical_mask_type3,"data/abc_jedn_pie_sys_logic_mask_oddz_sys.rds")

write_rds(type4_ready,"data/abc_jedn_pie_oddz.rds")
write_rds(logical_mask_type1,"data/abc_jedn_pie_sys_logic_mask_pie_oddz.rds")



# test UI -----------------------------------------------------------------



library(shiny)
library(tidyverse)


quest <- read_csv("C:/Users/wpiel/OneDrive/Desktop/gg") %>% drop_na()

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
      
      output$result <- renderText("")
      
    } else {
      
      output$result <- renderText("")
      
    }
    
    
  })
  
}

shinyApp(ui,server)




as.character(which(as.logical(logic_mask[1,]),TRUE))




# proba disable radio buttons ---------------------------------------------

library(shiny)

ui <- fluidPage(
  
  shinymaterial::material_radio_button("radio",
               "Check one option",
               choices = c("1"="1",
                           "2"="2",
                           "3"="3",
                           "4"="4")),
  
  shinymaterial::material_button("check","Check your answer!"),
  
  shinymaterial::material_button("unlock","Unlock selection"),
  
  
  shinyjs::useShinyjs()
  
)

server <- function(input,output,session){
  
  observeEvent(input$check, {
    
    shinyjs::disable("radio")
    
    if(input$radio=="2"){
      
      output$result <- renderText("Good answer!")
      
    } else output$result <- renderText("Try again!")
    
    
  })
  
  observeEvent(input$unlock,{
    
    shinyjs::enable("radio")
    
  })
  
}

shiny::shinyApp(ui,server)

