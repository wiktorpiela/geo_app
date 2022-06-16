library("tidyverse")
library("readxl")

tables <- list()

ranges <- c("B3:F76","J3:N76","R3:V76")

for(x in seq_along(ranges)){
  
  tables[[x]] <- read_excel("data/strat_tab.xlsx",
                     range = ranges[x],
                     sheet = "Arkusz2") %>% 
    select(-c(1))
  
  colnames(tables[[x]]) <- c("ERA","SYSTEM","ODDZIAL","PIETRO")
  
  tables[[x]] <- tables[[x]] %>%
    fill(c(ERA,SYSTEM,ODDZIAL,PIETRO), .direction = "down") %>%
    distinct()
}

tables <- reduce(tables, bind_rows) %>%
  mutate(SYSTEM = str_replace(SYSTEM, c("ę"),"e"),
         PIETRO = str_replace_all(PIETRO, c("ł" = "l",
                                            "ż"="z",
                                            "ń"="n",
                                            "ę"="e",
                                            "ó"="o",
                                            "ś"="s")),
         ODDZIAL = str_replace_all(ODDZIAL, c("ś"="s",
                                              "ó"="o")))

tables %>% print(n=Inf)


# write_rds(tables, "data/prepared_data.rds")