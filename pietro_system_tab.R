library("shiny")

pietro_system_data <- read_rds("data/pietro_system_data.rds")

sys <- pietro_system_data %>% 
  mutate(quest = word(quest,-1)) %>% 
  select(quest) %>% pull()


pietro_system_tab <- tabPanel("Pietro system",
                              
                              actionButton("ps_los", "Losuj"),
                              textOutput("ps_txt_out1"),
                              br(),
                              textInput("ps_txt_in1",label=NULL),
                              actionButton("ps_check", "Sprawdz"),
                              textOutput("xxx"),
                              textOutput("tab"),
                              textOutput("ps_result"),
                              uiOutput("ps_image"),
                              tableOutput("ps_asnwer_tab"),
                              actionButton("ps_show","Pokaz/ukryj odpowiedzi"),
                              actionButton("ps_clear","Wyczysc"))