# geo_app
education geologic app

in order to run this app directly from server, use below link:

https://wpiela.shinyapps.io/geo_app/

or locally on your desktop, use below code:

```
list_of_packages <- c("shiny","tidyverse","shinyjs","shinyWidgets","shinyBS","sortable")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

shiny::runGitHub(repo="geo_app", username="wiktorpiela", ref="main")

```
