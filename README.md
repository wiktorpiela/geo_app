# geo_app
education geologic app

in order to run this app directly from server, use below link:

https://wpiela.shinyapps.io/geo_app/

or locally on your desktop, use below code:

```
if(!require(c("shiny","tidyverse","shinyjs","shinyWidgets","shinyBS","sortable"))) {
  
  install.packages(c("shiny","tidyverse","shinyjs","shinyWidgets","shinyBS","sortable"))
  
}
library(shiny)

runGitHub(repo="geo_app", username="wiktorpiela", ref="main")
```



