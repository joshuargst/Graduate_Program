

#intro stuff
library(devtools)
install_github('plotly/dashR', upgrade = TRUE)
install.packages('tinytex')
tinytex::install_tinytex()
remove.packages("tinytex")
update.packages(ask = FALSE, checkBuilt = TRUE)  # update R packages
tinytex::tlmgr_update()
tinytex::reinstall_tinytex()

install.packages(c('openintro','OIdata','devtools','tidyverse', 'ggplot2',
                   'psych','reshape2','knitr','markdown','shiny','R.rsp',
                   'fivethirtyeight'))
library(devtools)
devtools::install_github("jbryer/DATA606")

setwd('C:/Users/joshu/OneDrive - CDM Smith/Documents/CUNY SPS/606/Assignments')
setwd('C:/Users/registejh/OneDrive - CDM Smith/Documents/CUNY SPS/606/Assignments')
?kmeans
library(DATA606)
###setwd first!!!!!
startLab('Lab9')
iris
remove.packages('digest')
install.packages('glue')
tinytex:::is_tinytex()
tinytex:::install_prebuilt()
devtools::install_github('yihui/tinytex')
tinytex:::install_prebuilt()
library(tinytex)
