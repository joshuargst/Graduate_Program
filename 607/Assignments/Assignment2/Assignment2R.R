
library(dbConnect)
library(RMySQL)
library(tidyverse)

connection = dbConnect(MySQL(),user = 'root', password = 'root', dbname = 'database1', host = 'localhost', port = 3306)



query1 = paste0("SELECT * from ",dbListTables(connection)[1])
movie.survey<-dbGetQuery(connection,query1)

View(movie.survey)

library('mongolite')
library('knitr')

mongo2 <- mongo(collection = 'Movies', db = 'database1')
?mongo()
