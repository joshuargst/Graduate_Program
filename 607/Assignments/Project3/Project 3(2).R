library(rvest)
library(tidyverse)
library(data.table)
library(DT)
library(magrittr)
library(digest)

library(RPostgreSQL)
library(tidytext)
library(config)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(stringr)
library(zoo)
library(ggplot2)
library(knitr)

listings <- data.frame(title=character(),
                       company=character(), 
                       location=character(), 
                       summary=character(), 
                       link=character(), 
                       description = character(),
                       State = character(),
                       stringsAsFactors=FALSE) 

statenames[j]
j
i

statenames<- state.name %>% str_replace_all(" ","\\+")
for (j in 1:length(statenames)){
for (i in seq(0, 990, 10)){

  url_ds <- paste0('https://www.indeed.com/jobs?q=data+scientist&l=',statenames[j],'&start=',i)

    varfunc <- function() {
    read_html(url_ds)
  }
  
  var <- NULL
  attempt <- 1
  while( is.null(var) && attempt <= 3 ) {
    attempt <- attempt + 1
    try(
      var <- varfunc()
    )
  } 
  #var<- read_html("scrapedpage.html")
html_nodes(var,'#resultsCol .jobtitle')%>% html_text()
  
#job title
  title <-  var %>% 
    html_nodes('#resultsCol .jobtitle') %>%
    html_text() %>%
    str_extract("(\\w+.+)+") 
  
  #company
  company <- var %>% 
    html_nodes('#resultsCol .company') %>%
    html_text() %>%
    str_extract("(\\w+).+") 
  
  #location
  location <- var %>%
    html_nodes('#resultsCol .location') %>%
    html_text() %>%
    str_extract("(\\w+.)+,.[A-Z]{2}")   
  #summary
  summary <- var %>%
    html_nodes('#resultsCol .summary') %>%
    html_text() %>%
    str_extract(".+")
  
  #link
  link <- var %>%
    html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
    html_attr('href') 
  link <- paste0("https://www.indeed.com",link)
  
  listingstemp <- data.frame(title=character(),
                             company=character(), 
                             location=character(), 
                             summary=character(), 
                             link=character(), 
                             description = character(),
                             State = character(),
                             stringsAsFactors=FALSE) 
  
  listingstemp <- rbind(listingstemp, as.data.frame(cbind(title,
                                                  company,
                                                  location,
                                                  summary,
                                                  link)))
  listingstemp$State = statenames[j]
  listings<- bind_rows(listings,listingstemp)
  
}
  #create a unique id for each job posting attribute combination
  listings$uniqueid <- mapply(function(x, y, z) digest(paste0(x,y,z)), listings$title, listings$location, listings$company)
  
  #remove duplicate unique ids
  listings %<>%
    distinct(uniqueid, .keep_all = TRUE)
  #remove duplicate links
  listings %<>%
    distinct(link, .keep_all = TRUE)

}
warnings()
j
i
datatable(listings)

write.csv(listings, file = "joblistings.csv")
#create a unique id for each job posting attribute combination
listings$uniqueid <- mapply(function(x, y, z) digest(paste0(x,y,z)), listings$title, listings$location, listings$company)

#remove duplicate unique ids
listings %<>%
  distinct(uniqueid, .keep_all = TRUE)
#remove duplicate links
listings %<>%
  distinct(link, .keep_all = TRUE)
datatable(listings)
i
link[1]
for (i in (4219:length(listings$link))){
  desciption <- tryCatch(
    html_text(html_node(read_html(as.character(listings$link[i])),'.jobsearch-JobComponent-description')),
    error=function(e){NA}
  )
  if (is.null(desciption)){
    desc <- NA
  }
  listings$description[i] <- desciption
}

i
wordcloud(words = listings$summary, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
