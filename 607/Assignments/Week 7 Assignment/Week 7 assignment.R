
library(readxl)
library(xtable)
library(rjson)
library(jsonlite)
library(kulife)

#creating XML files from excel dataframe
Books <- read_excel("Data 607/Assignments/Week 7 Assignment/Books.xlsx")
print(xtable(Books),type = "html", file = "Books.html")

toJSON(Books, pretty = TRUE)
?toJSON()

write(toJSON(Books, pretty = TRUE), "Books.json")

write_json(Books, "Books2.json")
write.xml(Books, "Books.xml")


write.xml(Books,"Books.xml")
warnings()
xmlTree()
str(Books)
Books_xml <- lapply(purrr::transpose(Books),
                   function(x) {
                     as_xml_document(list(Trial = lapply(x, as.list)))
                   })
Books_xml

dat<-head(iris,3)
as_xml_document(dat)
as_xml_document(Books_xml)

#reading files for assignment
library(rvest)
library(xml2)
library(jsonlite)
library(XML)


books.xml<-read_xml("https://raw.githubusercontent.com/joshuargst/Data607Week7/master/Books.xml") %>%xmlParse()
books.xml


xmlToDataFrame(books.xml)


Books.html<- read_html("https://raw.githubusercontent.com/joshuargst/Data607Week7/master/Books.html") 
Books.html
Books.html%>% html_table()

books.html<- Books.html[[1]]

read_json("https://raw.githubusercontent.com/joshuargst/Data607Week7/master/Books.json")

fromJSON("https://raw.githubusercontent.com/joshuargst/Data607Week7/master/Books.json")
