library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(httr)
library(rvest)
library(digest)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(XML)
library(stringr)
library(ggplot2)
library(tm)

library(magrittr)
library(rworldmap)

library(caret)
library(rpart)
library(rpart.plot)


library(ggcorrplot)
library(Hmisc)


setwd('C:/Users/joshu/OneDrive - CDM Smith/Documents/CUNY SPS/Data 607/Assignments/Final Project')
setwd('C:/Users/registejh/OneDrive - CDM Smith/Documents/CUNY SPS/Data 607/Assignments/Final Project')
ArticleListings <- data.frame(title=character(),
                              summary=character(), 
                              link=character(), 
                              description = character(),
                              stringsAsFactors=FALSE) 

#causes of climate change searched on Science news
for (i in seq(0,40,1)){
  
  var<-read_html(paste0('https://www.sciencenews.org/page/',i,'?s=cause+of+climate+change&topic=&start-date=&end-date=&orderby=relevance'))
  title<-html_nodes(var,'.post-item-river__title___J3spU a') %>% html_text(trim = TRUE)
  link<-html_nodes(var,'.post-item-river__title___J3spU a') %>% html_attr('href')
  summary<-html_nodes(var,'.post-item-river__excerpt___3ok6B') %>% html_text(trim = TRUE)
  
  
  ArticleListingstemp <- data.frame(title=character(),
                                    summary=character(), 
                                    link=character(),
                                    stringsAsFactors=FALSE) 
  
  ArticleListingstemp <- rbind(ArticleListingstemp, as.data.frame(cbind(title,
                                                                        summary,
                                                                        link)))
  
  ArticleListings<- bind_rows(ArticleListings,ArticleListingstemp)
  
}

#causes of global warming searched on science articles

for (i in seq(0,40,1)){
  
  var<-read_html(paste0('https://www.sciencenews.org/page/',i,'?s=cause+of+global+warming&topic=&start-date=&end-date=&orderby=relevance'))
  title<-html_nodes(var,'.post-item-river__title___J3spU a') %>% html_text(trim = TRUE)
  link<-html_nodes(var,'.post-item-river__title___J3spU a') %>% html_attr('href')
  summary<-html_nodes(var,'.post-item-river__excerpt___3ok6B') %>% html_text(trim = TRUE)
  
  
  ArticleListingstemp <- data.frame(title=character(),
                                    summary=character(), 
                                    link=character(),
                                    stringsAsFactors=FALSE) 
  
  ArticleListingstemp <- rbind(ArticleListingstemp, as.data.frame(cbind(title,
                                                                        summary,
                                                                        link)))
  
  ArticleListings<- bind_rows(ArticleListings,ArticleListingstemp)
  
}

#pulling descriptions for every word
for (i in 1:nrow(ArticleListings)){
  
  ArticleListings[i,4]<- read_html(ArticleListings[i,3])%>%html_nodes('.single__rich-text___BlzVF p')%>% html_text()%>%paste(collapse = " ")
}
rm(ArticleListingstemp)
#write.csv(ArticleListings,"articlelistings.csv")
ArticleListings<-read.csv('articlelistings.csv')


wordextract<- function(dfcolumn){
descriptionwords <- dfcolumn %>% str_replace_all("^<[:graph:]*>$","")%>%
  str_replace_all("\\\n"," ") %>%
  str_replace_all("[^[:alpha:] ]"," ") %>% str_replace_all("\\s[:alpha:]{1,2}\\s","")%>% tolower()

#pulling all individual words into a dataframe and counting
word_counts <- as.data.frame(table(unlist(strsplit(descriptionwords, "\\s+"))))
colnames(word_counts)<-c("word","Freq")
#removing filler words and sentimental words
word_counts <-anti_join(word_counts,get_stopwords())%>%
  anti_join(get_sentiments())
wordcloud(words = word_counts$word, freq = word_counts$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

}

wordextract(ArticleListings$title)
wordextract(ArticleListings$summary)
wordextract(ArticleListings$description)



GHG_Inventory<- read_csv("greenhouse_gas_inventory_data_data.csv")
GHG_unique_cat<-unique(GHG_Inventory$category)
GHG_unique_loc<-unique(GHG_Inventory$country_or_area)


{
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[2]],"GHG_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[1]],"CO2_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[3]],"GHGEmissions_W/O_LULUCF")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[4]],"HFC_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[5]],"CH4_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[6]],"NF3_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[7]],"N2O_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[8]],"PFC_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[9]],"SF6_Emissions")
GHG_Inventory$category<- GHG_Inventory$category %>% str_replace_all(GHG_unique_cat[[10]],"HFC\\+PFC")
}
GHG_unique_cat<-unique(GHG_Inventory$category)
GHG_unique_cat
GHG_Inventoryfreq<-GHG_Inventory %>%filter(category==GHG_unique_cat[[1]]) %>% 
  group_by(country_or_area)%>%
  mutate(percbyloc=value/sum(value))




#GHG_Inventoryfreq$country_or_area<-as.factor(GHG_Inventoryfreq$country_or_area)

map.world<-map_data("world")

GHG_Inventoryfreq$country_or_area<- str_replace_all(GHG_Inventoryfreq$country_or_area,"Russian Federation","Russia" )
GHG_Inventoryfreq$country_or_area<- str_replace_all(GHG_Inventoryfreq$country_or_area,"United Kingdom","UK" )
GHG_Inventoryfreq$country_or_area<- str_replace_all(GHG_Inventoryfreq$country_or_area,"United States of America","USA" )
map.world2<-left_join(map.world, GHG_Inventoryfreq, by=c('region'='country_or_area'))

unique(GHG_Inventoryfreq$country_or_area)

ggplot()+
 theme(legend.title = element_text("Percent"))+
 geom_map(data=map.world2, map=map.world2, aes(map_id=region, x=long, y=lat, fill=percbyloc))+
 scale_fill_gradient(low = "lightblue",high = "purple", guide = "colourbar")+
 coord_equal()+ 
  labs(x = "Latitude",
       y = 'Longitude',
       title = 'Percent of Global CO2 Emissions by Country')


GHG_Inventory%>% #filter(category==GHG_unique_cat[1])%>%
  filter(country_or_area==GHG_unique_loc[1])%>%
  ggplot()+
  geom_point(mapping = aes(year,value, color = category))+geom_smooth(mapping = aes(year,value, color = category))+
  facet_wrap(category~.,scales='free_y')+
  
  theme(strip.text.x=element_blank(),legend.position = 'bottom', axis.text.x = element_text(angle = 90))+
  labs(title = "U.S. Greenhouse Gas Emissions Over Time",
       x = "Eq Metric Tons CO2",
       y = "Year")


climatechange<-read_excel("climate_change_download_0.xls")



#convert to long
climatechange_gather<-climatechange%>%gather(key = "DATE",value = "VALUE",-(1:6),(7:ncol(climatechange)))

climatechangeunique<-unique(climatechange_gather$`Series name`)
climatechange_gather<-climatechange_gather[c(2,4,7,8)]
climatechange_gather$VALUE<-as.numeric(climatechange_gather$VALUE)

ClimatechangeUS<-climatechange_gather%>%filter(`Country name`=="United States")#%>%group_by(`Series name`)%>%summarise(VALUE= mean(VALUE,na.rm = T))



ClimatechangeUSTree<- ClimatechangeUS %>% spread(`Series name`,VALUE)


ClimatechangeUSTree$CO2Thresh<-as.factor(
ntile(ClimatechangeUSMatrix$`CO2 emissions, total (KtCO2)`, 4))
ClimatechangeUSTree$CO2Thresh

ClimatechangeUSMatrix<-ClimatechangeUSTree[c(-1,-2,-13,-14)]

ClimatechangeUSTree<-ClimatechangeUSTree[c(-1,-2,-13,-14,-15)]


ClimatechangeUSTree <- ClimatechangeUSTree[,colSums(is.na(ClimatechangeUSTree))<nrow(ClimatechangeUSTree)-5]

ClimatechangeUSTree<- ClimatechangeUSTree%>%filter(is.na(CO2Thresh)==FALSE)

ClimatechangeUSTreeS<- sample_n(ClimatechangeUSTree,1000,replace= TRUE)

SingleTree <- rpart(formula = CO2Thresh ~ ., 
                    data = ClimatechangeUSTreeS,
                    method = 'class')

rpart.plot(x = SingleTree, yesno = 2, type = 5,extra = 108,tweak = 1, shadow.col = "lightgrey")


Pearson_Matrix<-function(Data,siglevel,statmethod,labsize){
  #get all the numeric data out for meatrix
  Data<-select(Data,which(sapply(Data,class)=="numeric"))
  cor_data<- rcorr(as.matrix(Data),type = statmethod)
  M <- cor_data$r
  #p to designate significant correlations
  p_mat <- cor_data$P
  
  for(i in 1:ncol(Data)){
    try(p_mat[i,i]<-0)
  }
  
  Matrix<-ggcorrplot(M,method = "square", colors = c("#E46726", "white","#6D9EC1"),lab = TRUE, 
                     lab_size = labsize,type = "full", p.mat=p_mat, sig.level = siglevel,legend.title = statmethod,
                     title = paste0(statmethod," Correlation Matrix" ))
  print(Matrix)
}
ClimatechangeUSMatrix<- ClimatechangeUSMatrix[-length(ClimatechangeUSMatrix)]
ClimatechangeUSMatrix <- ClimatechangeUSMatrix[,colSums(is.na(ClimatechangeUSMatrix))<nrow(ClimatechangeUSMatrix)-15]

ClimatechangeUSTreeS<- sample_n(ClimatechangeUSTree,1000,replace= TRUE)
Pearson_Matrix(ClimatechangeUSMatrix,.05,"pearson",3)

