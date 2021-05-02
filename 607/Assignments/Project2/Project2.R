library(tidyverse)
library(dplyr)
library(stringr)
library(reshape2)
library(tidyr)
library(data.table)
library(readr)
library(readxl)


candyhierarchy2017 <- read.csv("C:/Users/joshu/OneDrive - CDM Smith/Documents/CUNY SPS/Data 607/Assignments/Project2/candyhierarchy2017.csv", dec=",")
candyhierarchy2017 <- read.csv("https://raw.githubusercontent.com/joshuargst/Data607Project2/master/candyhierarchy2017.csv", dec=",") 

#the following database from the University of british columbia talks about the candy hierarchy 
head(candyhierarchy2017)
str(candyhierarchy2017)

#as you can see, all data has been imported as factors and this survey is represented primarily by classification-type data.the dataset has approximately 2460 observations (rows) by 120 variables (columns)
#we can start by gathering all the columns that share the same type of question (e.g. Q6. represents all the candies spread out over various columns)

#Start by extracting based on patterns in column names. All columns with questions begin with Q, so we will extract all the questionaire items first. and we will create a function to do so while keeping the unique id
NamesVector<-colnames(candyhierarchy2017)

gatherer<- function(StringtoGatherOn){
  Question<-str_which(NamesVector,paste0("\\b^",StringtoGatherOn,"\\b"))
  wideQs<- candyhierarchy2017[c(1,Question)]
  LongQs<-gather(wideQs, key = "tempcol", value = "Response", -Internal.ID)
  LongQs$QuestionID<-str_sub(LongQs[[2]], 1,nchar(StringtoGatherOn))
  LongQs$Question<-str_sub(LongQs[[2]],nchar(StringtoGatherOn)+1,-1L)%>% str_replace_all("^\\.+","")%>%str_replace_all("\\."," ")
  LongQs<-LongQs[c(1,4,5,3)]
  LongQs
}

Questionlist<-str_extract_all(NamesVector,"Q\\d*")%>% unlist() %>% unique()

CandySurvey<-data.frame()
for( i in 1:length(Questionlist)){
  
  Questionsubset<-gatherer(Questionlist[i])
  Questionsubset$Response<-as.character(Questionsubset$Response)
  CandySurvey<-bind_rows(CandySurvey,Questionsubset)
  
}


CandyPreferenceSummary<-
  CandySurvey%>%
  filter(QuestionID=="Q6")%>% 
  group_by(Question,Response)%>%
  summarise(n())
setnames(CandyPreferenceSummary, "n()","Count")

CandyPreferenceSummary$Response[CandyPreferenceSummary$Response==""]<-"No Response"
CandyPreferenceSummary<-as.data.frame(CandyPreferenceSummary)


CandyPreferenceSummary%>%filter(Response == "JOY")%>% top_n(n = c(25),Count)%>%
  ggplot(aes(reorder(Question,Count),y = Count))+
  geom_col(color ="black",fill = "skyblue3",alpha = .5)+facet_grid(Response~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))+
  coord_flip()+
  labs(x = "Candy",
       y = "Count of Candies",
       title = "Top 25 Candies With Positive Responses")


CandyPreferenceSummary%>%filter(Response == "MEH")%>% top_n(n = c(25), Count)%>%
  ggplot(aes(reorder(Question,Count),y = Count))+
  geom_col(color ="black",fill = "orange3",alpha = .5)+facet_grid(Response~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))+
  coord_flip()+
  labs(x = "Candy",
       y = "Count of Candies",
       title = "Top 25 Candies With Indifferent Responses")


CandyPreferenceSummary%>%filter(Response == "DESPAIR")%>% top_n(n = c(25),Count)%>%
  ggplot(aes(reorder(Question,Count),y = Count))+
  geom_col(color ="black",fill = "orange3",alpha = .5)+facet_grid(Response~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))+
  coord_flip()+
  labs(x = "Candy",
       y = "Count of Candies",
       title = "Top 25 Candies With \nNegative Responses")


CandyPreferenceSummary%>%filter(Response == "No Response")%>% top_n(n = c(25),Count)%>%
  ggplot(aes(reorder(Question,Count),y = Count))+
  geom_col(color ="black",fill = "purple3",alpha = .5)+facet_grid(Response~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))+
  coord_flip()+
  labs(x = "Candy",
       y = "Count of Candies with No Response",
       title = "Top 25 Candies With No Response")


semi_WideSurvey<-data.frame(candyhierarchy2017[c(1,2,3,4,5,6)])
Question_6<-gatherer("Q6")
semi_WideSurvey<-left_join(semi_WideSurvey,Question_6, by = "Internal.ID")



CandyPreferenceSummary2<-
  semi_WideSurvey%>%
  group_by(Q2..GENDER, Question,Response)%>%
  summarise(n())
setnames(CandyPreferenceSummary2, "n()","Count")
setnames(CandyPreferenceSummary2, "Q2..GENDER","Gender")
CandyPreferenceSummary2$Response[CandyPreferenceSummary2$Response==""]<-"No Response"

q <- coord_flip()+aes(stringr::str_wrap(Question, 15), Count)+ ylab(yaxis_label)

CandyPreferenceSummary2<-as.data.frame(CandyPreferenceSummary2)
CandyPreferenceSummary2%>%filter(Response == "JOY")%>%group_by(Gender)%>%
  ggplot(aes(reorder(Question,Count),y = Count))+
  geom_col(color ="black",aes(fill = Gender),alpha = .5)+facet_grid(Response~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))+
  coord_flip()+aes(stringr::str_wrap(Question, 15), Count)+
  labs(x = "Candy",
       y = "Count of Candies with Positive Responses",
       title = "All Candies ")


# Second Dataset ----------------------------------------------------------


rm(list = ls())

GRAD_RATE_AND_OUTCOMES_2018 <- read_csv("C:/Users/joshu/OneDrive - CDM Smith/Documents/CUNY SPS/Data 607/Assignments/Project2/GRAD_RATE_AND_OUTCOMES_2018.csv", 
                                        col_types = cols(BOCES_CODE = col_character(), 
                                                         BOCES_NAME = col_character(), COUNTY_CODE = col_character(), 
                                                         COUNTY_NAME = col_character(), DROPOUT_PCT = col_number(), 
                                                         GED_PCT = col_number(), GRAD_PCT = col_number(), 
                                                         LEA_BEDS = col_character(), LEA_NAME = col_character(), 
                                                         NON_DIPLOMA_CREDENTIAL_PCT = col_number(),
                                                         LOCAL_PCT = col_number(),
                                                         NRC_CODE = col_character(), NRC_DESC = col_character(), 
                                                         NYC_IND = col_character(), REG_ADV_PCT = col_number(), 
                                                         REG_PCT = col_number(), STILL_ENR_PCT = col_number()))


ELA_AND_MATH_RESEARCHER_FILE_2019 <- read_excel("Data 607/Assignments/Project2/3-8_ELA_AND_MATH_RESEARCHER_FILE_2019.xlsx", 
                                                     col_types = c("text", "numeric", "text", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "text", "text", "text", "numeric", 
                                                                   "text", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric"))
GET("https://github.com/joshuargst/Data607Project2/blob/master/3-8_ELA_AND_MATH_RESEARCHER_FILE_2019.xlsx?raw=true",
    write_disk(tf2<-tempfile(fileext = ".xlsx")))

ELA_AND_MATH_RESEARCHER_FILE_2019 <- read_excel(tf2,col_types = c("text", "numeric", "text", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "text", "text", "text", "numeric", 
                                                                  "text", "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric"))
View(ELA_AND_MATH_RESEARCHER_FILE_2019)
           
colnames(ELA_AND_MATH_RESEARCHER_FILE_2019)
#we can remove unecessary information that we wont use in any downstream analysis. this includes information like the first column which has the same value for the entire dataset, County code, bedscode, subgroup code, and NRC code

ELA_AND_MATH_RESEARCHER_FILE_2019<- ELA_AND_MATH_RESEARCHER_FILE_2019[-c(1,2,4,6,10)]

#we can extract the names of all of our columns so that we may manipulate the data frame based on patterns in any of the column name strings
NamesVector<-colnames(ELA_AND_MATH_RESEARCHER_FILE_2019)

#because the percent information was imported as numeric values, we need to convert these to decimals and can do this by exctracting by the common string "PCT" within our dataframe.
Percent_Columns<-str_which(NamesVector,"PCT$")
ELA_AND_MATH_RESEARCHER_FILE_2019<- ELA_AND_MATH_RESEARCHER_FILE_2019[c(1:7,Percent_Columns)]

#now we can tidy this data by aggredateing 
NamesVector<-colnames(ELA_AND_MATH_RESEARCHER_FILE_2019)

ELA_Math.long<- gather(ELA_AND_MATH_RESEARCHER_FILE_2019,key = "Level", value = "Percentage",-NRC_DESC,-COUNTY_DESC,-NAME,-ITEM_SUBJECT_AREA,-ITEM_DESC,-SUBGROUP_NAME,-TOTAL_TESTED)

ELA_Math.long$Level<- ELA_Math.long$Level%>% str_replace_all("L","Level")%>% str_replace_all("_PCT","")
unique(ELA_Math.long$Level)

ELA_Math.long %>%
ggplot(aes(x = Level, y = Percentage)) +stat_summary(geom = "bar", fun.y = "mean")


ELA_Math.long %>%
  ggplot(aes(x = Level, y = Percentage)) +stat_summary(geom = "bar", fun.y = "mean",aes(fill = ITEM_SUBJECT_AREA))+
  facet_grid(ITEM_SUBJECT_AREA~.)


ELA_Math.long %>%filter(SUBGROUP_NAME != "All Students")%>%
  ggplot(aes(x = Level, y = Percentage)) +stat_summary(geom = "bar", fun.y = "mean",aes(fill = SUBGROUP_NAME),color = "black")


# Third Dataset ----------------------------------------------------------

RatesDeaths_AllIndicators <- read_excel("C:/Users/joshu/OneDrive - CDM Smith/Documents/CUNY SPS/Data 607/Assignments/Project2/RatesDeaths_AllIndicators.xlsx")

library(httr)
GET("https://github.com/joshuargst/Data607Project2/blob/master/RatesDeaths_AllIndicators.xlsx?raw=true",
    write_disk(tf<-tempfile(fileext = ".xlsx")))

RatesDeaths_AllIndicators <- read_excel(tf)



head(RatesDeaths_AllIndicators)
str(RatesDeaths_AllIndicators)
#initiate a column ID to create reference for wide dataset
RatesDeaths_AllIndicators$IDWide<-seq.int(nrow(RatesDeaths_AllIndicators))

#reorder so that ID is shown as the first column
RatesDeaths_AllIndicators<- RatesDeaths_AllIndicators[c(ncol(RatesDeaths_AllIndicators),1:ncol(RatesDeaths_AllIndicators)-1)]

NamesVector<-colnames(RatesDeaths_AllIndicators)
mortalityratescolumns<-
  str_extract_all(NamesVector,"[:graph:]*MR*[:graph:]*")%>%unlist
deathratescolumns<-
  str_extract_all(NamesVector,"[:graph:]*Death*[:graph:]*")%>%unlist

#using gather to Tidy Data
RatesDeathTidy <- gather(RatesDeaths_AllIndicators,key = "Category", value = "Count", -IDWide,-`ISO Code`,-CountryName,-`Uncertainty bounds*`)



#removing all NA values from our Count column that was a result of tidying data and aggregating columns that may not have had any values in a particular row x column.
RatesDeathTidy<- RatesDeathTidy[is.na(RatesDeathTidy$Count)==FALSE,]

RatesDeathTidy$Year<-
  str_extract_all(RatesDeathTidy$Category,"\\d{4}$")%>% unlist

RatesDeathTidy$Category<- str_replace_all(RatesDeathTidy$Category,"\\.\\d{4}$","")




RatesDeathTidy$StatType<-RatesDeathTidy$Category
RatesDeathTidy$StatType <- str_replace_all(RatesDeathTidy$Category,"[:graph:]*MR*[:graph:]*", "Mortality Rate")
RatesDeathTidy$StatType <- str_replace_all(RatesDeathTidy$StatType,"[:graph:]*Death*[:graph:]*", "Death Count")
unique(RatesDeathTidy$StatType)
unique(RatesDeathTidy$Category)
RatesDeathTidy$Category<-str_replace_all(RatesDeathTidy$Category,"U5MR", "Under.five")
RatesDeathTidy$Category<-str_replace_all(RatesDeathTidy$Category,"IMR", "Infant")
RatesDeathTidy$Category<-str_replace_all(RatesDeathTidy$Category,"NMR", "NeoNatal")
RatesDeathTidy$Category<-str_replace_all(RatesDeathTidy$Category,"\\.Deaths", "")


RatesDeathTidy%>% filter(StatType == "Mortality Rate")%>%
  ggplot(aes(y = Count, x = Category))+geom_boxplot()


RatesDeathTidy$Year<- as.numeric(RatesDeathTidy$Year)

ListofCountries<-unique(RatesDeathTidy$CountryName)


RatesDeathTidy%>%
  filter(CountryName %in% ListofCountries[11:20])%>%  
  filter(Category== "Under.five")%>% 
  filter(StatType == "Mortality Rate")%>%
  ggplot(aes(x = Year,y = Count))+geom_point(aes(color = `Uncertainty bounds*`))+facet_wrap(CountryName~.,labeller = labeller(CountryName=label_wrap_gen(width = 10)),scales = "free_y")

