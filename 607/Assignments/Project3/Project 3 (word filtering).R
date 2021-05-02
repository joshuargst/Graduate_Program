
library(wordcloud)
library(tidyr)
library(dplyr)
library(stringr)
library(tm)
library(tidytext)
library(textdata)
library(tidyverse)
library(digest)
library(mapproj)
library(ggmap)
library(maps)


joblistings<- as.data.frame(readr::read_csv("https://raw.githubusercontent.com/joshuargst/607project3/master/joblistings.csv"))



#extracting all description words into a vector
descriptionwords <- joblistings$description %>% str_replace_all("^<[:graph:]*>$","")%>%
  str_replace_all("\\\n"," ") %>%
  str_replace_all("[^[:alpha:] ]"," ") %>% tolower()


#pulling all individual words into a dataframe and counting
word_counts <- as.data.frame(table(unlist(strsplit(descriptionwords, "\\s+"))))

colnames(word_counts)<-c("word","Freq")


#removing filler words and sentimental words
word_counts <-anti_join(word_counts,get_stopwords())%>%
  anti_join(get_sentiments())

#removing noisy words with low counts for table Size reduction
word_counts<-word_counts[word_counts$Freq>200,]


#preliminary wordcloud
wordcloud(words = word_counts$word, freq = word_counts$Freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



soft_skills <- c("understand","analytical","professional","management","team","leadership","business","driven","communication","lead","creative","interpersonal","flexible")



hard_skills <- c("R", "PYTHON", "SQL", "JAVA","PERL","C","HADOOP","APACHE","ORACLE","SCALA","ACCESS", "SAS","LINUX", "AZURE", "EXCEL","Metlab","AWS","TABLEAU","SPARK","HIVE","GIS") %>%tolower()
hard_skills.freq<-filter(word_counts, word %in% hard_skills)%>%
  mutate(perc=Freq/sum(Freq))
soft_skills.freq<-filter(word_counts, word %in% soft_skills)%>%
  mutate(perc=Freq/sum(Freq))


ggplot(hard_skills.freq, aes(x = reorder(toupper(word),-Freq), y = Freq, fill = Freq, label = scales::percent(perc))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme(axis.text=element_text(angle=90))+  
  labs(title = "Hard Skills Needed for Data Science",
       x = "Frequency of Skill",
       y = "Skill")


ggplot(soft_skills.freq, aes(x = reorder(toupper(word),-Freq), y = Freq, fill = Freq, label = scales::percent(perc))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme(axis.text=element_text(angle=90))+  
  labs(title = "Soft Skills Needed for Data Science",
       x = "Frequency of Skill",
       y = "Skill")


{
joblistingsLower<-joblistings
joblistingsLower$description<-tolower(joblistings$description)
}


descriptionsbystate <-
  joblistingsLower[c(7,8)] %>% group_by(State) %>%
  mutate(description= paste(description, collapse = " ")) %>% unique()%>%ungroup()%>%mutate(abb = state.abb)

HardSkillfreq_byState<-data.frame()
for (i in 1:length(hard_skills)){
HardSkillfreq_byState.temp<-data.frame()
HardSkillfreq_byState.temp<-
bind_rows(HardSkillfreq_byState.temp,data.frame(freq = str_count(descriptionsbystate$description,paste0("\\b",tolower(hard_skills[i]),"\\b"))))%>%
bind_cols(HardSkillfreq_byState.temp,data.frame(State = state.name))%>%
bind_cols(HardSkillfreq_byState.temp,data.frame(abb = state.abb))

HardSkillfreq_byState.temp$Hard.Skill<-hard_skills[i]
HardSkillfreq_byState<- bind_rows(HardSkillfreq_byState.temp,HardSkillfreq_byState)
rm(HardSkillfreq_byState.temp)
}

HardSkillfreq_byState<-HardSkillfreq_byState%>%group_by(State)%>% mutate(percbystate=freq/sum(freq)) 




HardSkillfreq_byState$State<-tolower(HardSkillfreq_byState$State)
HardSkillfreq_byState<- right_join(state.fips,HardSkillfreq_byState)




colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", 
           "#980043")

HardSkillfreq_byState$colorbuckets <- as.numeric(cut(HardSkillfreq_byState$percbystate, c(0, .05, .1, .15, .2, 
                                                                                          .5, 1)))


HardSkillfreq_byState2<- HardSkillfreq_byState %>%filter(Hard.Skill=="python")
colorsmatched <- HardSkillfreq_byState2$colorbuckets[match(state.fips$abb, HardSkillfreq_byState2$abb)]

map("state", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("Percent of Python in searches by State")
leg.txt <- c("<0-5%", "5-10%", "10-15%", "15-20%", "20-50%", "50%")
legend("bottom", leg.txt, horiz = TRUE, fill = colors)





HardSkillfreq_byState2<- HardSkillfreq_byState %>%filter(Hard.Skill=="r")
colorsmatched <- HardSkillfreq_byState2$colorbuckets[match(state.fips$abb, HardSkillfreq_byState2$abb)]

map("state", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("Percent of R in searches by State")
leg.txt <- c("<0-5%", "5-10%", "10-15%", "15-20%", "20-50%", "50%")
legend("bottom", leg.txt, horiz = TRUE, fill = colors)


HardSkillfreq_byState2<- HardSkillfreq_byState %>%filter(Hard.Skill=="sql")
colorsmatched <- HardSkillfreq_byState2$colorbuckets[match(state.fips$abb, HardSkillfreq_byState2$abb)]

map("state", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("Percent of SQL in searches by State")
leg.txt <- c("<0-5%", "5-10%", "10-15%", "15-20%", "20-50%", "50%")
legend("bottom", leg.txt, horiz = TRUE, fill = colors)

