
library(readr)
library(stringr)
library(dplyr)
library(tidyr)

#project scope
#look at player numeber 1 and player number 37 and delete all of the other players that they didnt play. player 1, only keep recotrs of his opponents. we neet to extract pre-performance rating. and divide them by the number of games played to get average rating of player # 1s ratings. 

tournamentinfo<-read_file("https://raw.githubusercontent.com/joshuargst/Data607Proj1/master/tournamentinfo.txt")
#extracting PreScore
Pre_Score<-unlist(str_extract_all(tournamentinfo, "R:\\s+[:digit:]{1,4}"))%>% str_extract_all("[:digit:]+")%>%unlist%>%as.numeric()

#extracting PostScore
Post_Score<-unlist(str_extract_all(tournamentinfo, "->\\s*[:digit:]{1,4}"))%>% str_extract_all("[:digit:]+")%>%unlist%>%as.numeric()

#extracting Total points of each player
Total_Points<-unlist(str_extract_all(tournamentinfo, "\\d+\\.\\d+"))%>%as.numeric()



tournamentinfo <- read_delim("https://raw.githubusercontent.com/joshuargst/Data607Proj1/master/tournamentinfo.txt", "|", escape_double = FALSE, trim_ws = TRUE, skip = 2)


tournamentinfo<-tournamentinfo[-grep("--+",tournamentinfo$Num),]

tournamentinfo1<-tournamentinfo[-grep("[[:alpha:]]",tournamentinfo$Num),]

#exctracting states
tournamentinfo2<-tournamentinfo[-grep("[[:digit:]]",tournamentinfo$Num),][1]
colnames(tournamentinfo2)[1]<-"Player_State"

  
WinsLosses<-separate(tournamentinfo1,'1',c("game_1","Game_1"), sep = "\\s+")%>%
  separate('2',c("game_2","Game_2"), sep = "\\s+")%>%
  separate('3',c("game_3","Game_3"), sep = "\\s+")%>%
  separate('4',c("game_4","Game_4"), sep = "\\s+")%>%
  separate('5',c("game_5","Game_5"), sep = "\\s+")%>%
  separate('6',c("game_6","Game_6"), sep = "\\s+")%>%
  separate('7',c("game_7","Game_7"), sep = "\\s+")

colnames(WinsLosses)[1]<-"PlayerID"
colnames(WinsLosses)[2]<-"PlayerName"
WinsLosses<-WinsLosses[c(-3,-18)]


PlayerInfo<-cbind.data.frame(WinsLosses[c(1,2)],tournamentinfo2[1],Pre_Score,Post_Score,Total_Points)

WinsLosses<-gather(WinsLosses, Game,W_L_D, -Game_1,-Game_2,-Game_3,-Game_4,-Game_5,-Game_6,-Game_7,-PlayerName,-PlayerID)[c(1,2,10,11)]


WinsLosses2<-separate(tournamentinfo1,'1',c("game_1","Game_1"), sep = "\\s+")%>%
  separate('2',c("game_2","Game_2"), sep = "\\s+")%>%
  separate('3',c("game_3","Game_3"), sep = "\\s+")%>%
  separate('4',c("game_4","Game_4"), sep = "\\s+")%>%
  separate('5',c("game_5","Game_5"), sep = "\\s+")%>%
  separate('6',c("game_6","Game_6"), sep = "\\s+")%>%
  separate('7',c("game_7","Game_7"), sep = "\\s+")

colnames(WinsLosses2)[1]<-"PlayerID"
colnames(WinsLosses2)[2]<-"PlayerName"
WinsLosses2<-WinsLosses2[c(-3,-18)]


WinsLosses2<-gather(WinsLosses2, Game2,OpponentID, -game_1,-game_2,-game_3,-game_4,-game_5,-game_6,-game_7,-PlayerName,-PlayerID)[c(1,2,10,11)]

WinsLosses<-cbind(WinsLosses,WinsLosses2)[c(1,2,3,4,8)]

WinsLosses$Game<-str_replace_all(WinsLosses$Game,"game_","")

WinsLosses$Game<-str_replace_all(WinsLosses$Game,"Game_","")


#summarise opponent scores


FinalTable<-
  left_join(WinsLosses,PlayerInfo, by = c("OpponentID"="PlayerID"))%>%  
  group_by(PlayerID)%>%
  summarise(Avg_Opponent_Score = round(mean(Pre_Score,na.rm = TRUE),0))%>%
  left_join(PlayerInfo,by = c("PlayerID"= "PlayerID"))

#reorder table for cleanliness
FinalTable<-FinalTable[c(1,3:7,2)]
library(tidyverse)
FinalTable
ggplot(FinalTable, aes(x = PlayerName))+geom_col(aes(y=Pre_Score))+
  theme(axis.text.x = element_text(angle=90, hjust = 1))
