library(tidyverse)
library(readxl)
library(xlsx)
library(reshape2)
library(lubridate)
library(RColorBrewer)
library(scales)
library(tidymodels)
library(Hmisc)
library(skimr)
library(ggcorrplot)
library(rpart)
library(rpart.plot)
library(vip)
library(data.table)
library(zoo)
library(tidyselect)

{
  
  
  rm(list = ls())
  
  # Script Start ------------------------------------------------------------
  
  wd<-"C:\\Users\\registejh\\OneDrive - CDM Smith\\Documents\\R_Directory\\PR_Analysis\\"
  DirList<-list(wd,paste0(wd,"data\\raw\\"),paste0(wd,"outputs\\"),paste0(wd,"data\\interim\\"))
  rm(wd)
  
  setwd(paste0(DirList[3]))
  
  
  #daybufferTRC <- read_excel("3daybufferTRC_52.xlsx", 
  #                            col_types = c("skip", "date", "text", "numeric"))
  #daybufferTRC<- daybufferTRC[!duplicated(daybufferTRC$Date),]
  
  #extract DMR sheets to lists
  path <- paste0(DirList[2],"CombinedDataSetsTrimmed.xlsx")
  sheetnames <- excel_sheets(path)
  mylist <- lapply(excel_sheets(path), read_excel, path = path)
  # name the dataframes
  names(mylist) <- sheetnames
  rm(path,sheetnames)
  

  #For DMR Data
  {
    #bind all items in list together
    PR_Long<-mylist %>% reduce(left_join, by = "Date")
    
    
    #gather all numeric Data
    PR_N<-gather(select(PR_Long,Date,which(sapply(PR_Long,class)=="numeric")),Parameter,VALUE,-Date)
    
    PR_C<-gather(select(PR_Long,Date,which(sapply(PR_Long,class)=="character"),which(sapply(PR_Long,class)=="logical")),Parameter,VALUE,-Date)
    
    PR_N<-filter(PR_N, is.na(VALUE)==FALSE)
    PR_C<-filter(PR_C, is.na(VALUE)==FALSE)
    
    
    
    
    #lubridate data
    
    getSeason <- function(DATES) {
      WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
      SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
      SS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
      FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox
      
      # Convert dates from any year to 2012 dates
      d <- as.Date(strftime(DATES, format="2012-%m-%d"))
      
      ifelse (d >= WS | d < SE, "Winter",
              ifelse (d >= SE & d < SS, "Spring",
                      ifelse (d >= SS & d < FE, "Summer", "Fall")))
    }
    
    PR_N$Date<-as_datetime(PR_N$Date)
    PR_N$MonthR<-lubridate::month(PR_N$Date, label = TRUE, abbr = FALSE)
    PR_N$Season<-getSeason(as.Date(PR_N$Date,format = "%Y-%m-%d"))
    PR_N$Year <- year(PR_N$Date)
    
    PR_C$Date<-as_datetime(PR_C$Date)
    PR_C$MonthR<-lubridate::month(PR_C$Date, label = TRUE, abbr = FALSE)
    PR_C$Season<-getSeason(as.Date(PR_C$Date,format = "%Y-%m-%d"))
    PR_C$Year <- year(PR_C$Date)
    
    
    #join xday buffer to TRC
    #PR_N<-left_join(PR_N,daybufferTRC,by = "Date")
    
    
    #determine percentile by parameter
    PR_N<-(PR_N %>% 
             group_by(Parameter) %>%
             mutate(VALUERank = percent_rank(VALUE)))
    
    
    PR_N_Unique<-unique(PR_N$Parameter)
    
    #select percentile value to be greater than or less than a particular threshold
    #PR_N$CorrDates<-ifelse(PR_N$CorrTest=="TRUE"& PR_N$VALUERank>=0.75, as.Date(PR_N$Date),NA)
    #class(PR_N$CorrDates)<-'Date'
    #PR_N$CorrDates<-as.POSIXct(as.Date(PR_N$CorrDates))
    
    
    #geom_vline(aes(xintercept=Date),data = exceedancedates,alpha = 0.3)
    exceedancedates<-filter(PR_N,Parameter == "Chlorine_ResidualEffluent__mgperL_Daily_Average" & VALUE>.52)
    
    #reorder with chlorine first
    PR_N_Unique<-PR_N_Unique[c(20,1:19,21:length(PR_N_Unique))]
    
    PR_N<-filter(PR_N,Year>=2015)
    
    #pop = period of performance, dechlor = when chlorination system came online
    PoP<-as.Date(c("11/01/2015","11/01/2016"),"%m/%d/%Y")
    Dechlor<-as.Date(c("03/01/2016"),"%m/%d/%Y")
  }

}



Datasetspreader<-function(Data){
  pivot_wider(Data,
              id_cols = c('Date','Parameter','VALUE'), 
              names_from = Parameter, 
              values_from = VALUE, 
              values_fn = list(VALUE = mean))
  
}
PR_N<-filter(PR_N,VALUE>=0)
Data<-Datasetspreader(PR_N)

# Bivariate Analysis ------------------------------------------------------



library(Hmisc)
Stat_Matrix<-function(Data,statmethod){
  #get all the numeric data out for meatrix
  Data<-select(Data,which(sapply(Data,class)=="numeric"))
  cor_data<- rcorr(as.matrix(Data),type = statmethod)
  M <- data.frame(cor_data$r)
  #p to designate significant correlations
  p_mat <- data.frame(cor_data$P)
  data.frame(row.names(M),
             M$Chlorine_ResidualEffluent__mgperL_Daily_Average,
             p_mat$Chlorine_ResidualEffluent__mgperL_Daily_Average)
}
Stat_Matrix3<-function(Data,statmethod){
  #get all the numeric data out for meatrix
  Data<-select(Data,which(sapply(Data,class)=="numeric"))
  cor_data<- rcorr(as.matrix(Data),type = statmethod)
  M <- data.frame(cor_data$r)
  #p to designate significant correlations
  p_mat <- data.frame(cor_data$P)
  data.frame(row.names(M),
             M$Reported.Effuent.TRC,
             p_mat$Reported.Effuent.TRC)
}

#All data
Spear_Correlations<-data.frame()
Spear_Correlationstemp<-Stat_Matrix(Data,'spearman')
Spear_Correlationstemp$set<-'PR_N6'
Spear_Correlations<-bind_rows(Spear_Correlations,Spear_Correlationstemp)
write.csv(Spear_Correlations, paste0(DirList[[3]],"Spearman.csv"))

#filtered dataset
#for PRN 1,3 and 6
Spear_Correlations<-data.frame()
Data<-Datasetspreader(filter(PR_N6, Date>=as.Date(Dechlor)))
Spear_Correlationstemp<-Stat_Matrix(Data,'spearman')
Spear_Correlationstemp$set<-'PR_N6'
Spear_Correlations<-bind_rows(Spear_Correlations,Spear_Correlationstemp)
write.csv(Spear_Correlations, paste0(DirList[[3]],"Spearman_AfterUpgrade.csv"))


library(outliers)
outlier(Data$Chlorine_ResidualEffluent__mgperL_Daily_Average, opposite = T)
boxplot(Data$Chlorine_ResidualEffluent__mgperL_Daily_Average)
boxplot(Data[          
  scores(Data$Chlorine_ResidualEffluent__mgperL_Daily_Average,type = "z", prob =pnorm(3))
  ,]$Chlorine_ResidualEffluent__mgperL_Daily_Average)

summary(Data[          
  scores(Data$Chlorine_ResidualEffluent__mgperL_Daily_Average,type = "z", prob =pnorm(3))
  ,]$Chlorine_ResidualEffluent__mgperL_Daily_Average)


rm.outlier(Data[2:3])
Data.test<-apply(Data[2:148], 2, rm.outlier, fill=T, median=T) %>% as.data.frame()

summary(Data.test$Chlorine_ResidualEffluent__mgperL_Daily_Average)


# Data Model Processing -----------------------------------------------------


#starting by taking all data and creating binary problem with exceedance and not exceedance
#

Data$Chlorineexceedance<-if_else(Data$Chlorine_ResidualEffluent__mgperL_Daily_Average>0.52,"Exceedance","Non_Exceedance") 



# For numerical models, will use this as basis for predicting chlorine residual
#Data_subQ<-Data[c(2,5,8,10,13,15,20,21)]
#colnames(Data_subQ)[colnames(Data_subQ) == 'Chlorine_ResidualEffluent__mgperL_Daily_Average'] <- 'Chlorineexceedance'
#classification
#Data_sub<-Data[c(2,5,8,10,13,15,20,149)]
removed_Params<-c("Volume_of_Sewage_TreatedInst_MaxMGD","Volume_of_Sewage_TreatedInst_MinMGD",
                  "Volume_of_Sewage_TreatedDry_FlowMGD","Temperature__C_EffluentMaximum","Temperature__C_InfluentMinimum",
                  "Temperature__C_EffluentMinimum","Settleable_Solids__mlperL_InfluentAverage",
                  "CBOD5__mgperL_Effluent24_hour_comp","Orthophosphate__mgperL_Effluent24_hour_comp",
                  "Orthophosphate__mgperL_Influent24_hour_comp",
                  "DAY","SUSPENDED_SOLIDS__mgperl_Raw_SewageTOTAL","SUSPENDED_SOLIDS_PRIMTANKEFF_mgperL",
                  "SUSPENDED_SOLIDS_THICKEFF","5CBODPLANTEFFLUENTTOTAL","FINALpHMIN","CHL_RES_AT_TIME_OF_FECAL_SAMPLE",
                  "FECAL_COLIFORMNATURALLOG","SEWAGE_TREATEDRATE__MGD_AER","LOADDRYTONS","GALLONS_FORDisinfection_onlyGALperMG",
                  "GALLONS_FORDisinfectionTOTAL","PercentREM_OVAL","SUSPENDED_SOLIDSDRYTONSREMOVED","RAWSSxFLOW",
                  "RAWBODxFLOW","RETURNSLUDGE","LOADINGDRY_LBS","PRIMARY_EFFLUENT__mgperl_NITROGENNO3",
                  "PRIMARY_EFFLUENT__mgperl_NITROGENTKN","PRIMARY_EFFLUENT__mgperl_NITROGENNO2",
                  "PRIMARY_EFFLUENT__mgperl_NITROGENNH3","TSS_InfLoadinglbperd","TSS_EFFLoadinglbperd",
                  "TSSpercentRemoval","CBOD_InfLoadinglbperd","CBODlbremoved","CBOD_EffLoadinglbperd",
                  "PSTpercentRemoval","Pri_Eff_TSSLoadinglbperd","Total_WasteML___Eff___RASwaslbperday",
                  "WAS_Sludge__Aer_eff_loadinglbperd","AT_SOLIDSMLSSlbs","TargetMLSS_lbs6point5","PST_InfTSS_loading",
                  "pHRAWMAX","PLANTpHMAX","Primary_SludgeLoadinglbperday","Prim_eff_flowmgd",
                  "Prim_Effloadinglbperd","pHRAWMIN","PLANTpHMIN","CHLORINE_RESIDUALMAX","PLANTTEMPCAvg",
                  "TotalAmountStored8_AM","TotalAmountTruck_Deliveries__gallons_Tk._1",
                  "TotalAmountTruck_Deliveries__gallons_Tk._3","TotalAmountTruck_Deliveries__gallons_Total",
                  "TotalAmountTruck_Deliveries__gallons_Tk._2","WAS_Sludge__RAS_LOADINGLBperD",
                  "Polymer_Use_lb","ResidualperTarget","DT__hrs_VolperFlow","New_TRC_Limit","Effluent_Target",
                  "Year","Month","Day","Flow","SLUDGE_DENSITYINDEXNo1","SLUDGE_DENSITYINDEXNo3","SLUDGE_DENSITYINDEXNo2",
                  "SLUDGE_DENSITYINDEXNo4","SDIAvg","GPH","SEWAGE_pHRAWMIN","TSS_effconcmgperL","AT_Effloadinglbperd",
                  "Aer_EffTSSmgperL","SNWD","FINALTANKSinService","AerationTanksInService","PrimaryTanksInService",
                  "TANKS_IN_SERVICEPRI","TANKS_IN_SERVICEAER",
                  "CHLORINE_RESIDUALSMPL","gal_per_CCTgalpermg","RETURN_SLUDGEPercentSEWAGEFLOW","CBODpercentRemoval",
                  "5CBODPRIMARYEFFLUENTTOTAL","5CBODRAWSEWAGETOTAL","Chlorine_ResidualEffluent__mgperL_Daily_Average",
                  "CYLINDERSAVERAGEmLperL","DETENTION_PERIOD___HRSFINALTANKS","DETENTION_PERIOD___HRSAERTANKS",
                  "DETENTION_PERIOD___HRSPRITANKS","Temperature__C_InfluentMaximum",'SNOW','ChloridesInfluentmgperL',
                  "SUSPENDED_SOLIDSPRI_EFFTONSLOAD","Avg_AerTSSmgperL","SludgeAgedays","WasteAct_SludgeWAS_lbsperday",
                  "TANKS_IN_SERVICEFINAL","Prim_Eff_TSSavgmgperL","SUSPENDED_SOLIDS_RETURNSLUDGETOTAL_mgperL",
                  "Primary_EffCBODmgperL","Total_Suspended_Solids__mgperL_Influent24_hour_comp")
#removed_Params<- append(removed_Params,"Disinfection_Dose")
Data_sub<-Data %>% 
  filter(Date>=as.Date(Dechlor)) %>% 
  select(-all_of(removed_Params))
Data_sub<-Data_sub[is.na(Data_sub$Chlorineexceedance)==F,] 
sort(colnames(Data_sub))

Data_sub_long<- Data_sub %>% pivot_longer(cols = c(-Date),names_to = "Parameter", values_to = "Value", values_drop_na = T)
renamers<-read_excel(paste0(DirList[[3]],"columnnames.xlsx"))
names(Data_sub)<-renamers$Variable[match(names(Data_sub),renamers$Parameter)]



#Data_sub<-Data %>% 
#  select(Chlorineexceedance,SludgeAgedays,PLANTBODxFLOW,Total_Phosphorous___mgperL_Effluent24_hour_comp,WasteAct_SludgeWAS_lbsperday) 
#sort(colnames(Data_sub))


#names(Data_sub)<-str_replace_all(names(Data_sub), c("%"="percent","\\."="_","5"=""))
Data_sub<- Data_sub %>% mutate_if(is.character,factor)
Data_sub<- Data_sub %>% mutate_if(is.numeric,log(.))
#setting up test/train/split


# Checking Variable Importance --------------------------------------------

type = "tree"
i <-1
topn=10
variableimp_function <- function(seedstart,seedend,type,topn){
  VimpF<-data.frame()
  for (i in seq(seedstart,seedend,1)){
  set.seed(i)
  PR_Split<-Data_sub %>% initial_split(strata = Chlorineexceedance,prop = 3/4)
  PR_Train<- training(PR_Split)
  
  #setting up predictor recipe
  #to figure out
  
  #data processing and feature engineering
  PR_Recipe<- recipe(Chlorineexceedance ~ .,data=PR_Train) %>%
    step_zv(all_numeric()) %>%
    step_meanimpute(all_numeric()) %>% #mean imputation returns most probable results
    step_downsample(Chlorineexceedance) %>% 
    #step_upsample(Chlorineexceedance) %>% 
    #step_dummy(all_nominal(),-all_outcomes()) %>% # not needed without any character predictor variables
    #step_normalize(all_numeric()) %>% # very important for k-nearest neighbors, normalizes and centers our data
    prep()  
  
  if (type == "tree"){
    tree_spec<-decision_tree() %>% 
      set_engine("rpart") %>% 
      set_mode('classification')
    
    tree_fit<-tree_spec %>% 
      fit(Chlorineexceedance~.,
          data= juice(PR_Recipe))
    

    VimpF<- bind_rows(VimpF, rownames_to_column(data.frame(MeanDecreaseGini=tree_fit$fit$variable.importance),"Variable"))
      
    Vimpagg<-aggregate(MeanDecreaseGini~Variable,FUN='mean',data=VimpF)

}else if (type =="rf"){
  rf_spec<-rand_forest() %>% 
    set_engine("randomForest", importance = TRUE) %>% 
    set_mode('classification')
  
  rf_fit<-rf_spec %>% 
    fit(Chlorineexceedance~.,
        data= juice(PR_Recipe))

  VimpF<- bind_rows(VimpF, rownames_to_column(data.frame(rf_fit$fit$importance),"Variable")) 
  
  Vimpagg<-aggregate(MeanDecreaseGini~Variable,FUN='mean',data=VimpF)
    
  }else if (type =="logit"){
    logit_spec<-logistic_reg() %>% 
      set_engine("glm") %>% 
      set_mode('classification')
    
    logit_fit<-logit_spec %>% 
      fit(Chlorineexceedance~.,
          data= juice(PR_Recipe))

    VimpF<- bind_rows(VimpF, rownames_to_column(data.frame(caret::varImp(logit_fit$fit)),"Variable"))

    Vimpagg<-aggregate(Overall~Variable,FUN='mean',data=VimpF) 
    
    names(Vimpagg)[2]<-paste("MeanDecreaseGini")
  }
  }
  if (type == "tree"){
    charttitle= "SVM Decision Tree \nVariable Importance"
    
  }else if (type =="rf"){
    charttitle= "Random Forest \nVariable Importance"
    
  }else if (type =="logit"){
    charttitle= "Logistic Regression \nVariable Importance"
  } else {
    print("nothing")
  }
  
  Vimpagg<-Vimpagg %>% top_n(MeanDecreaseGini, n = topn) %>% 
    mutate(Relativedecrease= MeanDecreaseGini/sum(MeanDecreaseGini))
  Vimpagg %>% 
    ggplot(mapping = aes(fct_reorder(Variable,Relativedecrease), Relativedecrease))+
    geom_col(aes(fill = Relativedecrease),alpha = 0.5)+
    theme_bw()+
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
  #        axis.text.x = element_text(angle = 90),
          legend.position = 'none')+
    scale_fill_gradient2(low = "white", 
                         mid = "skyblue3",
                          high = "darkblue",
                         midpoint= quantile(top_n(Vimpagg,Relativedecrease, n = topn)$Relativedecrease,.5))+
    coord_flip()+
    labs(title = charttitle,
         x= "",
         y="Relative Variable Importance")

}

topn=10
variableimp_function(1,15,"tree",20)
ggsave(paste0(DirList[[3]],"treeimportance3.png"),width = 3.75,height =5)
variableimp_function(1,15,"rf",20)
ggsave(paste0(DirList[[3]],"rfimportance3.png"),width = 3.75,height = 5)
variableimp_function(1,15,"logit",20)
ggsave(paste0(DirList[[3]],"logitimportance3.png"),width = 4,height = 5.15)

#miscilaneaous plot, does not really belong in this R file
{
head(PR_Long)
match(PR_N,exceedancedates)
test<-data.frame(Date = PR_Long$Date)
test$Exceeds<- ifelse(test$Date %in% exceedancedates$Date,"Y","N")
table(test$Exceeds)
view(PR_N_Unique)
PR_Long$exceeds<-ifelse(PR_Long$Date %in% exceedancedates$Date,"Y","N")
PR_Long %>% filter(Date>=Dechlor) %>% 
  ggplot(aes(x = Date,y = PLANTpHMAX,color = exceeds))+
  geom_point(alpha = 0.3, size = 2)+geom_smooth(alpha = 0.1)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
}
# Model Performance Setup -------------------------------------------------

#841, 82 for others,39 for tree,40
#2,9 58 71
set.seed(82)
PR_Split<-Data_sub %>% initial_split(strata = Chlorineexceedance,prop = 0.75)
PR_Train<- training(PR_Split)
PR_test<- testing(PR_Split)

#setting up predictor recipe
#to figure out

#data processing and feature engineering
PR_Recipe<- recipe(Chlorineexceedance ~ .,data=PR_Train) %>%
  step_zv(all_numeric()) %>%
  step_meanimpute(all_numeric()) %>% #mean imputation returns most probable results
  #step_upsample(Chlorineexceedance) %>% 
  #step_downsample(Chlorineexceedance,under_ratio = 1) %>% 
  #step_dummy(all_nominal(),-all_outcomes()) %>% # not needed without any character predictor variables
  #step_normalize(all_numeric()) %>% # very important for k-nearest neighbors, normalizes and centers our data
  prep()  
#step_downsample
#?step_upsample
test_proc<-bake(PR_Recipe,new_data = PR_test)

#used to acctually select our data
juice(PR_Recipe) %>% count(Chlorineexceedance)


#prop.table(table(Data_sub$Chlorineexceedance))
#table(Data_sub$Chlorineexceedance)
validation_splits<-mc_cv(juice(PR_Recipe),prop =  0.9,strata = Chlorineexceedance)

# Decision Tree -----------------------------------------------------------

#?decision_tree
#?rpart.plot
tree_spec<-decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode('classification')

tree_fit<-tree_spec %>% 
  fit(Chlorineexceedance~.,
      data= juice(PR_Recipe))

png(paste0("9singletreer_sub.png"), width = 1800, height = 900)
rpart.plot(tree_fit$fit, yesno = 2, type = 5,extra = 108,tweak = 2.5, shadow.col = "lightgrey",roundint = FALSE)
dev.off()
vip(tree_fit,
    fill = "skyblue", 
    alpha = 0.7)+theme_bw()+
  labs(title = "Decision Tree Variable Importance")
#evaluate tree model


tree_res<-fit_resamples(
  tree_spec,
  PR_Recipe,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

tree_res %>% 
  collect_metrics()


tree_res %>%
  unnest(.predictions) %>% 
  conf_mat(Chlorineexceedance,.pred_class) %>% autoplot()
  


test_proc

tree_fit %>% 
  predict(new_data=test_proc,type='prob') %>% 
  mutate(truth=PR_test$Chlorineexceedance) %>% 
  roc_auc(truth,.pred_Exceedance)
  

#write.csv(names(Data),paste0(DirList[[3]],"Columnnames.csv"))
#write.csv(removed_Params,paste0(DirList[[3]],"selectedcolumns.csv"))
#png(paste0("singletreer_sub.png"), width = 1800, height = 900)
#rpart.plot(tree_fit$fit, yesno = 2, type = 5,extra = 108,tweak = 1, shadow.col = "lightgrey",roundint = FALSE)
#dev.off()



# Random Forest Model -----------------------------------------------------

rf_spec<-rand_forest() %>%
  #set_engine(engine = 'ranger', importance="impurity") %>% 
  set_engine(engine = 'randomForest',
             importance=TRUE,
             #ntree=500,
             #mtry=tune(),
             strata= Chlorineexceedance) %>% 
  set_mode('classification')


rf_fit<-rf_spec %>% 
  fit(Chlorineexceedance~.,
      data=juice(PR_Recipe))

oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf_fit$fit$err.rate), times=3),
  Type=rep(c("OOB", "Exceedance", "Non_Exceedance"), each=nrow(rf_fit$fit$err.rate)),
  Error=c(rf_fit$fit$err.rate[,"OOB"],
          rf_fit$fit$err.rate[,"Exceedance"],
          rf_fit$fit$err.rate[,"Non_Exceedance"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

vip(rf_fit,
    fill = "skyblue", 
    alpha = 0.7)+theme_bw()+
  labs(title = "Random Forest Variable Importance")

rf_res<-fit_resamples(
  rf_spec,
  PR_Recipe,
  validation_splits,
  control = control_resamples(save_pred = TRUE))

rf_res %>% 
  collect_metrics()

rf_res %>%
  unnest(.predictions) %>% 
  conf_mat(Chlorineexceedance,.pred_class) %>% autoplot()

rf_fit %>% 
  predict(new_data=test_proc,type='prob') %>% 
  mutate(truth=PR_test$Chlorineexceedance) %>% 
  roc_auc(truth,.pred_Exceedance)


# K Nearest Neighbors -----------------------------------------------------


knn_spec<-nearest_neighbor() %>% 
  set_engine(engine="kknn") %>% 
  set_mode(mode = "classification")

knn_fit<-knn_spec %>% 
  fit(Chlorineexceedance~.,
      data= juice(PR_Recipe))

#evaluate KNN Model

knn_res<-fit_resamples(
  knn_spec,
  PR_Recipe,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

knn_res %>% 
  collect_metrics()

knn_res %>%
  unnest(.predictions) %>% 
  conf_mat(Chlorineexceedance,.pred_class) %>% autoplot()


knn_fit %>% 
  predict(new_data=test_proc,type='prob') %>% 
  mutate(truth=PR_test$Chlorineexceedance) %>% 
  roc_auc(truth,.pred_Exceedance)


# Logistic Regression Model -----------------------------------------------


logit_spec<-logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")
Data$Chlorineexceedance<- as.factor(Data$Chlorineexceedance)

forlogitcurve<-glm(Chlorineexceedance~`Disinfection Dose`, data = juice(PR_Recipe), family = 'binomial')

#library(binaryLogic)


juice(PR_Recipe) %>% #colnames
  mutate(probss=predict(forlogitcurve,juice(PR_Recipe), type = "response")) %>% 
  ggplot(aes(y = 1-probss,x =`Disinfection Dose`))+
  #geom_point(aes(y=mutate(juice(PR_Recipe),Chlorineexceedance=ifelse(Chlorineexceedance=='Exceedance',1,0))$Chlorineexceedance,color = Chlorineexceedance) )+
  #geom_line(size = 2, color = "blue", alpha = .5)+
  stat_smooth(method="glm",
              method.args =  list(family=binomial),
              alpha = 0.25)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'grey80',linetype = 2),
        #panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")+
  coord_cartesian(ylim =c(0,1))+
  labs(x = "Disinfection Dose (mg/L)",
       y = "Probability of Exceedance" )+
  scale_y_continuous(labels= scales::percent_format(accuracy = 1))
  scale_x_continuous(breaks=seq(0,6,1))

  ggsave(paste0(DirList[[3]],"Logisticregplot7.png"), width = 6, height = 3)
{
rssummary<-data.frame()
for (i in 1:10000){
  c<-c(seq(0,1, by = .01))
  
  rs<- sample(c,1000, replace = TRUE)

  rssummary<-bind_rows(rssummary,data.frame(mean = mean(rs),stdev= sd(rs)))  
}
  }
  100
  
  hist(rssummary$mean)
  hist(rssummary$stdev)
  median(rssummary$stdev)
  0.291/(sqrt(96))*1.96

  test_proc %>% 
  mutate(probss=predict(forlogitcurve,test_proc, type = "response")) %>% 
  ggplot(aes(y = 1-probss,x = Disinfection_Dose))+
  geom_point(aes(y=mutate(test_proc,Chlorineexceedance=ifelse(Chlorineexceedance=='Exceedance',1,0))$Chlorineexceedance,color = Chlorineexceedance) )+
  #geom_line(size = 2, color = "blue", alpha = .5)+
  stat_smooth(method="glm",
              method.args =  list(family=binomial),
              alpha = 0.25)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank())+
  coord_cartesian(ylim =c(0,1))+
  labs(x = "Disinfection Dose (mg/L)",
       y = "Probability of Exceedance" )+
  scale_x_continuous(breaks=seq(0,6,1))

logit_fit<-logit_spec %>% 
  fit(Chlorineexceedance~.,
               data=juice(PR_Recipe))

vip(logit_fit,
    fill = "skyblue", 
    alpha = 0.7)+theme_bw()+
  labs(title = "Logistic Regression Variable Importance")

logit_res<-fit_resamples(
  logit_spec,
  PR_Recipe,
  validation_splits,
  control= control_resamples(save_pred =  TRUE))


logit_res %>% 
  collect_metrics()

logit_res %>%
  unnest(.predictions) %>% 
  conf_mat(Chlorineexceedance,.pred_class) %>% autoplot()

logit_fit %>% 
  predict(new_data=test_proc,type='prob') %>% 
  mutate(truth=PR_test$Chlorineexceedance) %>% 
  roc_auc(truth,.pred_Exceedance)


  
# Compare accross models --------------------------------------------------

knn_res %>% 
  collect_metrics() %>% 
  mutate(model = 'K-Nearest Neighbors') %>% 
  bind_rows(tree_res %>% 
              collect_metrics() %>% 
              mutate(model = 'SVM Decision Tree'),
            rf_res %>% 
              collect_metrics() %>% 
              mutate(model = 'Random Forest'),
            logit_res %>% 
              collect_metrics() %>% 
              mutate(model = 'Logistic Regression'))

knn_res %>%
  unnest(.predictions) %>% 
  mutate(Model ="K-Nearest Neighbors") %>%
  bind_rows(tree_res %>%
              unnest(.predictions) %>% 
              mutate(Model ="SVM Decision Tree"),
            rf_res %>%
              unnest(.predictions) %>% 
              mutate(Model ="Random Forest"),
            logit_res %>%
              unnest(.predictions) %>% 
              mutate(Model ="Logistic Regression")) %>% 
  group_by(Model) %>% 
  roc_curve(Chlorineexceedance,.pred_Exceedance) %>% 
  autoplot()

#knn_res %>%
 #unnest(.predictions) %>% 
  #mutate(Model ="K-Nearest Neighbors") %>%
  bind_rows(tree_res %>%
              unnest(.predictions) %>% 
              mutate(Model ="SVM Decision Tree"),
            rf_res %>%
              unnest(.predictions) %>% 
              mutate(Model ="Random Forest"),
            logit_res %>%
              unnest(.predictions) %>% 
              mutate(Model ="Logistic Regression")) %>% 
  group_by(Model) %>% 
  roc_curve(Chlorineexceedance,.pred_Exceedance) %>% 
  autoplot() + theme(panel.border = element_blank(),
                     panel.grid = element_blank(),
                     legend.position = c(0.8, 0.2),
                     legend.title = element_blank())+
    labs(title="Model Comparison ROC Curve")
  
  ggsave(paste0(DirList[[3]],"Model_Performance2.png"),width=5,height = 4)

# Linear Regression -----------------------------------------------------



  #settingup model specification

lm_spec<- linear_reg() %>% 
  set_engine(engine ="lm")

lm_fit<-lm_spec %>% 
  fit(Chlorineexceedance ~.,
      data = PR_Train)


lm_fit

#model specifications for randomforest

rf_spec<-rand_forest(mode= 'regression') %>% 
  set_engine("ranger")

rf_fit<-rf_spec %>% 
  fit(Chlorineexceedance ~.,
      data = PR_Train)

rf_fit

#evaluating model


results_train<-lm_fit %>% 
  predict(new_data = PR_Train) %>% 
  mutate(truth=PR_Train$Chlorineexceedance,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data=PR_Train) %>% 
              mutate(truth=PR_Train$Chlorineexceedance,
                     model="rf"))


results_test<- lm_fit %>% 
  predict(new_data = PR_test) %>% 
  mutate(truth=PR_test$Chlorineexceedance,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data=PR_test) %>% 
              mutate(truth=PR_test$Chlorineexceedance,
                     model="rf"))



results_train %>% 
  group_by(model) %>% 
  rmse(truth=truth, estimate=.pred)


results_test %>% 
  group_by(model) %>% 
  rmse(truth=truth, estimate=.pred)


results_test %>% 
  mutate(train="testing") %>% 
  bind_rows(results_train %>% 
              mutate(train="training")) %>% 
  ggplot(aes(truth,.pred,color = model))+
  geom_abline(lty = 2, color = "gray80")+
  geom_point(alpha = 0.5)+
  facet_wrap(~train)

#resampling due to overfitting

set.seed(1234)

PR_folds<-vfold_cv(PR_Train,strata= Chlorineexceedance)

rf_results<- fit_resamples(rf_spec,
                           Chlorineexceedance ~.,
                           resamples = PR_folds,
                           control = control_resamples(save_pred = TRUE)
                           )

rf_results %>% 
  collect_metrics()

rf_results %>% unnest(.predictions) %>% 
  ggplot(aes(Chlorineexceedance,.pred,color =id))+
  geom_abline(lty=2, color = "gray80",size = 1.5)+
  geom_point(alpha = 0.5)



# Logistic Regression -----------------------------------------------------
#library(caTools)
library(ROCR)
#oversampling
#creating binary classification
Data$Chlorineexceedance<-if_else(Data$Chlorine_ResidualEffluent__mgperL_Daily_Average>0.52,1,0)
#extracting only relevant data
Logitdata<-Data[c(2,5,8,10,13,15,20,149)]
Logitdata<-Logitdata[is.na(Logitdata$Chlorineexceedance)==F,]
Logitdata<-as.data.frame(apply(Logitdata,2,impute ))

Logitdata$Chlorineexceedance<-as.factor(Logitdata$Chlorineexceedance)
summary(Logitdata$Chlorineexceedance)

#Logitdata$id<-1:nrow(Logitdata)
set.seed(1)
(modeller<-paste0("Chlorineexceedance ~ ",paste0(colnames(Logitdata)[-c(ncol(Logitdata),ncol(Logitdata)-1)],sep = "", collapse=' + ')))
(modeller<-paste0("Chlorineexceedance ~."))


set.seed(2)

logitsplit<-initial_split(Logitdata,prop=.75)
train<-training(logitsplit)
test<-testing(logitsplit)
table(train$Chlorineexceedance)
table(test$Chlorineexceedance)



modeller<- recipe(Chlorineexceedance ~.,data=train) %>% 
  step_medianimpute(~.) %>% prep() %>% bake()

bake(modeller)
PR_Forest<-rand_forest(mode="classification", mtry = 3,trees = 100) %>% 
  set_engine("ranger") %>% 
  fit(modeller)


train2<-bind_rows(sample(train[train$Chlorineexceedance==1,],100, replace = TRUE),sample(train[train$Chlorineexceedance==0,],100, replace = TRUE))
test2<-bind_rows(sample(test[test$Chlorineexceedance==1,],100, replace = TRUE),sample(test[test$Chlorineexceedance==0,],100, replace = TRUE))



train2<-rbind(train[train$Chlorineexceedance==1,][sample(c(1:ncol(train[train$Chlorineexceedance==1,])),size = 100,replace=TRUE),],
              train[train$Chlorineexceedance==0,][sample(c(1:ncol(train[train$Chlorineexceedance==0,])),size = 100,replace=TRUE),])

varImp()

test2<-rbind(test[test$Chlorineexceedance==1,][sample(c(1:ncol(test[test$Chlorineexceedance==1,])),size = 100,replace=TRUE),],
             test[test$Chlorineexceedance==0,][sample(c(1:ncol(test[test$Chlorineexceedance==0,])),size = 100,replace=TRUE),])
#colnames(Logitdata)[-c(ncol(Logitdata),ncol(Logitdata)-1)]
exceedance_logit_M<-glm(modeller,data=train, family = binomial(link='logit'))
exceedance_logit_M<-glm(modeller,data=train2, family = binomial(link='logit'))
#exceedance_logit_M<-glm(Chlorineexceedance ~.,data=train2, family = binomial(link='logit'))
summary(exceedance_logit_M)


predictTrain<-predict(exceedance_logit_M,type = 'response')

ROCRpred<-prediction(predictTrain,train2$Chlorineexceedance)

ROCRperf<-performance(ROCRpred,"tpr","fpr")

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


table(train2$Chlorineexceedance, predictTrain > 0.4)

predictTest <- predict(exceedance_logit_M, type = "response", newdata = test2)
?predict
table(test2$Chlorineexceedance, predictTest > 0.4)

prop.table(table(test2$Chlorineexceedance))
prop.table(table(Data$Chlorineexceedance))



class(test2$Chlorineexceedance)
as.integer(predictTest>.4) 
confusionMatrix(data=as.factor(as.integer(predictTest>.4)),reference= as.factor(test2$Chlorineexceedance))





# Misc Report Plots -------------------------------------------------------

#TRC vs time
TRC_Limit<-0.52
Data %>% 
  ggplot(mapping = aes(Date,Chlorine_ResidualEffluent__mgperL_Daily_Average))+
  geom_point(fill = "skyblue", pch = 21, color = "blue", alpha = .2,size = 2)+
  geom_hline(yintercept = TRC_Limit, linetype=2, color = "red", size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x = "Date",
       y = "TRC (mg/L)")+
  annotate(geom='text',x = as.POSIXct("2019/01/01"),y = 0.58, label = "TRC Limit", color = "red")
ggsave(paste0(DirList[[4]],"TRC.png"),width=6,height = 4)

rm.outlier(PR_N_Sub$Volume_of_Sewage_TreatedInst_MinMGD)
?outliers::scores

i<-4
z_score=3
parameterID=5

bivarplot<-function(parameterID,removeoutliers, z_score,saveit){
Val<-sort(unique(PR_N$Parameter))[c(parameterID,19)]

PR_N_Sub<-PR_N[,1:3] %>% ungroup() %>% 
  filter(Date>=as.Date(Dechlor)) %>% 
  filter(Parameter %in% Val) %>%
  pivot_wider(names_from = Parameter,values_from = VALUE, values_fn = list(VALUE=mean))

if (removeoutliers==T){

PR_N_Sub<- PR_N_Sub %>% 
  filter(is.na(PR_N_Sub[[3]])==F)
PR_N_Sub<-PR_N_Sub %>% 
  mutate(z1 = outliers::scores(PR_N_Sub[[2]],type = c("z")),
         z2 = outliers::scores(PR_N_Sub[[3]],type = c("z"))) %>% 
  filter(abs(z1)<=z_score & abs(z2)<=z_score)
}

labels<-paste(
  renamers$Variable[match(names(PR_N_Sub),renamers$Parameter)],
  renamers$Units[match(names(PR_N_Sub),renamers$Parameter)])


stest<-spearman.test(PR_N_Sub[[2]],PR_N_Sub[[3]])


M<-PR_N_Sub %>% 
  ggplot(aes_string(Val[1], Val[2]) )+
    geom_point(color = "skyblue2", alpha = .5)+
  geom_smooth(color = "darkblue", alpha = 0.3)+
  #scale_x_log10()+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())+
  labs(x = labels[3],
       y = labels[2])+
  annotate(geom = "text",
           label = paste0("Spearman R^2 = ",round(stest[[1]],3),
                          "\nP-Value = ",round(stest[[5]],3)),
           size = 3.5,hjust = 1,vjust = 2,
           x = quantile(PR_N_Sub[[3]],1,na.rm = T),
           y = quantile(PR_N_Sub[[2]],1,na.rm = T))
if (saveit==TRUE){
ggsave(paste0(DirList[[3]],Val[1],".png"),width = 6, height = 3)
}else{
M
}
}
PR_N_Unique
Dose
length(plots_of_Interest)
i<-4
i
for (i in 1:length(plots_of_Interest)){
bivarplot(plots_of_Interest[i],T,3,T)
}
Q
plots_of_Interest<-sort(c(35,28,65,138,69,145,99,104,94,87,93,65,68))


renamers<-read_excel(paste0(DirList[[3]],"columnnames.xlsx"))



exceedancedates[[1,1]]+86400
BufferedDates<-
exceedancedates[1] %>% 
  mutate(buffer = 0) #%>%
  bind_rows(data.frame(exceedancedates[1]-86400,buffer = -1),
            data.frame(exceedancedates[1]-86400*2,buffer = -2),
            data.frame(exceedancedates[1]-86400*3,buffer = -3))
  
  parameterID<-19
  Val<-sort(unique(PR_N$Parameter))[c(parameterID,19)]
  
  PR_N_Sub<-PR_N[,1:3] %>% ungroup() %>% 
    filter(Date>=as.Date(Dechlor)) %>% 
    filter(Parameter %in% Val) %>%
    filter(VALUE>=0) %>% 
    pivot_wider(names_from = Parameter,values_from = VALUE, values_fn = list(VALUE=mean)) 
  PR_N_Sub<- PR_N_Sub%>% 
    mutate(Exceedance=match(PR_N_Sub$Date,exceedancedates$Date)) %>% 
    mutate(Exceedance=ifelse(is.na(Exceedance),"False","True"))
  
  labels<-paste(
    renamers$Variable[match(names(PR_N_Sub),renamers$Parameter)],
    renamers$Units[match(names(PR_N_Sub),renamers$Parameter)])
  renamers$Variable[match(names(PR_N_Sub),renamers$Parameter)]
  
  PR_N_Sub %>% 
    ggplot(aes_string("Date", Val[1]) )+
    geom_point(alpha = .3,aes(color = Exceedance), size = 2)+
    #geom_smooth(color = "darkblue", alpha = 0.3)
    #scale_x_log10()+
    scale_color_manual(values =c(False = "skyblue",True="red" ))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_blank())+
    labs(y = labels[2])+
    geom_hline(yintercept = TRC_Limit, linetype=2, color = "red", size = 1)

  PeriodDefinition <- read_excel("C:/Users/registejh/OneDrive - CDM Smith/Documents/R_Directory/PR_Analysis/data/raw/PeriodDefinition.xlsx")
periodlist<-  
PeriodDefinition %>% 
  rowwise() %>% 
  do(data.frame(datelist = seq(.$MinDate,.$MaxDate,86400)))
periodlist %>% View
PR_N_Sub %>% 
  ggplot(aes_string("Date", y = Val[1]) )+
  geom_point(alpha = .3,aes(color = Exceedance), size = 2)+
  #geom_smooth(color = "darkblue", alpha = 0.3)
  #scale_x_log10()+
  scale_color_manual(values =c(False = "skyblue",True="red" ))+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = "NA"),
        legend.position = "top")+
    scale_y_continuous(breaks = seq(0,1.5,.25))+
    coord_cartesian(ylim = c(0,NA))+
  labs(y = labels[2])+
  geom_hline(yintercept = TRC_Limit, linetype=2, color = "red",size = 2)+
  annotate(geom='text',x = as.POSIXct("2016/06/01"),y = 0.54, label = "TRC Limit", color = "black")+
    geom_vline(xintercept = periodlist$datelist, color = "grey50",alpha = 0.2,size =.5)+
    labs(title = "Periods Requested for Additional Data",
         subtitle = "Grey bars indicate the additional periods in which PLC and Log Data was requested")

ggsave(paste0(DirList[[3]],"TRC vs Time Req.png"), width = 7,height = 4)
  #geom_vline(xintercept = BufferedDates$Date,alpha = 0.3, color = "orange3")



