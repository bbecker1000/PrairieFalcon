######################################################################################################
## Project: SFAN Falcon Occupancy Analysis
## Script Purpose: Code takes SFAN falcon data export from database, formats it for analysis and graphing
## Date: 08/07/2019
## Author: Wakamiya, Sarah
## R version 3.5.2 "Eggshell Igloo"
## Libraries and versions used: tidyverse, lubridate 
#####################################################################################################
##### Prepare workspace #####
#falcon_packages <- c("tidyverse", "lubridate")
#install.packages(falcon_packages)

library(tidyverse)
library(lubridate)
library(ggplot2)

##### Inspect Data Files #####
setwd("~/Desktop/Falcon2022/PrairieFalcon")
library(readxl)
data_csv <- read.csv('Data/qrpt_FalconObservations.csv') #1984-2019
data_2019 <- read_excel('Data/qrpt_FalconObservations_2019.xlsx') #2003 - 2021
data_2022 <- read_excel('Data/qrpt_RaptorObservations_2022.xlsx') #1984-2021

cols_2019 <- colnames(data_2019) 
cols_2022 <- colnames(data_2022) 
cols_csv <- colnames(data_csv)
## ID == Event_ID, cols_csv == cols_2019


###### Import dataset and format #######
#falcons<-read_csv("data/qrpt_FalconObservations.csv")
#falcons<-read_csv("data/qrpt_FalconObservations.csv",
#    guess_max = nrow(falcons)) # - uses all records to guess field type
falcons <- data_csv
falcons<- as_tibble(falcons)

#fix date
falcons <- falcons %>%
  mutate(Start_Date=mdy_hms(Start_Date))

# Use to filter out casual/unknown, survey areas, and inadequate/unknown surveys
# Only use data from 2003 for occupancy analysis. Data prior to 2003 is presence only (presence of any species not just falcons)
falcons<- falcons %>%
  filter(Year>2002 
         & !Survey_Purpose %in% c("Casual", "Unknown") 
         & Territory_Type=="Territory"
         & str_detect(Detection_Result,"^Yes:"))

AT <- falcons %>% group_by(Territory_Name, Area_Type) %>% count() %>% select(-n)
#Separate prairie and peregrine falcon data
PRFA<-filter(falcons, Species=="PRFA")
PEFA<-filter(falcons, Species=="PEFA")

###### Create test dfs to find differences btw data_2019 and falcons #####
# CONCLUSION: after the same filtering, data_2019 has four more entries than data_csv, with Event_IDs: 13501, 13501, 13506, 13508, in June and July
test_2019<- data_2019 %>% 
  filter(Year>2002  & !Survey_Purpose %in% c("Casual", "Unknown") & Territory_Type=="Territory"& str_detect(Detection_Result,"^Yes:")) %>% filter((Species == 'PRFA')|(Species == 'PEFA')) %>% select(-c(OBS_Ftime, OBS_Stime, Start_Date, Duration))
test_2019<-as_tibble(test_2019)
cols.num <- c("Fledglings","Nestlings","OtherAge")
test_2019[cols.num] <- sapply(test_2019[cols.num],as.integer)
test_falcons<- falcons %>% filter((Species == 'PRFA')|(Species == 'PEFA')) %>% select(-c(OBS_Ftime, OBS_Stime, Start_Date, Duration))

anti_join(test_2019, test_falcons, by = c('Event_ID','Observation_ID','Year', 'Territory_Name'))

# Identify surveys conducted where PRFA/PEFA were not detected but a 0 count was not given (change detections of other species to 0 counts of PRFA) ?
NoFalcon<- falcons %>%
  mutate(Falcon= ifelse(Species=="PRFA" | Species=="PEFA", 1, 0)) %>%
  group_by(Territory_Name, Area_Type, Start_Date) %>%
  summarise(Detection=sum(Falcon)) %>%
  filter(Detection<1) %>%
  mutate(Species= "PRFA", Year = year(Start_Date))
#Add missing surveys to PRFA dataframe
PRFASurveys<-bind_rows(PRFA, NoFalcon)
rm(NoFalcon)

#Convert "NA" counts to 0 - since filtered out surveys that were incomplete (e.g. Detection Result doesn't begin with Yes) it is safe to assume
#that the remaining NA's are 0
PRFASurveys <-PRFASurveys %>%
  mutate(Adults = coalesce(Adults,0),
         Fledglings = coalesce(Fledglings, 0),
         Nestlings = coalesce(Nestlings, 0))
#coalesce replaces NAs in Adults with 0, etc.
#write_csv(PRFASurveys, path = "Data/PRFASurveys.csv")


###### Create datasets for Modeling #######
#- Create PEFA Detections by Year - (to use as a potential covariate for PRFA)
PEFADets<-PEFA %>%
  group_by(Territory_Name, Year) %>%
  summarise(PEFADet=mean(Detection))%>%
  mutate(PEFADet=ifelse(PEFADet>0, 1, 0))

#Merge Annual PEFA Detections with PRFASurveys
PRFASurveys<-left_join(PRFASurveys, PEFADets, by=c("Territory_Name", "Year"))%>%
  mutate(PEFADet=coalesce(PEFADet, 0))

#Count PRFA Surveys by Area Type 
# (Avg number of visits each year each area type - core vs non-core)
SurveyAreaType <- PRFASurveys %>%
  count(Year, Territory_Name, Area_Type)%>% #count the number of each combination
  group_by(Year, Area_Type)%>%
  summarise(AvgVisits=mean(n), SDVisits = sd(n))
#Plot visit summary
ggplot(SurveyAreaType, aes(x= Year, y = AvgVisits)) + geom_smooth(aes(color = Area_Type), size = 1.5)


#Create PEFA covariate list
PEFACov<-PRFASurveys%>%
  select(Year, Territory_Name, PEFADet)%>%
  group_by(Year,Territory_Name)%>%
  summarise(PEFADet = max(PEFADet))%>%
  arrange(Territory_Name, Year)%>%
  spread("Year", "PEFADet", "0") # same as pivot_wider()
colnames(PEFACov)[2:18]<-paste("PEFA",colnames(PEFACov)[2:18], sep="")
rm(PEFADets)


#- Create PRFA detection history -
PRFAStates<- PRFASurveys %>%
  #Sort and group so number visits in the correct order (give all visits per terri per year a number label in the same order as visit time)
  arrange(Territory_Name, Start_Date)%>% 
  group_by(Territory_Name, Year)%>%
  mutate(Visit=row_number())%>%
  #Create YearVisit field (e.g. 2003v01 - first visit in 2003 of this territory)
  mutate(VisitC = ifelse(Visit<10, paste(0, Visit,sep=""), Visit), YearVisit = paste(Year, VisitC, sep = "v"))%>%
  group_by(Territory_Name, Area_Type, YearVisit)%>%
  #Determine state (0 = Unoccupied, 1= Occupied non-breeding, 2 = Occupied breeding)
  #TODO: Deoending on question may need to change condition for state 2
  mutate(State=ifelse(Fledglings>0|Nestlings>0, 2, ifelse(Adults>0, 1,0)))%>%
  #Add Late effect for where the probability of classifying sites as reproductively successful varied before and after May 1
  # (considered late if started after April) --- what is late effect ???
  mutate(Late=ifelse(month(Start_Date)>4 & month(Start_Date)<10, 1, 0))

PRFADetectHistory<-PRFAStates %>%
  #Drop columns no longer needed
  select(Territory_Name, Area_Type, YearVisit, State)%>%
  #Pivot table to give detection history
  spread("YearVisit", "State", ".")%>%
  arrange(Territory_Name, Area_Type)
#Export Detection history to excel
write_csv(PRFADetectHistory, path = "Data/PRFADetectHistory.csv")

#---check lists---
# #Create PRFA annual survey list to determine if years that territories were not surveyed match those of the PEFA covariate list ---- what does this mean?
PRFACheck <-PRFADetectHistory %>%
  gather(Session, Detection, 3:289) %>%
  mutate(Year=substr(Session, 0,4)) %>%
  group_by(Year, Territory_Name) %>%
  summarise(MaxDet = max(Detection)) %>%
  spread(Year, MaxDet, ".")
# #Compare csv files
# write_csv(PEFACov, path = "PEFACov.csv")
# write_csv(PRFACheck, path = "PRFACheck.csv")

# #- Plot Data --
PlotStates<- ggplot(PRFAStates, aes(x = Visit, y = fct_rev(as_factor(Territory_Name)), color = as.factor(State))) + geom_point()+
  facet_grid(cols = vars(Year))+
  labs(color = "States")
PlotStates

#- Create RMark File -
#create detection string called 'ch'
PRFA_Mark<- unite(PRFADetectHistory, 3:276, col="ch", sep="")%>%
  ungroup()%>%
  select(3,1,2)%>%
  mutate(Area_Type = as.factor(Area_Type))%>%
  
  #add in PEFA annual occupancy covariate data
  left_join(PEFACov, by="Territory_Name")%>%
  #add in Late covariate data
  #left_join(LateCov, by="Territory_Name")%>%
  
  #format data for rmark
  mutate(Territory_Name = paste("/*", Territory_Name, "*/"))
#PRFA_Mark[4:293]<- lapply(PRFA_Mark[4:293], factor)
#PRFA_Mark[4:293]<- lapply(PRFA_Mark[4:293], as.numeric)

write.table(PRFA_Mark, file = paste(wd,"/data/PRFA_Mark.txt", sep=""), sep=" ", col.names = TRUE)

#create time interval string (e.g. number of visits per year by territory)
TimeInterval_Cnt <-PRFAStates %>%
  group_by(Year)%>%
  summarise(Visits = max(Visit))%>%
  spread(Year, Visits)
#write_csv( TimeInterval_Cnt, path = " TimeInterval_Cnt.csv")

TimeInterval <-mutate_all(TimeInterval_Cnt,funs(paste(replicate(.-1, "0,"), collapse="")))%>%
  mutate_at(vars(-"2018"),funs(paste(.,"1,", sep="")))

TimeInterval <-unite(TimeInterval,"time_interval", 1:ncol(TimeInterval), sep="")
Time_Interval<-as.list(strsplit(as.character(TimeInterval), split=",")[[1]])
rm(TimeInterval)

# #- Naive Occupancy
#     NaivePRFA <- PRFAStates %>%
#       group_by(Year, Area_Type, Territory_Name) %>%
#       summarise(Occupied=max(Detection), State = max(State))
#     
#     NaiveStates<- NaivePRFA %>%
#       group_by(Year, Area_Type, State) %>%
#       summarise(nOccupied=n())%>%
#       group_by(Year, Area_Type)%>%
#       mutate(nSites = rowsum(nOccupied, Year))%>%
#       mutate(Occupancy=nOccupied/nSites)
#     
#     #Bring in PEFA data with PRFA to compare naive occupancy
#     NaiveOcc<-left_join(NaivePRFA , PEFADets, by=c("Year", "Territory_Name"))%>%
#       mutate(PEFADet = coalesce(PEFADet, 0))%>%
#       group_by(Year, Area_Type) %>%
#       summarise(nSites=n(), nOccupied =sum(Occupied), nPEFA = sum(PEFADet))%>%
#       mutate(Occupancy=nOccupied/nSites, PEFAOcc = nPEFA/nSites)
# 
#     #-Plot Naive Occupancy
#     plt_NaiveSpp <-ggplot(NaiveOcc, aes(x=Year, y=Occupancy,group=Area_Type)) +geom_line(aes(color=Area_Type), size=1.5) +
#                 geom_line(aes(x=Year, y=PEFAOcc, group=Area_Type, color=Area_Type), linetype="dotdash", size=1.5)
#     plt_NaiveStates <-ggplot(NaiveStates, aes(x=Year, y=Occupancy)) + geom_line(aes(color=as.factor(State)), size=1.5)+
#       labs(color = "States")+    
#       facet_grid(rows=vars(Area_Type))

##### Run Occupancy Analysis #####
install.packages("RMark")
library(RMark)

file<-paste(wd, "/data/PRFA_Mark.txt", sep="")
#Import mark file 
PRFAmark=import.chdata(paste(wd, "/data/PRFA_Mark.txt", sep=""),header=TRUE, field.types=c("s","f",rep("f",16)), use.comments = TRUE)

#make PEFA covariates time-varying
PRFAmark=make.time.factor(PRFAmark, "PEFA", 2003:2018, intercept=0)


#Process mark file using Robust Design Multiple-State Occupancy Estimation Conditional Binomial Model
PRFA_Process<-process.data(PRFAmark, model="RDMSOccRepro", begin.time = 2003,  time.intervals = Time_Interval, groups="Area_Type")

#Create design data  
PRFA_ddl <-make.design.data(PRFA_Process)

# Examine parameteres to confirm structure
head(PRFA_ddl$Phi0) #initial occupancy probabilities in each state
head(PRFA_ddl$Psi)  #probability of occupancy in t+1 given prior year's state
head(PRFA_ddl$R)    #probability of successfully reproducing (state 2), given prior year's state
head(PRFA_ddl$p)    #probability of detecting occupancy given it's state
head(PRFA_ddl$Delta)#probability of detecting successful reproduction given it occurred

#------ Define models and run in function ------#

run.PRFA=function()
{
  # Phi0 - initial occupancy probabilities
  # Phi0.dot=list(formula=~1) #held constant
  
  # Psi - probability of occupancy in t+1 given prior year's state
  # Psi.Year=list(formula=~time)
  # Psi.Area_Type=list(formula=~Area_Type)
  # Psi.stratum=list(formula=~stratum)
  # Psi.PEFA=list(formula=~PEFA1)
  # Psi.stratum.Time=list(formula=~stratum+Time)
  # Psi.stratumxTime=list(formula=~stratum*Time)
  
  # R - probability of successfully reproducing (state 2), given prior year's state
  # R.Year=list(formula=~time)
  
  # p - probability of detecting occupancy given it's state
  # p.Year=list(formula=~session)
  
  # Delta - probability of detecting successful reproduction given it occurred
  #Delta.Year=list(formula=~session)
  
  #Create model list and run set of models
  PRFA.model.list=create.model.list("RDMSOccRepro")
  PRFA.results= mark.wrapper(PRFA.model.list,data=PRFA_Process,ddl=PRFA_ddl,delete=TRUE)
}

PRFA.results = run.PRFA()
print(PRFA.results)


############## To Delete ###################
#Define models for Step 1 - determining p while holding all other parameters constant
p.dot=list(formula=~1)
#p.time=list(formula=~time)
p.Time=list(formula=~Time)
p.session=list(formula=~session)
p.Area_Type=list(formula=~Area_Type)
p.stratum=list(formula=~stratum)
#p.late=list(formula=~Late)
p.PEFA=list(formula=~PEFA1)
p.stratum.Time=list(formula=~stratum+Time)
p.stratumxTime=list(formula=~stratum*Time)
p.stratum.session=list(formula=~stratum+session)
p.stratumxsession=list(formula=~stratum*session)
R.stratumxTime=list(formula=~stratum*Time)
Psi.stratumxTime=list(formula=~stratum*Time)

#Create model list
cml=create.model.list("RDMSOccRepro")
# run and return models


Step1_results=do.PRFA.Step1()
print(Step1_results)

#Step 2 - determine best model for Delta
sink('analysis_output.txt')
do.PRFA.Step2 =function()
{
  PRFA_Process<-process.data(PRFAmark, model="RDMSOccRepro", begin.time = 2003, 
                             time.intervals = Time_Interval, groups="Area_Type")
  PRFA_ddl <-make.design.data(PRFA_Process)
  
  #PRFA_test <-mark(PRFA_Process, PRFA_ddl)
  
  #Review non-simplified PIM for phi0
  #PIMS(PRFA_test,"phi0",simplified=FALSE)
  
  #Define models for Step 2 - determining Delta while holding p with best model from Step 1 and all other parameters constant
  p.stratum.session=list(formula=~stratum+session)
  R.stratum.time=list(formula=~stratum*Time)
  Psi.stratum.time=list(formula=~stratum*Time)
  Delta.dot=list(formula=~1)
  Delta.Time=list(formula=~Time)
  Delta.session=list(formula=~session)
  Delta.Area_Type=list(formula=~Area_Type)
  Delta.PEFA=list(formula=~PEFA1)
  Delta.PEFA.Time=list(formula=~PEFA1+Time)
  Delta.PEFAxTime=list(formula=~PEFA1*Time)
  Delta.session.Time=list(formula=~session+Time)
  Delta.sessionxTime=list(formula=~session*Time)
  Delta.Area_Type.Time=list(formula=~Area_Type+Time)
  Delta.Area_TypexTime=list(formula=~Area_Type*Time)
  
  #Create model list
  cml=create.model.list("RDMSOccRepro")
  # run and return models
  
  return(mark.wrapper(cml,data=PRFA_Process,ddl=PRFA_ddl,delete=TRUE))
}

Step2_results=do.PRFA.Step2()
print(Step2_results)
sink()

#Step 3 - determine best model for Psi
sink('analysis_output.txt')
do.PRFA.Step3 =function()
{
  PRFA_Process<-process.data(PRFAmark, model="RDMSOccRepro", begin.time = 2003, 
                             time.intervals = Time_Interval, groups="Area_Type")
  PRFA_ddl <-make.design.data(PRFA_Process)
  
  #PRFA_test <-mark(PRFA_Process, PRFA_ddl)
  
  #Review non-simplified PIM for phi0
  #PIMS(PRFA_test,"phi0",simplified=FALSE)
  
  #Define models for Step 3 - determining Psi while holding p and Delta with best model from Step 1 and Step 2
  #and holding all other parameters constant
  
  p.stratum.session=list(formula=~stratum+session)
  Delta.sessionxTime=list(formula=~session*Time)
  R.stratum.time=list(formula=~stratum*Time)
  Psi.dot=list(formula=~1)
  Psi.Time=list(formula=~Time)
  Psi.Area_Type=list(formula=~Area_Type)
  Psi.stratum=list(formula=~stratum)
  Psi.PEFA=list(formula=~PEFA1)
  Psi.stratum.Time=list(formula=~stratum+Time)
  Psi.stratumxTime=list(formula=~stratum*Time)
  # Psi.stratum.time=list(formula=~stratum+time)
  # Psi.stratumxtime=list(formula=~stratum*time)
  Psi.PEFA.Time=list(formula=~PEFA1+Time)
  Psi.PEFAxTime=list(formula=~PEFA1*Time)
  #Psi.PEFA.time=list(formula=~PEFA1+time)
  #Psi.PEFAxtime=list(formula=~PEFA1*time)
  
  #Create model list
  cml=create.model.list("RDMSOccRepro")
  # run and return models
  
  return(mark.wrapper(cml,data=PRFA_Process,ddl=PRFA_ddl,delete=TRUE))
}


Step3_results=do.PRFA.Step3()
print(Step3_results)
sink()

#Step 4 - determine best model for R
sink('analysis_output.txt')
do.PRFA.Step4 =function()
{
  PRFA_Process<-process.data(PRFAmark, model="RDMSOccRepro", begin.time = 2003, 
                             time.intervals = Time_Interval, groups="Area_Type")
  PRFA_ddl <-make.design.data(PRFA_Process)
  
  #Define models for Step 4 - determining R while holding p, Delta, and Psi with best model from Step 1 to Step 3
  #and holding Phi0 constant
  
  p.stratum.session=list(formula=~stratum+session)
  Delta.sessionxTime=list(formula=~session*Time)
  Psi.stratum.Time=list(formula=~stratum+Time)
  R.dot=list(formula=~1)
  R.Time=list(formula=~Time)
  R.Area_Type=list(formula=~Area_Type)
  R.stratum=list(formula=~stratum)
  R.PEFA=list(formula=~PEFA1)
  R.stratum.Time=list(formula=~stratum+Time)
  R.stratumxTime=list(formula=~stratum*Time)
  R.PEFA.Time=list(formula=~PEFA1+Time)
  R.PEFAxTime=list(formula=~PEFA1*Time)
  
  #Create model list
  cml=create.model.list("RDMSOccRepro")
  # run and return models
  
  return(mark.wrapper(cml,data=PRFA_Process,ddl=PRFA_ddl,delete=TRUE))
}

Step4_results=do.PRFA.Step4()
print(Step4_results)

sink()

write.table(Step1_results$p.stratumxsession$design.matrix, file = "pStratumxSession.txt", sep=" ", col.names = TRUE)
write.table(Step3_results$Psi.stratum.time.p.stratumxsession.Delta.time$design.matrix, file="PsiDM.txt", sep=" ", col.names=TRUE)
summary(Step1_results$p.PEFA)
summary(Step1_results$p.session)

Step1_results$p.stratum_Time_full$design.matrix




