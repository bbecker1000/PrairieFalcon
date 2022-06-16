######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code modified from 2019 code of Sarah Wakamiya for analysis of 2022 data. Code takes SFAN falcon data export from database, formats it for analysis and graphing
## Date: 
## Author: 
## R version 
## Libraries and versions used: tidyverse, lubridate .....
#####################################################################################################

##### Prepare workspace #####
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
setwd("~/Desktop/Falcon2022/PrairieFalcon")

###### Import dataset and format #######
data_2022 <- read_excel('Data/qrpt_RaptorObservations_2022.xlsx') #1984-2021 July
falcons_2022<- as_tibble(data_2022)

#fix date
falcons_2022 <- falcons_2022 %>%
  mutate(SurveyDate=ymd(SurveyDate))

# Use to filter out FALSE propotal, and inadequate/unknown surveys
# Only use data from 2003 for occupancy analysis. Data prior to 2003 is presence only (presence of any species not just falcons)
falcons_2022<- falcons_2022 %>%
  filter(Year>2002 
         & IsProtocolSurvey == TRUE 
         & str_detect(`Sample Qualifier`,"^Yes:")) %>% 
  mutate(Detection = ifelse(AdultCount >0 |FledglingCount>0|NestlingCount>0|OtherCount>0, 1,0))
        #& Territory_Type=="Territory" (Not needed bc all territories)
falcons_2022 <- left_join(falcons_2022, AT, by = c("TerritoryName"= "Territory_Name"))
#Separate prairie and peregrine falcon data
PRFA_2022<-filter(falcons_2022, CommonName=="Prairie Falcon")
PEFA_2022<-filter(falcons_2022, CommonName=="Peregrine Falcon")

cols.num <- c("AdultCount", "FledglingCount","NestlingCount","OtherCount")
PRFA_2022[cols.num] <- sapply(PRFA_2022[cols.num],as.integer)
PEFA_2022[cols.num] <- sapply(PEFA_2022[cols.num],as.integer)
 
PRFASurveys_2022 <-PRFA_2022 %>%
  mutate(AdultCount = coalesce(AdultCount,0),
         FledglingCount = coalesce(FledglingCount, 0),
         NestlingCount = coalesce(NestlingCount, 0))
#coalesce replaces NAs in Adults with 0, etc.
write_csv(PRFASurveys_2022, path = "Data/PRFASurveys_2022.csv", append = FALSE)

###### Create datasets for Modeling #######
#- Create PEFA Detections by Year - (to use as a potential covariate for PRFA)
PEFADets_2022<-PEFA_2022 %>%
  group_by(TerritoryName, Year) %>%
  summarise(PEFADet=mean(Detection))%>%
  mutate(PEFADet=ifelse(PEFADet>0, 1, 0))

#Merge Annual PEFA Detections with PRFASurveys
PRFASurveys_2022<-left_join(PRFASurveys_2022, PEFADets_2022, by=c("TerritoryName", "Year"))%>%
  mutate(PEFADet=coalesce(PEFADet, 0))

#Count PRFA Surveys by Area Type  
# (Avg number of visits each year each area type - core vs non-core)
SurveyAreaType_2022 <- PRFASurveys_2022 %>%
  count(Year, TerritoryName, Area_Type)%>% #count the number of each combination
  group_by(Year, Area_Type)%>%
  summarise(AvgVisits=mean(n), SDVisits = sd(n))
#Plot visit summary
ggplot(SurveyAreaType_2022, aes(x= Year, y = AvgVisits)) + geom_smooth(aes(color = Area_Type), size = 1.5) #(TODO: check Area_Type == NA)


#Create PEFA covariate list
PEFACov_2022<-PRFASurveys_2022 %>%
  select(Year, TerritoryName, PEFADet)%>%
  group_by(Year,TerritoryName)%>%
  summarise(PEFADet = max(PEFADet))%>%
  arrange(TerritoryName, Year)%>%
  spread("Year", "PEFADet", "0") # same as pivot_wider()
colnames(PEFACov_2022)[2:20]<-paste("PEFA",colnames(PEFACov_2022)[2:20], sep="")
rm(PEFADets_2022)

#- Create PRFA detection history -
PRFAStates_2022<- PRFASurveys_2022 %>%
  #Sort and group so number visits in the correct order (give all visits per terri per year a number label in the same order as visit time)
  arrange(TerritoryName, SurveyDate)%>% 
  group_by(TerritoryName, Year)%>%
  mutate(Visit=row_number())%>%
  #Create YearVisit field (e.g. 2003v01 - first visit in 2003 of this territory)
  mutate(VisitC = ifelse(Visit<10, paste(0, Visit,sep=""), Visit), YearVisit = paste(Year, VisitC, sep = "v"))%>%
  group_by(TerritoryName, Area_Type, YearVisit)%>%
  #Determine state (0 = Unoccupied, 1= Occupied non-breeding, 2 = Occupied breeding)
  #TODO: Depending on question may need to change condition for state 2
  mutate(State=ifelse(FledglingCount>0|NestlingCount>0, 2, ifelse(AdultCount>0, 1,0)))%>%
  #Add Late effect for where the probability of classifying sites as reproductively successful varied before and after May 1
  # (considered late if started after April) --- what is late effect ???
  mutate(Late=ifelse(month(SurveyDate)>4 & month(SurveyDate)<10, 1, 0))

PRFADetectHistory_2022<-PRFAStates_2022 %>%
  #Drop columns no longer needed
  select(TerritoryName, Area_Type, YearVisit, State)%>%
  #Pivot table to give detection history
  spread("YearVisit", "State", ".")%>%
  arrange(TerritoryName, Area_Type)
#Export Detection history to excel
write_csv(PRFADetectHistory_2022, path = "Data/PRFADetectHistory_2022.csv", append= FALSE)

#Plot data
PlotStates_2022<- ggplot(PRFAStates_2022, aes(x = Visit, y = fct_rev(as_factor(TerritoryName)), color = as.factor(State))) + geom_point()+
  facet_grid(cols = vars(Year))+
  labs(color = "States")
PlotStates_2022
