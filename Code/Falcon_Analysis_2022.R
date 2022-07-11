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
library(unmarked)
setwd("~/Desktop/Falcon2022/PrairieFalcon")


###### Import dataset and format #######
data_2019 <- read_excel('Data/qrpt_FalconObservations_2019.xlsx') #2003 - 2021
data_2022 <- read_excel('Data/qrpt_RaptorObservations_2022.xlsx') #1984-2021 July
falcons_2022<- as_tibble(data_2022)

#fix date
falcons_2022 <- falcons_2022 %>%
  mutate(SurveyDate=ymd(SurveyDate))

# Use to filter out FALSE protocol, and inadequate/unknown surveys
# Only use data from 2003 for occupancy analysis. Data prior to 2003 is presence only (presence of any species not just falcons)
falcons_2022<- falcons_2022 %>%
  filter(Year>2002 
         & IsProtocolSurvey == TRUE 
         & str_detect(`Sample Qualifier`,"^Yes:")) %>% 
  mutate(Detection = ifelse(AdultCount >0|FledglingCount>0|NestlingCount>0|OtherCount>0, 1,0)) %>% 
  mutate(Detection = coalesce(Detection,0), #replace NA's with 0
         JulianDay = as.integer(format(SurveyDate, "%j"))) # add Julian day column
        #filter(Territory_Type=="Territory") not needed bc all territories

# Change the count columns to integers
cols.num <- c("AdultCount", "FledglingCount","NestlingCount","OtherCount")
falcons_2022[cols.num] <- sapply(falcons_2022[cols.num],as.integer)

# Get list of territory names from the 2019 dataset which is the same as the names in the .csv after filtering
terr_name_list = unique(data_2019$Territory_Name)

# Find territories in 2022 but not the other two (8 total)
extra_terr_list = setdiff(unique(falcons_2022$TerritoryName), terr_name_list)
# Surveys in these territories (64 total)
extra_surveys = falcons_2022 %>% filter(TerritoryName %in% extra_terr_list) 

# Create a df with all territories in the 2019 dataset and their corresponding type (core vs. non-core)
area_types <- data_2019 %>% group_by(Territory_Name, Area_Type) %>% count() %>% select(-n)

# Add a Area_Type column to the 2022 df (some territories may have NA in this col)
falcons_2022 <- left_join(falcons_2022, area_types, by = c("TerritoryName"= "Territory_Name"))

#Separate prairie and peregrine falcon data
PRFA_2022<-filter(falcons_2022, CommonName=="Prairie Falcon")
PEFA_2022<-filter(falcons_2022, CommonName=="Peregrine Falcon")

# After filtering, safe to assume NA == 0
PRFASurveys_2022 <-PRFA_2022 %>%
  mutate(AdultCount = coalesce(AdultCount,0),
         FledglingCount = coalesce(FledglingCount, 0),
         NestlingCount = coalesce(NestlingCount, 0)) %>% #coalesce replaces NAs in Adults with 0
  arrange(TerritoryName, SurveyDate)

#save cleaned and formatted PRFA dataset 
write_csv(PRFASurveys_2022, path = "Data/PRFASurveys_2022.csv", append = FALSE)



###### Create datasets for Modeling #######
# Create PEFA Detections by Year - (to use as a potential yearly site covariate for PRFA)
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
ggplot(SurveyAreaType_2022, aes(x= Year, y = AvgVisits)) + geom_smooth(aes(color = Area_Type), size = 1.5) 


#Create PEFA covariate list (territories as rows, years as cols)
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
  #Determine inital state (0 = Unoccupied, 1= adults only, 1.5 = nestlings present and no fledglings, 2 = fledgling present)
  mutate(initialState=ifelse(FledglingCount>0, 2, ifelse(NestlingCount >0, 1.5, ifelse(AdultCount>0, 1,0)))) 

# Look at observations per territory per year
ObsSummary <- PRFASurveys_2022 %>%
  group_by(TerritoryName, Year)%>%
  count() %>% 
  mutate(Year = as.character(Year)) %>% 
  pivot_wider(names_from = Year, values_from = n, values_fill= 0)

##long version of same data
# longObsSummary<- PRFASurveys_2022 %>%
#   group_by(TerritoryName, Year)%>%
#   count() %>% 
#   mutate(Year = as.character(Year))
# 
# ggplot(longObsSummary,aes(x = TerritoryName,y = n)) + 
#   geom_bar(aes(fill = Year),stat = "identity",position = "dodge") + 
#   scale_y_log10() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y = element_blank())

### Remove observations for each territory for each year after the first 2 is seen
# Find the DayFromDec1st on which the first initial state 2 is observed for each territory each year
findFirstTwo<- PRFAStates_2022 %>%  # could write this into a function
  arrange(TerritoryName, Year, JulianDay) %>% 
  group_by(TerritoryName, Year) %>% 
  filter(initialState == 2) %>% 
  summarise(firstTwoDay = as.numeric(min(JulianDay)))

# add firstTwoDay column, 
# filter to keep only obs on or before this day for each territory each year,
# remove this column since it's not needed anymore
PRFAStates_2022 <- left_join(PRFAStates_2022, findFirstTwo, by = c("TerritoryName", "Year")) %>%
  filter(JulianDay <= firstTwoDay | is.na(firstTwoDay)) %>% 
  select(-firstTwoDay)


### Finalize the initial 1.5's (change to 1 or 2) for observations that do not have a yearly maxObs of 2 (scheme detailed in agenda 06-23 To-Do #8)
PRFAStates_2022 = PRFAStates_2022 %>% 
  group_by(TerritoryName, Year) %>% 
  mutate(maxState =  max(initialState),
         finalState = ifelse(
           initialState == 0, 0, ifelse(
             initialState == 1, 1, ifelse(
               initialState == 2, 2, ifelse(
                 maxState == 2, 2, 1
             )))))

### Find Late Effect cutoff using updated states (i.e. finalState)
# Late cutoff day is the average earliest day per territory per year to observe state 2 i.e., sign of successful breeding) (considered late (1) if after the calculated Late Effect cutoff and before Oct -this is from Sarah's code)
# The probability of classifying sites as reproductively successful varied before and after the calculated Late Effect cutoff. Before cutoff - harder to observe success due to obsense of nestlings. After cutoff - easier to observe due to presense of nestlings.

min_First_Two_Date = PRFAStates_2022 %>% 
  group_by(TerritoryName, Year) %>% 
  filter(finalState == 2) %>% 
  summarise(minDate = min(SurveyDate),
            minDay = as.numeric(min(JulianDay)))

LateCutOff = mean(min_First_Two_Date$minDay)

# PRFAStates_2022_with_cutoff <- left_join(PRFAStates_2022, min_fledge_date, by = c("TerritoryName", "Year"))

# add LateEffect as a column 
# TODO: the condition may need to be reconsidered after breedingYear is added
PRFAStates_2022$LateEffect = ifelse(PRFAStates_2022$JulianDay >= LateCutOff & month(PRFAStates_2022$SurveyDate)<10, 1, 0)

### Get number of years of surveys for each site
Years_per_Location_2003_2021 <- PRFAStates_2022 %>% 
  group_by(TerritoryName) %>%
  summarize(years = n_distinct(Year)) 

### Get number of surveys for each site --- might be useful for selecting sites
Surveys_per_Location_2003_2021 <- PRFAStates_2022 %>% 
  group_by(TerritoryName) %>%
  summarize(surveys = n())


### Pool data into bi-weekly bins
#calculate avg number of days between each obs per territory per year
find_diff = PRFAStates_2022 %>% 
  group_by(TerritoryName, Year) %>% 
  summarise(diff = diff(JulianDay))
summary(find_diff$diff) #mean: 29

# add biweeklyPeriod column to PRFAStates_2022 and pool
PRFAStates_2022_pooled = PRFAStates_2022 %>% 
  mutate(biweeklyPeriod =  paste(month(SurveyDate, label = TRUE), ifelse(day(SurveyDate)<15, "1", "2"), sep = ""),
         biweeklyPeriod = paste(year(SurveyDate), biweeklyPeriod, sep = "_")) %>% 
  as_tibble(PRFAStates_2022) %>%  
  group_by(TerritoryName, Year, biweeklyPeriod) %>%  
  slice_max(n = 1, order_by = finalState, with_ties = FALSE) %>% #keep only the row with the max finalState for each biweekly period
  arrange(TerritoryName, SurveyDate)

# Create a list of all unique biweekly periods present
# period_list <- c("Dec2", "Jan1", "Jan2", "Feb1", "Feb2", "Mar1", "Mar2","Apr1", "Apr2", "May1", "May2", "Jun1", "Jun2", "Jul1", "Jul2", "Aug2", "Oct2") 
#TODO: can we get rid of the Aug and Oct observations?

### Get number of years of surveys for each site
Years_per_Location_pooled <- PRFAStates_2022_pooled %>% 
  group_by(TerritoryName) %>%
  summarize(years = n_distinct(Year)) 

### Get number of surveys for each site --- might be useful for selecting sites
Surveys_per_Location_pooled <- PRFAStates_2022_pooled %>% 
  group_by(TerritoryName) %>%
  summarize(surveys = n())

### Get number of surveys for each site per year
Surveys_per_Location_per_Year_pooled  = PRFAStates_2022_pooled %>% 
  group_by(TerritoryName,Year)  %>%
  summarize(surveys = n())
hist(Surveys_per_Location_per_Year_pooled$surveys)
table(Surveys_per_Location_per_Year_pooled$surveys) # most are 3-5

### Add rows to States df (resulting in the states_append df) so that each year has same number of visits
## Method 1: use period_list to match (not used)---------------
# maxNumList = length(period_list) #17
# numYear = max(PRFAStates_2022$Year) - min(PRFAStates_2022$Year) + 1 #19
# # create a name list, each name repeated maxNumList*numYear times
# all_terr_types <- PRFAStates_2022 %>% group_by(TerritoryName, Area_Type) %>% count() %>% select(-n)
# TerritoryRep = all_terr_types[rep(1:nrow(all_terr_types),each = maxNumList*numYear),] #length 17119 = 53 terr's x 19 years x 17 surveys per year
# # create a empty biweeklyPeriod list and then fill with for loop
# biweeklyPeriod = rep("", each = numYear*maxNumList)
# for (i in 1:numYear) {
#   for (j in 1:maxNumList) {
#     x = period_list[j]
#     y = paste(i+2002, x, sep = "_")
#     biweeklyPeriod[i*j + (maxNumList-j)*(i-1)] = y
#   }
# }
# biweeklyPeriod = rep(biweeklyPeriod, times = length(unique(PRFAStates_2022$TerritoryName)))
# # add the biweeklyPeriod list to TerritoryRep as a column
# TerritoryRep$biweeklyPeriod = biweeklyPeriod
# 
# # left_join so that the final df has the same number of rows per territory per year
# PRFAStates_2022_append = left_join(TerritoryRep,PRFAStates_2022, by = c("TerritoryName","biweeklyPeriod","Area_Type"))
# 
# # almost 80% missing value (almost 70% if remove Aug/Oct obs)
# sum(is.na(PRFAStates_2022_append$finalState))/nrow(PRFAStates_2022_append)
# -----------

## Method 2: Add visit number 
PRFAStates_2022_pooled = PRFAStates_2022_pooled %>%
   #Sort and group so number visits in the correct order (give all visits per terri per year a number label ordered by visit time)
   arrange(TerritoryName, SurveyDate)%>%
   group_by(TerritoryName, Year)%>%
   mutate(Visit=row_number())%>%
   #Create YearVisit field (e.g. 2003v01 - first visit in 2003 of this territory)
   mutate(VisitC = ifelse(Visit<10, paste(0, Visit,sep=""), Visit),
          YearVisit = paste(Year, VisitC, sep = "v")) %>%
   select(-VisitC)

maxNum = max(PRFAStates_2022_pooled$Visit) #11
numYear = max(PRFAStates_2022_pooled$Year) - min(PRFAStates_2022$Year) + 1 #19
# create a name list, each name repeated maxNum times
all_terr_types <- PRFAStates_2022_pooled %>% group_by(TerritoryName, Area_Type) %>% count() %>% select(-n)
TerritoryRep = all_terr_types[rep(1:nrow(all_terr_types),each = maxNum*numYear),] #length 17119 = 53 terr's x 19 years x 11 surveys per year
 # create a empty YearVisit list and then fill with for loop
YearVisit = rep("", each = numYear*maxNum)
for (i in 1:numYear) {
  for (j in 1:maxNum) {
    x = ifelse(j<10, paste(0, j,sep=""), j)
    y = paste(i+2002, x, sep = "v")
    YearVisit[i*j + (maxNum-j)*(i-1)] = y
  }
}
YearVisit = rep(YearVisit, times = length(unique(PRFAStates_2022_pooled$TerritoryName)))
 # add the YearVisit list to TerritoryRep as a column
TerritoryRep$YearVisit = YearVisit

 # left_join so that the final df has the same number of rows per territory per year
PRFAStates_2022_append = left_join(TerritoryRep,PRFAStates_2022_pooled, by = c("TerritoryName","YearVisit","Area_Type"))

#check percentage of NA's
sum(is.na(PRFAStates_2022_append$finalState))/nrow(PRFAStates_2022_append) # 68% comparing to 78% with unpooled data
#-----------

###### Create detection hist df
PRFADetectHistory_2022<-PRFAStates_2022_append %>%
  #Drop columns no longer needed
  select(TerritoryName, Area_Type, YearVisit, finalState)%>%
  #Pivot table to give detection history
  spread("YearVisit", "finalState", NA)%>%
  arrange(TerritoryName, Area_Type)

#Export Detection history to excel
write_csv(PRFADetectHistory_2022, path = "Data/PRFADetectHistory_2022.csv", append= FALSE)

#Plot data
PlotStates_2022<- ggplot(PRFAStates_2022, aes(x = JulianDay, y = fct_rev(as_factor(TerritoryName)), color = as.factor(finalState))) + geom_point()+
  facet_grid(cols = vars(Year))+
  labs(color = "finalStates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y = element_blank())+
  geom_vline(xintercept= LateCutOff, linetype="dotdash", color = "grey")
PlotStates_2022 #some were done at the end of december as the start of the next season. One October, Sep, Aug surveys in 2013

#save iamge
# png(file="Data/SurveyPlot2022.png",
#     width=1200, height=700)
# PlotStates_2022
# dev.off()

###### Dynamic Multi-State Modeling with Unmarked #######
num_Year = max(PRFAStates_2022_pooled$Year) - min(PRFAStates_2022_pooled$Year) + 1 #19
num_Site = length(unique(PRFAStates_2022_pooled$TerritoryName)) #53
num_Rep = max(PRFAStates_2022_pooled$Visit) #number of visits within each year

# Get PERA Covs into right format
PEFACovs = pivot_longer(data = PEFACov_2022, cols = starts_with("PEFA"), values_to = "PEFA") #column 3 - yearly site cov: nrows should equal nsite*nyear
PEFACovs["PEFA"] <- sapply(PEFACovs["PEFA"],as.integer)

AreaTypeCovs = all_terr_types %>% 
  mutate(AreaType = case_when(Area_Type=="Core" ~ 1,
                              Area_Type=="Non-core" ~ 0,
                              TRUE ~ as.numeric(NA))) #column 3 - site cov: nrows should equal nsite. IF USE: need to remove territories with NA Area_Type.

###Create unmarked frame
# umf <- unmarkedFrameOccuMS(y=y, siteCovs=site_covs,
#                            obsCovs=obs_covs,
#                            yearlySiteCovs=yearly_site_covs,
#                            numPrimary=3)
umf_w_PEFA <- unmarkedFrameOccuMS(y=PRFADetectHistory_2022[,-c(1,2)], # y: 53 x 209
                           yearlySiteCovs=PEFACovs[3], # TODO: might need to check expected order
                           numPrimary=num_Year) #19
summary(umf_w_PEFA)

###Formulas
#Initial occupancy
psiformulas <- c('~1','~1') #siteCovs on psi[1] and psi[2]

#Transition probs (using condbinom rn)
umf_w_PEFA@phiOrder$cond_binom #guide to order
phiformulas <- c('~1','~1','~1','~1','~1','~1') # yearly site Covs on transition matrix elements. Currently random

#Detection probability
detformulas <- c('~1','~1','~1') # obsCovs on p1, p2 and delta 

#Fit model
fit_w_PEFA <- occuMS(detformulas=detformulas, psiformulas=psiformulas,
               phiformulas=phiformulas, data=umf_w_PEFA, parameterization = "condbinom")
coef(fit_w_PEFA)
plogis(coef(fit_w_PEFA))

#Compare fits
# modSel(fitList(fit,fit_null)) #example

#BackTransform - unmarked estimates are on the link-scale (logit for occu() since it uses a logit-link), and the backTransform() function coverts them back to the original scale. You need to specify a type of state or det for occupancy or detection covariates. If you have fit a model with covariates then you need to specify values for them (i.e. what is the probability of occupancy when CovA = X and CovB = Y)
#https://doi90.github.io/lodestar/fitting-occupancy-models-with-unmarked.html#fitting-a-model

psi_predict = predict(fit_w_PEFA, type = "psi") # has est. for psi1 and psi2 (i.e. R). Length of each df: 53. 
phi_predict = predict(fit_w_PEFA,type='phi') #has est. for phi0, phi1, phi2, R0, R1, R2. Length of each df: 954 WHY???
det_predict = predict(fit_w_PEFA,type='det') #has est. for p1, p2,delta. Length of each df: 19133 = 53*19*11
lapply(phi_predict,head)
#They all have the same values. WHY??

ggplot(psi_predict$psi, aes(x=Predicted)) + 
  geom_histogram(binwidth=0.01) + xlim(c(0.4, 0.6))


# Alt: use backTransform(). Didn't figure out how to make it work for our model
# backTransform(fit_w_PEFA) # set tyep to "det" / "state" 
# lc <- linearComb(fm1, c(Int = 1, cov1 = 0, cov2 = 0), type='state') #example
# backTransform(lc)


