######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code modified from 2019 code of Sarah Wakamiya for analysis of 2022 data. Code takes SFAN falcon data export from database, formats it for analysis and graphing
## Date: 
## Author: 
## R version:
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

# Use to filter out FALSE propotal, and inadequate/unknown surveys
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

# Filter falcons_2022 to only contain territories from 2019 dataset (Gavin said to ignore extras from 2022 dataset)
falcons_2022 <- falcons_2022[falcons_2022$TerritoryName %in% terr_name_list, ] 
#sort(unique(area_types$Territory_Name)) == sort(terr_name_list) #test

# Add a Area_Type column to the 2022 df
falcons_2022 <- left_join(falcons_2022, area_types, by = c("TerritoryName"= "Territory_Name"))
#sum(is.na(falcons_2022$Area_Type)) #test

# Add a BreedingYear column to falcons_2022 (Breeding year starts December 1st)
falcons_2022 <- falcons_2022 %>% 
  mutate(BreedingYear = ifelse( month(SurveyDate) == 12, Year+1, Year) )
#all(falcons_2022$Year == falcons_2022$BreedingYear) #test

#Add a DaySinceDec15th column to falcons_2022 (the earliest date in December is 15th)
falcons_2022 <- falcons_2022 %>% 
  mutate(DaySinceDec15th = ifelse(month(SurveyDate) == 12, day(SurveyDate)-14, JulianDay+17))


# Separate prairie and peregrine falcon data
PRFA_2022<-filter(falcons_2022, CommonName=="Prairie Falcon")
PEFA_2022<-filter(falcons_2022, CommonName=="Peregrine Falcon")

# After filtering, safe to assume NA == 0
PRFASurveys_2022 <-PRFA_2022 %>%
  mutate(AdultCount = coalesce(AdultCount,0),
         FledglingCount = coalesce(FledglingCount, 0),
         NestlingCount = coalesce(NestlingCount, 0)) #coalesce replaces NAs in Adults with 0, etc.

#save cleaned and formatted PRFA dataset 
write_csv(PRFASurveys_2022, path = "Data/PRFASurveys_2022.csv", append = FALSE)


###### Create datasets for Modeling #######

### Prepare PEFA data
# Create PEFA Detections by Year - (to use as a potential yearly site covariate for PRFA)
PEFADets_2022<-PEFA_2022 %>%
  group_by(TerritoryName, BreedingYear) %>%
  summarise(PEFADet=mean(Detection))%>%
  mutate(PEFADet=ifelse(PEFADet>0, 1, 0))

# Merge Annual PEFA Detections with PRFASurveys
PRFASurveys_2022<-left_join(PRFASurveys_2022, PEFADets_2022, by=c("TerritoryName", "BreedingYear"))%>%
  mutate(PEFADet=coalesce(PEFADet, 0))

# Create PEFA covariate list (territories as rows, years as cols)
PEFACov_2022<-PRFASurveys_2022 %>%
  select(BreedingYear, TerritoryName, PEFADet)%>%
  group_by(BreedingYear,TerritoryName)%>%
  summarise(PEFADet = max(PEFADet))%>%
  arrange(TerritoryName, BreedingYear)%>%
  spread("BreedingYear", "PEFADet", "0") # same as pivot_wider()
colnames(PEFACov_2022)[2:20]<-paste("PEFA",colnames(PEFACov_2022)[2:20], sep="")
rm(PEFADets_2022)


### Count PRFA Surveys by Area Type (Avg number of visits each year each area type - core vs non-core)
SurveyAreaType_2022 <- PRFASurveys_2022 %>%
  count(BreedingYear, TerritoryName, Area_Type)%>% #count the number of each combination
  group_by(BreedingYear, Area_Type)%>%
  summarise(AvgVisits=mean(n), SDVisits = sd(n))
#Plot visit summary 
ggplot(SurveyAreaType_2022, aes(x= BreedingYear, y = AvgVisits)) + geom_smooth(aes(color = Area_Type), size = 1.5) 


### Assign the initial state for each observation 
PRFAStates_2022<- PRFASurveys_2022 %>%
  #Determine inital state (0 = Unoccupied, 1= adults only, 1.5 = nestlings present and no fledglings, 2 = fledgling present)
  mutate(initialState=ifelse(
    FledglingCount>0, 2, ifelse(
      NestlingCount >0, 1.5, ifelse(
        AdultCount>0, 1,0)))) 


### Remove observations for each territory for each year after the first initial 2 is seen
# Find the DayFromDec1st on which the first initial state 2 is observed
findFirstTwo<- PRFAStates_2022 %>%  # could write this into a function
  arrange(TerritoryName, BreedingYear, DayFromDec1st) %>% 
  group_by(TerritoryName, BreedingYear) %>% 
  filter(initialState == 2) %>% 
  summarise(firstTwoDay = as.numeric(min(DayFromDec1st)))

# Add firstTwoDay column, 
# filter to keep only obs on or before this DayFromDec1st day for each territory each year,
# remove this column since it's not needed anymore
PRFAStates_2022 <- left_join(PRFAStates_2022, findFirstTwo, by = c("TerritoryName", "BreedingYear")) %>%
  filter(DayFromDec1st <= firstTwoDay | is.na(firstTwoDay)) %>% 
  select(-firstTwoDay)


### Update states
# Finalize the initial 1.5's (change to 1 or 2) for observations that do not have a yearly maxObs of 2 (scheme detailed in agenda 06-23 To-Do #8)
PRFAStates_2022 = PRFAStates_2022 %>% 
  group_by(TerritoryName, BreedingYear) %>% 
  mutate(maxState =  max(initialState),
         finalState = ifelse(
           initialState == 0, 0, ifelse(
             initialState == 1, 1, ifelse(
               initialState == 2, 2, ifelse(
                 maxState == 2, 2, 1
               )))))


### Find Late Effect cutoff using updated states (i.e. finalState)
# Late Effect cutoff day is the earliest day to observe an updated state 2 across averaged all territory all year (i.e., sign of successful breeding) 
# (considered late (1) if after the calculated Late Effect cutoff and before Oct -this is from Sarah's code)
# The probability of classifying sites as reproductively successful varied before and after the calculated Late Effect cutoff. 
# Before cutoff - harder to observe success due to obsense of nestlings. After cutoff - easier to observe due to presense of nestlings.

min_First_Two_Date = PRFAStates_2022 %>% 
  group_by(TerritoryName, BreedingYear) %>% 
  filter(finalState == 2) %>% 
  summarise(minDate = min(SurveyDate),
            minDay = as.numeric(min(DayFromDec1st)))

LateCutOff = mean(min_First_Two_Date$minDay)

# PRFAStates_2022_with_cutoff <- left_join(PRFAStates_2022, min_fledge_date, by = c("TerritoryName", "Year"))

# add LateEffect as a column 
# TODO: need to consider that one observation in October
PRFAStates_2022$LateEffect = ifelse(PRFAStates_2022$DayFromDec1st >= LateCutOff & month(PRFAStates_2022$SurveyDate)<10, 1, 0)


### Pool data into bi-weekly bins

# find avg number of days between each obs per territory per year
find_diff = PRFAStates_2022 %>%
  arrange(TerritoryName,BreedingYear,DayFromDec1st)%>%
  group_by(TerritoryName, BreedingYear) %>% 
  summarise(diff = diff(DayFromDec1st))
summary(find_diff$diff) #mean: 19, max:205 (Tunnel 2013 has one in Oct after March)

# find the 85 percentile of #visits to a site in a year
Surveys_per_Location_per_Year  = PRFAStates_2022 %>% 
  group_by(TerritoryName,BreedingYear)  %>%
  summarize(surveys = n())
quantile(Surveys_per_Location_per_Year$surveys, 0.85) #9

# TODO: how to find the best width of the bins? 

# add triweeklyPeriod column to PRFAStates_2022 and pool in to triweekly bins so that max #visits to a site in a year is 9
PRFAStates_2022_pooled = PRFAStates_2022 %>% 
  mutate(
    triweeklyPeriod =  paste("Triweekly", ceiling(DaySinceDec15th/21), sep = "_")) %>%
  as_tibble(PRFAStates_2022_pooled) %>%  
  group_by(TerritoryName, BreedingYear, triweeklyPeriod) %>%  
  slice_max(n = 1, order_by = finalState, with_ties = FALSE) %>% #keep only the row with the max finalState for each triweekly period (referenced Ben's code)
  arrange(TerritoryName, SurveyDate)


### Add rows to States df (resulting in the PRFAStates_2022_append df) so that each year has same number of visits
# Add visit number 
PRFAStates_2022_pooled = PRFAStates_2022_pooled %>%
  #Sort and group so number visits in the correct order for each BreedingYear
  #(give all visits per terri per year a number label ordered by visit time)
  arrange(TerritoryName, BreedingYear, DaySinceDec15th)%>% #can't just arrange with Site and SurveyDate since using BreedingYear now
  group_by(TerritoryName, BreedingYear)%>%
  mutate(Visit=row_number())%>%
  #Create YearVisit field (e.g. 2003v01 - first visit in 2003 of this territory)
  mutate(VisitC = ifelse(Visit<10, paste(0, Visit,sep=""), Visit),
         YearVisit = paste(BreedingYear, VisitC, sep = "v")) %>%
  select(-VisitC)

# Find max number of surveys per site per year, and the total number of years 
maxNum = max(PRFAStates_2022_pooled$Visit) 
numYear = max(PRFAStates_2022_pooled$BreedingYear) - min(PRFAStates_2022$BreedingYear) + 1 #19

# Create a name list, each name repeated maxNum times
all_terr_types <- PRFAStates_2022_pooled %>% group_by(TerritoryName, Area_Type) %>% count() %>% select(-n)
TerritoryRep = all_terr_types[rep(1:nrow(all_terr_types),each = maxNum*numYear),] 

# Create a empty YearVisit list and then fill with for loop
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

# check percentage of NA's
sum(is.na(PRFAStates_2022_append$finalState))/nrow(PRFAStates_2022_append) # %59.4 with triweekly bins

# Create detection hist df
PRFADetectHistory_2022<-PRFAStates_2022_append %>%
  #Drop columns no longer needed
  select(TerritoryName, Area_Type, YearVisit, finalState)%>%
  #Pivot table to give detection history
  spread("YearVisit", "finalState", NA)%>%
  arrange(TerritoryName, Area_Type)

#Export Detection history to excel
#write_csv(PRFADetectHistory_2022, path = "PRFADetectHistory_2022.csv", append= FALSE)



###### Exploratory Data Analysis & Visualization #######

### Get number of years of UNPOOLED surveys for each site (code from Ben)
Years_per_Location <- PRFAStates_2022 %>% 
  group_by(TerritoryName) %>%
  summarize(years = n_distinct(BreedingYear)) 

### Get number of UNPOOLED surveys for each site (code from Ben)
Surveys_per_Location <- PRFAStates_2022 %>% 
  group_by(TerritoryName) %>%
  summarize(surveys = n())

### Get number of UNPOOLED surveys for each site each year
Surveys_per_Location_per_Year  = PRFAStates_2022 %>% 
  group_by(TerritoryName,BreedingYear)  %>%
  summarize(surveys = n())
# add %>% pivot_wider(names_from = BreedingYear, values_from = n, values_fill= 0) to make into wide format

### Get number of years of POOLED surveys for each site 
Years_per_Location_pooled <- PRFAStates_2022_pooled %>% 
  group_by(TerritoryName) %>%
  summarize(years = n_distinct(BreedingYear)) 

### Get number of POOLED surveys for each site
Surveys_per_Location_pooled <- PRFAStates_2022_pooled %>% 
  group_by(TerritoryName) %>%
  summarize(surveys = n())

### Get number of POOLED surveys for each site each year
Surveys_per_Location_per_Year_pooled  = PRFAStates_2022_pooled %>% 
  group_by(TerritoryName,BreedingYear)  %>%
  summarize(surveys = n())
#hist(Surveys_per_Location_per_Year_pooled$surveys)
#table(Surveys_per_Location_per_Year_pooled$surveys) # most are 3-5

### Plot unpooled data
PlotStates_2022<- ggplot(PRFAStates_2022, aes(x = DayFromDec1st, y = fct_rev(as_factor(TerritoryName)), color = as.factor(finalState))) + geom_point()+
  facet_grid(cols = vars(BreedingYear))+
  labs(color = "finalStates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y = element_blank())+
  geom_vline(xintercept= LateCutOff, linetype="dotdash", color = "grey")
PlotStates_2022 #some were done at the end of december as the start of the next season. One October, Sep, Aug surveys in 2013


### TODO: Plot pooled data 
#PlotStates_2022_pooled <- ggplot(PRFAStates_2022_pooled, aes(x = biweeklyPeriod, y = fct_rev(as_factor(TerritoryName)), color = as.factor(finalState))) + 
#  geom_point()+
#  facet_grid(cols = vars(BreedingYear))+
#  labs(color = "finalStates") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y = element_blank())
#PlotStates_2022_pooled


### Naive Occupancy Overtime
num_Site <- length(unique(PRFAStates_2022$TerritoryName))

PRFAStates_2022_probs <- PRFAStates_2022 %>% group_by(BreedingYear,TerritoryName) %>% summarise(seasonalFinalState = max(finalState))
PRFAStates_2022_probs$oneOrTwoState <- PRFAStates_2022_probs$seasonalFinalState > 0
PRFAStates_2022_probs <- PRFAStates_2022_probs %>% group_by(BreedingYear) %>% summarise(oneOrTwoStateProportion = sum(oneOrTwoState)/num_Site)

ggplot(PRFAStates_2022_probs, aes(x=BreedingYear, y=oneOrTwoStateProportion)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  labs(
    title = "Naive Occupancy Time Series",
    subtitle = "Data from 2003 to 2021",
    x = "Breeding Year",
    y = "Occupancy Proportion") +
  theme(
    plot.title = element_text(color = "lightsalmon", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5), 
    axis.title.x = element_text(size = 16, face = "italic"),
    axis.title.y = element_text(size = 16, face = "italic"))


### Naive Successful Reproduction overtime
PRFAStates_2022_probs <- PRFAStates_2022 %>% group_by(BreedingYear,TerritoryName) %>% summarise(seasonalFinalState = max(finalState))
PRFAStates_2022_probs$twoState <- PRFAStates_2022_probs$seasonalFinalState > 1
PRFAStates_2022_probs <- PRFAStates_2022_probs %>% group_by(BreedingYear) %>% summarise(twoStateProportion = sum(twoState)/num_Site)

ggplot(PRFAStates_2022_probs, aes(x=BreedingYear, y=twoStateProportion)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  labs(
    title = "Naive Successful Reproduction Time Series",
    subtitle = "Data from 2003 to 2021",
    x = "Breeding Year",
    y = "Successful Reproduction Proportion") +
  theme(
    plot.title = element_text(color = "lightsalmon", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5), 
    axis.title.x = element_text(size = 16, face = "italic"),
    axis.title.y = element_text(size = 16, face = "italic"))


### Total number fledglings per year
total_chicks_per_year = PRFAStates_2022 %>%
  group_by(TerritoryName, BreedingYear) %>%
  summarise(max_fledge = max(FledglingCount)) %>%
  group_by(BreedingYear) %>%
  summarise(NumFledge = sum(max_fledge))

# Plot
ggplot(total_chicks_per_year, aes(x=BreedingYear, y=NumFledge, label = NumFledge)) +
  geom_line(size=1, color = "grey") +
  geom_point(shape = 21, color = "black", fill = "steelblue", size = 4) +
  geom_text(hjust = -0.5, vjust = -1)+
  labs(
    title = "Total Number of Fledglings Produced per Year 2003-2021",
    x = "Breeding Year",
    y = "Total Number of Fledglings") +
  theme_minimal()+
  theme(plot.title = element_text(face= 'bold', size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10))


### Average number of chicks per site per year.
average_chicks_per_year = PRFAStates_2022 %>%
  group_by(TerritoryName, BreedingYear) %>%
  summarise(max_fledge = max(FledglingCount)) %>%
  group_by(BreedingYear) %>%
  summarise(AvgFledge = mean(max_fledge))
average_chicks_per_year$AvgFledge = round(average_chicks_per_year$AvgFledge,2)
            
# Plot
ggplot(average_chicks_per_year, aes(x=BreedingYear, y=AvgFledge, label = AvgFledge)) +
  geom_line(size=1, color = "grey") +
  geom_point(shape = 21, color = "black", fill = "darkgreen", size = 4) +
  geom_text(hjust = -0.5, vjust = -1) +
  labs(title = "Average Number of Fledglings Produced per Site per Year 2003-2021",
       x = "Breeding Year",
       y = "Average Number of Fledglings per Site") +
  theme_minimal() +
  theme(plot.title = element_text(face= 'bold', size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10))  


###### Dynamic Multi-State Modeling with Unmarked #######

# Use PRFAStates_2022_pooled to calculate the following because PRFAStates_2022_append has NA's
num_Year = max(PRFAStates_2022_pooled$BreedingYear) - min(PRFAStates_2022_pooled$BreedingYear) + 1 #19
num_Site = length(unique(PRFAStates_2022_pooled$TerritoryName)) #46
num_Rep = max(PRFAStates_2022_pooled$Visit) #11 #number of visits within each year
# c(num_Year,num_Site,num_Rep ) #test

### Prepare Covariate Data
# Get PERA Covs into right format
PEFACovs = pivot_longer(data = PEFACov_2022, cols = starts_with("PEFA"), values_to = "PEFA") #column 3 - yearly site cov: nrows should equal nsite*nyear
PEFACovs["PEFA"] <- sapply(PEFACovs["PEFA"],as.integer)

# Get Area Type Covs into right format
AreaTypeCovs = all_terr_types %>% 
  mutate(AreaType = case_when(Area_Type=="Core" ~ 1,
                              Area_Type=="Non-core" ~ 0)) 

# Get Late Effect Covs into right format
LateEffectCovs <- PRFAStates_2022_append %>% 
  arrange(BreedingYear, Visit) %>%
  select(LateEffect)
LateEffectCovs['LateEffect'] <- sapply(LateEffectCovs['LateEffect'], as.integer)


### Create Unmarked Frame
# Basic format:
# umf <- unmarkedFrameOccuMS(y=y, siteCovs=site_covs,
#                            obsCovs=obs_covs,
#                            yearlySiteCovs=yearly_site_covs,
#                            numPrimary=3)
umf_w_covs <- unmarkedFrameOccuMS(y=PRFADetectHistory_2022[,-c(1,2)], 
                                  siteCovs=AreaTypeCovs[3],
                                  obsCovs=LateEffectCovs[3],
                                  yearlySiteCovs=PEFACovs[3], 
                                  numPrimary=num_Year) #19
summary(umf_w_covs)


### Set Formulas
#Initial occupancy
psiformulas <- c('~1','~1') #siteCovs on psi[1] and psi[2]

#Transition probs (using condbinom rn)
umf_w_covs@phiOrder$cond_binom #guide to order
phiformulas <- c('~1','~1','~1','~1','~1','~1') # yearly site Covs on transition matrix elements. Currently random

#Detection probability
detformulas <- c('~1','~1','~1') # obsCovs on p1, p2 and delta 


### Fit Model
fit_w_covs <- occuMS(detformulas=detformulas, 
                     psiformulas=psiformulas,
                     phiformulas=phiformulas, 
                     data=umf_w_covs, parameterization = "condbinom")
coef(fit_w_covs)


### TODO: Compare Fit
# Example: modSel(fitList(fit,fit_null))


### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
psi_predict = predict(fit_w_covs, type = "psi") # est. for psi1 and psi2 (i.e. R)
phi_predict = predict(fit_w_covs,type='phi') # est. for phi0, phi1, phi2, rho0, rho1, rho2. 
det_predict = predict(fit_w_covs,type='det') # est. for p1, p2, delta

#lapply(phi_predict,head)
