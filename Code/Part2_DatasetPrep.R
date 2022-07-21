######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code prepares PEFA dataset and PRFA dectection history dataset for subsequent analysis, visuaization and modeling.
## Libraries and versions used: tidyverse
#####################################################################################################

source("Code/Part1_InitialCleaning.R")

###### Create datasets for Modeling #######

### Prepare PEFA data -------
# Create PEFA Detections by Year to use as a covariate for PRFA
PEFADets_2022<-PEFA_2022 %>%
  group_by(TerritoryName, BreedingYear) %>%
  summarise(PEFADet=mean(Detection))%>%
  mutate(PEFADet=ifelse(PEFADet>0, 1, 0))

# Merge Annual PEFA Detections with PRFASurveys
PRFASurveys_2022<-left_join(PRFASurveys_2022, PEFADets_2022, by=c("TerritoryName", "BreedingYear"))%>%
  mutate(PEFADet=coalesce(PEFADet, 0))

# Create PEFA covariate data (territories as rows, years as cols)
PEFACov_2022<-PRFASurveys_2022 %>%
  select(BreedingYear, TerritoryName, PEFADet)%>%
  group_by(BreedingYear,TerritoryName)%>%
  summarise(PEFADet = max(PEFADet))%>%
  arrange(TerritoryName, BreedingYear)%>%
  spread("BreedingYear", "PEFADet", "0") # same as pivot_wider()
colnames(PEFACov_2022)[2:20]<-paste("PEFA",colnames(PEFACov_2022)[2:20], sep="")
rm(PEFADets_2022)


### Assign the initial state for each observation -------
PRFAStates_2022<- PRFASurveys_2022 %>%
  #Determine inital state (0 = Unoccupied, 1= adults only, 1.5 = nestlings present and no fledglings, 2 = fledgling present)
  mutate(initialState=ifelse(
    FledglingCount>0, 2, ifelse(
      NestlingCount >0, 1.5, ifelse(
        AdultCount>0, 1,0)))) 


### More Filtering ---------
## Remove observations for each territory for each year after the first initial 2 is seen 
# Find the DaySinceDec15th on which the first initial state 2 is observed
findFirstTwo<- PRFAStates_2022 %>%  # could write this into a function
  arrange(TerritoryName, BreedingYear, DaySinceDec15th) %>% 
  group_by(TerritoryName, BreedingYear) %>% 
  filter(initialState == 2) %>% 
  summarise(firstTwoDay = as.numeric(min(DaySinceDec15th)))

# Add firstTwoDay column, 
# filter to keep only obs on or before this firstTwoDay day for each territory each year,
# remove this column since it's not needed anymore
PRFAStates_2022 <- left_join(PRFAStates_2022, findFirstTwo, by = c("TerritoryName", "BreedingYear")) %>%
  filter(DaySinceDec15th <= firstTwoDay | is.na(firstTwoDay)) %>% 
  select(-firstTwoDay)


### Update states ---------
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


### Find Late Effect cutoff using updated states (i.e. finalState) ----------
# Late Effect cutoff day is the earliest day to observe an updated state 2 across averaged all territory all year (i.e., sign of successful breeding) 
# (considered late (1) if after the calculated Late Effect cutoff and before Oct -this is from Sarah's code)
# The probability of classifying sites as reproductively successful varied before and after the calculated Late Effect cutoff. 
# Before cutoff - harder to observe success due to obsense of nestlings. After cutoff - easier to observe due to presense of nestlings.

min_First_Two_Date = PRFAStates_2022 %>% 
  group_by(TerritoryName, BreedingYear) %>% 
  filter(finalState == 2) %>% 
  summarise(minDate = min(SurveyDate),
            minDay = as.numeric(min(DaySinceDec15th)))

LateCutOff = mean(min_First_Two_Date$minDay)

# PRFAStates_2022_with_cutoff <- left_join(PRFAStates_2022, min_fledge_date, by = c("TerritoryName", "Year"))

# Add LateEffect as a column 
# Late if survey did between LateCutOff (inclusive) and December
PRFAStates_2022$LateEffect = ifelse(PRFAStates_2022$DaySinceDec15th >= LateCutOff & month(PRFAStates_2022$SurveyDate)<12, 1, 0)


### Pool data into bi-weekly bins ---------

# find avg number of days between each obs per territory per year
find_diff = PRFAStates_2022 %>%
  arrange(TerritoryName,BreedingYear,DaySinceDec15th)%>%
  group_by(TerritoryName, BreedingYear) %>%
  summarise(diff = diff(DaySinceDec15th))
summary(find_diff$diff) #mean: 19, max:205 (Tunnel 2013 has one in Oct after March)

# find the 85 percentile of #visits to a site in a year
Surveys_per_Location_per_Year  = PRFAStates_2022 %>%
  group_by(TerritoryName,BreedingYear)  %>%
  summarize(surveys = n())
quantile(Surveys_per_Location_per_Year$surveys, 0.85) #9

# Add triweeklyPeriod column to PRFAStates_2022 and pool in to triweekly bins so that max #visits to a site in a year is 9
PRFAStates_2022_pooled = PRFAStates_2022 %>%
  mutate(
    triweeklyPeriod =  paste("Triweekly", ceiling(DaySinceDec15th/21), sep = "_"),
    triweeklyPeriodNum =  ceiling(DaySinceDec15th/21)) %>%
  as_tibble(PRFAStates_2022_pooled) %>%
  group_by(TerritoryName, BreedingYear, triweeklyPeriod) %>%
  slice_max(n = 1, order_by = finalState, with_ties = FALSE) %>% #keep only the row with the max finalState for each triweekly period (referenced Ben's code)
  arrange(TerritoryName, SurveyDate)

### Choose POOLED vs. UNPOOLED dataset ---------

# If using pooled data, run the following line (Not Run):
#PRFA2022_Data <- PRFAStates_2022_pooled

# If using unpooled data, run the following line:
PRFA2022_Data <- PRFAStates_2022


### Add rows to States df (resulting in the PRFAStates_2022_append df) so that each year has same number of rows ----------
# Add visit number 
PRFA2022_Data = PRFA2022_Data %>%
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
maxNum = max(PRFA2022_Data$Visit) 
numYear = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #19

# Create a name list, each name repeated maxNum times
all_terr_types <- PRFA2022_Data %>% group_by(TerritoryName, Area_Type) %>% count() %>% select(-n)
TerritoryRep = all_terr_types[rep(1:nrow(all_terr_types),each = maxNum*numYear),] 

# Create a empty YearVisit list and then fill with for loop
YearVisit = rep("", each = numYear*maxNum)
for (i in 1:numYear) {
  for (j in 1:maxNum) {
    x = ifelse(j<10, paste(0, j,sep=""), j)
    y = paste(i+2006, x, sep = "v")
    YearVisit[i*j + (maxNum-j)*(i-1)] = y
  }
}
YearVisit = rep(YearVisit, times = length(unique(PRFA2022_Data$TerritoryName)))

# add the YearVisit list to TerritoryRep as a column
TerritoryRep$YearVisit = YearVisit

# left_join so that the final df has the same number of rows per territory per year
PRFA2022_Data_append = left_join(TerritoryRep,PRFA2022_Data, by = c("TerritoryName","YearVisit","Area_Type")) %>% 
  mutate(BreedingYear = substr(YearVisit, 1,4),
         Visit = substr(YearVisit, 5,7)) 

# check percentage of NA's
sum(is.na(PRFA2022_Data_append$finalState))/nrow(PRFA2022_Data_append) # %69 with unpooled data


### Create detection hist df -------
PRFADetectHistory_2022<-PRFA2022_Data_append %>%
  #Drop columns no longer needed
  select(TerritoryName, Area_Type, YearVisit, finalState)%>%
  #Pivot table to give detection history
  spread("YearVisit", "finalState", NA)%>%
  arrange(TerritoryName, Area_Type)

### Create STACKED detection hist df -------
PRFADetectHistory_2022_stacked <- PRFA2022_Data_append %>%
  #Drop columns no longer needed
  select(TerritoryName, Area_Type, BreedingYear, Visit, finalState)%>%
  pivot_wider(names_from = Visit, values_from = finalState) %>% 
  unite("Site_Year", c(TerritoryName, BreedingYear))
  

#Export Detection history to excel
write_csv(PRFADetectHistory_2022, path = "Data/PRFADetectHistory_2022.csv", append= FALSE)
write_csv(PRFADetectHistory_2022_stacked, path = "Data/PRFADetectHistory_2022_stacked.csv", append= FALSE)
