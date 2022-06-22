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

# Add a Area_Type column to the 2022 df (some territories may have NA in this col)
falcons_2022 <- left_join(falcons_2022, area_types, by = c("TerritoryName"= "Territory_Name"))

#Separate prairie and peregrine falcon data
PRFA_2022<-filter(falcons_2022, CommonName=="Prairie Falcon")
PEFA_2022<-filter(falcons_2022, CommonName=="Peregrine Falcon")

# After filtering, safe to assume NA == 0
PRFASurveys_2022 <-PRFA_2022 %>%
  mutate(AdultCount = coalesce(AdultCount,0),
         FledglingCount = coalesce(FledglingCount, 0),
         NestlingCount = coalesce(NestlingCount, 0)) #coalesce replaces NAs in Adults with 0, etc.

#save cleaned and formatted PRFA dataset 
write_csv(PRFASurveys_2022, path = "Data/PRFASurveys_2022.csv", append = FALSE)
################

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
  #Sort and group so number visits in the correct order (give all visits per terri per year a number label ordered by visit time)
  arrange(TerritoryName, SurveyDate)%>% 
  group_by(TerritoryName, Year)%>%
  mutate(Visit=row_number())%>%
  #Create YearVisit field (e.g. 2003v01 - first visit in 2003 of this territory)
  mutate(VisitC = ifelse(Visit<10, paste(0, Visit,sep=""), Visit), 
         YearVisit = paste(Year, VisitC, sep = "v"))%>%
  group_by(TerritoryName, Area_Type, YearVisit)%>%
  #Determine state (0 = Unoccupied, 1= Occupied non-breeding, 2 = Occupied fledgling)
  mutate(State=ifelse(FledglingCount>0, 2, ifelse(AdultCount>0 | NestlingCount >0, 1,0)))

# Find Late Effect cutoff (earliest date AND Julian Day per territory per year to observe state 2 i.e., fledglings)
min_fledge_date = PRFAStates_2022 %>% 
  group_by(TerritoryName, Year) %>% 
  mutate(maxState =  max(State)) %>% 
  filter(maxState == 2) %>% 
  arrange(TerritoryName, Year, JulianDay) %>% 
  group_by(TerritoryName, Year) %>% 
  filter(State == 2) %>% 
  summarise(MinFledglingDate = min(SurveyDate),
            MinFledglingDay = min(JulianDay))

avg_min_day = mean(min_date_df$MinDay)

# PRFAStates_2022_with_cutoff <- left_join(PRFAStates_2022, min_fledge_date, by = c("TerritoryName", "Year"))

#Add Late effect for where the probability of classifying sites as reproductively successful varied before and after the calculated Late Effect cutoff (considered late if after the calculated Late Effect cutoff and before Oct)
PRFAStates_2022$Late = ifelse(PRFAStates_2022$JulianDay >= avg_min_day & month(PRFAStates_2022$SurveyDate)<10, 1, 0)

### Add rows to States df so that each year has same number of visits
maxNum = max(PRFAStates_2022$Visit) #19
numYear = max(PRFAStates_2022$Year) - min(PRFAStates_2022$Year) + 1 #19
# create a name list, each name repeated maxNum times
all_terr_types <- PRFAStates_2022 %>% group_by(TerritoryName, Area_Type) %>% count() %>% select(-n)
TerritoryRep = all_terr_types[rep(1:nrow(all_terr_types),each = maxNum*numYear),] #length 1007 = 53 terr's x 19 years x 19 surveys per year
# create a empty YearVisit list and then fill with for loop
YearVisit = rep("", each = numYear*maxNum)
for (i in 1:numYear) {
  for (j in 1:maxNum) {
    x = ifelse(j<10, paste(0, j,sep=""), j)
    y = paste(i+2002, x, sep = "v")
    YearVisit[i*j + (maxNum-j)*(i-1)] = y
  }
}
YearVisit = rep(YearVisit, times = length(all_terr_names))
TerritoryRep$YearVisit = YearVisit

PRFAStates_2022_append = left_join(TerritoryRep,PRFAStates_2022, by = c("TerritoryName","YearVisit","Area_Type"))

# Create detection hist df
PRFADetectHistory_2022<-PRFAStates_2022_append %>%
  #Drop columns no longer needed
  select(TerritoryName, Area_Type, YearVisit, State)%>%
  #Pivot table to give detection history
  spread("YearVisit", "State", NA)%>%
  arrange(TerritoryName, Area_Type)
## can probably drop the Area_Type is.na ones since scares observation? Might have to drop when used as covariate

#Export Detection history to excel
write_csv(PRFADetectHistory_2022, path = "Data/PRFADetectHistory_2022.csv", append= FALSE)

#Plot data
PlotStates_2022<- ggplot(PRFAStates_2022, aes(x = JulianDay, y = fct_rev(as_factor(TerritoryName)), color = as.factor(State))) + geom_point()+
  facet_grid(cols = vars(Year))+
  labs(color = "States") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y = element_blank())
PlotStates_2022 #some were done at the end of december as the start of the next season. One October, Sep, Aug surveys in 2013

#save iamge
# png(file="Data/SurveyPlot2022.png",
#     width=1200, height=700)
# PlotStates_2022
# dev.off()
################

###### Dynamic Multi-State Modeling with Unmarked #######
num_Year = max(PRFAStates_2022$Year) - min(PRFAStates_2022$Year) + 1 #19
num_Site = length(unique(PRFAStates_2022$TerritoryName)) #53
num_Rep = max(PRFAStates_2022$Visit) #19

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
umf_w_PEFA <- unmarkedFrameOccuMS(y=PRFADetectHistory_2022[,-c(1,2)], # y: 53 x 361
                           yearlySiteCovs=PEFACovs[3], 
                           numPrimary=num_Year) #19
summary(umf_w_PEFA)

###Formulas
#Initial occupancy
psiformulas <- c('~1','~1') #siteCovs on psi[1] and psi[2]

#Transition probs (using condbinom rn)
umf_w_PEFA@phiOrder$cond_binom #guide to order
phiformulas <- c('~PEFA','~1','~1','~1','~1','~1') # yearly site Covson transition matrix elements. Currently random

#Detection probability
detformulas <- c('~1','~1','~1') # obsCovs on p[1|1], p[1|2], p[2|2]

#Fit model
(fit_w_PEFA <- occuMS(detformulas=detformulas, psiformulas=psiformulas,
               phiformulas=phiformulas, data=umf_w_PEFA, parameterization = "condbinom"))
fit_w_PEFA
coef(fit_w_PEFA)

#Compare fits
modSel(fitList(fit,fit_null)) #example

#BackTransform? - unmarked estimates are on the link-scale (logit for occu() since it uses a logit-link), and the backTransform() function coverts them back to the original scale. You need to specify a type of state or det for occupancy or detection covariates. If you have fit a model with covariates then you need to specify values for them (i.e. what is the probability of occupancy when CovA = X and CovB = Y)
#https://doi90.github.io/lodestar/fitting-occupancy-models-with-unmarked.html#fitting-a-model
#TODO: figure out how to do this for our model
#Method 1:
backTransform(fit_w_PEFA[1]) # set tyep to "det" / "state" 
#Method 2: predicted abundance at specified covariate values 
lc <- linearComb(fm1, c(Int = 1, ufc = 0, trba = 0), type='state') #example
backTransform(lc)

################