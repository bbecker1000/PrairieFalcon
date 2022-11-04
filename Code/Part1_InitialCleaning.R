######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code takes 2022 Raptor Observations dataset, conducts initial filtering & cleaning, and adds necessary columns for subsequent analysis and graphing.
## Libraries and versions used: readxl, tidyverse, lubridate 
#####################################################################################################

##### Prepare workspace #####
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)


###### Import dataset and format #######

data_2019 <- read_excel('Data/qrpt_FalconObservations_2019.xlsx') #2003 - 2021. Used to determine which territories to keep and getting their Area Types
data_2022 <- read_excel('Data/qrpt_RaptorObservations_2022.xlsx') #1984-2021. Main dataset
falcons_2022<- as_tibble(data_2022)

#fix date
falcons_2022 <- falcons_2022 %>%
  mutate(SurveyDate=ymd(SurveyDate))

# Filter out FALSE protocol, and inadequate/unknown surveys
# Only use data from 2003 for occupancy analysis. Data prior to 2003 is presence only (presence of any species not just falcons)
falcons_2022<- falcons_2022 %>%
  filter(Year>2002 
         & IsProtocolSurvey == TRUE 
         & str_detect(`Sample Qualifier`,"^Yes:")) %>% 
  mutate(Detection = ifelse(AdultCount >0|FledglingCount>0|NestlingCount>0|OtherCount>0, 1,0),
         Detection = coalesce(Detection,0), #replace NA's with 0
         JulianDay = as.integer(format(SurveyDate, "%j"))) # add Julian day column

# Change the Count columns to integers
cols.num <- c("AdultCount", "FledglingCount","NestlingCount","OtherCount")
falcons_2022[cols.num] <- sapply(falcons_2022[cols.num],as.integer)

# Get list of territory names from the 2019 dataset which is the same as the names in the .csv file after filtering
terr_name_list = unique(data_2019$Territory_Name)

# Find territories in 2022 but not the other two (8 total)
extra_terr_list = setdiff(unique(falcons_2022$TerritoryName), terr_name_list)
# Find surveys in these territories (64 total)
extra_surveys = falcons_2022 %>% filter(TerritoryName %in% extra_terr_list) 

# Create a df with all unique territories in the 2019 dataset and their corresponding type (core vs. non-core)
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

#Add a DaySinceDec15th column to falcons_2022 (the earliest observation date in December is 15th)
falcons_2022 <- falcons_2022 %>% 
  mutate(DaySinceDec15th = ifelse(month(SurveyDate) == 12, day(SurveyDate)-14, JulianDay+17))

## Remove observations in Augest/September/October, since the monitoring season usually ends by the end of July.
# Remove years before 2007 due to scarce data
# Remove Cortadura Wall, Eagle Rock, Goat/Resurrection due to scarce data
falcons_2022 <- falcons_2022 %>%
  filter(month(SurveyDate)<8 | month(SurveyDate)>11) %>%
  filter(Year > 2006) %>%
  filter(!TerritoryName %in% c("Cortadura Wall", "Eagle Rock", "Goat/Resurrection"))

# Separate prairie and peregrine falcon data
PRFA_2022<-filter(falcons_2022, CommonName=="Prairie Falcon")
PEFA_2022<-filter(falcons_2022, CommonName=="Peregrine Falcon")

# After filtering, safe to assume Count NA == 0
PRFASurveys_2022 <-PRFA_2022 %>%
  mutate(AdultCount = coalesce(AdultCount,0),
         FledglingCount = coalesce(FledglingCount, 0),
         NestlingCount = coalesce(NestlingCount, 0),
         OtherCount = coalesce(OtherCount, 0),
         EggCountCode = coalesce(EggCountCode, "0")) 

#save cleaned and formatted PRFA dataset 
write_csv(PRFASurveys_2022, path = "Data/PRFASurveys_2022.csv", append = FALSE)

