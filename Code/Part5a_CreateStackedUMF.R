######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code prepares STACKED Unmarked Frame for modeling
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/DatasetPrep.R")
source("Code/GetWeatherData.R")
library(unmarked)

### Get basic STACKED model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
num_Year = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA2022_Data$TerritoryName)) #43

num_Row_stacked = num_Site*num_Year #645
num_Rep = max(PRFA2022_Data$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test


### Format STACKED Covariate Data 

## SITE Covariates ------------  645 rows

# Breeding Year - integer
BreedingYear_data_stacked = rep(c(2007:2021), time = num_Site)
  
# Area Type - factors 0/1
AreaTypeCovs_stacked = PRFADetectHistory_2022_stacked %>% 
  select(Area_Type) %>% 
  mutate(Area_Type = case_when(Area_Type=="Core" ~ 1,
                               Area_Type=="Non-core" ~ 0),
         Area_Type = as.factor(Area_Type)) 

# PERA - factor 0/1
PEFACovs = pivot_longer(data = PEFACov_2022, cols = starts_with("PEFA"), values_to = "PEFA") #column 3 - yearly site cov: nrows should equal nsite*nyear
PEFACovs$Year = str_sub(PEFACovs$name,-4) 
PEFACovs <- PEFACovs %>% filter(Year>2006)
PEFACovs["PEFA"] <- sapply(PEFACovs["PEFA"],as.factor)

# Dec to Feb Total Precipitation - continuous
# same for all site in each year (vector has the same value every 15 elements)
DecToFebTotal_data = rep(DecToFebTotal$Total, time = num_Site)

# Annual Visitors - numeric
annual_visitors_data = rep(annual_visitors$RecreationVisitors, time = num_Site)

# Extreme Events - factor 0/1
extreme_events_data = rep(PINN_extreme_events$ExtremeEvent, time = num_Site)


# create df
site_cov_df_stacked = data.frame(BreedingYear = BreedingYear_data_stacked - 2006,
                                 AreaType = AreaTypeCovs_stacked$Area_Type,
                                 PEFA = as.factor(PEFACovs$PEFA),
                                 DecToFebPrecipitation = scale(DecToFebTotal_data),
                                 AnnualVisitors= scale(annual_visitors_data),
                                 ExtremeEvents = as.factor(extreme_events_data))


## OBSERVATION Covariates - ordered by site-observation (territory-year-obs) ---------------
# Late Effect --- 0/1 as factors
LateEffectCovs <- PRFA2022_Data_append %>% 
  arrange(TerritoryName, BreedingYear, Visit) %>%
  mutate(LateEffect = as.factor(LateEffect))
# create df
obs_cov_df_stacked = LateEffectCovs['LateEffect']



### Create STACKED Unmarked Frame -------------
# Basic format:
# umf <- unmarkedFrameOccuMS(y=y, siteCovs=site_covs,
#                            obsCovs=obs_covs,
#                            yearlySiteCovs=yearly_site_covs,
#                            numPrimary=3)
umf_stacked <- unmarkedFrameOccuMS(y=PRFADetectHistory_2022_stacked[,-c(1,2)],
                                  siteCovs=site_cov_df_stacked,
                                  obsCovs=obs_cov_df_stacked)
summary(umf_stacked)
