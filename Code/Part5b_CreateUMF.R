######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code prepares unstacked Unmarked Frame for modeling
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part2_DatasetPrep.R")
source("Code/Part4_GetCovData.R")
library(unmarked)


### Get basic model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
num_Year = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA2022_Data$TerritoryName)) #43
num_Rep = max(PRFA2022_Data$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test

### Format Covariate Data 

## SITE Covariates ------------
# Area Type - factors 0/1
AreaTypeCovs = PRFADetectHistory_2022 %>% 
  select(Area_Type) %>% 
  mutate(Area_Type = case_when(Area_Type=="Core" ~ 1,
                              Area_Type=="Non-core" ~ 0),
         Area_Type = as.factor(Area_Type)) 

# create df
site_cov_df = data.frame(AreaType = AreaTypeCovs$Area_Type)


## YEARLY SITE Covariates -ordered by site-primary period -----------

# Breeding Year
BreedingYear_data = rep(c(2007:2021), time = num_Site)

# PERA - factor 0/1
PEFACovs = pivot_longer(data = PEFACov_2022, cols = starts_with("PEFA"), values_to = "PEFA") #column 3 - yearly site cov: nrows should equal nsite*nyear
PEFACovs$Year = str_sub(PEFACovs$name,-4) 
PEFACovs <- PEFACovs %>% filter(Year>2006)
PEFACovs["PEFA"] <- sapply(PEFACovs["PEFA"],as.factor)
PEFA_data = PEFACovs[["PEFA"]]

# Dec to Feb Total Precipitation 
# -same for all site in each year (vector has the same value every 15 elements)
DecToFebTotal_data = rep(DecToFebTotal$Total, time = num_Site)

# Annual Visitors
annual_visitors_data = rep(annual_visitors$RecreationVisitors, time = num_Site)

##Num days heavy rainfall (>8mm/day) during breeding season (03/15-06/15)
days_heavy_rainfall_data = rep(days_heavy_rainfall$daysHeavyRainfall, time=num_Site)

# Heat Degree Days - numeric
HDD_data = rep(HDD$YearlyHDD, time = num_Site)

# Nestling Period Hot Days - numeric
HotDays_data = rep(HotDays$n, time = num_Site)

# Nestling Period Cold Days - numeric
ColdDays_data = rep(ColdDays$n, time = num_Site)

# Short drought
ShortDrought_data = rep(ShortDroughtAvgDecToFeb$avg, time = num_Site)

# Long drought
LongDrought_data = rep(LongDroughtYearlyAverage$avg, time = num_Site)


# create df
yearly_site_cov_df = data.frame(BreedingYear = BreedingYear_data-2006,
                                PEFA = as.factor(PEFA_data),
                                DecToFebPrecipitation = scale(DecToFebTotal_data),
                                AnnualVisitors= scale(log(annual_visitors_data)),
                                HDD = scale(HDD_data),
                                HotDays = scale(HotDays_data),
                                ColdDays = scale(ColdDays_data),
                                HeavyRain = scale(days_heavy_rainfall_data),
                                ShortDrought = scale(ShortDrought_data),
                                LongDrought = scale(LongDrought_data))



## OBSERVATION Covariates - ordered by site-primary period-observatio ---------------
# Late Effect --- 0/1 as factors
LateEffectCovs <- PRFA2022_Data_append %>% 
  arrange(TerritoryName, BreedingYear, Visit) %>%
  mutate(LateEffect = as.factor(LateEffect))
# create df
obs_cov_df = LateEffectCovs['LateEffect']



### Create Unmarked Frame -------------
# Basic format:
# umf <- unmarkedFrameOccuMS(y=y, siteCovs=site_covs,
#                            obsCovs=obs_covs,
#                            yearlySiteCovs=yearly_site_covs,
#                            numPrimary=3)
umf_unstacked <- unmarkedFrameOccuMS(y=PRFADetectHistory_2022[,-c(1,2)],
                                  siteCovs=site_cov_df,
                                  obsCovs=obs_cov_df,
                                  yearlySiteCovs=yearly_site_cov_df,
                                  numPrimary=num_Year)
summary(umf_unstacked)

