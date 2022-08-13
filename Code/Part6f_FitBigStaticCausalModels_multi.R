######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code builds and runs Big Static Multi-State Models with stacked data with Unmarked using multinomial parameterization
## Libraries and versions used: unmarked
#####################################################################################################


## Thinking about this from CAUSAL INFERENCE perspective
## Run large model based on DAGs that test inclusion and removal of potential 


source("Code/Part5a_CreateStackedUMF.R")
library(unmarked)

###### Static Multi-State Modeling with Unmarked #######

### Get basic STACKED model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
num_Year = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA2022_Data$TerritoryName)) #43
num_Rowm_stacked = num_Site*num_Year #645
num_Rep = max(PRFA2022_Data$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test

############ modBig01m_stacked_CAUSAL ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType + AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType + AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig01m_stacked_CAUSAL <- occuMS(detformulas=modBig01m_stacked_detformulas, 
                               psiformulas=modBig01m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig01m_stacked_CAUSAL
#coef(modBig01m_stacked_fit)


Small_stacked_psiformulas <- c(
  '~BreedingYear + AreaType + PEFA + DecToFebPrecipitation',
  '~BreedingYear + PEFA') #siteCovs on psi[1] and psi[2]

# Detection probability
Small_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
Small_stacked_CAUSAL <- occuMS(detformulas=Small_stacked_detformulas, 
                                   psiformulas=Small_stacked_psiformulas,
                                   data=umf_stacked, parameterization = "multinomial")
Small_stacked_CAUSAL
#coef(modBig01m_stacked_fit)





BEST_MODEL <- modBig01m_stacked_CAUSAL

summary(BEST_MODEL)
BEST_MODEL_psi_predict = predict(BEST_MODEL, type = "psi") # est. for psi1 and psi2 (i.e. R)
BEST_MODEL_det_predict = predict(BEST_MODEL,type='det') # est. for p1, p2, delta






