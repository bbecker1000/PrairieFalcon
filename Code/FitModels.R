######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code runs Dynamic Multi-State Models with Unmarked
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/DatasetPrep.R")
#source("GetWeatherData.R")
library(unmarked)

###### Dynamic Multi-State Modeling with Unmarked #######

### Get basic model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
# Replace PRFAStates_2022 with PRFAStates_2022_pooled if using pooled data
num_Year = max(PRFAStates_2022$BreedingYear) - min(PRFAStates_2022$BreedingYear) + 1 #15
num_Site = length(unique(PRFAStates_2022$TerritoryName)) #43
num_Rep = max(PRFAStates_2022$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test

### Format Covariate Data ------------
# Get PERA Covs into right format
PEFACovs = pivot_longer(data = PEFACov_2022, cols = starts_with("PEFA"), values_to = "PEFA") #column 3 - yearly site cov: nrows should equal nsite*nyear

PEFACovs["PEFA"] <- sapply(PEFACovs["PEFA"],as.integer)

# Get Area Type Covs into right format
AreaTypeCovs = all_terr_types %>% 
  mutate(AreaType = case_when(Area_Type=="Core" ~ 1,
                              Area_Type=="Non-core" ~ 0)) 

# Get Late Effect Covs into right format
LateEffectCovs <- PRFA2022_Data_append %>% 
  arrange(BreedingYear, Visit) %>%
  select(LateEffect)
LateEffectCovs['LateEffect'] <- sapply(LateEffectCovs['LateEffect'], as.integer)


### Create Unmarked Frame -------------
# Basic format:
# umf <- unmarkedFrameOccuMS(y=y, siteCovs=site_covs,
#                            obsCovs=obs_covs,
#                            yearlySiteCovs=yearly_site_covs,
#                            numPrimary=3)
umf_w_covs <- unmarkedFrameOccuMS(y=PRFADetectHistory_2022[,-c(1,2)],
                                  siteCovs=AreaTypeCovs[3],
                                  obsCovs=LateEffectCovs[3],
                                  yearlySiteCovs=PEFACovs[3],
                                  numPrimary=num_Year)
summary(umf_w_covs)


### Set Formulas ------------
#Initial occupancy
psiformulas <- c('~1','~1') #siteCovs on psi[1] and psi[2]

#Transition probs (using condbinom rn)
umf_w_covs@phiOrder$cond_binom #guide to order
phiformulas <- c('~1','~1','~1','~1','~1','~1') # yearly site Covs on transition matrix elements. Currently random

#Detection probability
detformulas <- c('~1','~1','~1') # obsCovs on p1, p2 and delta 


### Fit Model -----------
fit_w_covs <- occuMS(detformulas=detformulas, 
                     psiformulas=psiformulas,
                     phiformulas=phiformulas, 
                     data=umf_w_covs, parameterization = "condbinom")
coef(fit_w_covs)


### TODO: Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))


### Get predicted parameter values using predict() -----------
# See Agenda 06-27 ToDO #3 for detailed descriptions
psi_predict = predict(fit_w_covs, type = "psi") # est. for psi1 and psi2 (i.e. R)
phi_predict = predict(fit_w_covs,type='phi') # est. for phi0, phi1, phi2, rho0, rho1, rho2. 
det_predict = predict(fit_w_covs,type='det') # est. for p1, p2, delta

#lapply(phi_predict,head)
