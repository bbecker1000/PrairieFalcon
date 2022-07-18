######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code builds and runs Dynamic Multi-State Models with unstacked data with Unmarked
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/CreateUMF.R")
library(unmarked)

###### Dynamic Multi-State Modeling with Unmarked #######

### Get basic model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
# Replace PRFAStates_2022 with PRFAStates_2022_pooled if using pooled data
nnum_Year = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA2022_Data$TerritoryName)) #43
num_Rep = max(PRFA2022_Data$Visit)   # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test


############ mod00 ########### 
### Set Formulas ------------
#Initial occupancy
mod00_psiformulas <- c('~1','~1') #siteCovs on psi[1] and psi[2]

#Transition probs (using condbinom rn)
umf_unstacked@phiOrder$cond_binom #guide to order
mod00_phiformulas <- c('~1','~1','~1','~1','~1','~1') # site Or yearly site covs on transition matrix elements. 

#Detection probability
mod00_detformulas <- c('~1','~1','~1') # site, yearly site OR obsCovs on p1, p2 and delta 


### Fit Model -----------
mod00_fit <- occuMS(detformulas=mod00_detformulas, 
                     psiformulas=mod00_psiformulas,
                     phiformulas=mod00_phiformulas, 
                     data=umf_unstacked, parameterization = "condbinom")
mod00_fit
coef(mod00_fit)


### TODO: Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))


### Get predicted parameter values using predict() -----------
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod00_psi_predict = predict(mod00_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod00_phi_predict = predict(mod00_fit,type='phi') # est. for phi0, phi1, phi2, rho0, rho1, rho2
mod00_det_predict = predict(mod00_fit,type='det') # est. for p1, p2, delta

#lapply(mod00_phi_predict,head)
