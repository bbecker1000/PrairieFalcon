######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code builds and runs Static Multi-State Models with stacked data with Unmarked
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part5a_CreateStackedUMF.R")
library(unmarked)

###### Static Multi-State Modeling with Unmarked #######

### Get basic STACKED model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
num_Year = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA2022_Data$TerritoryName)) #43
num_Row_stacked = num_Site*num_Year #645
num_Rep = max(PRFA2022_Data$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test



############ mod00_stacked ########### 
### Set Formulas 
# State Params
mod00_stacked_psiformulas <- c('~1','~1') #siteCovs on psi[1] and psi[2]

# Detection probability
mod00_stacked_detformulas <- c('~1','~1','~1') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod00_stacked_fit <- occuMS(detformulas=mod00_stacked_detformulas, 
                    psiformulas=mod00_stacked_psiformulas,
                    data=umf_stacked, parameterization = "condbinom")
mod00_stacked_fit
coef(mod00_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod00_stacked_psi_predict = predict(mod00_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod00_stacked_det_predict = predict(mod00_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod00_stacked_phi_predict,head)





############ mod00a_stacked ########### 
### Set Formulas 
# State Params
mod00a_stacked_psiformulas <- c('~BreedingYear','~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod00a_stacked_detformulas <- c('~1','~1','~1') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod00a_stacked_fit <- occuMS(detformulas=mod00a_stacked_detformulas, 
                            psiformulas=mod00a_stacked_psiformulas,
                            data=umf_stacked, parameterization = "condbinom")
mod00a_stacked_fit
#coef(mod00a_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod00a_stacked_psi_predict = predict(mod00a_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod00a_stacked_det_predict = predict(mod00a_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod00a_stacked_phi_predict,head)






############ mod00b_stacked ########### 
### Set Formulas 
# State Params
mod00b_stacked_psiformulas <- c('~DecToFebPrecipitation', '~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod00b_stacked_detformulas <- c('~1','~1','~1') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod00b_stacked_fit <- occuMS(detformulas=mod00b_stacked_detformulas, 
                            psiformulas=mod00b_stacked_psiformulas,
                            data=umf_stacked, parameterization = "condbinom")
mod00b_stacked_fit
#coef(mod00b_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod00b_stacked_psi_predict = predict(mod00b_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod00b_stacked_det_predict = predict(mod00b_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod00b_stacked_phi_predict,head)


############ mod01_stacked ########### 
### Set Formulas 
# State Params
mod01_stacked_psiformulas <- c('~BreedingYear','~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod01_stacked_detformulas <- c('~BreedingYear','~BreedingYear','~BreedingYear') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod01_stacked_fit <- occuMS(detformulas=mod01_stacked_detformulas, 
                             psiformulas=mod01_stacked_psiformulas,
                             data=umf_stacked, parameterization = "condbinom")
mod01_stacked_fit
#coef(mod01_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod01_stacked_psi_predict = predict(mod01_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod01_stacked_det_predict = predict(mod01_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod01_stacked_phi_predict,head)


############ mod02a_stacked ########### 
### Set Formulas 
# State Params
mod02a_stacked_psiformulas <- c('~BreedingYear','~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod02a_stacked_detformulas <- c('~BreedingYear','~BreedingYear+LateEffect','~BreedingYear+LateEffect') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod02a_stacked_fit <- occuMS(detformulas=mod02a_stacked_detformulas, 
                            psiformulas=mod02a_stacked_psiformulas,
                            data=umf_stacked, parameterization = "condbinom")
mod02a_stacked_fit
#coef(mod02a_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod02a_stacked_psi_predict = predict(mod02a_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod02a_stacked_det_predict = predict(mod02a_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod02a_stacked_phi_predict,head)



############ mod02b_stacked ########### 
### Set Formulas 
# State Params
mod02b_stacked_psiformulas <- c('~BreedingYear','~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod02b_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod02b_stacked_fit <- occuMS(detformulas=mod02b_stacked_detformulas, 
                            psiformulas=mod02b_stacked_psiformulas,
                            data=umf_stacked, parameterization = "condbinom")
mod02b_stacked_fit
#coef(mod02b_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod02b_stacked_psi_predict = predict(mod02b_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod02b_stacked_det_predict = predict(mod02b_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod02b_stacked_phi_predict,head)




############ mod03a_stacked ########### 
### Set Formulas 
# State Params
mod03a_stacked_psiformulas <- c('~BreedingYear + AreaType*AnnualVisitors','~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod03a_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod03a_stacked_fit <- occuMS(detformulas=mod03a_stacked_detformulas, 
                             psiformulas=mod03a_stacked_psiformulas,
                             data=umf_stacked, parameterization = "condbinom")
mod03a_stacked_fit
#coef(mod03a_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod03a_stacked_psi_predict = predict(mod03a_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod03a_stacked_det_predict = predict(mod03a_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod03a_stacked_phi_predict,head)


############ mod03b_stacked ########### 
### Set Formulas 
# State Params
mod03b_stacked_psiformulas <- c('~BreedingYear','~BreedingYear+ AreaType*AnnualVisitors') #siteCovs on psi[1] and psi[2]

# Detection probability
mod03b_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod03b_stacked_fit <- occuMS(detformulas=mod03b_stacked_detformulas, 
                             psiformulas=mod03b_stacked_psiformulas,
                             data=umf_stacked, parameterization = "condbinom")
mod03b_stacked_fit
#coef(mod03b_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod03b_stacked_psi_predict = predict(mod03b_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod03b_stacked_det_predict = predict(mod03b_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod03b_stacked_phi_predict,head)


############ mod04a_stacked ########### 
### Set Formulas 
# State Params
mod04a_stacked_psiformulas <- c('~BreedingYear+PEFA','~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
mod04a_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod04a_stacked_fit <- occuMS(detformulas=mod04a_stacked_detformulas, 
                             psiformulas=mod04a_stacked_psiformulas,
                             data=umf_stacked, parameterization = "condbinom")
mod04a_stacked_fit
#coef(mod04a_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod04a_stacked_psi_predict = predict(mod04a_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod04a_stacked_det_predict = predict(mod04a_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod04a_stacked_phi_predict,head)




############ mod04b_stacked ########### 
### Set Formulas 
# State Params
mod04b_stacked_psiformulas <- c('~BreedingYear','~BreedingYear+PEFA') #siteCovs on psi[1] and psi[2]

# Detection probability
mod04b_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 


### Fit Model 
mod04b_stacked_fit <- occuMS(detformulas=mod04b_stacked_detformulas, 
                             psiformulas=mod04b_stacked_psiformulas,
                             data=umf_stacked, parameterization = "condbinom")
mod04b_stacked_fit
#coef(mod04b_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
mod04b_stacked_psi_predict = predict(mod04b_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
mod04b_stacked_det_predict = predict(mod04b_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(mod04b_stacked_phi_predict,head)








### TODO: Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))
# fl <- fitList("00"= mod00_stacked_fit, "00a" = mod00a_stacked_fit)
# fl
# ms <- modSel(fl)
# ms
# posteriorSamples(modBig_stacked_fit, type = "psi")
