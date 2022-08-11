######################################################################################################
## Project: Falcon Occupancy Analysis 202m2
## Script Purpose: Code builds and runs Big Dynamic Multi-State Models with unstacked data with Unmarked using multinomial parameterization
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part5b_CreateUMF.R")
library(unmarked)

###### Static Multi-State Modeling with Unmarked #######

### Get basic model parameters ---------
# Use PRFAStates_202m2 to calculate the following because PRFA202m2_Data_append has NA's
num_Year = max(PRFA202m2_Data$BreedingYear) - min(PRFA202m2_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA202m2_Data$TerritoryName)) #43
num_Rep = max(PRFA202m2_Data$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test



############ modBig01m ########### 
# Rain before, temp before
### Set Formulas 
# State Params (site only)
modBig01m_psiformulas <- c(
  '~ AreaType',
  '~ AreaType') #siteCovs on psi[1] and psi[2]
# Transition params (site or yearly site)
modBig01m_phiformulas <- c('~BreedingYear','~BreedingYear','~BreedingYear','~BreedingYear','~BreedingYear','~BreedingYear')
# umf_unstacked@phiOrder$multinomial
# Detection probability (anything)
modBig01m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01m_fit <- occuMS(detformulas=modBig01m_detformulas, 
                       psiformulas=modBig01m_psiformulas,
                       phiformulas=modBig01m_phiformulas, 
                       data=umf_unstacked, parameterization = "multinomial")
modBig01m_fit
#coef(modBig01m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
modBig01m_psi_predict = predict(modBig01m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
modBig01m_det_predict = predict(modBig01m_fit,type='det') # est. for p11, p12, p22
#lapply(modBig01m_phi_predict,head)
modBig01m_phi_predict = predict(modBig01m_fit,type='phi')


############ modBig02m ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params

modBig02m_psiformulas <- c(
  '~ 1',
  '~ 1') #siteCovs on psi[1] and psi[2]
# Transition params (site or yearly site)
modBig02m_phiformulas <- c('~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
                          '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
                          '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
                          '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays',
                          '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays',
                          '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays')
#umf_unstacked@phiOrder$multinomial
# Detection probability
modBig02m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig02m_fit <- occuMS(detformulas=modBig02m_detformulas, 
                       psiformulas=modBig02m_psiformulas,
                       phiformulas=modBig02m_phiformulas, 
                       data=umf_unstacked, parameterization = "condbinom")
modBig02m_fit
#coef(modBig02m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig02m_psi_predict = predict(modBig02m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig02m_det_predict = predict(modBig02m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02mstacked_phi_predict,head)


############ modBig03m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig03m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig03m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig03m_fit <- occuMS(detformulas=modBig03m_detformulas, 
                               psiformulas=modBig03m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig03m_fit
#coef(modBig03m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig03m_psi_predict = predict(modBig03m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig03m_det_predict = predict(modBig03m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03mstacked_phi_predict,head)



############ modBig04m ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig04m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig04m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig04m_fit <- occuMS(detformulas=modBig04m_detformulas, 
                               psiformulas=modBig04m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig04m_fit
#coef(modBig04m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig04m_psi_predict = predict(modBig04m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig04m_det_predict = predict(modBig04m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04mstacked_phi_predict,head)


############ modBig05m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig05m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig05m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig05m_fit <- occuMS(detformulas=modBig05m_detformulas, 
                               psiformulas=modBig05m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig05m_fit
#coef(modBig05m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig05m_psi_predict = predict(modBig05m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig05m_det_predict = predict(modBig05m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05mstacked_phi_predict,head)



############ modBig06m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig06m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig06m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig06m_fit <- occuMS(detformulas=modBig06m_detformulas, 
                               psiformulas=modBig06m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig06m_fit
#coef(modBig06m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig06m_psi_predict = predict(modBig06m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig06m_det_predict = predict(modBig06m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06mstacked_phi_predict,head)



############ modBig07m ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig07m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig07m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig07m_fit <- occuMS(detformulas=modBig07m_detformulas, 
                               psiformulas=modBig07m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig07m_fit
#coef(modBig07m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig07m_psi_predict = predict(modBig07m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig07m_det_predict = predict(modBig07m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig07m_phi_predict,head)


############ modBig08m ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig08m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig08m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig08m_fit <- occuMS(detformulas=modBig08m_detformulas, 
                               psiformulas=modBig08m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig08m_fit
#coef(modBig08m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig08m_psi_predict = predict(modBig08m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig08m_det_predict = predict(modBig08m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig08m_phi_predict,head)



############ modBig09m ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig09m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig09m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig09m_fit <- occuMS(detformulas=modBig09m_detformulas, 
                               psiformulas=modBig09m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig09m_fit
#coef(modBig09m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig09m_psi_predict = predict(modBig09m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig09m_det_predict = predict(modBig09m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig09m_phi_predict,head)




############ modBig10m ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig10m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig10m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig10m_fit <- occuMS(detformulas=modBig10m_detformulas, 
                               psiformulas=modBig10m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig10m_fit
#coef(modBig10m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig10m_psi_predict = predict(modBig10m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig10m_det_predict = predict(modBig10m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02mstacked_phi_predict,head)





############ modBig11m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig11m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig11m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig11m_fit <- occuMS(detformulas=modBig11m_detformulas, 
                               psiformulas=modBig11m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig11m_fit
#coef(modBig11m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig11m_psi_predict = predict(modBig11m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig11m_det_predict = predict(modBig11m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03mstacked_phi_predict,head)




############ modBig12m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig12m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig12m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig12m_fit <- occuMS(detformulas=modBig12m_detformulas, 
                               psiformulas=modBig12m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig12m_fit
#coef(modBig12m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig12m_psi_predict = predict(modBig12m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
#modBig12m_det_predict = predict(modBig12m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03mstacked_phi_predict,head)
############ modBig13m ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig13m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig13m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig13m_fit <- occuMS(detformulas=modBig13m_detformulas, 
                               psiformulas=modBig13m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig13m_fit
#coef(modBig13m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig13m_psi_predict = predict(modBig13m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig13m_det_predict = predict(modBig13m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04mstacked_phi_predict,head)



############ modBig14m ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig14m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig14m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig14m_fit <- occuMS(detformulas=modBig14m_detformulas, 
                               psiformulas=modBig14m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig14m_fit
#coef(modBig14m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig14m_psi_predict = predict(modBig14m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig14m_det_predict = predict(modBig14m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04mstacked_phi_predict,head)
############ modBig15m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig15m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig15m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig15m_fit <- occuMS(detformulas=modBig15m_detformulas, 
                               psiformulas=modBig15m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig15m_fit
#coef(modBig15m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig15m_psi_predict = predict(modBig15m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig15m_det_predict = predict(modBig15m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05mstacked_phi_predict,head)



############ modBig16m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig16m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig16m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig16m_fit <- occuMS(detformulas=modBig16m_detformulas, 
                               psiformulas=modBig16m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig16m_fit
#coef(modBig16m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig16m_psi_predict = predict(modBig16m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig16m_det_predict = predict(modBig16m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05mstacked_phi_predict,head)
############ modBig17m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig17m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig17m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig17m_fit <- occuMS(detformulas=modBig17m_detformulas, 
                               psiformulas=modBig17m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig17m_fit
#coef(modBig17m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig17m_psi_predict = predict(modBig17m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig17m_det_predict = predict(modBig17m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06mstacked_phi_predict,head)




############ modBig18m ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig18m_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig18m_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig18m_fit <- occuMS(detformulas=modBig18m_detformulas, 
                               psiformulas=modBig18m_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig18m_fit
#coef(modBig18m_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig18m_psi_predict = predict(modBig18m_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig18m_det_predict = predict(modBig18m_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06mstacked_phi_predict,head)


### Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))
fl <- fitList("modBig01m"= modBig01m_fit, "modBig02m"= modBig02m_fit,"modBig03m"= modBig03m_fit,"modBig04m"= modBig04m_fit,"modBig05m"= modBig05m_fit,"modBig06m"= modBig06m_fit,"modBig07m" = modBig07m_fit, "modBig08m" = modBig08m_fit, "modBig09m" = modBig09m_fit, "modBig10m" = modBig10m_fit, "modBig11m" = modBig11m_fit, "modBig12m" = modBig12m_fit, "modBig13m" = modBig13m_fit, "modBig14m" = modBig14m_fit, "modBig15m" = modBig15m_fit, "modBig16m" = modBig16m_fit, "modBig17m" = modBig17m_fit, "modBig18m" = modBig18m_fit)
ms <- modSel(fl)
ms
#toExport <- as(ms, "data.frame")
