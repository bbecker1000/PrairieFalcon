######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code builds and runs Big Dynamic Multi-State Models with unstacked data with Unmarked
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part5b_CreateUMF.R")
library(unmarked)

###### Static Multi-State Modeling with Unmarked #######

### Get basic model parameters ---------
# Use PRFAStates_2022 to calculate the following because PRFA2022_Data_append has NA's
num_Year = max(PRFA2022_Data$BreedingYear) - min(PRFA2022_Data$BreedingYear) + 1 #15
num_Site = length(unique(PRFA2022_Data$TerritoryName)) #43
num_Rep = max(PRFA2022_Data$Visit)  # max number of visits within each year
# c(num_Year,num_Site,num_Rep) #test



############ modBig01 ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01_psiformulas <- c(
  '~ AreaType',
  '~ AreaType') #siteCovs on psi[1] and psi[2]
modBig01_phiformulas <- c('~1','~1','~1','~1','~1','~1')
# Detection probability
modBig01_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01_fit <- occuMS(detformulas=modBig01_detformulas, 
                       psiformulas=modBig01_psiformulas,
                       phiformulas=modBig01_phiformulas, 
                       data=umf_unstacked, parameterization = "condbinom")
modBig01_fit
#coef(modBig01_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
modBig01_psi_predict = predict(modBig01_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
modBig01_det_predict = predict(modBig01_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01_phi_predict,head)


############ modBig01a_noYear ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01a_noYear_psiformulas <- c(
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01a_noYear_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01a_noYear_fit <- occuMS(detformulas=modBig01a_noYear_detformulas, 
                                       psiformulas=modBig01a_noYear_psiformulas,
                                       data=umf, parameterization = "condbinom")
modBig01a_noYear_fit
#coef(modBig01a_noYear_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01a_noYear_psi_predict = predict(modBig01a_noYear_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01a_noYear_det_predict = predict(modBig01a_noYear_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01a__noYear_phi_predict,head)

# ## Find slope
# logit = function(x) {return(x/(1-x))}
# df = data.frame(Psi = logit(modBig01a_noYear_psi_predict$psi$Predicted),
#                 R = logit(modBig01a_noYear_psi_predict$R$Predicted),
#                 BreedingYear = BreedingYear_data - 2006)
# 
# lm.psi= lm(formula = Psi ~ BreedingYear,
#          data = df)
# coef(lm.psi)
# 
# lm.R= lm(formula = R ~ BreedingYear,
#            data = df)
# coef(lm.R)


############ modBig01b_onlyYear ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01b_onlyYear_psiformulas <- c(
  '~BreedingYear',
  '~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01b_onlyYear_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01b_onlyYear_fit <- occuMS(detformulas=modBig01b_onlyYear_detformulas, 
                                         psiformulas=modBig01b_onlyYear_psiformulas,
                                         data=umf, parameterization = "condbinom")
modBig01b_onlyYear_fit
#coef(modBig01b_onlyYear_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01b_onlyYear_psi_predict = predict(modBig01b_onlyYear_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01b_onlyYear_det_predict = predict(modBig01b_onlyYear_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01b_onlyYear_phi_predict,head)


############ modBig02 ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig02_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig02_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig02_fit <- occuMS(detformulas=modBig02_detformulas, 
                               psiformulas=modBig02_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig02_fit
#coef(modBig02_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig02_psi_predict = predict(modBig02_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig02_det_predict = predict(modBig02_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02stacked_phi_predict,head)


############ modBig03 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig03_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig03_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig03_fit <- occuMS(detformulas=modBig03_detformulas, 
                               psiformulas=modBig03_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig03_fit
#coef(modBig03_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig03_psi_predict = predict(modBig03_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig03_det_predict = predict(modBig03_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)



############ modBig04 ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig04_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig04_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig04_fit <- occuMS(detformulas=modBig04_detformulas, 
                               psiformulas=modBig04_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig04_fit
#coef(modBig04_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig04_psi_predict = predict(modBig04_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig04_det_predict = predict(modBig04_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)


############ modBig05 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig05_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig05_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig05_fit <- occuMS(detformulas=modBig05_detformulas, 
                               psiformulas=modBig05_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig05_fit
#coef(modBig05_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig05_psi_predict = predict(modBig05_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig05_det_predict = predict(modBig05_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)



############ modBig06 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig06_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig06_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig06_fit <- occuMS(detformulas=modBig06_detformulas, 
                               psiformulas=modBig06_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig06_fit
#coef(modBig06_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig06_psi_predict = predict(modBig06_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig06_det_predict = predict(modBig06_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)



############ modBig07 ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig07_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig07_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig07_fit <- occuMS(detformulas=modBig07_detformulas, 
                               psiformulas=modBig07_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig07_fit
#coef(modBig07_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig07_psi_predict = predict(modBig07_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig07_det_predict = predict(modBig07_fit,type='det') # est. for p1, p2, delta
#lapply(modBig07_phi_predict,head)


############ modBig08 ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig08_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig08_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig08_fit <- occuMS(detformulas=modBig08_detformulas, 
                               psiformulas=modBig08_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig08_fit
#coef(modBig08_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig08_psi_predict = predict(modBig08_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig08_det_predict = predict(modBig08_fit,type='det') # est. for p1, p2, delta
#lapply(modBig08_phi_predict,head)



############ modBig09 ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig09_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig09_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig09_fit <- occuMS(detformulas=modBig09_detformulas, 
                               psiformulas=modBig09_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig09_fit
#coef(modBig09_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig09_psi_predict = predict(modBig09_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig09_det_predict = predict(modBig09_fit,type='det') # est. for p1, p2, delta
#lapply(modBig09_phi_predict,head)




############ modBig10 ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig10_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig10_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig10_fit <- occuMS(detformulas=modBig10_detformulas, 
                               psiformulas=modBig10_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig10_fit
#coef(modBig10_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig10_psi_predict = predict(modBig10_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig10_det_predict = predict(modBig10_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02stacked_phi_predict,head)





############ modBig11 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig11_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig11_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig11_fit <- occuMS(detformulas=modBig11_detformulas, 
                               psiformulas=modBig11_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig11_fit
#coef(modBig11_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig11_psi_predict = predict(modBig11_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig11_det_predict = predict(modBig11_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)




############ modBig12 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig12_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig12_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig12_fit <- occuMS(detformulas=modBig12_detformulas, 
                               psiformulas=modBig12_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig12_fit
#coef(modBig12_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig12_psi_predict = predict(modBig12_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
#modBig12_det_predict = predict(modBig12_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)
############ modBig13 ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig13_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig13_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig13_fit <- occuMS(detformulas=modBig13_detformulas, 
                               psiformulas=modBig13_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig13_fit
#coef(modBig13_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig13_psi_predict = predict(modBig13_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig13_det_predict = predict(modBig13_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)



############ modBig14 ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig14_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~ AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig14_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig14_fit <- occuMS(detformulas=modBig14_detformulas, 
                               psiformulas=modBig14_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig14_fit
#coef(modBig14_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig14_psi_predict = predict(modBig14_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig14_det_predict = predict(modBig14_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)
############ modBig15 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig15_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig15_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig15_fit <- occuMS(detformulas=modBig15_detformulas, 
                               psiformulas=modBig15_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig15_fit
#coef(modBig15_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig15_psi_predict = predict(modBig15_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig15_det_predict = predict(modBig15_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)



############ modBig16 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig16_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig16_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig16_fit <- occuMS(detformulas=modBig16_detformulas, 
                               psiformulas=modBig16_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig16_fit
#coef(modBig16_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig16_psi_predict = predict(modBig16_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig16_det_predict = predict(modBig16_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)
############ modBig17 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig17_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig17_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig17_fit <- occuMS(detformulas=modBig17_detformulas, 
                               psiformulas=modBig17_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig17_fit
#coef(modBig17_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig17_psi_predict = predict(modBig17_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig17_det_predict = predict(modBig17_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)




############ modBig18 ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig18_psiformulas <- c(
  '~ AreaType*AnnualVisitors + PEFA + LongDrought',
  '~ AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig18_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig18_fit <- occuMS(detformulas=modBig18_detformulas, 
                               psiformulas=modBig18_psiformulas,
                               data=umf, parameterization = "condbinom")
modBig18_fit
#coef(modBig18_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig18_psi_predict = predict(modBig18_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig18_det_predict = predict(modBig18_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)


### Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))
fl <- fitList("modBig01"= modBig01_fit, "modBig02"= modBig02_fit,"modBig03"= modBig03_fit,"modBig04"= modBig04_fit,"modBig05"= modBig05_fit,"modBig06"= modBig06_fit,"modBig07" = modBig07_fit, "modBig08" = modBig08_fit, "modBig09" = modBig09_fit, "modBig10" = modBig10_fit, "modBig11" = modBig11_fit, "modBig12" = modBig12_fit, "modBig13" = modBig13_fit, "modBig14" = modBig14_fit, "modBig15" = modBig15_fit, "modBig16" = modBig16_fit, "modBig17" = modBig17_fit, "modBig18" = modBig18_fit)
ms <- modSel(fl)
ms
#toExport <- as(ms, "data.frame")
