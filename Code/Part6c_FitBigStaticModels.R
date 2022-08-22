######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code builds and runs Big Static Multi-State Models with stacked data with Unmarked using coniditonal binomial parameterization
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



############ modBig01_stacked ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01_stacked_fit <- occuMS(detformulas=modBig01_stacked_detformulas, 
                             psiformulas=modBig01_stacked_psiformulas,
                             data=umf_stacked, parameterization = "condbinom")
modBig01_stacked_fit
#coef(modBig01_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01_stacked_psi_predict = predict(modBig01_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01_stacked_det_predict = predict(modBig01_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01_stacked_phi_predict,head)
# predicted_state_params = data.frame(
#   psi = modBig01_stacked_psi_predict$psi$Predicted,
#   R = modBig01_stacked_psi_predict$R$Predicted) %>% 
#   mutate(psi1 = psi*(1-R),
#          psi2 = psi*R)
# predicted_det_params = data.frame(
#   p1 = modBig01_stacked_det_predict$`p[1]`$Predicted,
#   p2 = modBig01_stacked_det_predict$`p[2]`$Predicted,
#   delta = modBig01_stacked_det_predict$delta$Predicted) %>% 
#   mutate(p11 = p1,
#          p12 = p2*(1-delta),
#          p22 = p2*delta)


############ modBig01a_stacked_noYear ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01a_stacked_noYear_psiformulas <- c(
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01a_stacked_noYear_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01a_stacked_noYear_fit <- occuMS(detformulas=modBig01a_stacked_noYear_detformulas, 
                               psiformulas=modBig01a_stacked_noYear_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig01a_stacked_noYear_fit
#coef(modBig01a_stacked_noYear_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01a_stacked_noYear_psi_predict = predict(modBig01a_stacked_noYear_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01a_stacked_noYear_det_predict = predict(modBig01a_stacked_noYear_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01a_stacked__noYear_phi_predict,head)

# ## Find slope
# logit = function(x) {return(x/(1-x))}
# df = data.frame(Psi = logit(modBig01a_stacked_noYear_psi_predict$psi$Predicted),
#                 R = logit(modBig01a_stacked_noYear_psi_predict$R$Predicted),
#                 BreedingYear = BreedingYear_data_stacked - 2006)
# 
# lm.psi= lm(formula = Psi ~ BreedingYear,
#          data = df)
# coef(lm.psi)
# 
# lm.R= lm(formula = R ~ BreedingYear,
#            data = df)
# coef(lm.R)


############ modBig01b_stacked_onlyYear ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01b_stacked_onlyYear_psiformulas <- c(
  '~BreedingYear',
  '~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01b_stacked_onlyYear_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig01b_stacked_onlyYear_fit <- occuMS(detformulas=modBig01b_stacked_onlyYear_detformulas, 
                                       psiformulas=modBig01b_stacked_onlyYear_psiformulas,
                                       data=umf_stacked, parameterization = "condbinom")
modBig01b_stacked_onlyYear_fit
#coef(modBig01b_stacked_onlyYear_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01b_stacked_onlyYear_psi_predict = predict(modBig01b_stacked_onlyYear_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01b_stacked_onlyYear_det_predict = predict(modBig01b_stacked_onlyYear_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01b_stacked_onlyYear_phi_predict,head)


############ modBig02_stacked ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig02_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig02_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig02_stacked_fit <- occuMS(detformulas=modBig02_stacked_detformulas, 
                               psiformulas=modBig02_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig02_stacked_fit
#coef(modBig02_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig02_stacked_psi_predict = predict(modBig02_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig02_stacked_det_predict = predict(modBig02_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02stacked_phi_predict,head)


############ modBig03_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig03_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig03_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig03_stacked_fit <- occuMS(detformulas=modBig03_stacked_detformulas, 
                               psiformulas=modBig03_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig03_stacked_fit
#coef(modBig03_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig03_stacked_psi_predict = predict(modBig03_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig03_stacked_det_predict = predict(modBig03_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)



############ modBig04_stacked ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig04_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig04_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig04_stacked_fit <- occuMS(detformulas=modBig04_stacked_detformulas, 
                               psiformulas=modBig04_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig04_stacked_fit
#coef(modBig04_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig04_stacked_psi_predict = predict(modBig04_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig04_stacked_det_predict = predict(modBig04_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)


############ modBig05_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig05_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig05_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig05_stacked_fit <- occuMS(detformulas=modBig05_stacked_detformulas, 
                               psiformulas=modBig05_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig05_stacked_fit
#coef(modBig05_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig05_stacked_psi_predict = predict(modBig05_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig05_stacked_det_predict = predict(modBig05_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)



############ modBig06_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig06_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig06_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig06_stacked_fit <- occuMS(detformulas=modBig06_stacked_detformulas, 
                               psiformulas=modBig06_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig06_stacked_fit
#coef(modBig06_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig06_stacked_psi_predict = predict(modBig06_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig06_stacked_det_predict = predict(modBig06_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)



############ modBig07_stacked ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig07_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig07_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig07_stacked_fit <- occuMS(detformulas=modBig07_stacked_detformulas, 
                               psiformulas=modBig07_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig07_stacked_fit
#coef(modBig07_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig07_stacked_psi_predict = predict(modBig07_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig07_stacked_det_predict = predict(modBig07_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig07_stacked_phi_predict,head)


############ modBig08_stacked ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig08_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig08_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig08_stacked_fit <- occuMS(detformulas=modBig08_stacked_detformulas, 
                               psiformulas=modBig08_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig08_stacked_fit
#coef(modBig08_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig08_stacked_psi_predict = predict(modBig08_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig08_stacked_det_predict = predict(modBig08_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig08_stacked_phi_predict,head)



############ modBig09_stacked ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig09_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig09_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig09_stacked_fit <- occuMS(detformulas=modBig09_stacked_detformulas, 
                               psiformulas=modBig09_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig09_stacked_fit
#coef(modBig09_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig09_stacked_psi_predict = predict(modBig09_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig09_stacked_det_predict = predict(modBig09_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig09_stacked_phi_predict,head)




############ modBig10_stacked ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig10_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig10_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig10_stacked_fit <- occuMS(detformulas=modBig10_stacked_detformulas, 
                               psiformulas=modBig10_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig10_stacked_fit
#coef(modBig10_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig10_stacked_psi_predict = predict(modBig10_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig10_stacked_det_predict = predict(modBig10_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02stacked_phi_predict,head)





############ modBig11_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig11_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig11_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig11_stacked_fit <- occuMS(detformulas=modBig11_stacked_detformulas, 
                               psiformulas=modBig11_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig11_stacked_fit
#coef(modBig11_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig11_stacked_psi_predict = predict(modBig11_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig11_stacked_det_predict = predict(modBig11_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)




############ modBig12_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig12_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig12_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig12_stacked_fit <- occuMS(detformulas=modBig12_stacked_detformulas, 
                               psiformulas=modBig12_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig12_stacked_fit
#coef(modBig12_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig12_stacked_psi_predict = predict(modBig12_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
#modBig12_stacked_det_predict = predict(modBig12_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)
############ modBig13_stacked ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig13_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig13_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig13_stacked_fit <- occuMS(detformulas=modBig13_stacked_detformulas, 
                               psiformulas=modBig13_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig13_stacked_fit
#coef(modBig13_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig13_stacked_psi_predict = predict(modBig13_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig13_stacked_det_predict = predict(modBig13_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)



############ modBig14_stacked ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig14_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig14_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig14_stacked_fit <- occuMS(detformulas=modBig14_stacked_detformulas, 
                               psiformulas=modBig14_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig14_stacked_fit
#coef(modBig14_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig14_stacked_psi_predict = predict(modBig14_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig14_stacked_det_predict = predict(modBig14_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)
############ modBig15_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig15_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig15_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig15_stacked_fit <- occuMS(detformulas=modBig15_stacked_detformulas, 
                               psiformulas=modBig15_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig15_stacked_fit
#coef(modBig15_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig15_stacked_psi_predict = predict(modBig15_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig15_stacked_det_predict = predict(modBig15_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)



############ modBig16_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig16_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig16_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig16_stacked_fit <- occuMS(detformulas=modBig16_stacked_detformulas, 
                               psiformulas=modBig16_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig16_stacked_fit
#coef(modBig16_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig16_stacked_psi_predict = predict(modBig16_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig16_stacked_det_predict = predict(modBig16_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)
############ modBig17_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig17_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig17_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig17_stacked_fit <- occuMS(detformulas=modBig17_stacked_detformulas, 
                               psiformulas=modBig17_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig17_stacked_fit
#coef(modBig17_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig17_stacked_psi_predict = predict(modBig17_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig17_stacked_det_predict = predict(modBig17_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)




############ modBig18_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig18_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig18_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p1, p2 and delta 

### Fit Model 
modBig18_stacked_fit <- occuMS(detformulas=modBig18_stacked_detformulas, 
                               psiformulas=modBig18_stacked_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig18_stacked_fit
#coef(modBig18_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig18_stacked_psi_predict = predict(modBig18_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig18_stacked_det_predict = predict(modBig18_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)


### Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))
fl <- fitList("modBig01_stacked"= modBig01_stacked_fit, "modBig02_stacked"= modBig02_stacked_fit,"modBig03_stacked"= modBig03_stacked_fit,"modBig04_stacked"= modBig04_stacked_fit,"modBig05_stacked"= modBig05_stacked_fit,"modBig06_stacked"= modBig06_stacked_fit,"modBig07_stacked" = modBig07_stacked_fit, "modBig08_stacked" = modBig08_stacked_fit, "modBig09_stacked" = modBig09_stacked_fit, "modBig10_stacked" = modBig10_stacked_fit, "modBig11_stacked" = modBig11_stacked_fit, "modBig12_stacked" = modBig12_stacked_fit, "modBig13_stacked" = modBig13_stacked_fit, "modBig14_stacked" = modBig14_stacked_fit, "modBig15_stacked" = modBig15_stacked_fit, "modBig16_stacked" = modBig16_stacked_fit, "modBig17_stacked" = modBig17_stacked_fit, "modBig18_stacked" = modBig18_stacked_fit)
ms <- modSel(fl)
ms
#toExport <- as(ms, "data.frame")
