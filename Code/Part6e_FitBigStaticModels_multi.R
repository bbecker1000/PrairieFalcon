######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code builds and runs Big Static Multi-State Models with stacked data with Unmarked using multinomial parameterization
## Libraries and versions used: unmarked
#####################################################################################################

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



############ modBig01m_stacked ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig01m_stacked_fit <- occuMS(detformulas=modBig01m_stacked_detformulas, 
                               psiformulas=modBig01m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig01m_stacked_fit
#coef(modBig01m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01m_stacked_psi_predict = predict(modBig01m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01m_stacked_det_predict = predict(modBig01m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01m_stacked_phi_predict,head)


############ modBig01am_stacked_noYear ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01am_stacked_noYear_psiformulas <- c(
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01am_stacked_noYear_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig01am_stacked_noYear_fit <- occuMS(detformulas=modBig01am_stacked_noYear_detformulas, 
                                       psiformulas=modBig01am_stacked_noYear_psiformulas,
                                       data=umf_stacked, parameterization = "multinomial")
modBig01am_stacked_noYear_fit
#coef(modBig01am_stacked_noYear_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01am_stacked_noYear_psi_predict = predict(modBig01am_stacked_noYear_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01am_stacked_noYear_det_predict = predict(modBig01am_stacked_noYear_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01am_stacked__noYear_phi_predict,head)

# ## Find slope
# logit = function(x) {return(x/(1-x))}
# df = data.frame(Psi = logit(modBig01am_stacked_noYear_psi_predict$psi$Predicted),
#                 R = logit(modBig01am_stacked_noYear_psi_predict$R$Predicted),
#                 BreedingYear = BreedingYear_datam_stacked - 2006)
# 
# lm.psi= lm(formula = Psi ~ BreedingYear,
#          data = df)
# coef(lm.psi)
# 
# lm.R= lm(formula = R ~ BreedingYear,
#            data = df)
# coef(lm.R)


############ modBig01bm_stacked_onlyYear ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig01bm_stacked_onlyYear_psiformulas <- c(
  '~BreedingYear',
  '~BreedingYear') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig01bm_stacked_onlyYear_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig01bm_stacked_onlyYear_fit <- occuMS(detformulas=modBig01bm_stacked_onlyYear_detformulas, 
                                         psiformulas=modBig01bm_stacked_onlyYear_psiformulas,
                                         data=umf_stacked, parameterization = "multinomial")
modBig01bm_stacked_onlyYear_fit
#coef(modBig01bm_stacked_onlyYear_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig01bm_stacked_onlyYear_psi_predict = predict(modBig01bm_stacked_onlyYear_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig01bm_stacked_onlyYear_det_predict = predict(modBig01bm_stacked_onlyYear_fit,type='det') # est. for p1, p2, delta
#lapply(modBig01bm_stacked_onlyYear_phi_predict,head)


############ modBig02m_stacked ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig02m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig02m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig02m_stacked_fit <- occuMS(detformulas=modBig02m_stacked_detformulas, 
                               psiformulas=modBig02m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig02m_stacked_fit
#coef(modBig02m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig02m_stacked_psi_predict = predict(modBig02m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig02m_stacked_det_predict = predict(modBig02m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02stacked_phi_predict,head)


############ modBig03m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig03m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig03m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig03m_stacked_fit <- occuMS(detformulas=modBig03m_stacked_detformulas, 
                               psiformulas=modBig03m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig03m_stacked_fit
#coef(modBig03m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig03m_stacked_psi_predict = predict(modBig03m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig03m_stacked_det_predict = predict(modBig03m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)



############ modBig04m_stacked ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig04m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig04m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig04m_stacked_fit <- occuMS(detformulas=modBig04m_stacked_detformulas, 
                               psiformulas=modBig04m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig04m_stacked_fit
#coef(modBig04m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig04m_stacked_psi_predict = predict(modBig04m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig04m_stacked_det_predict = predict(modBig04m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)


############ modBig05m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig05m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig05m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig05m_stacked_fit <- occuMS(detformulas=modBig05m_stacked_detformulas, 
                               psiformulas=modBig05m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig05m_stacked_fit
#coef(modBig05m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig05m_stacked_psi_predict = predict(modBig05m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig05m_stacked_det_predict = predict(modBig05m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)



############ modBig06m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig06m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig06m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig06m_stacked_fit <- occuMS(detformulas=modBig06m_stacked_detformulas, 
                               psiformulas=modBig06m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig06m_stacked_fit
#coef(modBig06m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig06m_stacked_psi_predict = predict(modBig06m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig06m_stacked_det_predict = predict(modBig06m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)



############ modBig07m_stacked ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig07m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig07m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig07m_stacked_fit <- occuMS(detformulas=modBig07m_stacked_detformulas, 
                               psiformulas=modBig07m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig07m_stacked_fit
#coef(modBig07m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig07m_stacked_psi_predict = predict(modBig07m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig07m_stacked_det_predict = predict(modBig07m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig07m_stacked_phi_predict,head)


############ modBig08m_stacked ########### 
# Rain before, temp before
### Set Formulas 
# State Params
modBig08m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig08m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig08m_stacked_fit <- occuMS(detformulas=modBig08m_stacked_detformulas, 
                               psiformulas=modBig08m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig08m_stacked_fit
#coef(modBig08m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig08m_stacked_psi_predict = predict(modBig08m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig08m_stacked_det_predict = predict(modBig08m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig08m_stacked_phi_predict,head)



############ modBig09m_stacked ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig09m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig09m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig09m_stacked_fit <- occuMS(detformulas=modBig09m_stacked_detformulas, 
                               psiformulas=modBig09m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig09m_stacked_fit
#coef(modBig09m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig09m_stacked_psi_predict = predict(modBig09m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig09m_stacked_det_predict = predict(modBig09m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig09m_stacked_phi_predict,head)




############ modBig10m_stacked ########### 
# Rain before, Low temp during
### Set Formulas 
# State Params
modBig10m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig10m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig10m_stacked_fit <- occuMS(detformulas=modBig10m_stacked_detformulas, 
                               psiformulas=modBig10m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig10m_stacked_fit
#coef(modBig10m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig10m_stacked_psi_predict = predict(modBig10m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig10m_stacked_det_predict = predict(modBig10m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig02stacked_phi_predict,head)





############ modBig11m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig11m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig11m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig11m_stacked_fit <- occuMS(detformulas=modBig11m_stacked_detformulas, 
                               psiformulas=modBig11m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig11m_stacked_fit
#coef(modBig11m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
modBig11m_stacked_psi_predict = predict(modBig11m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
modBig11m_stacked_det_predict = predict(modBig11m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)




############ modBig12m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig12m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig12m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig12m_stacked_fit <- occuMS(detformulas=modBig12m_stacked_detformulas, 
                               psiformulas=modBig12m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig12m_stacked_fit
#coef(modBig12m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig12m_stacked_psi_predict = predict(modBig12m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
#modBig12m_stacked_det_predict = predict(modBig12m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig03stacked_phi_predict,head)
############ modBig13m_stacked ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig13m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig13m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig13m_stacked_fit <- occuMS(detformulas=modBig13m_stacked_detformulas, 
                               psiformulas=modBig13m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig13m_stacked_fit
#coef(modBig13m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig13m_stacked_psi_predict = predict(modBig13m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig13m_stacked_det_predict = predict(modBig13m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)



############ modBig14m_stacked ########### 
# Long Drought, Low Temp During
### Set Formulas 
# State Params
modBig14m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig14m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig14m_stacked_fit <- occuMS(detformulas=modBig14m_stacked_detformulas, 
                               psiformulas=modBig14m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig14m_stacked_fit
#coef(modBig14m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig14m_stacked_psi_predict = predict(modBig14m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig14m_stacked_det_predict = predict(modBig14m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig04stacked_phi_predict,head)
############ modBig15m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig15m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig15m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig15m_stacked_fit <- occuMS(detformulas=modBig15m_stacked_detformulas, 
                               psiformulas=modBig15m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig15m_stacked_fit
#coef(modBig15m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig15m_stacked_psi_predict = predict(modBig15m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig15m_stacked_det_predict = predict(modBig15m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)



############ modBig16m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig16m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig16m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig16m_stacked_fit <- occuMS(detformulas=modBig16m_stacked_detformulas, 
                               psiformulas=modBig16m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig16m_stacked_fit
#coef(modBig16m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig16m_stacked_psi_predict = predict(modBig16m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig16m_stacked_det_predict = predict(modBig16m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig05stacked_phi_predict,head)
############ modBig17m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig17m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig17m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig17m_stacked_fit <- occuMS(detformulas=modBig17m_stacked_detformulas, 
                               psiformulas=modBig17m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig17m_stacked_fit
#coef(modBig17m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig17m_stacked_psi_predict = predict(modBig17m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig17m_stacked_det_predict = predict(modBig17m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)




############ modBig18m_stacked ########### 
# Long Drought, Temp before
### Set Formulas 
# State Params
modBig18m_stacked_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + ColdDays + HotDays') #siteCovs on psi[1] and psi[2]

# Detection probability
modBig18m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig18m_stacked_fit <- occuMS(detformulas=modBig18m_stacked_detformulas, 
                               psiformulas=modBig18m_stacked_psiformulas,
                               data=umf_stacked, parameterization = "multinomial")
modBig18m_stacked_fit
#coef(modBig18m_stacked_fit)

### Get predicted parameter values using predict() 
# See Agenda 06-27 ToDO #3 for detailed descriptions
# modBig18m_stacked_psi_predict = predict(modBig18m_stacked_fit, type = "psi") # est. for psi1 and psi2 (i.e. R)
# modBig18m_stacked_det_predict = predict(modBig18m_stacked_fit,type='det') # est. for p1, p2, delta
#lapply(modBig06stacked_phi_predict,head)


### Compare Fit -------------
# Example: modSel(fitList(fit,fit_null))
fl_m <- fitList("modBig01m_stacked"= modBig01m_stacked_fit, "modBig02m_stacked"= modBig02m_stacked_fit,"modBig03m_stacked"= modBig03m_stacked_fit,"modBig04m_stacked"= modBig04m_stacked_fit,"modBig05m_stacked"= modBig05m_stacked_fit,"modBig06m_stacked"= modBig06m_stacked_fit,"modBig07m_stacked" = modBig07m_stacked_fit, "modBig08m_stacked" = modBig08m_stacked_fit, "modBig09m_stacked" = modBig09m_stacked_fit, "modBig10m_stacked" = modBig10m_stacked_fit, "modBig11m_stacked" = modBig11m_stacked_fit, "modBig12m_stacked" = modBig12m_stacked_fit, "modBig13m_stacked" = modBig13m_stacked_fit, "modBig14m_stacked" = modBig14m_stacked_fit, "modBig15m_stacked" = modBig15m_stacked_fit, "modBig16m_stacked" = modBig16m_stacked_fit, "modBig17m_stacked" = modBig17m_stacked_fit, "modBig18m_stacked" = modBig18m_stacked_fit)
ms_m <- modSel(fl_m)
ms_m
#toExport <- as(ms, "data.frame")