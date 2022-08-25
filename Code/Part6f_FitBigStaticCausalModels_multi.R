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

############ modBig01_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig01_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig01_stacked_CAUSAL_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig01_stacked_CAUSAL <- occuMS(detformulas=modBig01_stacked_CAUSAL_detformulas, 
                               psiformulas=modBig01_stacked_CAUSAL_psiformulas,
                               data=umf_stacked, parameterization = "condbinom")
modBig01_stacked_CAUSAL
#coef(modBig01_stacked_CAUSAL)

############ modBig02_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig02_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig02_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig02_stacked_CAUSAL <- occuMS(detformulas=modBig02_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig02_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig02_stacked_CAUSAL
#coef(modBig02_stacked_CAUSAL)


############ modBig03_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig03_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig03_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig03_stacked_CAUSAL <- occuMS(detformulas=modBig03_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig03_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig03_stacked_CAUSAL
#coef(modBig03_stacked_CAUSAL)


############ modBig04_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig04_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + LongDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig04_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig04_stacked_CAUSAL <- occuMS(detformulas=modBig04_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig04_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig04_stacked_CAUSAL
#coef(modBig04_stacked_CAUSAL)


############ modBig05_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig05_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig05_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig05_stacked_CAUSAL <- occuMS(detformulas=modBig05_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig05_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig05_stacked_CAUSAL
#coef(modBig05_stacked_CAUSAL)


############ modBig06_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig06_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig06_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig06_stacked_CAUSAL <- occuMS(detformulas=modBig06_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig06_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig06_stacked_CAUSAL
#coef(modBig06_stacked_CAUSAL)


############ modBig07_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig07_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + LongDrought+ ShortDrought',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig07_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig07_stacked_CAUSAL <- occuMS(detformulas=modBig07_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig07_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig07_stacked_CAUSAL
#coef(modBig07_stacked_CAUSAL)







############ modBig08_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig08_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig08_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig08_stacked_CAUSAL <- occuMS(detformulas=modBig08_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig08_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig08_stacked_CAUSAL
#coef(modBig08_stacked_CAUSAL)


############ modBig09_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig09_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig09_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig09_stacked_CAUSAL <- occuMS(detformulas=modBig09_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig09_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig09_stacked_CAUSAL
#coef(modBig09_stacked_CAUSAL)


############ modBig10_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig10_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig10_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig10_stacked_CAUSAL <- occuMS(detformulas=modBig10_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig10_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig10_stacked_CAUSAL
#coef(modBig10_stacked_CAUSAL)


############ modBig11_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig11_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + LongDrought + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig11_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig11_stacked_CAUSAL <- occuMS(detformulas=modBig11_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig11_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig11_stacked_CAUSAL
#coef(modBig11_stacked_CAUSAL)


############ modBig12_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig12_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig12_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig12_stacked_CAUSAL <- occuMS(detformulas=modBig12_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig12_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig12_stacked_CAUSAL
#coef(modBig12_stacked_CAUSAL)


############ modBig13_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig13_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + LongDrought + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig13_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig13_stacked_CAUSAL <- occuMS(detformulas=modBig13_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig13_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig13_stacked_CAUSAL
#coef(modBig13_stacked_CAUSAL)


############ modBig14_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig14_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + LongDrought + ShortDrought + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig14_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig14_stacked_CAUSAL <- occuMS(detformulas=modBig14_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig14_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig14_stacked_CAUSAL
#coef(modBig14_stacked_CAUSAL)


############ modBig15_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig15_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi and R

# Detection probability
modBig15_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig15_stacked_CAUSAL <- occuMS(detformulas=modBig15_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig15_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig15_stacked_CAUSAL
#coef(modBig15_stacked_CAUSAL)


############ modBig16_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig16_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig16_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig16_stacked_CAUSAL <- occuMS(detformulas=modBig16_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig16_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig16_stacked_CAUSAL
#coef(modBig16_stacked_CAUSAL)


############ modBig17_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig17_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig17_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig17_stacked_CAUSAL <- occuMS(detformulas=modBig17_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig17_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig17_stacked_CAUSAL
#coef(modBig17_stacked_CAUSAL)





############ modBig18_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig18_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig18_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig18_stacked_CAUSAL <- occuMS(detformulas=modBig18_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig18_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig18_stacked_CAUSAL
#coef(modBig18_stacked_CAUSAL)



############ modBig19_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig19_stacked_CAUSAL_psiformulas <- c(
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig19_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig19_stacked_CAUSAL <- occuMS(detformulas=modBig19_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig19_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig19_stacked_CAUSAL
#coef(modBig19_stacked_CAUSAL)



############ modBig20_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig20_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig20_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig20_stacked_CAUSAL <- occuMS(detformulas=modBig20_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig20_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig20_stacked_CAUSAL
#coef(modBig20_stacked_CAUSAL)




############ modBig21_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig21_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig21_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig21_stacked_CAUSAL <- occuMS(detformulas=modBig21_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig21_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig21_stacked_CAUSAL
#coef(modBig21_stacked_CAUSAL)



############ modBig22_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig22_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig22_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig22_stacked_CAUSAL <- occuMS(detformulas=modBig22_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig22_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig22_stacked_CAUSAL
#coef(modBig22_stacked_CAUSAL)



############ modBig23_stacked_CAUSAL ########### 
### Set Formulas 
# State Params
modBig23_stacked_CAUSAL_psiformulas <- c(
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation',
  '~BreedingYear + AreaType*AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + ColdDays + HotDays') #siteCovs on psi and R

# Detection probability
modBig23_stacked_CAUSAL_detformulas <- c('~1','~LateEffect','~LateEffect') # site OR obs Covs on p11, p12, and p22 

### Fit Model 
modBig23_stacked_CAUSAL <- occuMS(detformulas=modBig23_stacked_CAUSAL_detformulas, 
                                  psiformulas=modBig23_stacked_CAUSAL_psiformulas,
                                  data=umf_stacked, parameterization = "condbinom")
modBig23_stacked_CAUSAL
#coef(modBig23_stacked_CAUSAL)









# ############ modBig01m_stacked_CAUSAL ########### 
# # Rain before, temp before
# ### Set Formulas 
# # State Params
# modBig01m_stacked_psiformulas <- c(
#   '~BreedingYear + AreaType + AnnualVisitors + PEFA + DecToFebPrecipitation',
#   '~BreedingYear + AreaType + AnnualVisitors + PEFA + DecToFebPrecipitation + HeavyRain + HDD + HotDays') #siteCovs on psi[1] and psi[2]
# 
# ## Adding psi2 long drought has no effect on winter rain
# ## adding psi 2 shortdrought weakens winter rain effect, but still a strong effect.  
# ## so some additional information from the short drought effect.  Mention in results
# ## Cold days and min temp not important
# 
# # Detection probability
# modBig01m_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 
# 
# ### Fit Model 
# modBig01m_stacked_CAUSAL <- occuMS(detformulas=modBig01m_stacked_detformulas, 
#                                    psiformulas=modBig01m_stacked_psiformulas,
#                                    data=umf_stacked, parameterization = "condbinom")
# modBig01m_stacked_CAUSAL
# #coef(modBig01m_stacked_CAUSAL)

# Small_stacked_psiformulas <- c(
#   '~BreedingYear + AreaType + PEFA + DecToFebPrecipitation',
#   '~BreedingYear + PEFA') #siteCovs on psi[1] and psi[2]
# 
# # Detection probability
# Small_stacked_detformulas <- c('~1','~1+LateEffect','~1+LateEffect') # site OR obs Covs on p11, p12, and p22 
# 
# ### Fit Model 
# Small_stacked_CAUSAL <- occuMS(detformulas=Small_stacked_detformulas, 
#                                    psiformulas=Small_stacked_psiformulas,
#                                    data=umf_stacked, parameterization = "multinomial")
# Small_stacked_CAUSAL
# #coef(modBig01m_stacked_fit)





BEST_MODEL <- modBig01_stacked_fit # Temporary (cond_binom). Update after selecting best model

summary(BEST_MODEL)
BEST_MODEL_psi_predict = predict(BEST_MODEL, type = "psi") # est. for psi1 and psi2 (i.e. R)
BEST_MODEL_det_predict = predict(BEST_MODEL,type='det') # est. for p1, p2, delta

# If cond_binom, calculate psi1 and psi2 with psi and R:
BEST_MODEL_state_params = data.frame(
  psi = BEST_MODEL_psi_predict$psi$Predicted,
  R = BEST_MODEL_psi_predict$R$Predicted) %>% 
  mutate(psi1 = psi*(1-R),
         psi2 = psi*R)

# If cond_binom, calculate p11 and p12 nnd p22 with p1 and p2 and delta:
BEST_MODEL_det_params = data.frame(
  p1 = BEST_MODEL_det_predict$`p[1]`$Predicted,
  p2 = BEST_MODEL_det_predict$`p[2]`$Predicted,
  delta = BEST_MODEL_det_predict$delta$Predicted) %>% 
  mutate(p11 = p1,
         p12 = p2*(1-delta),
         p22 = p2*delta)

# TODO: run best model in multinimial parameterization as well to assess GOF






