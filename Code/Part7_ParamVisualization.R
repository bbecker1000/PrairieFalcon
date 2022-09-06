######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code visualizes predicted parameter values
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part6c_FitBigStaticModels.R")
source("Code/Part6e_FitBigStaticModels_multi.R")
source("Code/Part6f_FitBigStaticCausalModels_multi.R")

##### Prepare predicted data - multinomial #####
### Using the best stacked static multinomial model modBig11m_stacked_fit
# Psi1

# adding BEST_MODEL


predicted_psi1 = BEST_MODEL_psi_predict$`psi[1]` # changed to BEST_MODEL
predicted_psi1 = predicted_psi1 %>% 
  rename(
    Psi1 = Predicted,
    SE.psi = SE,
    lower.psi1 = lower,
    upper.psi1 = upper
  ) %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         AreaType = PRFADetectHistory_2022_stacked$Area_Type,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L))
  
# Psi2
predicted_psi2 = BEST_MODEL_psi_predict$`psi[2]` # changed to BEST_MODEL
predicted_psi2 = predicted_psi2 %>% 
  rename(
    Psi2 = Predicted,
    SE.R = SE,
    lower.psi2 = lower,
    upper.psi2 = upper
  ) %>% 
  mutate(
         Territory = PEFACovs$TerritoryName,
         AreaType = PRFADetectHistory_2022_stacked$Area_Type,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L),
         PEFA = PEFACovs$PEFA,
         WinterPrecip = DecToFebTotal_data)

#Merge together
predicted = merge(x = predicted_psi1, y =predicted_psi2, by = c("Territory", "BreedingYear","YearDate", "AreaType")) %>% arrange(Territory, BreedingYear)

# Yearly average 
AnnualAvgPredicted = predicted %>% 
  group_by(AreaType, YearDate) %>% 
  summarise(meanPsi1 =  mean(Psi1),
            high.psi1 = quantile(Psi1, 0.975),
            low.psi1 = quantile(Psi1, 0.025),
            meanPsi2 =  mean(Psi2),
            high.psi2 = quantile(Psi2, 0.975),
            low.psi2 = quantile(Psi2, 0.025),) %>% 
  mutate(WinterPrecip = DecToFebTotal$Total)


##### Plot average predictions by Area Type by Year - multinomial #####
# psi1
ggplot(AnnualAvgPredicted, aes(x= YearDate, y = meanPsi1, group  = AreaType, color = AreaType, label= round(meanPsi1,2)))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)+
  #geom_text(vjust = -0.2, hjust = -0.2)+
  geom_errorbar(aes(ymin=low.psi1, ymax=high.psi1), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi1 by Area Type", x="Year", y = "psi1",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 

# psi2
# this plot should be adjusted to combine AreaType -- BB 2022-08-12
ggplot(AnnualAvgPredicted, aes(x= YearDate, y = meanPsi2, 
                               group  = AreaType, 
                               color = AreaType, 
                               label = round(meanPsi2,2)))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)  +
  #geom_text(vjust = -0.2, hjust = -0.2)+
  geom_errorbar(aes(ymin=low.psi2, ymax=high.psi2), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi2 by Area Type", x="Year", y = "psi2",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 

  
# ## dot plot (doesn't look great)
# # psi
# p_psi = ggplot(predicted, aes(x=as.factor(BreedingYear), y=Psi, fill=AreaType)) +
#   geom_dotplot(binaxis='y', stackdir='center', stackratio=0.08, dotsize=0.7, binwidth =1/100, position=position_dodge(0.8))+
#   labs(title="Predicted Psi by Area Type", x="Year", y = "Psi")+
#   scale_fill_manual(values=c('#E69F00','#999999')) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     axis.title.x = element_text(size = 12),
#     axis.title.y = element_text(size = 12),
#     axis.text.x = element_text(size = 10),
#     axis.text.y = element_text(size = 10))
# p_psi + stat_summary(fun.y=mean, geom="point", shape=18,
#                  size=2, color="red", position=position_dodge(0.8))


# # R
# p_R = ggplot(predicted, aes(x=as.factor(BreedingYear), y=R, fill=AreaType)) +
#   geom_dotplot(binaxis='y', stackdir='center', stackratio=0.08, dotsize=0.7, binwidth =1/100, position=position_dodge(0.8))+
#   labs(title="Predicted R by Area Type", x="Year", y = "R")+
#   scale_fill_manual(values=c('#E69F00','#999999')) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     axis.title.x = element_text(size = 12),
#     axis.title.y = element_text(size = 12),
#     axis.text.x = element_text(size = 10),
#     axis.text.y = element_text(size = 10)) +
#   xlim("2015", "2016")
# p_R + stat_summary(fun.y=mean, geom="point", shape=18,
#                      size=2, color="red", position=position_dodge(0.8))


## Please update to multinomial. - BB 2022-08-12


##### Prepare predicted data (psi1/psi2) - conditional binomial #####
# Yearly average 
AnnualAvgPredicted_condbinom = BEST_MODEL_state_params %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         AreaType = PRFADetectHistory_2022_stacked$Area_Type,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(AreaType, YearDate) %>% 
  summarise(meanPsi1 =  mean(psi1),
            high.psi1 = quantile(psi1, 0.975),
            low.psi1 = quantile(psi1, 0.025),
            meanPsi2 =  mean(psi2),
            high.psi2 = quantile(psi2, 0.975),
            low.psi2 = quantile(psi2, 0.025),) %>% 
  mutate(WinterPrecip = DecToFebTotal$Total)

##### Plot average predictions by Area Type by Year (psi1/psi2) - cndbinom #####
ggplot(AnnualAvgPredicted_condbinom, aes(x= YearDate, y = meanPsi1, group  = AreaType, color = AreaType, label= round(meanPsi1,2)))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)+
  geom_errorbar(aes(ymin=low.psi1, ymax=high.psi1), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi1 by Area Type (cond_binom)", x="Year", y = "psi1",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 

# psi2
AnnualAvgPredictedPsi2_condbinom = BEST_MODEL_state_params %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(YearDate) %>% 
  summarise(meanPsi2 =  mean(psi2),
            high.psi2 = quantile(psi2, 0.975),
            low.psi2 = quantile(psi2, 0.025),) 

ggplot(AnnualAvgPredictedPsi2_condbinom, aes(x= YearDate, y = meanPsi2))+
  geom_line()+
  geom_point(size = 3)  +
  geom_errorbar(aes(ymin=low.psi2, ymax=high.psi2), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi2 by Area Type (cond_binom)", x="Year", y = "psi2",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 




##### Prepare predicted data (psi/R) - conditional binomial #####
# Yearly average 
AnnualAvgPredicted_condbinom.psi.R = BEST_MODEL_state_params %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         AreaType = PRFADetectHistory_2022_stacked$Area_Type,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(AreaType, YearDate) %>% 
  summarise(meanPsi =  mean(psi),
            high.psi = quantile(psi, 0.975),
            low.psi = quantile(psi, 0.025),
            meanR =  mean(R),
            high.R = quantile(R, 0.975),
            low.R = quantile(R, 0.025),) 

##### Plot average predictions by Area Type by Year (psi/R) - cndbinom #####
ggplot(AnnualAvgPredicted_condbinom.psi.R, aes(x= YearDate, y = meanPsi, group  = AreaType, label= round(meanPsi,2)))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)+
  geom_errorbar(aes(ymin=low.psi, ymax=high.psi), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi by Area Type", x="Year", y = "psi",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 

# R
AnnualAvgPredictedR_condbinom = BEST_MODEL_state_params %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(YearDate) %>% 
  summarise(meanR =  mean(R),
            high.R = quantile(R, 0.975),
            low.R = quantile(R, 0.025),) 

ggplot(AnnualAvgPredictedR_condbinom, aes(x= YearDate, y = meanR))+
  geom_line()+
  geom_point(size = 3)  +
  geom_errorbar(aes(ymin=low.R, ymax=high.R), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted R by Area Type", x="Year", y = "R",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 



##### Plot psi2 predictions with Winter Precipitation by Year - multinomial #####

df= AnnualAvgPredicted %>% pivot_longer(cols = c('meanPsi2','WinterPrecip'), names_to = 'variables',values_to = 'values')

ggplot(df, aes(x= YearDate, y = values, group  = variables, color = variables))+
  geom_line(aes(linetype=variables))+
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi2 with Winter Precip", x="Year", y = "Psi2")+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  facet_wrap(vars(variables), scales = "free_y")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45),
    axis.text.y = element_text(size = 10))


# ggplot(AnnualAvgPredicted, aes(x= WinterPrecip, y = meanR))+
#   geom_point()+
#   geom_smooth()

# plot(modBig02_stacked_fit) may work for multinomial parametarization


##### Plot R predictions with PEFA ??? #####
# library(ggeffects) --- doesn't work
dataPred = umf_stacked@siteCovs
df = dataPred %>% summarise_if(is.numeric, mean)

occuPred <- predict(modBig01_stacked_fit,
                    type = "psi",
                    newdata = dataPred,
                    na.rm = TRUE,
                    inf.rm = TRUE)



######### plot covariate effects ---------------------------------------

preddata.model <- predict(BEST_MODEL, type = "psi", appendData = TRUE, level = 0.95)  

preddata.model.psi<- as_tibble(preddata.model[["psi"]])
preddata.model.R<- as_tibble(preddata.model[["R"]])

WinterRain.df <- as_tibble(umf_stacked@siteCovs[["DecToFebPrecipitation"]])
Core.df <- as_tibble(umf_stacked@siteCovs[["AreaType"]])
PEFA.df <- as_tibble(umf_stacked@siteCovs[["PEFA"]])

WinterRain_Core_PEFA.df.psi <-  cbind(preddata.model.psi, WinterRain.df, Core.df, PEFA.df)
WinterRain_Core_PEFA.df.psi <- WinterRain_Core_PEFA.df.psi %>%
  rename("WinterRain" = 5,
         "Core" = 6,
         "PEFA" = 7)
WinterRain_Core_PEFA.df.psi

WinterRain_Core_PEFA.df.R <-  cbind(preddata.model.R, WinterRain.df, Core.df, PEFA.df)
WinterRain_Core_PEFA.df.R <- WinterRain_Core_PEFA.df.R %>%
  rename("WinterRain" = 5,
         "Core" = 6,
         "PEFA" = 7)
WinterRain_Core_PEFA.df.R

# replace 1 with core and 0 with non-core
WinterRain_Core_PEFA.df.R$Core <- ifelse(WinterRain_Core_PEFA.df.R$Core == "0", "Non-core", "Core") 
WinterRain_Core_PEFA.df.psi$Core <- ifelse(WinterRain_Core_PEFA.df.psi$Core == "0", "Non-core", "Core") 

# replace 1 with PEFA present and 0 with PEFA absent
WinterRain_Core_PEFA.df.R$PEFA <- ifelse(WinterRain_Core_PEFA.df.R$PEFA == "0", "PEFA absent", "PEFA present") 


## plot Winter Rain vs R
p1.WinterRain.R <- ggplot(WinterRain_Core_PEFA.df.R, aes(WinterRain, Predicted)) + 
  geom_smooth(method = "loess") + 
  geom_jitter() +
  xlab("Winter Rainfall") + ylab("Conditional R") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1) + theme_minimal()
p1.WinterRain.R



## plot Area Type vs R
p1.Core.R <- ggplot(WinterRain_Core_PEFA.df.R, aes(Core, Predicted)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(fill = 'grey', width = 0.6) +
  xlab("Area Type") + ylab("Conditional R") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1)+ theme_minimal()
p1.Core.R


# plot PEFA vs R
p1.PEFA.R <- ggplot(WinterRain_Core_PEFA.df.R, aes(PEFA, Predicted)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(fill = 'grey', width = 0.6) +
  xlab("PEFA") + ylab("Conditional R") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1)+ theme_minimal()
p1.PEFA.R

## plot Area Type vs psi
p1.Core.psi <- ggplot(WinterRain_Core_PEFA.df.psi, aes(Core, Predicted)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(fill = 'grey', width = 0.6) +
  xlab("Area Type") + ylab("probability of occupancy") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1)+ theme_minimal()
p1.Core.psi

# --------------------------------------------------------------