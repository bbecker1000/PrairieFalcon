######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code visualizes predicted parameter values
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part6c_FitBigStaticModels.R")
source("Code/Part6e_FitBigStaticModels_multi.R")

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

##### Prepare predicted data - conditional binomial #####
# Yearly average 
AnnualAvgPredicted_condbinom = predicted_state_params %>% 
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

##### Plot average predictions by Area Type by Year - cndbinom #####
ggplot(AnnualAvgPredicted_condbinom, aes(x= YearDate, y = meanPsi1, group  = AreaType, color = AreaType, label= round(meanPsi1,2)))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)+
  geom_text(vjust = -0.2, hjust = -0.2)+
  geom_errorbar(aes(ymin=low.psi1, ymax=high.psi1), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi1 by Area Type (condbinom)", x="Year", y = "psi1",
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
ggplot(AnnualAvgPredicted_condbinom, aes(x= YearDate, y = meanPsi2, group  = AreaType, color = AreaType, label = round(meanPsi2,2)))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)  +
  geom_text(vjust = -0.2, hjust = -0.2)+
  geom_errorbar(aes(ymin=low.psi2, ymax=high.psi2), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi2 by Area Type (condbinom)", x="Year", y = "psi2",
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

##### ##### Plot R predictions with PEFA ??? #####
# library(ggeffects) --- doesn't work
dataPred = umf_stacked@siteCovs
df = dataPred %>% summarise_if(is.numeric, mean)

occuPred <- predict(modBig01_stacked_fit,
                    type = "psi",
                    newdata = dataPred,
                    na.rm = TRUE,
                    inf.rm = TRUE)










