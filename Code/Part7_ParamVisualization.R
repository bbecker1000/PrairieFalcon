######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code visualizes predicted parameter values
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part6c_FitBigStaticModels.R")

##### Prepare predicted data #####
# Psi
predicted_psi = modBig01_stacked_psi_predict$psi
predicted_psi = predicted_psi %>% 
  rename(
    Psi = Predicted,
    SE.psi = SE,
    lower.psi = lower,
    upper.psi = upper
  ) %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         AreaType = PRFADetectHistory_2022_stacked$Area_Type,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L))
  
# R
predicted_R = modBig01_stacked_psi_predict$R
predicted_R = predicted_R %>% 
  rename(
    R = Predicted,
    SE.R = SE,
    lower.R = lower,
    upper.R = upper
  ) %>% 
  mutate(
         Territory = PEFACovs$TerritoryName,
         AreaType = PRFADetectHistory_2022_stacked$Area_Type,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L),
         PEFA = PEFACovs$PEFA,
         WinterPrecip = DecToFebTotal_data)

#Merge together
predicted = merge(x = predicted_psi, y =predicted_R, by = c("Territory", "BreedingYear","YearDate", "AreaType")) %>% arrange(Territory, BreedingYear)
# R: core, year, PEFA, DecToFebPrecipitation

# Yearly average 
AnnualAvgPredicted = predicted %>% 
  group_by(AreaType, YearDate) %>% 
  summarise(meanPsi =  mean(Psi),
            high.psi = quantile(Psi, 0.975),
            low.psi = quantile(Psi, 0.025),
            meanR =  mean(R),
            high.R = quantile(R, 0.975),
            low.R = quantile(R, 0.025),) %>% 
  mutate(WinterPrecip = DecToFebTotal$Total)


##### Plot average predictions by Area Type by Year #####
# psi
ggplot(AnnualAvgPredicted, aes(x= YearDate, y = meanPsi, group  = AreaType, color = AreaType))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)+
  geom_errorbar(aes(ymin=low.psi, ymax=high.psi), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted Psi by Area Type", x="Year", y = "psi",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
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
ggplot(AnnualAvgPredicted, aes(x= YearDate, y = meanR, group  = AreaType, color = AreaType))+
  geom_line(aes(linetype=AreaType))+
  geom_point(aes(shape=AreaType), size = 3)  +
  geom_errorbar(aes(ymin=low.R, ymax=high.R), width = 150, size = 0.6, position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted R by Area Type", x="Year", y = "R",
       subtitle = "with the 2.5th percentile and 97.5th percentile")+
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



##### ##### Plot R predictions with Winter Precipitation by Year #####

df= AnnualAvgPredicted %>% pivot_longer(cols = c('meanR','WinterPrecip'), names_to = 'variables',values_to = 'values')

ggplot(df, aes(x= YearDate, y = values, group  = variables, color = variables))+
  geom_line(aes(linetype=variables))+
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(breaks = scales::breaks_pretty(8))+  
  labs(title="Yearly Average Predicted R with Winter Precip", x="Year", y = "R")+
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
