######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code visualizes predicted parameter values
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part6c_FitBigStaticModels.R")
source("Code/Part6e_FitBigStaticModels_multi.R")
source("Code/Part6f_FitBigStaticCausalModels.R")
library(cowplot)

##### Prepare predicted data - multinomial #####
### Using the best stacked static multinomial model modBig11m_stacked_fit
# Psi1

# adding BEST_MODEL


predicted_psi1 = BEST_MODEL_psi_predict$`psi` # changed to BEST_MODEL  if multi the psi[1] or use psi for condbinom
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
predicted_psi2 = BEST_MODEL_psi_predict$`R` # changed to BEST_MODEL  # if multi then `psi[2]`
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
            meanPsiCI.lower = mean(psiCI.lower),
            meanPsiCI.upper = mean(psiCI.upper),
            meanR =  mean(R),
            high.R = quantile(R, 0.975),
            low.R = quantile(R, 0.025),
            meanRCI.lower= mean(RCI.lower),
            meanRCI.upper = mean(RCI.upper)) 


##### Plot average predictions by Area Type by Year (psi/R) - cndbinom #####
# title="Yearly Average Predicted Psi by Area Types with mean 95% CI"
psi <- ggplot(AnnualAvgPredicted_condbinom.psi.R, aes(x= YearDate, y = meanPsi, group = AreaType, 
                                                      shape = AreaType, line = AreaType, label= round(meanPsi,2))) +
  
  geom_line(aes(linetype=AreaType), position=position_dodge(width=150)) +
  geom_pointrange(aes(ymin=meanPsiCI.lower, ymax=meanPsiCI.upper), position=position_dodge(width=150), size = 0.65) +
  scale_x_date(breaks = scales::breaks_pretty(10))+
  labs( x="Year", y = "psi")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_color_manual(values=c('#E69F00','#999999')) + 
  theme_classic(base_size = 20) +
  theme(plot.margin = unit(c(2,3,2,3), "lines"),
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = c(0.8, 0.8),
        legend.title=element_blank(),
        legend.text=element_text(size=14)) 
psi 
# R
AnnualAvgPredictedR_condbinom = BEST_MODEL_state_params %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(YearDate) %>% 
  summarise(meanR =  mean(R),
            high.R = quantile(R, 0.975),
            low.R = quantile(R, 0.025),
            meanRCI.lower= mean(RCI.lower),
            meanRCI.upper = mean(RCI.upper)) 
# title="Yearly Average Predicted R with mean 95% CI"
R <- ggplot(AnnualAvgPredicted_condbinom.psi.R, aes(x= YearDate, y = meanR), 
            group  = AreaType, shape = AreaType, line = AreaType, label= round(meanR,2))+
  geom_line(aes(linetype=AreaType), position=position_dodge(width=150)) +
  geom_pointrange(aes(shape = AreaType, ymin=meanRCI.lower, ymax=meanRCI.upper), position=position_dodge(width=150), size = 0.65) +
  scale_x_date(breaks = scales::breaks_pretty(10))+
  scale_y_continuous(breaks = scales::breaks_pretty(10))+  
  labs(x="Year", y = "R")+
  ylim(0,1)+
  scale_shape_manual(values=c(15, 17))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_classic(base_size = 20) +
  theme(plot.margin = unit(c(2,3,2,3), "lines"),
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = c(0.8, 0.9),
        legend.title=element_blank(),
        legend.text=element_text(size=11)) 
R 

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

occuPred <- predict(BEST_MODEL,
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
PEFAState.df <- as_tibble(umf_stacked@siteCovs[["PEFAState"]])

WinterRain_Core_PEFA.df.psi <-  cbind(preddata.model.psi, WinterRain.df, Core.df, PEFA.df, PEFAState.df)
WinterRain_Core_PEFA.df.psi <- WinterRain_Core_PEFA.df.psi %>%
  rename("WinterRain" = 5,
         "Core" = 6,
         "PEFA" = 7,
         "PEFAState" = 8)
WinterRain_Core_PEFA.df.psi

WinterRain_Core_PEFA.df.R <-  cbind(preddata.model.R, WinterRain.df, Core.df, PEFA.df, PEFAState.df)
WinterRain_Core_PEFA.df.R <- WinterRain_Core_PEFA.df.R %>%
  rename("WinterRain" = 5,
         "Core" = 6,
         "PEFA" = 7,
         "PEFAState" = 8)
WinterRain_Core_PEFA.df.R

# replace 1 with core and 0 with non-core
WinterRain_Core_PEFA.df.R$Core <- ifelse(WinterRain_Core_PEFA.df.R$Core == "0", "Non-core", "Core") 
WinterRain_Core_PEFA.df.psi$Core <- ifelse(WinterRain_Core_PEFA.df.psi$Core == "0", "Non-core", "Core") 

# replace 1 with PEFA present and 0 with PEFA absent
WinterRain_Core_PEFA.df.R$PEFA <- ifelse(WinterRain_Core_PEFA.df.R$PEFA == "0", "PEFA absent", ifelse(WinterRain_Core_PEFA.df.R$PEFA == "1", "PEFA present", "PEFA breeding"))


## plot Winter Rain vs R
p1.WinterRain.R <- ggplot(WinterRain_Core_PEFA.df.R, aes(WinterRain, Predicted)) + 
  geom_smooth(method = "loess") + 
  geom_jitter() +
  xlab("Winter Rainfall") + ylab("R") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1) + theme_minimal() +
  labs( x="Normalized Winter Precipitation", y = "R")+
  theme_classic(base_size = 20) +
  theme(plot.margin = unit(c(2,3,2,3), "lines"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
p1.WinterRain.R


## plot Area Type vs R
p1.Core.R <- ggplot(WinterRain_Core_PEFA.df.R, aes(Core, Predicted)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(fill = 'grey', width = 0.6) +
  xlab("Area Type") + ylab("Conditional R") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1)+ theme_minimal()
p1.Core.R


# plot PEFAState vs Psi
p1.PEFAState.Psi <- ggplot(WinterRain_Core_PEFA.df.psi, aes(x=reorder(PEFAState,-Predicted), y=Predicted)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(fill = 'grey', width = 0.6) +
  geom_jitter(color="black", size=1, alpha=0.5)+
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1)+ 
  labs(x = NULL,y = "psi")+
  scale_x_discrete(labels=c("PEFA State 0","PEFA State 1","PEFA State 2")) +
  theme_classic(base_size = 20) +
  theme(plot.margin = unit(c(2,3,2,3), "lines"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) 
p1.PEFAState.Psi

# # plot PEFAState vs R
# p1.PEFAState.R <- ggplot(WinterRain_Core_PEFA.df.R, aes(x=reorder(PEFAState,-Predicted), y=Predicted)) + 
#   stat_boxplot(geom ='errorbar', width = 0.6) +
#   geom_boxplot(fill = 'grey', width = 0.6) +
#   #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
#   ylim(0,1)+ theme_minimal()+
#   labs(x = NULL,y = "R")+
#   theme(
#     axis.title.x = element_text(size = 15),
#     axis.title.y = element_text(size = 15),
#     axis.text.x = element_text(size = 13),
#     axis.text.y = element_text(size = 13)) 
# p1.PEFAState.R




## plot Area Type vs psi
p1.Core.psi <- ggplot(WinterRain_Core_PEFA.df.psi, aes(Core, Predicted)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(fill = 'grey', width = 0.6) +
  xlab("Area Type")+ylab("Psi") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, data = WinterRain.df.R)+
  ylim(0,1)+ theme_minimal()+
  theme(plot.margin = unit(c(2,3,2,3), "lines"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
p1.Core.psi

top_row <- plot_grid(psi, R, labels = c('(a)', '(b)'), label_size = 13, rel_widths = c(1, 1), hjust = 0)
bottom_row = plot_grid(p1.PEFAState.Psi, p1.WinterRain.R, labels = c('(c)', '(d)'), label_size = 13, hjust = 0)
plot_grid(top_row, bottom_row, label_size = 13, ncol = 1)

# --------------------------------------------------------------

## marginal effects plots 2022-09-12 BB






## plot of total occupied and reproductive sites
## multiple by # sites
## quick and dirty for now.  this needs to be fixed to account for core and non-core hen doing Psi.

PRFA_2022 %>%
  group_by(Area_Type) %>%
  summarise(count = n_distinct(TerritoryName))

N_Core_territories <- 27
N_Non_Core_territories <- 16

print(AnnualAvgPredicted_condbinom.psi.R, n = 43)
AnnualAvgPredicted_condbinom.psi.R$Territories <- ifelse(AnnualAvgPredicted_condbinom.psi.R$AreaType == "Core", 
                                                         N_Core_territories,
                                                         N_Non_Core_territories)
BEST_MODEL_state_params.sum_plot = data.frame(
  psi.predicted = BEST_MODEL_psi_predict$psi$Predicted,
  psi.lower = BEST_MODEL_psi_predict$psi$lower,
  psi.upper = BEST_MODEL_psi_predict$psi$upper,
  R.predicted = BEST_MODEL_psi_predict$R$Predicted,
  R.lower = BEST_MODEL_psi_predict$R$lower,
  R.upper = BEST_MODEL_psi_predict$R$upper)


BEST_MODEL_state_params.sum_plot %>% 
  mutate(Territory = PEFACovs$TerritoryName,
         BreedingYear = rep(2007:2021, time = 43),
         YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(YearDate) %>% 
  summarise(psi.predicted =  mean(psi.predicted),
            psi.lower = mean(psi.lower),
            psi.upper = mean(psi.lower),
            R.predicted =  mean(R.predicted),
            R.lower = mean(R.lower),
            R.upper = mean(R.lower)) 









## plot of total occupied and reproductive sites
## multiple by # sites
## quick and dirty for now.  this needs to be fixed to account for core and non-core when doing Psi and R

PRFA_2022 %>%
    group_by(Area_Type) %>%
      summarise(count = n_distinct(TerritoryName))

N_Core_territories <- 27
N_Non_Core_territories <- 16
Territories = N_Core_territories + N_Non_Core_territories

#Territories <- ifelse(AnnualAvgPredicted_condbinom.psi.R$AreaType == "Core", 
 #                                                        N_Core_territories,
  #                                                       N_Non_Core_territories)
BEST_MODEL_state_params.sum_plot = data.frame(
  psi.predicted = BEST_MODEL_psi_predict$psi$Predicted,
  psi.lower = BEST_MODEL_psi_predict$psi$lower,
  psi.upper = BEST_MODEL_psi_predict$psi$upper,
  R.predicted = BEST_MODEL_psi_predict$R$Predicted,
  R.lower = BEST_MODEL_psi_predict$R$lower,
  R.upper = BEST_MODEL_psi_predict$R$upper)

BEST_MODEL_state_params.sum_plot <- BEST_MODEL_state_params.sum_plot %>% 
       mutate(Territory = PEFACovs$TerritoryName,
       BreedingYear = rep(2007:2021, time = 43),
       YearDate = ymd(rep(2007:2021, time = 43), truncated = 2L)) %>% 
  group_by(YearDate) %>% 
  summarise(psi.predicted =  mean(psi.predicted),
            psi.lower = mean(psi.lower),
            psi.upper = mean(psi.upper),
            R.predicted =  mean(R.predicted),
            R.lower = mean(R.lower),
            R.upper = mean(R.upper)) 


p.occ.territories <- ggplot(BEST_MODEL_state_params.sum_plot, aes(x= YearDate, y = psi.predicted*Territories))+
  geom_line()+
  geom_point(size = 3)  +
  geom_errorbar(aes(ymin=psi.lower*Territories, ymax=psi.upper*Territories), width = 150, size = 0.6, 
                position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(10))+
  scale_y_continuous(breaks = scales::breaks_pretty(10))+  
  labs(x="Year", y = "Occupied Territories")+
  ylim(0,30)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_classic(base_size = 20) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 


p.repr.territories <- ggplot(BEST_MODEL_state_params.sum_plot, aes(x= YearDate, y = psi.predicted*Territories*R.predicted))+
  geom_line()+
  geom_point(size = 3)  +
  geom_errorbar(aes(ymin=psi.lower*Territories*R.lower, ymax=psi.upper*Territories*R.upper), width = 150, size = 0.6, 
                position = position_dodge(.5)) +
  scale_x_date(breaks = scales::breaks_pretty(10))+
  scale_y_continuous(breaks = scales::breaks_pretty(10))+  
  labs(x="Year", y = "Reproductive Territories")+
  ylim(0,30)+
  scale_shape_manual(values=c(15, 17))+
  scale_linetype_manual(values  = c("solid", "dashed"))+
  scale_color_manual(values=c('#E69F00','#999999')) +
  theme_classic(base_size = 20) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) 

library(cowplot)
plot_grid(p.occ.territories, p.repr.territories)

plot_grid(psi, R, p1.PEFAState.Psi, p1.WinterRain.R, p.occ.territories, p.repr.territories,
          ncol = 2)

