#full model treeplot

library(readr)
fullModelForestPlot <- read_csv("Data/fullModelForestPlot.csv")
(fullModelForestPlot)


fullModelForestPlot$lo <- fullModelForestPlot$Estimate - fullModelForestPlot$SE*1.96
fullModelForestPlot$hi <- fullModelForestPlot$Estimate + fullModelForestPlot$SE*1.96


dodge <- position_dodge(width=0.6) 

ggplot(fullModelForestPlot, aes(Covariate, Estimate, color = Parameter)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position=dodge) +
  coord_flip() +
  ylim(-2.7, 5) +
  geom_hline(yintercept= 0, linetype = 2) +
  scale_x_discrete(limits=rev) +
  theme_minimal(base_size = 18)

#drop the late effect since delta is large and p2 had no solutions

fullModelForestPlot <- fullModelForestPlot %>% filter(Covariate != "LateEffect1")

ggplot(fullModelForestPlot, aes(Covariate, Estimate, color = Parameter)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position=dodge, size = 0.7) +
  coord_flip() +
  ylim(-2.7, 2.7) +
  geom_hline(yintercept= 0, linetype = 2) +
  scale_x_discrete(limits=rev) +
  theme_minimal(base_size = 18)

ggsave("Output/forestplot.jpeg", width = 20, height = 25, units = "cm")
