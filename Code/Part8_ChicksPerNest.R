# Part8_ChicksPerNest
library(readr)
library(tidyverse)
library(tidyr)

ChicksPerNest <- read_csv('Data/ChickPerNest.csv') 
View(ChicksPerNest)

unique (ChicksPerNest$Variable)
ChicksPerNest <- ChicksPerNest %>% filter(Variable != "PEFAChicksPerNest")


p.fledge <- ggplot(ChicksPerNest, aes(Year, Value, 
                                      #color = Variable, 
                                      linetype = Variable,
                                      shape = Variable)) +
  geom_pointrange(aes(ymin=Value-SD, ymax = Value+SD), position = position_dodge(width = 0.2), size = 0.75) +
  geom_line(aes(linetype=Variable), size = 1, position = position_dodge(width = 0.2)) +
  theme_classic(base_size = 20) +
  ylab(NULL) +
  theme(legend.position = "none") +
  geom_text(x=2016, y=1.75, label="Nesting PEFA Pairs", 
            #color = "#F8766D", 
            size = 6) +
  geom_text(x=2017, y=5, label="Mean(SD) PRFA \nfledges per nest", 
            #color = "#00BFC4", 
            size = 6) +
  scale_y_continuous(limits = c(0, 5.5), breaks = c(seq(0,5,by = 1))) + 
  scale_x_continuous(limits = c(2006.5, 2021.1), breaks = c(seq(2008,2020,by = 2))) 
  
p.fledge

## r2

t1 <- ChicksPerNest %>% select(-SD) %>%
  pivot_wider(names_from = Variable, values_from = Value) 
   
with(t1, cor.test(NestingPEFAPairs, PRFAChicksPerNest))


