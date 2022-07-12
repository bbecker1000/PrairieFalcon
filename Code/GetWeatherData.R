library(tidyverse)
library(lubridate)
library(ggplot2)
setwd("~/Desktop/Falcon2022/PrairieFalcon")

# Monthly Precipitation
monthly_precipitation = read.csv("Data/Pinnacles_monthly_precipitation_2003_2021.csv")

monthly_precipitation = monthly_precipitation %>% 
  group_by(Year) %>% 
  summarise(RNF_MM= mean(RNF_MM))
# Annual Visitors
annual_visitors = read.csv("Data/PINN_Annual_Visitors.csv", skip=2) %>% 
  filter(Year >=2003) %>% 
  select(-3)
#write_csv(annual_visitors, path = "Data/PINN_Annual_Visitors_filtered.csv", append= FALSE)

# Long term drought

long_term_drought_PINN = read.csv("Data/LongTermDroughtBlendPINN.csv") 
colnames(long_term_drought_PINN) = c("Date", "Data")

long_term_drought_PINN = long_term_drought_PINN %>% 
  mutate(Year = year(Date),
         Month = month(Date)) %>% 
  group_by(Year, Month) %>% 
  summarise(Data = mean(Data))

# Others
spi = read.csv("Data/SPI-san-benito-county-ca.csv")
usdm = read.csv("Data/USDM-san-benito-county-ca.csv")

# Storm
storms = read.csv("Data/storm_data_search_results.csv")
unique(storms$EVENT_TYPE)



plot(monthly_precipitation$RNF_MM, long_term_drought$Data)

cor(long_term_drought$Data, long_term_drought_PINN$Data)

