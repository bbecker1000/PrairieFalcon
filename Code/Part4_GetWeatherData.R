######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code prepares weather data for modeling as covariates
## Libraries and versions used: tidyverse, lubridate
#####################################################################################################



library(tidyverse)
library(lubridate)
library(ggplot2)

### Monthly Precipitation ---------
monthly_precipitation = read.csv("Data/PINN_monthly_precipitation.csv", skip = 9) %>% select(-4)
monthly_precipitation$DATE_TIME <- mdy_hms(monthly_precipitation$DATE_TIME) 
monthly_precipitation <- monthly_precipitation %>% 
  mutate(Year = year(monthly_precipitation$DATE_TIME),
         Month = month(monthly_precipitation$DATE_TIME),
         BreedingYear = ifelse(Month > 11, Year+1, Year))
DecToFebTotal = monthly_precipitation %>% 
  filter(Month > 11 | Month < 3)%>% 
  filter(BreedingYear > 2006 & BreedingYear < 2022) %>% 
  group_by(BreedingYear) %>% 
  summarise(Total = sum(RNF_MM))
 

### Annual Visitors ----------
annual_visitors = read.csv("Data/PINN_Annual_Visitors.csv", skip=2) %>% 
  filter(Year >=2007) %>% 
  select(-3) %>% 
  mutate(RecreationVisitors = as.numeric(gsub(",", "", RecreationVisitors)))
# class(annual_visitors$RecreationVisitors) #test


### Long term drought ----------
# Long-term Blend= 0.35 *(PDSI/2) + 0.15 * SPI180d + 0.2 * SPI1y + 0.2 *SPI2y + 0.1 * SPI5y
# Experimental
long_term_drought_PINN = read.csv("Data/LongTermDroughtBlendPINN.csv") 
colnames(long_term_drought_PINN) = c("Date", "Data")

monthly_drought_PINN = long_term_drought_PINN %>% 
  mutate(Year = year(Date),
         Month = month(Date)) %>% 
  group_by(Year, Month) %>% 
  summarise(Data = mean(Data))

### Temp (choose one)
# Monthly Max Temp --------
monthyl_max_temp = read.csv("Data/PINN_monthly_max_temp.csv")
colnames(monthyl_max_temp) = c("Date", "Data")
monthyl_max_temp = monthyl_max_temp[c(2:229),]

# Monthly Max Temp --------
monthyl_min_temp = read.csv("Data/PINN_monthly_min_temp.csv")
colnames(monthyl_min_temp) = c("Date", "Data")

### SPI ---------
# 2 year
SPI_2year = read.csv("Data/SPI_2year.csv")
colnames(SPI_2year) = c("Date", "Data")
SPI_2year$Date <- ymd(SPI_2year$Date) 
Monthly_SPI_2year = SPI_2year %>% 
  mutate(Year = year(Date),
         Month = month(Date)) %>% 
  group_by(Year, Month) %>% 
  summarise(Data = mean(Data))

# 5 year
SPI_5year = read.csv("Data/SPI_5year.csv")
colnames(SPI_5year) = c("Date", "Data")
SPI_5year$Date <- ymd(SPI_5year$Date) 
Monthly_SPI_5year = SPI_5year %>% 
  mutate(Year = year(Date),
         Month = month(Date)) %>% 
  group_by(Year, Month) %>% 
  summarise(Data = mean(Data))

### Test correlations --------
cor(monthyl_max_temp$Data,monthyl_min_temp$Data) # 0.92
cor(monthyl_max_temp$Data,monthly_drought_PINN$Data) #-0.01
cor(Monthly_SPI_2year$Data,monthly_drought_PINN$Data) #0.85
cor(Monthly_SPI_2year$Data,monthyl_max_temp$Data) #-0.05
cor(monthly_drought_PINN$Data,Monthly_SPI_5year$Data) #0.45
cor(Monthly_SPI_2year$Data,Monthly_SPI_5year$Data) #0.45

#choose: blends, monthly max?


### SExtreme Events------
#use extremeEvent column (0/1) - simpler + account for inconsistent reporting
# San Benito
extreme_events_san_benito = read.csv("Data/storm_data_San_Benito.csv")
unique(extreme_events_san_benito$CZ_NAME_STR)
PINN_extreme_events_san_benito = extreme_events_san_benito %>% 
  filter(grepl('PINNACLES', CZ_NAME_STR),
         !grepl('Low Tide', EVENT_TYPE)) %>% 
  select(c("BEGIN_DATE", "EVENT_TYPE")) %>% 
  mutate(BEGIN_DATE = mdy(BEGIN_DATE),
         Year = year(BEGIN_DATE))
# unique(PINN_extreme_events_san_benito$EVENT_TYPE) #test

# Monterey (same events)
# extreme_events_monterey = read.csv("Data/storm_data_Monterey_county.csv")
# unique(extreme_events_monterey$CZ_NAME_STR)
# PINN_extreme_events_monterey = extreme_events_monterey %>%
#   filter(grepl('PINNACLES', CZ_NAME_STR),,
#          !grepl('Low Tide', EVENT_TYPE))%>%
#   select(c("BEGIN_DATE", "EVENT_TYPE"),) %>%
#   mutate(BEGIN_DATE = mdy(BEGIN_DATE),
#          Year = year(BEGIN_DATE))
# unique(PINN_extreme_events_monterey$EVENT_TYPE)

PINN_extreme_events <- PINN_extreme_events_san_benito %>% 
  group_by(Year) %>% 
  count() %>% 
  mutate(ExtremeEvent = ifelse(n>0, 1, 0))
years = data.frame(Year = c(2007:2021))
PINN_extreme_events <- left_join(years, PINN_extreme_events) %>% 
  mutate(n = coalesce(n,0),
         ExtremeEvent = coalesce(ExtremeEvent,0))

### Plots


