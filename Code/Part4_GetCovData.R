######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code prepares weather data for modeling as covariates
## Libraries and versions used: tidyverse, lubridate
#####################################################################################################



library(tidyverse)
library(lubridate)
library(ggplot2)

###### TEMPERATURE ######
# Load data
daily_mean_temp = read.csv("Data/PINN_DailyMeanTemp.csv") 
colnames(daily_mean_temp) = c("Date","MeanTemp")
daily_max_temp = read.csv("Data/PINN_DailyMaxTemp.csv") 
colnames(daily_max_temp) = c("Date","MaxTemp")
daily_min_temp = read.csv("Data/PINN_DailyMinTemp.csv") 
colnames(daily_min_temp) = c("Date","MinTemp")

# mean
# Season: 1-Winter, 2- incubation, 3-nestling, 0-other
daily_mean_temp <- daily_mean_temp %>% 
  mutate(Date = ymd(Date),
         Month = month(Date),
         Day = day(Date),
         Year = year(Date),
         BreedingYear = ifelse(Month > 11, Year+1, Year),
         Season = ifelse(
           Month %in% c(12,1,2), 1, ifelse(
             (Month==4&Day>=15) | Month==5 | (Month==6&Day<=15), 3, ifelse(
               (Month==3&Day>=15) | (Month==4&Day<=15),2,0
             )))) %>%
  filter(BreedingYear > 2006 & BreedingYear<2022) %>% 
  select(-c(Year, Day))
# max
# Season: 1-Winter, 2- incubation, 3-nestling, 0-other
daily_max_temp <- daily_max_temp %>% 
  mutate(Date = ymd(Date),
         Month = month(Date),
         Day = day(Date),
         Year = year(Date),
         BreedingYear = ifelse(Month > 11, Year+1, Year),
         Season = ifelse(
           Month %in% c(12,1,2), 1, ifelse(
             (Month==4&Day>=15) | Month==5 | (Month==6&Day<=15), 3, ifelse(
               (Month==3&Day>=15) | (Month==4&Day<=15),2,0
             )))) %>%
  filter(BreedingYear > 2006 & BreedingYear<2022) %>% 
  select(-c(Year, Day))

# min
# Season: 1-Winter, 4 - incubation + three weeks, 0-other
daily_min_temp <- daily_min_temp %>% 
  mutate(Date = ymd(Date),
         Month = month(Date),
         Day = day(Date),
         Year = year(Date),
         BreedingYear = ifelse(Month > 11, Year+1, Year),
         Season = ifelse(
           Month %in% c(12,1,2), 1, ifelse(
             (Month==3&Day>=15) | Month==4 | (Month==5&Day<=7), 4, 0))) %>%
  filter(BreedingYear > 2006 & BreedingYear<2022) %>% 
  select(-c(Year))


### Winter (Dec-Feb: before & at beginning of breeding season) 
# Dependent variable: R
# Measurement variable: Heating degree day (HDD) - the difference between the daily temperature mean and 65°F. HDD = sum(65 - daily mean temp)

# HDD
HDD <- daily_mean_temp %>% 
  filter(Season == 1) %>% 
  mutate(hdd = 65 - MeanTemp) %>% 
  group_by(BreedingYear) %>% 
  summarise(YearlyHDD = sum(hdd))

# #cold days (not used - correlated with HDD r = 0.99)
# WinterColdDays <- daily_min_temp %>%
#   filter(Season == 1 & MinTemp <=35) %>% 
#   group_by(BreedingYear) %>%
#   count()
# cor(WinterColdDays$n, HDD$YearlyHDD)


## Plot: dashed line - 15-year winter average temp
ggplot(daily_mean_temp, aes(x=Date, y=MeanTemp, color = as.factor(Season))) +
  geom_point(size=1)+
  geom_hline(yintercept= 65, linetype="dotdash", color = "black")+
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(n.breaks = 8)+
  scale_color_manual(values = c("1"= "blue", "2" = "pink", "3" = "orange", "0" = "darkgrey"))+
  labs(
    title = "Daily Mean Temp",
    subtitle = "winter = blue, incubation = pink , nestling = orange, other = grey",
    x = "Date",
    y = "Daily Mean Temp (degree F)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none") 

### NESTLING PERIOD (April 15th - June 15th)
# Dependent variable: R
# Measurement variable: number of days during nestling period per year exceeding 90 degree F
HotDays <- daily_max_temp %>% 
  filter(Season == 3 & MaxTemp >= 90) %>% # 90 is ~80th percentile
  group_by(BreedingYear) %>% 
  count()

## Plot: dashed line - 15-year winter average temp
ggplot(daily_max_temp, aes(x=Date, y=MaxTemp, color = as.factor(Season))) +
  geom_point(size=1)+
  scale_x_date(breaks = scales::breaks_pretty(15))+
  geom_hline(yintercept= 90, linetype="dotdash", color = "black")+
  scale_y_continuous(n.breaks = 8)+
  scale_color_manual(values = c("1"= "blue", "2" = "pink", "3" = "orange", "0" = "darkgrey"))+
  labs(
    title = "Daily Max Temp",
    subtitle = "winter = blue, incubation = pink , nestling = orange, other = grey",
    x = "Date",
    y = "Daily Max Temp (degree F)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none") 

ColdDays <- daily_min_temp %>% 
  filter(Season == 4) %>% 
  filter(MinTemp <=35)%>% 
  group_by(BreedingYear) %>% 
  count()
ColdDays <- left_join(data.frame(BreedingYear = c(2007:2021)), ColdDays) %>% 
  mutate(n=coalesce(n, 0))

## Plot: dashed line - 15-year winter average temp
ggplot(daily_min_temp, aes(x=Date, y=MinTemp, color = as.factor(Season))) +
  geom_point(size=1)+
  geom_hline(yintercept= 35, linetype="dotdash", color = "black")+
  scale_x_date(breaks = scales::breaks_pretty(15))+
  scale_y_continuous(n.breaks = 8)+
  scale_color_manual(values = c("1"= "blue", "4" = "darkgreen", "0" = "darkgrey"))+
  labs(
    title = "Daily Min Temp",
    subtitle = "winter = blue, incubation + three weeks = green, other = grey",
    x = "Date",
    y = "Daily Min Temp (degree F)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none") 

cor(HDD$YearlyHDD, HotDays$n) # -0.15
cor(HDD$YearlyHDD, ColdDays$n) # 0.64 --- Remove ColdDays
cor(HotDays$n, ColdDays$n) # -0.27




##### Monthly Precipitation #####
monthly_precipitation = read.csv("Data/PINN_monthly_precipitation.csv", skip = 9) %>% select(-4)
monthly_precipitation$DATE_TIME <- mdy_hms(monthly_precipitation$DATE_TIME) 
monthly_precipitation <- monthly_precipitation %>% 
  mutate(Year = year(DATE_TIME),
         Month = month(DATE_TIME),
         BreedingYear = ifelse(Month > 11, Year+1, Year))
DecToFebTotal = monthly_precipitation %>% 
  filter(Month > 11 | Month < 3)%>% 
  filter(BreedingYear > 2006 & BreedingYear < 2022) %>% 
  group_by(BreedingYear) %>% 
  summarise(Total = sum(RNF_MM))
 

##### Annual Visitors #####
annual_visitors = read.csv("Data/PINN_Annual_Visitors.csv", skip=2) %>% 
  filter(Year >=2007) %>% 
  select(-3) %>% 
  mutate(RecreationVisitors = as.numeric(gsub(",", "", RecreationVisitors)))
# class(annual_visitors$RecreationVisitors) #test
# hist(log(annual_visitors$RecreationVisitors))

##### Short term drought #####
short_term_drought = read.csv("Data/ShortTermDrought.csv") 
colnames(short_term_drought) = c("Date", "Data")

monthly_short_drought = short_term_drought %>% 
  mutate(Date = ymd(Date),
         Year = year(Date),
         Month = month(Date),
         BreedingYear = ifelse(Month > 11, Year+1, Year)) %>% 
  group_by(BreedingYear, Month) %>% 
  summarise(Data = mean(Data))


##### Long term drought #####
long_term_drought = read.csv("Data/longTermDrought.csv") 
colnames(long_term_drought) = c("Date", "Data")

monthly_long_drought =long_term_drought %>% 
  mutate(Date = ymd(Date),
         Year = year(Date),
         Month = month(Date),
         BreedingYear = ifelse(Month > 11, Year+1, Year)) %>% 
  group_by(BreedingYear, Month) %>% 
  summarise(Data = mean(Data))



### Test correlations --------






# ### SPI (not used)---------
# # 2 year
# SPI_2year = read.csv("Data/SPI_2year.csv")
# colnames(SPI_2year) = c("Date", "Data")
# SPI_2year$Date <- ymd(SPI_2year$Date) 
# Monthly_SPI_2year = SPI_2year %>% 
#   mutate(Year = year(Date),
#          Month = month(Date)) %>% 
#   group_by(Year, Month) %>% 
#   summarise(Data = mean(Data))
# 
# # 5 year
# SPI_5year = read.csv("Data/SPI_5year.csv")
# colnames(SPI_5year) = c("Date", "Data")
# SPI_5year$Date <- ymd(SPI_5year$Date) 
# Monthly_SPI_5year = SPI_5year %>% 
#   mutate(Year = year(Date),
#          Month = month(Date)) %>% 
#   group_by(Year, Month) %>% 
#   summarise(Data = mean(Data))

### Extreme Events (not used) -------- 
# #use extremeEvent column (0/1) - simpler + account for inconsistent reporting
# # San Benito
# extreme_events_san_benito = read.csv("Data/storm_data_San_Benito.csv")
# unique(extreme_events_san_benito$CZ_NAME_STR)
# PINN_extreme_events_san_benito = extreme_events_san_benito %>% 
#   filter(grepl('PINNACLES', CZ_NAME_STR),
#          !grepl('Low Tide', EVENT_TYPE)) %>% 
#   select(c("BEGIN_DATE", "EVENT_TYPE")) %>% 
#   mutate(BEGIN_DATE = mdy(BEGIN_DATE),
#          Year = year(BEGIN_DATE))
# # unique(PINN_extreme_events_san_benito$EVENT_TYPE) #test
# 
# # Monterey (same events)
# # extreme_events_monterey = read.csv("Data/storm_data_Monterey_county.csv")
# # unique(extreme_events_monterey$CZ_NAME_STR)
# # PINN_extreme_events_monterey = extreme_events_monterey %>%
# #   filter(grepl('PINNACLES', CZ_NAME_STR),,
# #          !grepl('Low Tide', EVENT_TYPE))%>%
# #   select(c("BEGIN_DATE", "EVENT_TYPE"),) %>%
# #   mutate(BEGIN_DATE = mdy(BEGIN_DATE),
# #          Year = year(BEGIN_DATE))
# # unique(PINN_extreme_events_monterey$EVENT_TYPE)
# 
# PINN_extreme_events <- PINN_extreme_events_san_benito %>% 
#   group_by(Year) %>% 
#   count() %>% 
#   mutate(ExtremeEvent = ifelse(n>0, 1, 0))
# years = data.frame(Year = c(2007:2021))
# PINN_extreme_events <- left_join(years, PINN_extreme_events) %>% 
#   mutate(n = coalesce(n,0),
#          ExtremeEvent = coalesce(ExtremeEvent,0))

### Plots


