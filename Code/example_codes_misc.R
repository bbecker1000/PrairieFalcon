##### 1 -Purpose: classify initial states #####
##Shared by Ben in email

#MAKE A FIELD FOR OBSERVED STATE BY VISIT (0=NO MATURE OR FLEDGLINGS; 1A=SINGLE MATURE, NO FLEDGLINGS, 1B=2 MATURE, NO FLEDGLINGS, 2A=1 FLEDGLING (REGARDLESS OF #MATURE), 2B=2 OR 3 FLEDGLINGS (REGARDLESS OF #MATURE)
data11$STATE<-NA
data11$STATE<-replace(data11$STATE,data11$NumAdults==0 & data11$NumFledglings==0,0)
data11$STATE<-replace(data11$STATE,data11$NumAdults==1 & data11$NumFledglings==0,"1A")
data11$STATE<-replace(data11$STATE,data11$PAIR==1 & data11$NumFledglings==0,"1B")
data11$STATE<-replace(data11$STATE,data11$NumFledglings==1,"2A")
# only 15 visits with 3 fledglings, so combined with visits with 2 fledglings
data11$STATE<-replace(data11$STATE,data11$NumFledglings==2 | data11$NumFledglings==3,"2B")



##### 2 -Purpose: pooling detection history into bi-weekly periods #####
##shared by Ben in Agenda

# a. Get minimum date for each year of first transition from 1→2
# figure out mean of first detection of pair with chicks
plotdata %>%
  group_by(ObsOcc, Year) %>%
  summarize(min = min(jdate)) %>%
  group_by(ObsOcc) %>%
  summarize(min = mean(min))

# b.Get maximum detection value within a survey period (for example 2 week bins) 
top_week <- tbl_df(plotdata.A) %>%  # use as_tibble() instead
  group_by(LOCATION, Year, jbiweek) %>%  #jbiweek is in dataframe as biweekly period    
  top_n(n = 1, row_number(ObsOcc))  # use slice_max() instead. row_number(x) removes ties

# c.Get number of years of surveys for each site
Surveys_per_Location_2000_2020 <- top_week %>% 
  filter(Year >= 2000 & Year <= 2020) %>%
  group_by(LOCATION) %>%
  summarize(years = n_distinct(Year)) %>%
  filter(years >= 15)  # number of sites with 15+ years

