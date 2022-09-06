######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code conducts exploratory analysis and visualization
## Libraries and versions used: readxl, tidyverse, lubridate, ggplot2 
#####################################################################################################

source("Code/Part2_DatasetPrep.R")

###### Exploratory Data Analysis & Visualization #######
test = PRFASurveys_2022 %>% 
  group_by(BreedingYear, TerritoryName, Area_Type) %>% 
  summarize(count = n()) %>% 
  group_by(Area_Type) %>% 
  summarize(Avg = mean(count))
### Yearly PRFA Surveys per Site by Area Type -------
# Avg number of visits each year each area type - core vs non-core (code from Sarah)
SurveyAreaType_2022 <- PRFASurveys_2022 %>%
  count(BreedingYear, TerritoryName, Area_Type)%>% #count the number of each combination
  group_by(BreedingYear, Area_Type)%>%
  summarise(AvgVisits=mean(n), SDVisits = sd(n))
# Plot
ggplot(SurveyAreaType_2022, aes(x= BreedingYear, y = AvgVisits)) + 
  geom_smooth(aes(linetype = Area_Type), color = "black",size = 1.5) +
  ylim(0, 9) +
  labs(title = "Yearly PRFA Surveys per Site by Area Type",
       x = 'Breeding Year',
       y = 'Avearge Visit Number per Site')+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)) 


### Number of years of UNPOOLED surveys for each site (code from Ben)--------
Years_per_Location <- PRFAStates_2022 %>% 
  group_by(TerritoryName) %>%
  summarize(years = n_distinct(BreedingYear)) 

### Total number of UNPOOLED surveys for each site (code from Ben)--------
Surveys_per_Location <- PRFAStates_2022 %>% 
  group_by(TerritoryName) %>%
  summarize(surveys = n())

### Number of UNPOOLED surveys for each site each year ---------
Surveys_per_Location_per_Year  = PRFAStates_2022 %>% 
  group_by(TerritoryName,Area_Type, BreedingYear)  %>%
  summarize(surveys = n())
# add %>% pivot_wider(names_from = BreedingYear, values_from = n, values_fill= 0) to make into wide format
ggplot(Surveys_per_Location_per_Year, aes(x=surveys)) + 
  geom_histogram(binwidth = 1,aes(fill=Area_Type))+
  theme_minimal()+
  scale_x_continuous(breaks = scales::breaks_pretty(6))+
  labs(title="Dsitribution of Number of Surveys per Territory per Year",
       x = 'Number of Surveys')+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)) 

table(Surveys_per_Location_per_Year$surveys) # most are 3-5

# ### Same thing for POOLED dataset (Not Run) ---------
# # Get number of years of POOLED surveys for each site 
# Years_per_Location_pooled <- PRFAStates_2022_pooled %>% 
#   group_by(TerritoryName) %>%
#   summarize(years = n_distinct(BreedingYear)) 
# 
# # Get number of POOLED surveys for each site
# Surveys_per_Location_pooled <- PRFAStates_2022_pooled %>% 
#   group_by(TerritoryName) %>%
#   summarize(surveys = n())
# 
# # Get number of POOLED surveys for each site each year
# Surveys_per_Location_per_Year_pooled  = PRFAStates_2022_pooled %>% 
#   group_by(TerritoryName,BreedingYear)  %>%
#   summarize(surveys = n())
# hist(Surveys_per_Location_per_Year_pooled$surveys)
# table(Surveys_per_Location_per_Year_pooled$surveys) # most are 3-5

### Plot unpooled data -----------
yColor <- ifelse(PRFAStates_2022$Area_Type == "Core", "plain", "bold.italic")
PlotStates_2022<- ggplot(PRFAStates_2022, aes(x = DaySinceDec15th, y = fct_rev(as_factor(TerritoryName)), fill = as.factor(finalState))) + 
  geom_point(shape = 21)+
  scale_fill_manual(values = c("white", "burlywood", "red4")) + 
  facet_grid(cols = vars(BreedingYear))+
  labs(fill = "States") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.text.y = element_text(face = yColor),
        axis.title.y = element_blank())+
  geom_vline(xintercept= LateCutOff, linetype="dotdash", color = "black", size = 0.3)
PlotStates_2022 


### Plot pooled data ----------
PlotStates_2022_pooled <- ggplot(PRFAStates_2022_pooled, aes(x = as.factor(triweeklyPeriodNum), y = fct_rev(as_factor(TerritoryName)), color = as.factor(finalState))) +
 geom_point()+
 facet_grid(cols = vars(BreedingYear))+
 labs(color = "finalStates",
      x = "Triweekly Period") +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5), axis.title.y = element_blank())
PlotStates_2022_pooled


### Naive Occupancy Overtime --------
num_Site <- length(unique(PRFAStates_2022$TerritoryName))

PRFAStates_2022_probs <- PRFAStates_2022 %>% 
  group_by(BreedingYear,TerritoryName, Area_Type) %>% 
  summarise(seasonalFinalState = max(finalState))
PRFAStates_2022_probs$oneOrTwoState <- PRFAStates_2022_probs$seasonalFinalState > 0
PRFAStates_2022_probs <- PRFAStates_2022_probs %>% 
  group_by(BreedingYear, Area_Type) %>% 
  summarise(oneOrTwoStateProportion = sum(oneOrTwoState)/num_Site)

ggplot(PRFAStates_2022_probs, aes(x=BreedingYear, y=oneOrTwoStateProportion)) +
  geom_line(size=0.75, aes(linetype = Area_Type)) +
  geom_point(size=2, aes(shape = Area_Type)) +
  ylim(0,1) +
  labs(
    title = "Naive Occupancy Time Series from 2007 to 2021",
    x = "Breeding Year",
    y = "Occupancy Proportion") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13))


### Naive Successful Reproduction overtime ---------
PRFAStates_2022_probs <- PRFAStates_2022 %>% 
  group_by(BreedingYear,TerritoryName) %>% 
  summarise(seasonalFinalState = max(finalState))
PRFAStates_2022_probs$twoState <- PRFAStates_2022_probs$seasonalFinalState > 1
PRFAStates_2022_probs <- PRFAStates_2022_probs %>% 
  group_by(BreedingYear) %>% 
  summarise(twoStateProportion = sum(twoState)/num_Site)

ggplot(PRFAStates_2022_probs, aes(x=BreedingYear, y=twoStateProportion)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  ylim(0,1) +
  labs(
    title = "Naive R Time Series from 2007 to 2021",
    x = "Breeding Year",
    y = "Successful Reproduction Proportion") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))


### Total number fledglings per year -----------
total_chicks_per_year = PRFAStates_2022 %>%
  group_by(TerritoryName,Area_Type, BreedingYear) %>%
  summarise(max_fledge = max(FledglingCount)) %>%
  group_by(BreedingYear, Area_Type) %>%
  summarise(NumFledge = sum(max_fledge))

# Plot
ggplot(total_chicks_per_year, aes(x=BreedingYear, y=NumFledge, label = NumFledge)) +
  geom_line(aes(linetype=Area_Type),size=0.6) +
  geom_point(aes(shape=Area_Type),size = 3) +
  ylim(0,30) +
  labs(
    title = "Total Number of Fledglings Produced per Year 2007-2021",
    x = "Breeding Year",
    y = "Total Number of Fledglings") +
  theme_minimal()+
  theme(plot.title = element_text(face= 'bold', size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10))

# + geom_text(hjust = -0.4, vjust = -0.8, size = 3) to label value of each point


### Average number of chicks per site per year ------------
average_chicks_per_year = PRFAStates_2022 %>%
  group_by(TerritoryName, BreedingYear) %>%
  summarise(max_fledge = max(FledglingCount)) %>%
  group_by(BreedingYear) %>%
  summarise(AvgFledge = mean(max_fledge))
average_chicks_per_year$AvgFledge = round(average_chicks_per_year$AvgFledge,2)

# Plot
ggplot(average_chicks_per_year, aes(x=BreedingYear, y=AvgFledge, label = AvgFledge)) +
  geom_line(size=1, color = "grey") +
  geom_point(shape = 21, color = "black", fill = "darkgreen", size = 4) +
  ylim(0,1.1) +
  geom_text(hjust = -0.2, vjust = -1, size = 3) +
  labs(title = "Average Number of Fledglings Produced per Site per Year 2007-2021",
       x = "Breeding Year",
       y = "Average Number of Fledglings per Site") +
  theme_minimal() +
  theme(plot.title = element_text(face= 'bold', size = 15, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10))  



###### BreedingYear vs. PEFA #####
data = umf_stacked@siteCovs %>% 
  select(BreedingYear, PEFA) %>% 
  mutate(Site = factor(rep(c(1:43), each = 15))) %>% 
  add_count(Site, PEFA, wt = NULL)

ggplot(data, aes(x=BreedingYear, y=PEFA)) +
  geom_jitter()+
  scale_x_continuous(n.breaks = 14)

Year.PEFA = umf_stacked@siteCovs %>% 
  select(BreedingYear, PEFA, AreaType)

m = glm(PEFA ~ BreedingYear+ (1|AreaType), family = binomial, data = Year.PEFA)
summary(m)
plot(m)

