# Script to identify and pull data for groups in NAFO data that can fill in comland data gaps
library(dplyr)
library(ggplot2)

# Identify groups with missing data ------------------------

#Load comland catch data (aggregated by spp and year)
data.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/"
fig.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/"
load(paste0(data.dir,'Catch_Comparison.RData'))

#Which don't go back to 1964
no.start = new.comland.stat.df %>%
  group_by(Group) %>%
  summarize(min.year = min(Year,na.rm=T)) %>%
  filter(min.year > 1964)

#Which don't go up until 2018
no.end = new.comland.stat.df %>%
  group_by(Group) %>%
  summarize(max.year = max(Year,na.rm=T)) %>%
  filter(max.year <2018)
  
