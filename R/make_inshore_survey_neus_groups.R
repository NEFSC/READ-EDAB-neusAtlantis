#Convert inshore survey data inot NEUS groups
#Tidy dataframe
#Note: Zeros were caught but no weights provided: Chosen to remove
library(dplyr)

#Read Inshore survey biomass data
survey.dat = read.csv(here::here('data','Inshore_Survey_Biomass.csv'))
#Read Inshore survey tow data
tow.dat = read.csv(here::here('data','Inshore_Survey_Tows.csv'))
#Polygon to Survey index
survey.index = read.csv(here::here('data','NHRA_Footprint_Index.csv'))
#Read Survey Footprint
survey.footprint = readRDS(here::here('data','Inshore_Survey_Area.rds'))%>%
  st_drop_geometry()%>%
  left_join(survey.index)%>%
  group_by(SOURCE)%>%
  summarise(area.m2 = sum(area.m2,na.rm=T))

#Read in NEUS to NHRA species conversion
nhra2neus = read.csv(here::here('data','NHRA_NEUS_Index.csv'))%>%
  rename(Code = 'NEUS.Code',COMNAME = 'NHRA.COMNAME')

data.survey = survey.dat %>%
  left_join(nhra2neus)%>%
  group_by(Code,SOURCE,YEAR)%>%
  summarise(BIOMASS = sum(BIOMASS,na.rm=T))%>%
  filter(BIOMASS>0)

#Non-overlapping surveys For complete coastal coverage
survey.keep = c('NEAMAP_Trawl','MA_Trawl','ME-NH_Trawl')

data.domain = data.survey %>%
  filter(SOURCE %in% survey.keep) %>%
  left_join(select(survey.footprint))
  
  
  
  
  group_by(Code,YEAR)%>%
  summarise(BIOMASS = sum(BIOMASS,na.rm=T))
