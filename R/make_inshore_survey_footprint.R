#Determine total survey area for each inshore survey
library(sf)

survey_index = read.csv(here::here('data','NHRA_Footprint_Index.csv'))
  

survey.dat = st_read(here::here('Geometry','gis','Survey_Footprint.kml'))%>%
  left_join(survey_index)%>%
  filter(SOURCE %in% c('NEMAP_Trawl','MA_Trawl','ME-NH_Trawl'))
plot(survey.dat)

survey.ma = survey.dat %>%  filter(Name == 'MA_Footprint')
survey.mv =survey.dat %>%  filter(Name == 'MA_Footprint_MarthasVineyard')
survey.ma2 = st_difference(survey.ma,survey.mv) %>%
  select(-Name.1,-Description.1,-SOURCE.1)

survey.dat2 = survey.dat %>%
  filter(SOURCE != 'MA_Trawl')%>%
  rbind(survey.ma2)

plot(survey.dat2)

survey.dat2$area.m2 = st_area(survey.dat2)

saveRDS(survey.dat2,here::here('data','Inshore_Survey_Area.RDS'))
