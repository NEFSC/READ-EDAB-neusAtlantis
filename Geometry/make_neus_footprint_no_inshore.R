#Script to generate a new NEUS outer polygon for a survdat pull that excludes inshore habitats
library(sf)
library(dplyr)
library(tidyverse)

# survey.index = read.csv(here::here('data','NHRA_Footprint_Index.csv'))
# survey.poly = st_read(here::here('data','Inshore_Survey_Footprint.kml'))%>%
#   left_join(survey.index)%>%
#   filter(SOURCE %in% c('NEMAP_Trawl','MA_Trawl','ME-NH_Trawl'))%>%
#   st_union()
survey.poly = readRDS(here::here('data','Inshore_Survey_Area.RDS'))%>%
  st_sf()%>%
  st_union()%>%
  st_zm(drop = T, what = 'ZM')

neus.poly = st_read(here::here('Geometry','gis','NEUS_ll_0p01.shp'))%>%
  filter(BOX_ID %in% 1:22)%>%
  st_union()%>%
  st_zm(drop = T, what = 'ZM')

neus.poly.noinshore = st_difference(neus.poly,survey.poly)%>%
  st_sf()%>%
  rowid_to_column()%>%
  st_cast('POLYGON')%>%
  mutate(area = st_area(geometry))%>%
  group_by(rowid)%>%
  top_n(1,area)%>%
  select(-area)%>%
  st_sf()%>%
  st_zm(drop = T, what = 'ZM')

st_write(neus.poly.noinshore,here::here('Geometry','gis','NEUS_NoInshore.shp'))

# plot(neus.poly)
# plot(survey.poly)
# plot(neus.poly.noinshore)
# 
# survey.poly2 = survey.poly %>% st_sf() %>%rowid_to_column()
# neus.poly.noinshore2 = neus.poly.noinshore %>% rowid_to_column()
# test = rbind(survey.poly2,neus.poly.noinshore2)
# test$rowid = 1:2
# plot(test)

# x = read_sf(here::here('Geometry','gis','NEUS_NoInshore.shp'))
# plot(x)
