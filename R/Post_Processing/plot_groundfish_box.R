#Script to plot maps of all groundfish distribution by box
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleets_example'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

neus.map = map_data('worldHires',region = c('USA','Canada'))

bgm.file = here::here('geometry','neus_tmerc_RM2.bgm')
boxes = atlantistools::convert_bgm(bgm.file)%>%
  dplyr::mutate(polygon = as.factor(polygon))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

biomass = readRDS(paste0(run.dir,'/Post_Processed/Data/biomass_box.rds'))

gf.spp = c('COD','HAD','YTF','POL','PLA','WTF','WHK','WIF','RED','HAL', 'WPF','OPT','WOL')
gf.spp.long = fgs$LongName[match(gf.spp,fgs$Code)]
  
gf.bio = biomass %>%
  filter(species %in% gf.spp.long, time > 30)%>%
  group_by(species,polygon)%>%
  summarise(biomass = mean(atoutput,na.rm=T))%>%
  group_by(polygon)%>%
  summarise(biomass = sum(biomass,na.rm=T))%>%
  mutate(biomass.tot = sum(biomass),
         biomass.prop = biomass/biomass.tot)


