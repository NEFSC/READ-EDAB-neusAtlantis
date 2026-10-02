#function to test persistence for a single box or aggregation of boxes
library(dplyr)

run.name = 'ZM_Spatial_4'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/')

source(here::here('R','make_biomass_EPU.R'))
make_biomass_EPU(
  run.name = run.name,
  run.dir = run.dir,
  epu.file = here::here('Geometry','box2epu.csv'),
  groups.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_groups.csv',
  bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm',
  out.dir = paste0(run.dir,'Post_Processed/'),
  out.name = run.name
)

boxes = c(8,12:15)
fgs = atlantisom::load_fgs(dir = here::here('currentVersion'),'neus_groups.csv') %>% 
  dplyr::filter(IsTurnedOn == T)

fgs2 = fgs %>%
  dplyr::select(Code,Name)%>%
  rename(species = 'Name',spp = 'Code')

box.bio = read.csv(paste0(run.dir,'Post_Processed/',run.name,'_box_age_biomass.csv'))

box.bio = box.bio %>%
  dplyr::filter(box %in% boxes)%>%
  mutate(date = as.POSIXct(time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')))%>%
  dplyr::group_by(year,spp) %>%
  dplyr::summarise(biomass = mean(value,na.rm=T))%>%
  select(spp,year,biomass)%>%
  left_join(fgs2)
  # rename(code = 'spp',time ='year')%>%
  # mutate(agecl = NA, polygon = NA, layer = NA) %>%
  # select(species,agecl,polygon,layer,time,atoutput,code)

bio.init = box.bio %>% 
  filter(year == 1964)%>%
  rename(init.biomass = 'biomass')%>%
  ungroup()%>%
  select(spp,init.biomass)

persist.box = box.bio %>%
  left_join(bio.init)%>%
  mutate(relBiomass = biomass/init.biomass)%>%
  group_by(spp)%>%
  summarise(init.biomass = mean(init.biomass,na.rm=T),
            min.biomass = min(relBiomass,na.rm=T))%>%
  mutate(persist = min.biomass>0.1)
