#Script to plot maps of fleet-wide catch
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleet_calibration_4_q'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%
  select(LongName,Code)

neus.map = map_data('worldHires',region = c('USA','Canada'))

catch.fleet.model = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))%>%
  mutate(time = (time*365))

catch.fleet.tot = catch.fleet.model %>%
  group_by(time, fleet)%>%
  summarise(catch = sum(atoutput,na.rm=T))%>%
  mutate(var = 'model')

catch.fleet.spp = catch.fleet.model %>%
  group_by(time,fleet,species)%>%
  summarise(catch = sum(atoutput,na.rm=T)) %>% 
  mutate(var = 'model')

catch.spp = catch.fleet.model %>%
  group_by(time,species)%>%
  summarise(catch = sum(atoutput,na.rm=T)) %>% 
  mutate(var = 'model')

catch.ref.gf =readRDS(here::here('data-raw','data','groundfishFleetData.rds'))$landings%>%
  mutate(fleet = paste0('gf',gsub(' ','',tolower(newport))))%>%
  select(Year,Box,fleet,Code,landings)%>%
  group_by(Year,fleet,Code)%>%
  summarise(catch = sum(landings,na.rm=T))
catch.ref.sca = readRDS(here::here('data-raw','data','scallopFleetData.rds'))$landings%>%
  mutate(fleet = paste0('SCA',gsub(' ','',tolower(newport))))%>%
  select(Year,Box,fleet,Code,landings)%>%
  group_by(Year,fleet,Code)%>%
  summarise(catch = sum(landings,na.rm=T))
catch.ref.ts = readRDS(here::here('data','neusCatchData.rds'))%>% 
  filter(!(Code %in% c(unique(catch.ref.gf$Code),'SCA')))%>%
  rename(Year = 'YEAR',
         catch = 'value')%>%
  mutate(fleet = 'catchall')
         # catch = catch/1000)
catch.ref = catch.ref.gf %>%
  bind_rows(catch.ref.sca)%>%
  bind_rows(catch.ref.ts)%>%
  mutate(time = (Year-1964)*365)%>%
  left_join(fgs)%>%
  rename(species = 'LongName')

catch.ref.fleet.tot = catch.ref %>%
  group_by(time, fleet)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(var = 'reference')

catch.ref.fleep.spp = catch.ref %>%
  group_by(time,fleet, species)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(var = 'reference')

catch.ref.spp = catch.ref %>%
  group_by(time,species)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(var = 'reference')

#Plot Catch by Fleet
catch.fleet.tot.all = catch.fleet.tot %>%
  bind_rows(catch.ref.fleet.tot)

ggplot(data = catch.fleet.tot.all, aes(x= time, y = catch, col = var))+
  geom_line()+
  facet_wrap(~fleet,scale = 'free_y')
ggsave(paste0(figure.dir,run.name,'_catch_fleet_total.png'))

#Plot Catch by species
catch.spp.all = catch.spp %>%
  bind_rows(catch.ref.spp)

ggplot(data = catch.spp.all, aes(x= time, y = catch, col = var))+
  geom_line()+
  facet_wrap(~species,scale = 'free_y')
ggsave(paste0(figure.dir,run.name,'_catch_species_total.png'))

#Fleet species Combs
catch.fleet.spp.all = catch.fleet.spp %>%
  bind_rows(catch.ref.fleep.spp)
fleet.names = sort(unique(catch.fleet.spp.all$fleet))

pdf(paste0(figure.dir,run.name,'_catch_fleet_species.pdf'))
for(i in 1:length(fleet.names)){
  
  this.catch = catch.fleet.spp.all %>%
    filter(fleet == fleet.names[i])

  p = ggplot(data = this.catch, aes(x= time, y = catch, col = var))+
    geom_line()+
    facet_wrap(~species,scale = 'free_y')+
    ggtitle(fleet.names[i])
  gridExtra::grid.arrange(p)
}
dev.off()
