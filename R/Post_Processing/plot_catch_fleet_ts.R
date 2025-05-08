#Script to plot maps of fleet-wide catch
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleet_calibration_8_revesc'

run.dir = here::here('Atlantis_Runs',run.name)
ref.years = c(20,60)
figure.dir = paste0(run.dir,'/Post_Processed/')
if(!dir.exists(figure.dir)){dir.create(figure.dir)}

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%
  select(LongName,Code)

neus.map = map_data('worldHires',region = c('USA','Canada'))

catch.fleet.model = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))%>%
  # filter(species == 'Atlantic halibut')%>%
  # group_by(fleet)%>%
  # summarise(catch = sum(atoutput,na.rm=T))

  rename(year = 'time')%>%
  mutate(time = (year*365))%>%
  filter(year >= ref.years[1] & year <= ref.years[2])

#   
# ggplot(catch.fleet.model,aes(x=time, y = catch))+geom_line()

  

catch.fleet.tot = catch.fleet.model %>%
  group_by(time, fleet)%>%
  summarise(catch = sum(atoutput,na.rm=T))%>%
  mutate(var = 'model')

catch.fleet.spp = catch.fleet.model %>%
  # filter(fleet == 'gfboston' & species == 'Atlantic halibut')%>%
  group_by(time,fleet,species)%>%
  summarise(catch = sum(atoutput,na.rm=T)) %>% 
  mutate(var = 'model')
  # filter(species == 'Atlantic halibut')%>%
  # group_by(fleet)%>%
  # summarise(catch = sum(catch))

catch.spp = catch.fleet.model %>%
  group_by(time,species)%>%
  summarise(catch = sum(atoutput,na.rm=T)) %>% 
  mutate(var = 'model')

catch.ref.gf =readRDS(here::here('data-raw','data','groundfishFleetData.rds'))$landings%>%
  mutate(fleet = paste0('gf',gsub(' ','',tolower(newport))))%>%
  filter(Year>= (1964+ref.years[1]) & Year <= (1964+ref.years[2])&Box %in% 1:22)%>%
  select(Year,Box,fleet,Code,landings)%>%
  group_by(Year,fleet,Code)%>%
  summarise(catch = sum(landings,na.rm=T))

catch.ref.sca = readRDS(here::here('data-raw','data','scallopFleetData.rds'))$landings%>%
  mutate(fleet = paste0('SCA',gsub(' ','',tolower(newport))))%>%
  select(Year,Box,fleet,Code,landings)%>%
  filter(Year >= (1964+ref.years[1]) & Year <= (1964+ref.years[2])&Box %in% 1:22)%>%
  group_by(Year,fleet,Code)%>%
  summarise(catch = sum(landings,na.rm=T))
catch.ref.ts = readRDS(here::here('data','neusCatchData.rds'))%>% 
  filter(!(Code %in% c(unique(catch.ref.gf$Code),'SCA')) & YEAR >= (1964+ref.years[1]) & YEAR <= (1964+ref.years[2]))%>%
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
  facet_wrap(~fleet,scale = 'free_y')+
  theme_bw()
ggsave(paste0(figure.dir,run.name,'_catch_fleet_total.png'), width = 12,height = 12, dpi = 250)

sweptarea.corr = catch.fleet.tot.all %>%
  tidyr::spread(var,catch)%>%
  filter(!is.na(reference) & model>0)%>%
  mutate(corr.ratio = reference/model)%>%
  group_by(fleet)%>%
  summarise(sweptarea.corr = mean(corr.ratio,na.rm=T))
write.csv(sweptarea.corr,here::here('Setup_Files',paste0(run.name,'_sweptarea_corrections.csv')),row.names =F)

#Plot Catch by species
catch.spp.all = catch.spp %>%
  bind_rows(catch.ref.spp)

ggplot(data = catch.spp.all, aes(x= time, y = catch, col = var))+
  geom_line()+
  facet_wrap(~species,scale = 'free_y')+
  theme_bw()
ggsave(paste0(figure.dir,run.name,'_catch_species_total.png'), width = 12,height = 12, dpi = 250)

#Fleet species Combs
catch.fleet.spp.all = catch.fleet.spp %>%
  bind_rows(catch.ref.fleep.spp)
fleet.names = sort(unique(catch.fleet.spp.all$fleet))
catch.corr.ls = list()
pdf(paste0(figure.dir,run.name,'_catch_fleet_species.pdf'), width = 12,height = 12)
for(i in 1:length(fleet.names)){
  
  this.catch = catch.fleet.spp.all %>%
    # filter(species == 'Atlantic halibut') %>%
    filter(fleet == fleet.names[i])%>%
    mutate(time = time/365)

  p = ggplot(data = this.catch, aes(x= time, y = catch, col = var))+
    geom_line()+
    facet_wrap(~species,scale = 'free_y')+
    ggtitle(fleet.names[i])+
    theme_bw()
  gridExtra::grid.arrange(p)
  
  catch.corr.ls[[i]] = this.catch %>%
    tidyr::spread(var,catch)%>%
    # filter(!is.na(reference) & model>0)%>%
    group_by(fleet,species)%>%
    summarise(model.mean = mean(model,na.rm=T),
              model.max = max(model,na.rm=T),
              ref.mean = mean(reference,na.rm=T),
              ref.max = max(reference,na.rm=T)) %>%
    mutate(corr.mean = ref.mean/model.mean,
           corr.max = ref.max/model.max)
}
dev.off()
catch.corr.df = bind_rows(catch.corr.ls)

write.csv(catch.corr.df, here::here('Setup_Files',paste0(run.name,'_q_corrections.csv')),row.names =F)

# x = catch.fleet.spp.all %>%
#   filter(species == 'Atlantic halibut') %>%
#   mutate(year = floor(time/365))%>%
#   group_by(fleet,year)%>%
#   summarise(catch = sum(catch,na.rm=T))
# ggplot(x, aes(x = year, y= catch))+geom_line()+facet_wrap(~fleet)
