#Script to plot maps of fleet-wide catch
library(dplyr)
library(ggplot2)
library(mapdata)
library(dplyr)
library(ggplot2)

#Specify setup.csv used to generate calibration run set
experiment.id = 'cloud_sca_fleet_revised_1'
setup.df = read.csv(here::here('Setup_Files','cloud_sca_fleet_revised_1.csv'),as.is=T)
setup.df$file.ID = 1:nrow(setup.df)
# experiment.dir = here::here('Atlantis_Runs',experiment.id,'')
experiment.dir = paste0('/atlantisdisk/',experiment.id,'/')
# figure.dir = here::here('Figures',experiment.id,'')
figure.dir = paste0('/model/Joseph.Caracappa/calibration/Figures/',experiment.id,'/')

system(paste0('sudo chmod 777 -R ',experiment.dir))
if(!dir.exists(figure.dir)){dir.create(figure.dir,recursive = T)}

ref.years = c(20,60)

fgs = read.csv('/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/currentVersion/neus_groups.csv',as.is = T)%>%
  filter(IsTurnedOn == T)

plot.colors = c(RColorBrewer::brewer.pal(12,'Paired'),
                RColorBrewer::brewer.pal(8,'Dark2'),
                RColorBrewer::brewer.pal(12,'Set3'))

#Subset to q runs
setup.df = setup.df %>% 
  filter(Type %in% c('q','sweptarea')) %>% 
  tidyr::separate(Code, c('fleet','spp'),sep = ':',remove = F) %>% 
  # mutate(Value.round = signif(Value,2)) %>%
  # tidyr::unite('Run.Group.new',c(Type,Value.round),sep = '_',remove =F) %>% 
  mutate(Run.Group = case_when(
    Type == 'q' ~ paste0('q_',spp),
    T ~ Run.Group
  )) %>% 
  select(Run.ID,Run.Group,Code, Type,Unit,Value)

run.groups = sort(unique(setup.df$Run.Group))
run.ids = sort(unique(setup.df$Run.ID))
group.index = setup.df %>% 
  distinct(Run.ID,Run.Group)
i=6

#Post Processing
catch.fleet.ls = list()
catch.spp.ls = list()
catch.fleet.spp.ls = list()

for(i in 1:length(run.ids)){
  
  # this.group.runs = setup.df$Run.ID[which(setup.df$Run.Group == run.groups[i])]
  this.run.dir = paste0(experiment.dir,experiment.id,'_',run.ids[i])
  if(dir.exists(this.run.dir)){
    
    param.ls = atlantisdiagnostics::get_atl_paramfiles(param.dir = here::here('currentVersion'),
                                                       atl.dir = this.run.dir,
                                                       run.prefix = 'neus_output',
                                                       include_catch = T
                                                        )
    this.dat = atlantisdiagnostics::process_catch_fleet(fishery.prm = param.ls$fishery.prm,
                                             catch = param.ls$catch ,
                                             groups.file = param.ls$groups.file)
    
    catch.fleet.ls[[i]] = this.dat %>% 
      group_by(time,fleet) %>% 
      summarise(atoutput = sum(atoutput,na.rm=T)) %>% 
      mutate(Run.ID = run.ids[i])
    
    catch.spp.ls[[i]] = this.dat %>% 
      group_by(time,species) %>% 
      summarise(atoutput = sum(atoutput,na.rm=T)) %>% 
      mutate(Run.ID = run.ids[i])
    
    catch.fleet.spp.ls[[i]] = this.dat %>% 
      group_by(time,fleet,species) %>% 
      summarise(atoutput = sum(atoutput,na.rm=T)) %>% 
      mutate(Run.ID = run.ids[i])
  }
}

catch.fleet.file = paste0(experiment.dir,'combined_catch_fleet.rds')
catch.spp.file = paste0(experiment.dir,'combined_catch_species.rds')
catch.fleet.spp.file = paste0(experiment.dir,'combined_catch_fleet_species.rds')

catch.fleet = bind_rows(catch.fleet.ls)
catch.spp = bind_rows(catch.spp.ls)
catch.fleet.spp = bind_rows(catch.fleet.spp.ls)

saveRDS(catch.fleet,catch.fleet.file)
saveRDS(catch.spp,catch.spp.file)
saveRDS(catch.fleet.spp, catch.fleet.spp.file)

## Get Reference data

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

catch.ref = catch.ref.gf %>%
  bind_rows(catch.ref.sca)%>%
  bind_rows(catch.ref.ts)%>%
  mutate(time = (Year-1964)*365,
         year = Year - 1964)%>%
  left_join(fgs)%>%
  rename(species = 'LongName')

catch.ref.fleet.tot = catch.ref %>%
  group_by(year,time, fleet)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(var = 'reference',
         Run.ID = 'reference')

catch.ref.fleet.spp = catch.ref %>%
  group_by(year,time,fleet, species)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(var = 'reference',
         Run.ID = 'reference')

catch.ref.spp = catch.ref %>%
  group_by(year,time,species)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(var = 'reference',
         Run.ID = 'reference')

#Plot catch by fleet
catch.fleet.model = readRDS(catch.fleet.file)%>%
  rename(year = 'time',
         catch = 'atoutput')%>%
  mutate(time = (year*365),
         year = floor(year))%>%
  filter(year >= ref.years[1] & year <= ref.years[2]) %>% 
  mutate(var = 'model')

catch.fleet.spp.model = readRDS(catch.fleet.spp.file) %>%
  rename(year = 'time',
         catch = 'atoutput')%>%
  mutate(time = (year*365),
         year = floor(year))%>%
  filter(year >= ref.years[1] & year <= ref.years[2]) %>% 
  mutate(var = 'model')
  
catch.spp.model = readRDS(catch.spp.file) %>%
  rename(year = 'time',
         catch = 'atoutput')%>%
  mutate(time = (year*365),
         year = floor(year))%>%
  filter(year >= ref.years[1] & year <= ref.years[2]) %>% 
  mutate(var = 'model')

#Plot Catch by Fleet
catch.fleet.all = catch.fleet.model %>%
  mutate(Run.ID = as.character(Run.ID)) %>% 
  bind_rows(catch.ref.fleet.tot)%>%
  group_by(Run.ID,year,fleet,var)%>%
  summarise(catch =sum(catch,na.rm=T))
  

fleet.names = sort(unique(catch.fleet.all$fleet))

pdf(paste0(figure.dir,experiment.id,'_catch_fleet_total.pdf'), width = 12,height = 12)
for(f in 1:length(run.groups)){
  
  run.id.match = group.index$Run.ID[which(group.index$Run.Group == run.groups[f])]
  fleet.dat = catch.fleet.all %>% 
    filter(Run.ID %in% c(run.id.match,'reference')) %>% 
    mutate(Run.ID = factor(Run.ID))
  
  this.cols = c(plot.colors[1:length(run.id.match)],'black')
  
  p = ggplot(data = fleet.dat, aes(x= year, y = catch, color = Run.ID))+
    geom_line()+
    scale_color_manual(values = this.cols)+
    facet_wrap(~fleet,scale = 'free_y')+
    theme_bw()+
    ggtitle(run.groups[f])+
    guides(color = guide_legend(position = 'bottom',nrow = 2))
    
  print(p)
}
dev.off()

# sweptarea.corr = catch.fleet.tot.all %>%
#   tidyr::spread(var,catch)%>%
#   filter(!is.na(reference) & model>0)%>%
#   mutate(corr.ratio = reference/model)%>%
#   group_by(fleet)%>%
#   summarise(sweptarea.corr.mean = mean(corr.ratio,na.rm=T),
#             sweptarea.corr.max = max(corr.ratio,na.rm=T),
#             sweptarea.corr.min = min(corr.ratio,na.rm=T))
# write.csv(sweptarea.corr,here::here('Setup_Files',paste0(run.name,'_sweptarea_corrections.csv')),row.names =F)

#Plot Catch by species
catch.spp.all = catch.spp.model %>%
  mutate(Run.ID = as.character(Run.ID)) %>% 
  bind_rows(catch.ref.spp) %>% 
  group_by(Run.ID,year,species,var)%>%
  summarise(catch =sum(catch,na.rm=T))

catch.spp.allyr = catch.spp.all %>%
  group_by(Run.ID,year,species,var)%>%
  summarise(catch = sum(catch,na.rm=T))

pdf(paste0(figure.dir,experiment.id,'_catch_species_total.pdf'), width = 24,height = 24)
for(f in 1:length(run.groups)){
  
  run.id.match = group.index$Run.ID[which(group.index$Run.Group == run.groups[f])]
  fleet.dat = catch.spp.allyr %>% 
    filter(Run.ID %in% c(run.id.match,'reference')) %>% 
    group_by(species) %>% 
    mutate(mean.catch = mean(catch,na.rm=T)) %>% 
    filter(mean.catch > 0) %>% 
    select(-mean.catch) %>% 
    mutate(Run.ID = factor(Run.ID),
           species = factor(species))

  this.cols = c(plot.colors[1:length(run.id.match)],'black')
  
  p = ggplot(data = fleet.dat, aes(x= year, y = catch, color = Run.ID))+
    geom_line()+
    scale_color_manual(values = this.cols)+
    facet_wrap(~species,scale = 'free')+
    theme_bw()+
    ggtitle(run.groups[f])+
    guides(color = guide_legend(position = 'bottom',nrow = 2))
  
  print(p)
}
dev.off()

#whole model

catch.whole.model =catch.spp.allyr %>%
  group_by(Run.ID,year,var)%>%
  summarise(catch = sum(catch,na.rm=T))

pdf(paste0(figure.dir,experiment.id,'_catch_total.pdf'), width = 8,height = 5)
for(f in 1:length(run.groups)){
  
  run.id.match = group.index$Run.ID[which(group.index$Run.Group == run.groups[f])]
  fleet.dat = catch.whole.model %>% 
    filter(Run.ID %in% c(run.id.match,'reference')) %>% 
    mutate(Run.ID = factor(Run.ID))
  
  this.cols = c(plot.colors[1:length(run.id.match)],'black')
  
  p = ggplot(data = fleet.dat, aes(x= year, y = catch, color= Run.ID))+
    geom_line()+
    scale_color_manual(values = this.cols)+
    theme_bw()+
    ggtitle(run.groups[f])+
    guides(color = guide_legend(position = 'bottom',nrow = 2))
  print(p)
}
dev.off()

#Fleet species Combs
catch.fleet.spp.all = catch.fleet.spp.model %>%
  mutate(Run.ID = as.character(Run.ID)) %>% 
  bind_rows(catch.ref.fleet.spp) %>% 
  group_by(Run.ID,year,fleet,species,var)%>%
  summarise(catch =sum(catch,na.rm=T))
  

fleet.names = sort(unique(catch.fleet.spp.all$fleet))
for(i in 1:length(run.groups)){
  
  run.id.match = group.index$Run.ID[which(group.index$Run.Group == run.groups[i])]
  this.cols = c(plot.colors[1:length(run.id.match)],'black')
  this.fig = paste0(figure.dir,experiment.id,'_',run.groups[i],'_catch_fleet_species.pdf')
  pdf(this.fig,width = 12,height = 12)

  for(f in 1:length(fleet.names)){
    
    this.catch = catch.fleet.spp.all %>%
      filter(fleet == fleet.names[f] & Run.ID %in% c(run.id.match,'reference'))%>%
      group_by(Run.ID,year,fleet,species,var)%>%
      summarise(catch = sum(catch,na.rm=T)) %>% 
      group_by(species) %>% 
      mutate(mean.catch = mean(catch,na.rm=T)) %>% 
      filter(mean.catch > 0) %>% 
      select(-mean.catch) %>% 
      mutate(Run.ID = factor(Run.ID),
             species = factor(species))
    
    p=ggplot(data = this.catch, aes(x= year, y = catch, color = Run.ID))+
      geom_line()+
      facet_wrap(~species,scale = 'free_y')+
      scale_color_manual(values = this.cols)+
      guides(color = guide_legend(position = 'bottom',nrow = 2))+
      ggtitle(fleet.names[i])+
      theme_bw()
    print(p)
  }
  
  dev.off()
}

