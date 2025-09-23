#For a target species, plot multiple output comparisons between a scenario and reference run
library(dplyr)
library(ggplot2)

run.dir = here::here('Atlantis_Runs','')

scenario.name = 'YTF_qx10'
scenario.long.name = 'More Targeted Yellowtail'
ref.name = 'dev_07152025'
ref.long.name = 'Normal Yellowtail'

target.code = 'YTF'

run.names = c(scenario.name,ref.name)
run.long.names = c(scenario.long.name, ref.long.name)

plot.cols = RColorBrewer::brewer.pal(7,'Set1')
#Species info
fgs = read.csv(here::here('currentVersion','neus_groups.csv'))
target.longname = fgs$LongName[which(fgs$Code == target.code)]

#Plot biomass
biomass.ls = lapply(run.names, function(x){
  data = readRDS(here::here('Atlantis_Runs',x,'Post_Processed','Data','biomass.rds')) %>% 
    filter(species == target.longname) %>% 
    mutate(run.name = x)
  return(data)
})
biomass.df = bind_rows(biomass.ls) %>% 
  left_join(data.frame(run.name = run.names, run.long.name = run.long.names))

ggplot(data= biomass.df, aes(x = time, y = atoutput, color = run.long.name))+
  geom_line()+
  scale_color_manual(name = 'Scenario',values = plot.cols[1:length(run.names)])+
  ggtitle(paste0(target.longname,' - Biomass'))+
  ylab('Biomass (mT)')+
  xlab('Year')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures',paste0(target.longname,' - Biomass.png')))

#Plot Target Catch
catch.ls = lapply(run.names, function(x){
  data = readRDS(here::here('Atlantis_Runs',x,'Post_Processed','Data','catch.rds')) %>% 
    filter(species == target.longname) %>% 
    mutate(run.name = x)
  return(data)
})
catch.df = bind_rows(catch.ls) %>% 
  group_by(run.name,species,time) %>% 
  summarise(atoutput = sum(atoutput,na.rm=T)) %>% 
  left_join(data.frame(run.name = run.names, run.long.name = run.long.names))

ggplot(data= catch.df, aes(x = time, y = atoutput, color = run.long.name))+
  geom_line()+
  scale_color_manual(name = 'Scenario',values = plot.cols[1:length(run.names)])+
  ggtitle(paste0(target.longname,' - Catch'))+
  ylab('Catch (mT)')+
  xlab('Year')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures',paste0(target.longname,' - Catch.png')))

#Plot Fleet Catch
catch.fleet.ls = lapply(run.names, function(x){
  data = readRDS(here::here('Atlantis_Runs',x,'Post_Processed','Data','catch_fleet.rds')) %>% 
    filter(species == target.longname & grepl('^gf',fleet)) %>% 
    mutate(run.name = x)
  return(data)
})
catch.fleet.df = bind_rows(catch.fleet.ls) %>% 
  group_by(run.name,species,fleet,time) %>% 
  summarise(atoutput = sum(atoutput,na.rm=T)) %>% 
  left_join(data.frame(run.name = run.names, run.long.name = run.long.names)) %>% 
  tidyr::separate(fleet,c('dum','port'),sep = 'gf',remove = F)


ggplot(data= catch.fleet.df, aes(x = time, y = atoutput, color = run.long.name))+
  geom_line()+
  facet_wrap(~port)+
  scale_color_manual(name = 'Scenario',values = plot.cols[1:length(run.names)])+
  ggtitle(paste0(target.longname,' - Catch by Fleet'))+
  ylab('Catch (mT)')+
  xlab('Year')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures',paste0(target.longname,' - Catch by Fleet.png')))

