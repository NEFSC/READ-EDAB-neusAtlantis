#Script to plot maps of fleet-wide catch center of mass
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'gfsca_bgmbox8deph50'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

neus.map = map_data('worldHires',region = c('USA','Canada'))

catch.fleet = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))

catch.fleet.comp = catch.fleet %>%
  mutate(polygon = as.factor(polygon),
         time = as.numeric(time))%>%
  filter(grepl('gf',fleet) & time >= 30)%>%
  group_by(fleet,species,time)%>%
  summarise(catch =sum(atoutput,na.rm=T))%>%
  group_by(fleet,time)%>%
  mutate(catch.tot = sum(catch,na.rm=T))%>%
  ungroup()%>%
  mutate(catch.prop = ifelse(catch.tot == 0,0,catch/catch.tot))
  
fleet.names = sort(unique(catch.fleet$fleet))

plot.cols = c(RColorBrewer::brewer.pal(12,'Paired'),'grey50','grey30')
i=1
pdf(paste0(figure.dir,run.name,'_fleet_catch_composition.pdf'))
for(i in 1:length(fleet.names)){
  
  plot.data = catch.fleet.comp %>%
    filter(fleet == fleet.names[i])
  
  p =ggplot(plot.data,aes(x = time, y = catch.prop, fill = species))+
    geom_bar(position = 'stack', stat = 'identity')+
    scale_fill_manual(values = plot.cols)+
    theme_bw()+
    ggtitle(paste0('Fleet: ',fleet.names[i]))+
    ylab('Proportion of Landings')
    
  gridExtra::grid.arrange(p)
    
}
dev.off()

#Do all over last 10 years

catch.fleet.comp.last10 =catch.fleet %>%
  mutate(time = as.numeric(time))%>%
  filter(time >= (max(time)-10) & grepl('^gf',fleet)) %>%
  group_by(fleet,species)%>%
  summarise(catch = sum(atoutput,na.rm=T))%>%
  group_by(fleet)%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot)

ggplot(catch.fleet.comp.last10,aes(x = fleet, y = catch.prop, fill = species))+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot.cols)+
  theme_bw()+
  ylab('Proportion of Landings')+
  theme(legend.position = 'bottom')

ggsave(paste0(figure.dir,run.name,'_catch_composition_all.png'),width = 12,height =10, units = 'in',dpi =300)


#Do all for reference data
catch.ref = readRDS(here::here('data','spatial_reference_landings_fleet.rds'))%>%
  filter(statistic == 'value' & grepl('^gf',fleet) & !is.na(species) & fleet != 'gfother')%>%
  group_by(fleet,species)%>%
  summarise(catch = sum(ref.value,na.rm=T))%>%
  group_by(fleet)%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot)
  
  
ggplot(catch.ref,aes(x = fleet, y = catch.prop, fill = species))+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(values = plot.cols)+
  theme_bw()+
  ylab('Proportion of Landings')+
  theme(legend.position = 'bottom')

ggsave(here::here('data','groundfish_catch_composition_reference.png'))
