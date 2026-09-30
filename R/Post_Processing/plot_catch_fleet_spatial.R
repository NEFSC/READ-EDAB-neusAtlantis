#Script to plot maps of fleet-wide catch
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'SCA_redist_init_3'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

neus.map = map_data('worldHires',region = c('USA','Canada'))

catch.fleet = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))

# catch.fleet %>%
#   filter(fleet == 'gfgloucester' & time == max(time) & species != 'Atlantic cod')%>%
#   # tidyr::spread(polygon,atoutput)
#   group_by(fleet,polygon)%>%
#   summarise(sum(atoutput,na.rm=T))%>%
#   print(n = 30)

catch.fleet.prop = catch.fleet %>%
  mutate(time = as.numeric(time))%>%
  filter(time > 30 )%>%
  group_by(fleet,polygon)%>%
  summarise(catch = sum(atoutput,na.rm= T))%>%
  mutate(polygon = as.factor(polygon)) %>%
  group_by(fleet)%>%
  mutate(catch.tot = sum(catch,na.rm=T))%>%
  ungroup()%>%
  mutate(catch.prop = round(catch/catch.tot,2))
catch.fleet.prop$catch.prop = ifelse(catch.fleet.prop$catch.prop==0,NA,catch.fleet.prop$catch.prop)

bgm.file = here::here('geometry','neus_tmerc_RM2.bgm')
boxes = atlantistools::convert_bgm(bgm.file)%>%
  dplyr::mutate(polygon = as.factor(polygon))

fleet.names = sort(unique(catch.fleet.prop$fleet))

i=1
pdf(paste0(figure.dir,run.name,'_fleet_catch_distriubtion.pdf'))
for(i in 1:length(fleet.names)){
  
  plot.data = boxes %>%
    left_join(filter(catch.fleet.prop,fleet == fleet.names[i]))
  
  p =ggplot(plot.data, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
    geom_polygon( color = 'black')+
    scale_fill_viridis_c(name = 'Catch Proportion')+
    annotation_map(neus.map,fill = 'grey80',color = 'black')+
    theme_bw()+
    ggtitle(paste0('Fleet: ',fleet.names[i]))+
    theme(legend.position = 'bottom')
  
  gridExtra::grid.arrange(p)
}
dev.off()

#Combined groundfish map
gf.catch.prop.all =catch.fleet %>%
  filter(time > 30 & grepl('^gf',fleet) & fleet != 'gfother')%>%
  group_by(fleet,species,polygon)%>%
  summarise(catch = mean(atoutput,na.rm=T))%>%
  group_by(polygon)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot,
         polygon = as.factor(polygon),
         catch.prop = ifelse(catch.prop ==0,NA,catch.prop))

gf.plot.data.all = boxes %>%
  left_join(gf.catch.prop.all)

ggplot(gf.plot.data.all, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
  geom_polygon( color = 'black')+
  scale_fill_viridis_c(name = 'Catch Proportion')+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave(paste0(figure.dir,'groundfish_catch_all.png'),width = 10,height =10,units ='in',dpi =300)

#Combined scallop map
sca.catch.prop.all =catch.fleet %>%
  filter(time > 30 & grepl('^SCA',fleet) & fleet != 'SCAother')%>%
  group_by(fleet,species,polygon)%>%
  summarise(catch = mean(atoutput,na.rm=T))%>%
  group_by(polygon)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot,
         polygon = as.factor(polygon),
         catch.prop = ifelse(catch.prop ==0,NA,catch.prop))

sca.plot.data.all = boxes %>%
  left_join(sca.catch.prop.all)

ggplot(sca.plot.data.all, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
  geom_polygon( color = 'black')+
  scale_fill_viridis_c(name = 'Catch Proportion')+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave(paste0(figure.dir,'scallop_catch_all.png'),width = 10,height =10,units ='in',dpi =300)

#Make plot of catch by box for grounfish for reference area
gf.catch.ref =readRDS(here::here('data','spatial_reference_landings_fleet.rds'))%>%
  filter(grepl('^gf',fleet)& statistic == 'value' & var.name == 'catch_fleet')%>%
  group_by(polygon)%>%
  summarise(catch = sum(ref.value,na.rm=T))%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot,
         polygon = factor(polygon))%>%
  mutate(catch.prop = ifelse(as.numeric(polygon) > 23, NA, catch.prop))

plot.gf.ref = boxes %>%
  left_join(gf.catch.ref)
  
ggplot(plot.gf.ref, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
  geom_polygon( color = 'black')+
  scale_fill_viridis_c(name = 'Catch Proportion')+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave(paste0(figure.dir,'groundfish_catch_ref.png'),width = 10,height =10,units ='in',dpi =300)

#Make plot of catch by box for scallop for reference area
sca.catch.ref =readRDS(here::here('data-raw','data','scallopFleetData.rds'))$landings %>% 
  group_by(Box)%>%
  summarise(catch = sum(landings,na.rm=T))%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot,
         Box = factor(Box))%>%
  mutate(catch.prop = ifelse(as.numeric(Box) > 23, NA, catch.prop))

plot.sca.ref = boxes %>%
  left_join(sca.catch.ref, by = c('polygon' ='Box'))

ggplot(plot.sca.ref, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
  geom_polygon( color = 'black')+
  scale_fill_viridis_c(name = 'Catch Proportion')+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave(paste0(figure.dir,'scallop_catch_ref.png'),width = 10,height =10,units ='in',dpi =300)

#Make plot of effort by box for scallop effort for reference area
sca.effort.ref =readRDS(here::here('data-raw','data','scallopFleetData.rds'))$effort %>% 
  group_by(Box)%>%
  summarise(effort = sum(effort,na.rm=T))%>%
  mutate(effort.tot = sum(effort,na.rm=T),
         effort.prop = effort/effort.tot,
         Box = factor(Box))%>%
  mutate(catch.prop = ifelse(as.numeric(Box) > 23, NA, effort.prop))

plot.sca.eff.ref = boxes %>%
  left_join(sca.effort.ref, by = c('polygon' ='Box'))

ggplot(plot.sca.eff.ref, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
  geom_polygon( color = 'black')+
  scale_fill_viridis_c(name = 'Catch Proportion')+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave(paste0(figure.dir,'scallop_effort_ref.png'),width = 10,height =10,units ='in',dpi =300)

