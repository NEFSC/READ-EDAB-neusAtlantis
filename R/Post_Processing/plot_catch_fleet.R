#Script to plot maps of fleet-wide catch
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleets_example'
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

gf.catch.prop.all =catch.fleet %>%
  filter(time > 30 & grepl('^gf',fleet) & fleet != 'gfother')%>%
  group_by(fleet,species,polygon)%>%
  summarise(catch = mean(atoutput,na.rm=T))%>%
  group_by(polygon)%>%
  summarise(catch = sum(catch,na.rm=T))%>%
  mutate(catch.tot = sum(catch,na.rm=T),
         catch.prop = catch/catch.tot,
         polygon = as.factor(polygon),
         catch.prop = ifelse(catch.prop < 0.01,NA,catch.prop))

plot.data.all = boxes %>%
  left_join(gf.catch.prop.all)

ggplot(plot.data.all, aes( x= long, y = lat, group = polygon, fill = catch.prop))+
  geom_polygon( color = 'black')+
  scale_fill_viridis_c(name = 'Catch Proportion')+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  theme(legend.position = 'bottom')
