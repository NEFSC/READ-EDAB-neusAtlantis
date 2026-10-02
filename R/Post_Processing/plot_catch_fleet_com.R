#Script to plot maps of fleet-wide catch center of mass
library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'gfsca_bgmbox8deph50'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

neus.map = map_data('worldHires',region = c('USA','Canada'))

catch.fleet = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))%>%
  mutate(polygon = as.factor(polygon),
         time = as.numeric(time))

bgm.file = here::here('geometry','neus_tmerc_RM2.bgm')
box.coords = atlantistools::convert_bgm(bgm.file)%>%
  dplyr::mutate(polygon = as.factor(polygon))
box.mid = box.coords %>%
  group_by(polygon)%>%
  summarise(lat.mid = mean(inside_lat),
            long.mid = mean(inside_long))

catch.fleet.com =catch.fleet %>%
  left_join(box.mid)%>%
  mutate(catch.wgt.x = long.mid * atoutput,
         catch.wgt.y = lat.mid * atoutput)%>%
  group_by(fleet,time)%>%
  summarise(com.x = sum(catch.wgt.x),
            com.y = sum(catch.wgt.y),
            catch.tot = sum(atoutput,na.rm=T))%>%
  mutate(com.x = com.x/catch.tot,
         com.y = com.y/catch.tot)
fleet.names = sort(unique(catch.fleet.com$fleet))

i =1 
pdf(paste0(figure.dir,run.name,'_fleet_com.pdf'))
for(i in 1:length(fleet.names)){
  
  plot.data = catch.fleet.com %>%
    filter(fleet == fleet.names[i])
  
  p =ggplot()+
    geom_polygon(data =box.coords,aes(x = long, y = lat, group = polygon),color = 'black',fill = 'transparent')+
    geom_line(data = plot.data,aes(x = com.x, y = com.y, color = time))+
    scale_color_viridis_c(name = 'year')+
    annotation_map(neus.map,fill = 'grey80',color = 'black')+
    theme_bw()+
    ggtitle(paste0('fleet: ',fleet.names[i]))+
    theme(legend.position = 'bottom')
    
  gridExtra::grid.arrange(p)
}
dev.off()


catch.fleet.com.10yr = catch.fleet.com %>%
  filter(grepl('^gf',fleet)) %>%
  mutate(decade = (floor((time)/10))*10)%>%
  filter(decade > 20)%>%
  group_by(fleet,decade)%>%
  summarise(com.x = mean(com.x,na.rm=T),
            com.y = mean(com.y,na.rm=T))

ggplot()+
  geom_polygon(data =box.coords,aes(x = long, y = lat, group = polygon),color = 'black',fill = 'transparent')+
  # annotate('polygon', x = box.coords$long, y = box.coords$lat, group = 'polygon', color = 'black')+
  geom_point(data = catch.fleet.com.10yr,aes(x = com.x, y = com.y, color = fleet), size = 8, alpha = 0.8)+
  scale_color_brewer(palette ='Paired')+
  facet_wrap(~decade,labeller = label_both)+
  annotation_map(neus.map,fill = 'grey80',color = 'black')+
  theme_bw()+
  ggtitle('Groundfish Fleets')+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,run.name,'_fleet_com_decade.png'),width = 12, height = 8, units = 'in', dpi = 300)
