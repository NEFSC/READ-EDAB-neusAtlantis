#Map of NEUS groundfish fleet port locations
library(ggplot2)
library(mapdata)
library(dplyr)

neus.map = map_data('worldHires',region = c('USA','Canada'))

bgm.file = here::here('geometry','neus_tmerc_RM2.bgm')
boxes = atlantistools::convert_bgm(bgm.file)%>%
  dplyr::mutate(polygon = as.factor(polygon))

port = read.csv(here::here('data','groundfish_ports.csv'))
port$hjust = c(rep(1.1,4),0,rep(1.1,3),0,0,0)
port$vjust = c(rep(-1,8),2,-1.5,-1.5)
port$color = c(rep('blue4',5),'purple3',rep('blue4',2),rep('red4',3))
port$group = c(rep('Groundfish',5),'Both',rep('Groundfish',2),rep('Scallop',3))
port$group = factor(port$group, levels = c('Groundfish','Scallop','Both'))
port = port[-9,]

colors = c('lightblue3','orange','purple1')

ggplot()+
  annotation_map(neus.map,fill = 'grey90',color = 'black')+
  geom_polygon( data =boxes, aes(x = long, y = lat, group = polygon),fill = 'transparent',color = 'black',lwd =1.1)+
  geom_point(data = port, aes(x = lon, y = lat, fill = group),size = 8, pch  = 21)+
  shadowtext::geom_shadowtext(data = port, aes(x = lon, y = lat, color = group,label = MainPort, hjust = hjust, vjust = vjust))+
  scale_color_manual(name = '',values = colors)+
  scale_fill_manual(name = '',values = colors)+
  # annotate('point',x = port$lon,y = port$lat,pch = 21, size =8, fill = port$color,alpha = 0.5)+
  # annotate('text',x = port$lon,y = port$lat, label = port$MainPort, color = port$color,hjust =port$hjust,vjust =port$vjust,fontface = 'bold')+
  xlim(-77,-63.5)+
  theme_bw()+
  theme(legend.position = 'bottom')
  
ggsave(here::here('geometry','neus_fleets_map.png'), width = 7, height = 8, units = 'in', dpi =300)
