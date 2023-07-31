library(dplyr)
library(ggplot2)


#Get Map Data
bgm.file = here::here('Geometry','neus_ll_WGS84.bgm')
bgm = rbgm::bgmfile(bgm.file)

#Get coastline and outer bounds
corners = read.csv(here::here('Geometry','RM_allboxes_NEUS_corners.csv'),as.is = T)
corners$nrLayers = factor(corners$nrLayers)
mids = bgm$boxes %>% select(.bx0,insideX,insideY)
colnames(mids) = c('Area','midx','midy')
corners2 = merge(corners,mids)

corners2$label = sapply(corners2$Area, function(x) {
  if(x %in% c(0,23:29)){
    return(paste0(x,'*'))
  } else {
    return(x)
  }
})

box.col = data.frame(Area = 0:29,boundary = factor(c(1,rep(0,21),rep(1,8))))
corners2 =corners2 %>%
  left_join(box.col)

c.l1= corners2 %>% mutate(is.boundary = ifelse(Area %in% c(0,23:24),1,0),level = 1)
c.l2 = corners2 %>% mutate(is.boundary = ifelse(Area %in% c(0:2,4,7,13,23:24),1,0),level = 2)
c.l3 = corners2 %>% mutate(is.boundary = ifelse(Area %in% c(0:2,4:10,12,13,15,17,18,22:25),1,0), level = 3)

corners2 = bind_rows(c.l1,c.l2,c.l3)
corners2$is.boundary = as.factor(corners2$is.boundary)


lev.names = data.frame(level = 1:3,
                       level.names = c('Level 1 (0-50 m)',
                                       'Level 2 (50-120 m)',
                                       'Level 3 (120-300 m)'))

corners2 = corners2 %>%
  rename(box = 'Area') %>%
  select(box,level,Lat1,Lon1,is.boundary)%>%
  filter(level == 1)
  
#Get temp output
deltaT.dir = here::here('Atlantis_Runs','cm2_6_2100_dev_deltaT_TempDependOff','')
base.dir  = here::here('Atlantis_Runs','cm2_6_2100_dev_baseline_TempDependOff','')

deltaT.temp.2020 = readRDS(paste0(deltaT.dir,'/Post_Processed/Data/physics_statevars.rds'))%>%
  filter(variable == 'Temp',layer == 0,time == 57)%>%
  rename(box = 'polygon',level = 'layer')%>%
  mutate(level = level +1)%>%
  right_join(corners2)%>%
  mutate(group = 'Baseline 2020')

deltaT.temp.2100 = readRDS(paste0(deltaT.dir,'/Post_Processed/Data/physics_statevars.rds'))%>%
    filter(variable == 'Temp',layer == 0, time == 136)%>%
    rename(box = 'polygon',level = 'layer')%>%
    mutate(level = level +1)%>%
    right_join(corners2)%>%
  mutate(group = 'CM2.6 2100')
  
t.2100 = deltaT.temp.2100 %>%
  select(box,time,Lat1,Lon1,is.boundary,atoutput)%>%
  rename(temp.2100 = 'atoutput')
t.2020 = deltaT.temp.2020 %>%
  select(box,time,Lat1,Lon1,is.boundary,atoutput)%>%
  rename(temp.2020 = 'atoutput')
temp.change = t.2100 %>%
  left_join(t.2020, by = c('box','Lon1','Lat1'))%>%
  mutate(temp.change = temp.2100- temp.2020)

base.temp.2100 = readRDS(paste0(deltaT.dir,'/Post_Processed/Data/physics_statevars.rds'))%>%
  filter(variable == 'Temp',layer == 0, time == 57)%>%
  rename(box = 'polygon',level = 'layer')%>%
  mutate(level = level +1)%>%
  right_join(corners2)%>%
  mutate(group = 'Baseline 2100')

temp.all = deltaT.temp.2020 %>%
  bind_rows(deltaT.temp.2100)
  


ggplot()+
  annotation_map(map_data('world'),fill = 'chartreuse4',alpha = 0.75)+
  geom_polygon(data = temp.all, aes(x= Lon1,y = Lat1,group = box,fill = atoutput),col = 'black',alpha = 0.75)+
  scale_fill_gradientn(name = 'Temperature',colours = colorRamps::blue2red(7))+
  coord_equal()+
  xlim(-77,-63)+
  ylim(34,45)+
  facet_wrap(~group,nrow = 1)+
  theme_bw()+
  xlab('Longitude')+
  ylab('Latitude')+
  # ggtitle(paste('Atlantis Level',plot.combs$level[i],':',plot.combs$level.name[i]))+
  theme(
    panel.grid.minor = element_blank(),
    plot.title= element_text(hjust = 0.5),
    legend.position = 'bottom'
  )
ggsave(here::here('Figures','CM2_6_2100_temperature_box.png'),width = 8,height = 4, units= 'in', dpi =300)


ggplot()+
  annotation_map(map_data('world'),fill = 'chartreuse4',alpha = 0.75)+
  geom_polygon(data = temp.change, aes(x= Lon1,y = Lat1,group = box,fill = temp.change),col = 'black',alpha = 0.75)+
  scale_fill_gradientn(name = 'Temperature',colours = colorRamps::blue2red(7))+
  # guides(fill = guide_legend())
  coord_equal()+
  xlim(-77,-63)+
  ylim(34,45)+
  theme_bw()+
  xlab('Longitude')+
  ylab('Latitude')+
  ggtitle('SST Change (2100-2020)')+
  # ggtitle(paste('Atlantis Level',plot.combs$level[i],':',plot.combs$level.name[i]))+
  theme(
    panel.grid.minor = element_blank(),
    plot.title= element_text(hjust = 0.5),
    legend.position = 'bottom'
  )
ggsave(here::here('Figures','CM2_6_2100_temperature_change_box.png'),width = 8,height = 4, units= 'in', dpi =300)
