#Plot box to EPU assignment
library(ggplot2)
library(ggforce)
library(maps)
library(mapdata)
library(dplyr)
library(magick)

bgm.file = here::here('Geometry','neus_ll_WGS84.bgm')

#Get full faces and coordinates data from bgm
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

#Read in box2epu
box2epu = read.csv(here::here('Geometry','box2epu.csv'))

bgm$boxes%>%
  filter(.bx0 %in% c(23,24))

corners2 = corners2 %>%
  left_join(box2epu, by = c('Area' = 'box'))

label.df = data.frame(x1 = c(-65,-67.5,-69,-72),
                      x2 = c(-65.5,-69,-70,-73.5),
                      y1 = c(40.5,39.5,38.5,38),
                      y2 = c(42,41,41,40),
                      label = c('NEC','GSC','NS','HC'))
island.df = data.frame(x1 = c(-67,-67),
                       x2 = c(-70.1,-68.3)-.25,
                       y1 = c(39,39),
                       y2 = c(41.3,44.3),
                       label = rep('Islands',2))

neus = maps::map('worldHires',xlim = c(-78,-62),ylim = c(34,46.5),plot = F)
plot.file = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/Figure_1_model_domain.png'

plot.cols = c(rgb(230,159,0,maxColorValue = 255),rgb(86,180,233, maxColorValue = 255),rgb(0,158,115, maxColorValue = 255))

ggplot()+ 
  annotation_map(map_data('worldHires'),fill = 'grey50',alpha = 0.8)+
  geom_polygon(data = corners2, aes(x= Lon1,y = Lat1,group = Area,fill = epu),col = 'black',alpha = 0.6,size = 0.5)+
  scale_fill_manual(name = '',labels = c("Georges Bank",'Gulf of Maine','Mid-Atlantic Bight', 'Boundary Box'),values = plot.cols)+
  geom_segment(data = island.df,aes(x=x1,y=y1,xend = x2, yend = y2),arrow = arrow(length = unit(0.25, "cm")),linewidth = 0.75)+
  geom_text(data = island.df,aes(x=x1+.25,y=y1,label =label),size = 4,hjust = 0)+
  coord_quickmap()+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme(
    legend.position = c(0.8,0.2), 
    legend.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust=0.5),
    plot.margin = grid::unit(c(0,0,0,0),'mm')
    
  )

ggsave(plot.file,width = 8, height = 8, units = 'in',dpi = 500)

m_png <- image_border(image_trim(image_read(plot.file)), "white", "30x30")
image_write(m_png,plot.file)

plot.file = here::here('Manuscript','Figures','Final','Fig_1_NEUS_Box_EPU_Simplified.png')
plot.cols = c(rgb(230,159,0,maxColorValue = 255),rgb(86,180,233, maxColorValue = 255),rgb(0,158,115, maxColorValue = 255))
ggplot()+ 
  annotation_map(map_data('worldHires'),fill = 'grey50',alpha = 0.8)+
  geom_polygon(data = corners2, aes(x= Lon1,y = Lat1,group = Area,fill = epu),col = 'black',alpha = 0.75,size = 0.5)+
  # scale_fill_manual(name = '',labels = c("Georges Bank",'Gulf of Maine','Mid-Atlantic Bight', 'Boundary Box'),values = RColorBrewer::brewer.pal(3,'Dark2'))+
  scale_fill_manual(name = '',labels = c("Georges Bank",'Gulf of Maine','Mid-Atlantic Bight', 'Boundary Box'),values = plot.cols)+
  # geom_segment(data = label.df,aes(x = x1, y = y1, xend = x2, yend = y2),
               # arrow = arrow(length = unit(0.25, "cm")),size = 0.75)+
  # geom_text(data = label.df,aes(x=x1-0.25,y=y1-0.25,label =label),size = 4,hjust = 0)+
  # geom_segment(data = island.df,aes(x=x1,y=y1,xend = x2, yend = y2),arrow = arrow(length = unit(0.25, "cm")),size = 0.75)+
  # geom_text(data = island.df,aes(x=x1-0.25,y=y1,label =label),size = 4,hjust = 1)+
  coord_quickmap()+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme(
    legend.position = c(0.8,0.2), 
    legend.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust=0.5),
    plot.margin = grid::unit(c(0,0,0,0),'mm')
    
  )+
  ggsave(plot.file,width = 4, height = 8, units = 'in',dpi = 300)

m_png <- image_border(image_trim(image_read(plot.file)), "white", "30x30")
image_write(m_png,plot.file)
# annotate("text", x=-65.5, y=35.5, label= "* denotes boundary",size=2) +
  
