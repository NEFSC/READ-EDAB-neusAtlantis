# Create box map color coded by depth

setwd('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/')
library(ggplot2)
library(rbgm)

corners = read.csv('RM_allboxes_NEUS_corners.csv',as.is =T)
bgm = bgmfile('neus_ll_WGS84.bgm')
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

ggplot()+ 
  geom_polygon(data = corners2, aes(x= Lon1,y = Lat1,group = Area,fill = nrLayers),col = 'black')+
  scale_fill_manual(name = "NEUS 1.5",values =c('grey60','lightsalmon','skyblue','royalblue2','slateblue2'),
                    labels = c('Island','0-50m','50-120m','120-300m','300m +'))+
  coord_fixed()+
  geom_text(data = corners2, aes(x = midx,y = midy, label = label),size = 4)+
  xlab('')+
  ylab('')+
  theme_void()+
  theme(
    legend.position = c(0.15,0.8),
    legend.key.size = 
  )+
  ggsave('Neus 1-5.pdf')


