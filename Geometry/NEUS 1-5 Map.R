# Create box map color coded by depth

# setwd('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/')
library(ggplot2)
library(rbgm)
library(dplyr)
library(maps)
library(mapdata)
library(ggmap)

create_neus_map <- function(saveToFile=F){

  corners = read.csv(here::here('Geometry','RM_allboxes_NEUS_corners.csv'),as.is =T)
  bgm = rbgm::bgmfile(here::here('Geometry','neus_ll_WGS84.bgm'))
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
  
  #viridisColors <- viridis::viridis_pal(begin = 0, end = 1, option="D")(5)
  viridisColors <- c("#FFFFFF","#FDE725FF","#73D055FF","#1F968BFF","#481567FF")
  
  # creates neus pdf
  faces = bgm$faces %>% left_join(bgm$facesXverts) %>% left_join(bgm$vertices) %>% group_by(.fx0) %>% 
    summarize(xmid = mean(x,na.rm=T),ymid = mean(y,na.rm=T))
  
  p1 <- ggplot()+ 
    geom_polygon(data = corners2, aes(x= Lon1,y = Lat1,group = Area,fill = nrLayers),col = 'black')+
    scale_fill_manual(name = "NEUS 1.5", values = viridisColors,
                      labels = c('Island','0-50m','50-120m','120-300m','300m +'))+
    coord_fixed()+
    geom_text(data = corners2, aes(x = midx,y = midy, label = label),size = 3) +
    xlab('')+
    ylab('')+
    theme_void()+
    theme(
      legend.position = c(0.15,0.8),
      legend.key.size = 
    )

  #values =c('grey60','lightsalmon','skyblue','royalblue2','slateblue2')
  # creates coastline jpg
  neus = maps::map('worldHires',xlim = c(-78,-62),ylim = c(34,46.5),plot = F)
  p2 <- ggplot()+ 
    annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.3)+
    geom_polygon(data = corners2, aes(x= Lon1,y = Lat1,group = Area,fill = nrLayers),col = 'black',alpha = 0.75,size = 0.5)+
    scale_fill_manual(name = "Maximum Depth Bin",values =viridisColors,
                      labels = c('Island/Land','0-50m','50-120m','120-300m','300m +'))+
    coord_quickmap()+
    geom_text(data = corners2, aes(x = midx,y = midy, label = label),size = 3) +
    ggtitle('Northeast US Atlantis Model')+
    xlab('')+
    ylab('')+
    theme_void()+
    theme(
      legend.position = c(0.88,0.30),
      plot.title = element_text(hjust=0.5)
    ) + 
    annotate("text", x=-65.5, y=35.5, label= "* denotes boundary",size=2) 

  # creates neus_faces
  p3 <- ggplot()+ 
    geom_polygon(data = corners2, aes(x= Lon1,y = Lat1,group = Area,fill = nrLayers),col = 'black',alpha = 0.75)+
    scale_fill_manual(name = "NEUS 1.5",values =viridisColors,
                      labels = c('Island','0-50m','50-120m','120-300m','300m +'))+
    geom_text(data = faces,aes(x = xmid,y=ymid, label = .fx0),size = 3, fontface = 'bold')+
    geom_text(data = corners2, aes(x = midx,y = midy, label = label),size = 4, fontface = 'bold')+
    coord_fixed()+
    xlab('')+
    ylab('')+
    theme_void()+
    theme(
      legend.position = c(0.15,0.8),
      text = element_text(face = 'bold')
    )
  
  if (saveToFile){
    ggsave(file="Geometry/Neus 1-5.pdf",plot=p1,width = 5,height = 5, dpi = 350)
    ggsave("Geometry/Neus 1-5 with Coastline.png",plot=p2,width = 5,height = 5, dpi = 350)
    ggsave("Geometry/Neus Faces.pdf",plot=p3,width = 20,height = 20, units = 'in', dpi = 300)
  } else{
    print(p1)
    print(p2)
    print(p3)
    
  }
    
}