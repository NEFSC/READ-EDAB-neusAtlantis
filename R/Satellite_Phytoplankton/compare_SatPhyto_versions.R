library(dplyr)
library(ggplot2)

micro.v4 = read.csv('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v4/DOY-OCCCI-ATLANTIS_NEUS-VER_1.CSV') %>%
  filter(PROD == 'MICRO')%>%
  tidyr::separate(PERIOD, c('id','DOY','start','stop'),sep = '_')%>%
  select(DOY, SUBAREA, MED)%>%
  mutate(source = 'v4',
         DOY = as.numeric(DOY))

hirata.v4 = read.csv('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v4/DOY-OCCCI-ATLANTIS_NEUS-HIRATA-DIATOM-PROP.CSV') %>%
  filter(PROD == 'MICRO')%>%
  tidyr::separate(PERIOD, c('id','DOY','start','stop'),sep = '_')%>%
  select(DOY, SUBAREA, MED)%>%
  mutate(source = 'v4',
         DOY = as.numeric(DOY))

clim.v4 = nc_open('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/v4/Phyto_Climatology.nc')
diatom.v4 = ncvar_get(clim.v4,'Diatom_N')[1,,]%>%
  reshape2::melt()%>%
  rename(box = 'Var1',time = 'Var2')%>%
  mutate(source = 'v4')


# load('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/v4/diatom_proportion_DOY.R')
# hirata.v4 = diatom.prop
# colnames(hirata.v4) = as.character(1:365)
# hirata.v4 = hirata.v4 %>%
#   as.data.frame()%>%
#   mutate(SUBAREA = 0:29)%>%
#   tidyr::gather(DOY,MED,-SUBAREA)%>%
#   mutate(source = 'v4',
#          DOY = as.numeric(DOY))


micro.v6 = read.csv('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_MICRO-TURNER.CSV')%>%
  tidyr::separate(PERIOD, c('id','DOY','start','stop'),sep = '_')%>%
  select(DOY, SUBAREA, MED)%>%
  mutate(source = 'v6',
         DOY = as.numeric(DOY))

hirata.v6 = read.csv('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_FDIATOM-HIRATA.CSV')%>%
  tidyr::separate(PERIOD, c('id','DOY','start','stop'),sep = '_')%>%
  select(DOY, SUBAREA, MED)%>%
  mutate(source = 'v6',
         DOY = as.numeric(DOY))


clim.v6 = nc_open('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/v6/Phyto_Climatology.nc')
diatom.v6 = ncvar_get(clim.v6,'Diatom_N')[1,,]%>%
  reshape2::melt()%>%
  rename(box = 'Var1',time = 'Var2')%>%
  mutate(source = 'v6')

# hirata.v6 = read.csv('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_DIATOM-HIRATA.CSV')%>%
#   tidyr::separate(PERIOD, c('id','DOY','start','stop'),sep = '_')%>%
#   select(DOY, SUBAREA, MED)%>%
#   mutate(source = 'v6',
#          DOY = as.numeric(DOY))

micro.all = bind_rows(micro.v4,micro.v6)
hirata.all = bind_rows(hirata.v4,hirata.v6)
diatom.all = bind_rows(diatom.v4,diatom.v6)

boxes = 0:29

pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/Micro_Comparison_v4_v6.pdf',onefile = T)

for(i in 1:length(boxes)){
  
  micro.box = micro.all %>% filter(SUBAREA == boxes[i])  
  
  p = ggplot(micro.box, aes(x= DOY, y = MED,color = source))+
    geom_line()+
    ggtitle(paste0('Box=',boxes[i]))+
    theme_bw()
  
  gridExtra::grid.arrange(p)
}
dev.off()


pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/Hirata_Diatom_Comparison_v4_v6.pdf',onefile = T)

for(i in 1:length(boxes)){
  
  hirata.box = hirata.all %>% filter(SUBAREA == boxes[i])  
  
  p = ggplot(hirata.box, aes(x= DOY, y = MED,color = source))+
    geom_line()+
    ggtitle(paste0('Box=',boxes[i]))+
    theme_bw()
  
  gridExtra::grid.arrange(p)
}
dev.off()

pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/Diatom_N_Comparison_v4_v6.pdf',onefile = T)

for(i in 1:length(boxes)){
  
  diatom.box = diatom.all %>% filter(box == boxes[i])  
  
  p = ggplot(diatom.box, aes(x= time, y = value,color = source))+
    geom_line()+
    ggtitle(paste0('Box=',boxes[i]))+
    theme_bw()
  
  gridExtra::grid.arrange(p)
}
dev.off()