#Compare box-level GLORYS and PSY data for the overlapping dates in 2020
library(dplyr)
library(ncdf4)
library(ggplot2)
library(gridExtra)

source(here::here('R','flatten_force.R'))

common.dates = seq.Date(from = as.Date('2020-11-01'),
                        to = as.Date('2020-12-31'),
                        by = '1 d')

#Read in GLORYS output
glorys.dir = 'D:/GLORYS/Atlantis_Format/2020/'
trans.glorys.nc = nc_open(paste0(glorys.dir,'GLORYS_Atlantis_transport_2020.nc'))
statevar.glorys.nc =nc_open(paste0(glorys.dir,'GLORYS_Atlantis_statevars_2020.nc'))


#Read in PSY output
psy.dir = 'D:/GLORYS/Atlantis_Format/2020b/'
trans.psy.nc = nc_open(paste0(psy.dir,'GLORYS_Atlantis_transport_2020.nc'))
statevar.psy.nc = nc_open(paste0(psy.dir,'GLORYS_Atlantis_statevars_2020.nc'))

#Truncate GLORYS to match common.dates
glorys.dates = as.Date(as.POSIXct(trans.glorys.nc$dim$time$vals,origin = '1964-01-01 00:00:00',tz = 'UTC'))
date.match =which(glorys.dates %in% common.dates)

#Subset glorys so dates match
trans.glorys = ncvar_get(trans.glorys.nc, 'transport')[,,date.match]
temp.glorys = ncvar_get(statevar.glorys.nc,'temperature')[,,date.match]
salt.glorys = ncvar_get(statevar.glorys.nc,'salinity')[,,date.match]

#extract PSY variables
trans.psy = ncvar_get(trans.psy.nc,'transport')
temp.psy = ncvar_get(statevar.psy.nc,'temperature')
salt.psy = ncvar_get(statevar.psy.nc,'salinity')


#Compare transport
trans.diff = reshape2::melt(trans.psy)%>%
  rename(value.psy = 'value')%>%
  left_join(reshape2::melt(trans.glorys))%>%
  rename(value.glorys = 'value',
         layer = 'Var1',
         face = 'Var2',
         time = 'Var3')%>%
  # mutate(value.diff = value.glorys-value.psy)%>%
  filter(!is.na(value.psy))%>%
  tidyr::gather('source','value',-layer,-face,-time)

faces = unique(trans.diff$face)

pdf(file = paste0('D:/GLORYS/Summary/GLORYS_v_PSY_transport.pdf'),onefile = T)
for(i in 1:length(faces)){
  p = ggplot(data = filter(trans.diff,face == faces[i]), aes(x = time,y=value,color = source))+
    geom_line()+
    facet_wrap(~layer,ncol = 1)+
    theme_bw()
  grid.arrange(p)
}
dev.off()

#compare temperature
temp.diff = reshape2::melt(temp.psy)%>%
  rename(value.psy = 'value')%>%
  left_join(reshape2::melt(temp.glorys))%>%
  rename(value.glorys = 'value',
         layer = 'Var1',
         box = 'Var2',
         time = 'Var3')%>%
  # mutate(value.diff = value.glorys-value.psy)%>%
  filter(!is.na(value.psy))%>%
  tidyr::gather('source','value',-layer,-box,-time)

boxes = unique(temp.diff$box)

# pdf(file = paste0('D:/GLORYS/Summary/GLORYS_v_PSY_temperature.pdf'),onefile = T)
# for(i in 1:length(boxes)){
#   p = ggplot(data = filter(temp.diff,box == boxes[i]), aes(x = time,y=value.diff))+
#     geom_line()+
#     geom_hline(yintercept = 0)+
#     facet_wrap(~layer,ncol = 1,scale = 'free_y')+
#     theme_bw()+
#     ylab('Temp: GLORYS - PSY')+
#     ggtitle(paste0('box ',boxes[i]-1))
#   grid.arrange(p)
# }
# dev.off()
pdf(file = paste0('D:/GLORYS/Summary/GLORYS_v_PSY_temperature.pdf'),onefile = T)
for(i in 1:length(boxes)){
  p = ggplot(data = filter(temp.diff,box == boxes[i]), aes(x = time,y=value,color = source))+
    geom_line()+
    facet_wrap(~layer,ncol = 1,scale = 'free_y')+
    theme_bw()+
    ylab('Temperature')+
    ggtitle(paste0('box ',boxes[i]-1))
  grid.arrange(p)
}
dev.off()

#compare salinity
salt.diff = reshape2::melt(salt.psy)%>%
  rename(value.psy = 'value')%>%
  left_join(reshape2::melt(salt.glorys))%>%
  rename(value.glorys = 'value',
         layer = 'Var1',
         box = 'Var2',
         time = 'Var3')%>%
  # mutate(value.diff = value.glorys-value.psy)%>%
  filter(!is.na(value.psy))%>%
  tidyr::gather('source','value',-layer,-box,-time)


pdf(file = paste0('D:/GLORYS/Summary/GLORYS_v_PSY_salinity.pdf'),onefile = T)
for(i in 1:length(boxes)){
  p = ggplot(data = filter(salt.diff,box == boxes[i]), aes(x = time,y=value,color = source))+
    geom_line()+
    facet_wrap(~layer,ncol = 1)+
    theme_bw()+
    ggtitle(paste0('box ',boxes[i]-1))
  grid.arrange(p)
}
dev.off()
