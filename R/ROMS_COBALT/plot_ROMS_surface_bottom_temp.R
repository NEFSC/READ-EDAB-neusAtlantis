
# Script to read in ROMS_COBALT daily output, get surface and bottom temperature, assign values to ecoregions, and export as timeseries

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/PreAggregated Temperature/SST and Bottom/'
plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/PreAggregated Temperature/'
temp.files = list.files(roms.dir, '.R')
years = 1981:2014

i=1
full.data.ls = list()
for(i in 1:length(temp.files)){
  
  load(paste0(roms.dir,temp.files[i]))
  
  date.match = data.frame(date = seq.Date(as.Date(paste0(years[i],'-01-01')),as.Date(paste0(years[i],'-12-31')),by = '+1 day'))
  date.match$julian = 1:nrow(date.match)
  
  
  box.agg = box_props %>% 
    group_by(band_level,.bx0,roms_level) %>%
    summarize(temp = median(temp,na.rm=T),
            maxz = median(maxz,na.rm=T))
  box.agg = box.agg %>% left_join(date.match,by= c('band_level' = 'julian'))
  
  full.data.ls[[i]] =box.agg
  
  print(i)
}

full.data = bind_rows(full.data.ls)
full.data$roms_level = as.factor(full.data$roms_level)

#By Raw
boxes = 0:29
b=1

pdf(paste0(plot.dir,'Top Bottom Temp - Box Raw.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(boxes)){
# for(b in 1){
  top = filter(ungroup(full.data),.bx0 == boxes[b] & roms_level == 40)
  bot = filter(ungroup(full.data),.bx0 == boxes[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  plot(temp~date,top,type = 'l',ylab = 'Temp (C)',xlab = '',ylim = c(0,32))
  mtext(paste0('Box ',boxes[b]),3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  plot(temp~date,bot,type = 'l',ylab = 'Temp (C)',xlab = '')
  mtext('Bottom',3,line = 0,adj=0)
  print(b)
}
dev.off()

# By Box - 12 mo. moving avg
pdf(paste0(plot.dir,'Top Bottom Temp - Box 12mo.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(boxes)){
  top = filter(ungroup(full.data),.bx0 == boxes[b] & roms_level == 40)
  bot = filter(ungroup(full.data),.bx0 == boxes[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  top$temp.12mo = zoo::rollmean(top$temp, 365, na.pad = T)
  bot$temp.12mo = zoo::rollmean(bot$temp, 365, na.pad = T)
  plot(temp.12mo~date,top,type = 'l',ylab = 'Temp (C)',xlab = '')
  mtext(paste0('Box ',boxes[b]),3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  plot(temp.12mo~date,bot,type = 'l',ylab = 'Temp (C)',xlab = '')
  mtext('Bottom',3,line = 0,adj=0)
  print(b)
}
dev.off()

#Plot Both
pdf(paste0(plot.dir,'Top Bottom Temp - Both.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(boxes)){
  top = filter(ungroup(full.data),.bx0 == boxes[b] & roms_level == 40)
  bot = filter(ungroup(full.data),.bx0 == boxes[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  top$temp.12mo = zoo::rollmean(top$temp, 365, na.pad = T)
  bot$temp.12mo = zoo::rollmean(bot$temp, 365, na.pad = T)
  plot(temp~date,top,type = 'l',ylab = 'Temp (C)',xlab = '')
  lines(temp.12mo~date,top,col = 'red',lwd = 2.5)
  mtext(paste0('Box ',boxes[b]),3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  plot(temp~date,bot,type = 'l',ylab = 'Temp (C)',xlab = '')
  lines(temp.12mo~date,bot,col = 'red',lwd = 2.5)
  mtext('Bottom',3,line = 0,adj=0)
  print(b)
}
dev.off()

#Box Anomaly 1961-2010 mean
full.data$year = as.numeric(format(full.data$date,format = '%Y'))
full.data.yr = full.data %>%
  group_by(.bx0, roms_level,year) %>%
  summarize(temp = median(temp,na.rm=T))

top.mean = ungroup(full.data) %>% filter(!is.na(.bx0) & year %in% c(1981,2010) & roms_level == 40) %>%
  group_by(.bx0) %>% summarize(temp = mean(temp,na.rm=T))
bot.mean = ungroup(full.data) %>% filter(!is.na(.bx0) & year %in% c(1981,2010) & roms_level == 1) %>%
  group_by(.bx0) %>% summarize(temp = mean(temp,na.rm=T))


pdf(paste0(plot.dir,'Top Bottom Temp - Box Anomaly.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(boxes)){
  top = filter(ungroup(full.data.yr),.bx0 == boxes[b] & roms_level == 40)
  bot = filter(ungroup(full.data.yr),.bx0 == boxes[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  
  # date.range = which(top$date>=as.Date('1981-01-01')&top$date<=as.Date('2010-12-31'))
  # top.mean = mean(top$temp[date.range])
  # bot.mean = mean(bot$temp[date.range])
  
  top$anomaly = top$temp-top.mean$temp[b]
  bot$anomaly = bot$temp-bot.mean$temp[b]
  
  lm.top = lm(anomaly~year,top)
  lm.bot = lm(anomaly~year,bot)
  
  plot(anomaly~year,top,type = 'l',ylab = 'Temp Anomaly s(C)',xlab = '')
  mtext(boxes[b],3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  abline(lm.top,col = 'red',lwd =2)
  plot(anomaly~year,bot,type = 'l',ylab = 'Temp Anomaly (C)',xlab = '')
  mtext('Bottom',3,line = 0,adj=0)
  abline(lm.bot,col = 'red',lwd =2)
  print(b)
}
dev.off()



# Aggregated over EPUs ----------------------------------------------------
library(sf)

epu.file =here::here('Geometry','EPU_NOESTUARIES.shp')
bgm.file = here::here('Geometry','Neus_ll_WGS84.bgm')

# epu_shp = rgdal::readOGR(epu.file)
epu_shp = read_sf(epu.file)
bgm = rbgm::bgmfile(bgm.file)

box.mid = bgm$boxes %>% select(.bx0,lon = insideX, lat = insideY)

pnts.shp = st_as_sf(box.mid, coords = c('lon','lat'),crs = st_crs(epu_shp))
# st_intersects(pnts.shp$geometry,epu_shp)

epu.match = pnts.shp %>% mutate(
  intersection = as.integer(st_intersects(geometry,epu_shp))
  ,area = if_else(is.na(intersection),'',epu_shp$EPU[intersection])
)

epu.match$area =sapply(epu.match$area, function(x) ifelse(x=='',NA,x))
epu.match = epu.match %>% select(.bx0,area)

full.data.epu = ungroup(full.data) %>% left_join(epu.match)

#Aggregate over epu
full.data.epu = ungroup(full.data.epu) %>% 
  group_by(area,date,roms_level) %>%
  summarize(temp = median(temp,na.rm=T))

epus = unique(full.data.epu$area)

# By EPU - Raw data
pdf(paste0(plot.dir,'Top Bottom Temp - EPU Raw.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(epus)){
  top = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 40)
  bot = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  plot(temp~date,top,type = 'l',ylab = 'Temp (C)',xlab = '',ylim = c(0,32))
  mtext(epus[b],3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  plot(temp~date,bot,type = 'l',ylab = 'Temp (C)',xlab = '')
  mtext('Bottom',3,line = 0,adj=0)
  print(b)
}
dev.off()

# By Box - 12 mo. moving avg
pdf(paste0(plot.dir,'Top Bottom Temp - EPU 12mo.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(epus)){
  top = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 40)
  bot = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  top$temp.12mo = zoo::rollmean(top$temp, 365, na.pad = T)
  bot$temp.12mo = zoo::rollmean(bot$temp, 365, na.pad = T)
  plot(temp.12mo~date,top,type = 'l',ylab = 'Temp (C)',xlab = '')
  mtext(epus[b],3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  plot(temp.12mo~date,bot,type = 'l',ylab = 'Temp (C)',xlab = '')
  mtext('Bottom',3,line = 0,adj=0)
  print(b)
}
dev.off()

#Plot Both
pdf(paste0(plot.dir,'Top Bottom Temp - EPU Both.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,0))
for(b in 1:length(epus)){
  top = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 40)
  bot = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
  top$temp.12mo = zoo::rollmean(top$temp, 365, na.pad = T)
  bot$temp.12mo = zoo::rollmean(bot$temp, 365, na.pad = T)
  plot(temp~date,top,type = 'l',ylab = 'Temp (C)',xlab = '')
  lines(temp.12mo~date,top,col = 'red',lwd = 2.5)
  mtext(epus[b],3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  plot(temp~date,bot,type = 'l',ylab = 'Temp (C)',xlab = '')
  lines(temp.12mo~date,bot,col = 'red',lwd = 2.5)
  mtext('Bottom',3,line = 0,adj=0)
  print(b)
}
dev.off()


#EPU Anomaly 1961-2010 mean

# pdf(paste0(plot.dir,'Top Bottom Temp - EPU Anomaly 12 mo.pdf'),onefile = T,width = 16, height = 6)
# par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,1))
# for(b in 1:length(epus)){
#   top = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 40)
#   bot = filter(ungroup(full.data.epu),area == epus[b] & roms_level == 1)
#   if(all(is.na(top$temp))){next()}
#   
#   date.range = which(top$date>=as.Date('1981-01-01')&top$date<=as.Date('2010-12-31'))
#   top.mean = mean(top$temp[date.range])
#   bot.mean = mean(bot$temp[date.range])
#   
#   top$anomaly = top$temp-top.mean
#   bot$anomaly = bot$temp-bot.mean
#   
#   top$anomaly.12mo = zoo::rollmean(top$anomaly,365,na.pad = T)
#   bot$anomaly.12mo = zoo::rollmean(bot$anomaly,365,na.pad = T)
#   
#   plot(anomaly.12mo~date,top,type = 'l',ylab = 'Temp Anomaly s(C)',xlab = '')
#   mtext(epus[b],3,line = -0.5,cex = 1.5,outer = T)
#   mtext('Surface',3,line = 0,adj = 0)
#   plot(anomaly.12mo~date,bot,type = 'l',ylab = 'Temp Anomaly (C)',xlab = '')
#   mtext('Bottom',3,line = 0,adj=0)
#   print(b)
# }
# dev.off()

#EPU Anomaly 1961-2010 mean - Yearly averages
full.data.epu$year = as.numeric(format(full.data.epu$date,format = '%Y'))
full.data.epu.yr = full.data.epu %>%
  group_by(area, roms_level,year) %>%
  summarize(temp = median(temp,na.rm=T))

top.mean = ungroup(full.data.epu) %>% filter(!is.na(area) & year %in% c(1981,2010) & roms_level == 40) %>%
  group_by(area) %>% summarize(temp = mean(temp,na.rm=T))
bot.mean = ungroup(full.data.epu) %>% filter(!is.na(area) & year %in% c(1981,2010) & roms_level == 1) %>%
  group_by(area) %>% summarize(temp = mean(temp,na.rm=T))

pdf(paste0(plot.dir,'Top Bottom Temp - EPU Annual Anomaly.pdf'),onefile = T,width = 16, height = 6)
par(mfrow=c(2,1),oma = c(0,0,1,0),mar = c(2,4,3,1))
for(b in 1:length(epus)){
  top = filter(ungroup(full.data.epu.yr),area == epus[b] & roms_level == 40)
  bot = filter(ungroup(full.data.epu.yr),area == epus[b] & roms_level == 1)
  if(all(is.na(top$temp))){next()}
 
  top$anomaly = top$temp-top.mean$temp[b]
  bot$anomaly = bot$temp-bot.mean$temp[b]
  
  lm.top = lm(anomaly~year,top)
  lm.bot = lm(anomaly~year,bot)
  
  plot(anomaly~year,top,type = 'l',ylab = 'Temp Anomaly s(C)',xlab = '')
  mtext(epus[b],3,line = -0.5,cex = 1.5,outer = T)
  mtext('Surface',3,line = 0,adj = 0)
  abline(lm.top,col = 'red',lwd =2)
  plot(anomaly~year,bot,type = 'l',ylab = 'Temp Anomaly (C)',xlab = '')
  mtext('Bottom',3,line = 0,adj=0)
  abline(lm.bot,col = 'red',lwd =2)
  print(b)
}
dev.off()


# Temp Anomaly no Altantis Aggregation ------------------------------------

roms.coords = angstroms::romscoords('D:/NWA_grd_NEUS_2.nc',spatial = c('lon_rho','lat_rho'))
roms.coords$longitude.of.RHO.points = ((roms.coords$longitude.of.RHO.points+180) %% 360) - 180



full.data.ls2 = list()
for(i in 1:length(temp.files)){
  
  load(paste0(roms.dir,temp.files[i]))
  
  date.match = data.frame(date = seq.Date(as.Date(paste0(years[i],'-01-01')),as.Date(paste0(years[i],'-12-31')),by = '+1 day'))
  date.match$julian = 1:nrow(date.match)
  
  box_props = box_props %>% left_join(date.match,by= c('band_level' = 'julian')) %>% 
    filter(roms_level == 1) %>%
    select(date,.bx0,cell,temp)
  
  if(i == 1){
    cell.index = data.frame(cell = unique(box_props$cell))
    coords = extract(readAll(roms.coords),cell.index$cell)
    colnames(coords) = c('lon.rho','lat.rho')
    cell.index = cbind(cell.index,coords)
    
    box.agg = box_props %>% filter(date == '1981-01-01') %>% left_join(cell.index) %>% select(cell,lon.rho,lat.rho)
    pnts.shp = st_as_sf(box.agg, coords = c('lon.rho','lat.rho'),crs = st_crs(epu_shp))
    
    epu.match = pnts.shp %>% 
      mutate(intersection = as.integer(st_intersects(geometry,epu_shp)),
             area = if_else(is.na(intersection),'',epu_shp$EPU[intersection])
    ) %>% select(cell,area)
    epu.match = as.data.frame(epu.match) %>% select(cell,area)
  }
  
  box_props2 = box_props %>% left_join(epu.match)
  
  full.data.ls2[[i]] = box_props2
  
  print(i)
}

roms.data = bind_rows(full.data.ls2)



# coordinates(box.mid) <- ~lon + lat
# proj4string(box.mid) = proj4string(epu_shp)
# 
# shp.match = over(box.mid,epu_shp)

# 
# roms.file = 'D:/NWA_Revised/2012/neusNWA_Cob10_avg_2012_227.nc'
# depths = c(1,40)
# 
# 
# 
# 
# gather_temp = function(roms.file, depths, epu.file){
#   # shp.file = raster::shapefile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/EPU_NOESTUARIES.shp')
#   # epu_shp = sf::read_sf(dsn = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/',layer = 'EPU_NOESTUARIES')
#   
#   
#   
#   roms.nc = ncdf4::nc_open(roms.file)
#   roms.temp = ncdf4::ncvar_get(roms.nc,'temp')
#   lat.rho = ncdf4::ncvar_get(roms.nc,'lat_rho')
#   lon.rho = ncdf4::ncvar_get(roms.nc,'lon_rho')
#   lon.rho = ((lon.rho+180) %% 360) - 180
#   
#   ((roms_ll_rho$longitude.of.RHO.points+180) %% 360) - 180
#   
#   lat.box = lat.rho > 42 & lat.rho < 44
#   lon.box = lon.rho > -70 & lon.rho < -68
#   temp.flag = lat.box*lon.box
#   
#   
#   sub.temp = roms.temp[,,40]*temp.flag
#   sub.temp[sub.temp == 0] =NA
#   
#   mean(sub.temp,na.rm=T)
#   filled.contour(sub.temp)
#   ncdf4::nc_close(roms.nc)
#   
#   
# }

# plot(temp~date,top,type = 'l',ylab = 'Temp (C)',xlab = '')
# lines(temp.12mo~date,top,col = 'red',lwd =3)
# mtext(paste0('Box ',boxes[b]),3,line = 2,cex = 1.5)
# mtext('Surface',3,line = 0)