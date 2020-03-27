#Generates the irradiance.ts files from ROMS output

#Assumes directory structure of annual folders with daily files within

root.dir = 'D:/NWA/'
years = 1980:2014
source(here::here('R','ROMS_irradiance.R'))

yr.ls = list()
for(yr in 1:length(years)){
  
  file.names = list.files(paste0(root.dir,years[yr],'/'),'RM_NWA*')
  yr.ls[[yr]] = daily.irradiance(roms.dir = paste0(root.dir,years[yr],'/'),
                                 file.names = file.names,
                                 bgm.at.file = 'neus_tmerc_RM2.bgm',
                                 start.date = '1980-02-01',
                                 reference.date = '1964-01-01')
  print(years[yr])
}
irradiance.all.years = dplyr::bind_rows(yr.ls)
# save(irradiance.all.years,file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/daily irradiance raw.R')
load(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/daily irradiance raw.R')

irradiance.all.years$irr.spline = spline(irradiance.all.years$day.from.start,irradiance.all.years$irradiance, n = nrow(irradiance.all.years))$y
irradiance.all.years$irr.smooth = smooth(irradiance.all.years$irradiance)
irradiance.all.years$irr.loess = predict(loess(irradiance~day.from.start, irradiance.all.years, span = 0.01))
irradiance.all.years$irr.smoothspline = smooth.spline(irradiance.all.years$day.from.start,irradiance.all.years$irradiance)$y

# png('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/irradiance_all_years.png',width = 24, height = 8,units = 'in',res= 400)
plot(as.Date(irradiance.all.years$date),irradiance.all.years$irradiance,type='l',lwd=0.25,ylab = 'Watts m ^-2',xlab = '',col= 'grey70')
lines(as.Date(irradiance.all.years$date),irradiance.all.years$irr.spline,col='red3',lwd=3)
lines(as.Date(irradiance.all.years$date),irradiance.all.years$irr.smooth,col='blue3',lwd=3)
lines(as.Date(irradiance.all.years$date),irradiance.all.years$irr.loess,col='green3',lwd=3)
lines(as.Date(irradiance.all.years$date),irradiance.all.years$irr.smoothspline,col='magenta',lwd=3)
# dev.off()

solar.out = subset(irradiance.all.years,select = c(day.from.start,irr.smoothspline))
write.table(solar.out,file = here::here('testing','tsfiles','ROMS_irradiance_smoothed.txt'),row.names = F,sep = ' ')
