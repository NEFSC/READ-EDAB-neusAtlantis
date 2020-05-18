dir = here::here()
roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/'
years = 1980:2014
year.dirs = paste0(roms.dir,years,'/')

source(here::here('R','ROMS_Timeseries.R'))


for(i in 1:length(year.dirs)){
  
  ROMS_Timeseries(year.dir = year.dirs[i],
                  which.face = 0:150,
                  which.box = 0:29,
                  plot.hflux = T,
                  plot.statevar = T,
                  plot.ltlvar = T,
                  which.levels =4,
                  plot.dir = year.dirs[i])
  print(years[i])
}