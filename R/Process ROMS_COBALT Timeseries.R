
source(here::here('R','Roms_to_Hydroconstruct.R'))

dir.names = dir('D:/NWA/')



for(yr in seq_along(dir.names)){
  if(!dir.names[yr] %in% dir('D:/Output')){
    dir.create(paste0('D:/Output/',dir.names[yr]))  
  }
  # Roms2Hydro(roms.dir = past0('D:/NWA/',dir.names[yr],'/'),
  #            roms.prefix = 'RM_NWA-SZ.HCob05T_avg_',
  #            out.dir = 'D:/Output/1980/',
  #            name.out = 'roms_cobalt_')
}
