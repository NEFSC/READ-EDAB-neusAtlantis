
source(here::here('R','make_ECCO_vflux.R'))
# source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_ROMS_files_new_levels.R')

dir.names = 1992:2017
# dir.names = 2010:2014

# ellapsed.t = list()
ecco.dir = 'C:/Users/joseph.caracappa/Documents/ECCO/'

for(yr in 1:length(dir.names)){
  
  if(!dir.exists(paste0(ecco.dir,'vflux_daily/',dir.names[yr]))){
    dir.create(paste0(ecco.dir,'vflux_daily/',dir.names[yr]))
  }
  # Set from and to directories
  local.dir = paste0(ecco.dir,'vflux_monthly/',dir.names[yr],'/')
  local.output.dir = paste0(ecco.dir,'vflux_daily/')
  final.output.dir = paste0(local.output.dir,dir.names[yr],'/')
  
  #Copy files from external to local directory
  files.in.local = list.files(local.dir,paste0('WVELMASS_*'),full.names = F)
  
  if(!dir.names[yr] %in% dir(local.output.dir)){
    dir.create(paste0(local.output.dir,dir.names[yr]))
  }
  
  make_ECCO_vflux(
    ecco.dir = local.dir,
    ecco.prefix = paste0('WVELMASS_*'),
    ecco.files = files.in.local,
    out.dir = paste0(local.output.dir,dir.names[yr],'/'),
    # dz.file = here::here('Geometry','dz.csv'),
    dz.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',
    # bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm'),
    bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_ll_WGS84.bgm',
    # shp.file = here::here('Geometry','Neus_ll_0p01.shp'),
    shp.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp',
    name.out = 'ECCO_vflux_Atlantis_'
  )
  
  gc()
  
  print(paste0('##################  ',yr,'   #################'))
  
}


# 
# ecco.dir = local.dir
# ecco.prefix = paste0('WVELMASS_*')
# ecco.files = files.in.local
# out.dir = paste0(local.output.dir,dir.names[yr],'/')
# # dz.file = here::here('Geometry','dz.csv')
# dz.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv'
# # bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
# bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_ll_WGS84.bgm'
# # shp.file = here::here('Geometry','Neus_ll_0p01.shp')
# shp.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp'
# name.out = 'ECCO_vflux_Atlantis_'
