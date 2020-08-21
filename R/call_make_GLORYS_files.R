
source(here::here('R','make_GLORYS_files.R'))
# source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_ROMS_files_new_levels.R')

# dir.names = 1993
# dir.names = 1993:2018
dir.names = c(1995,2004,2006,2008,2009,2013:2017)


# ellapsed.t = list()
glorys.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/'

for(yr in 1:length(dir.names)){
# for(yr in 1:length(dir.names)){
  if(!dir.exists(paste0(glorys.dir,'Atlantis_Format/',dir.names[yr]))){
    dir.create(paste0(glorys.dir,'Atlantis_Format/',dir.names[yr]))
  }
  # Set from and to directories
  local.dir = paste0(glorys.dir,'Data/',dir.names[yr],'/')
  local.output.dir = paste0(glorys.dir,'Atlantis_Format/')
  final.output.dir = paste0(local.output.dir,dir.names[yr],'/')
  
  #Copy files from external to local directory
  files.in.local = list.files(local.dir,paste0('GLORYS_REANALYSIS_*'),full.names = F)
  # test = nc_open(files.in.local[1])

  if(!dir.names[yr] %in% dir(local.output.dir)){
    dir.create(paste0(local.output.dir,dir.names[yr]))
  }
  
  make_GLORYS_files(
    glorys.dir = local.dir,
    glorys.prefix = paste0('GLORYS_REANALYSIS_*'),
    glorys.files = files.in.local,
    out.dir = paste0(local.output.dir,dir.names[yr],'/'),
    # dz.file = here::here('Geometry','dz.csv'),
    dz.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',
    # bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm'),
    bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm',
    bgm.ll.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_ll_WGS84.bgm',
    # shp.file = here::here('Geometry','Neus_ll_0p01.shp'),
    shp.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp',
    name.out = 'GLORYS_Atlantis_',
    make.hflux = T,
    make.physvars = T
  )
  
  gc()
  
  print(paste0('##################  ',yr,'   #################'))
  
}

