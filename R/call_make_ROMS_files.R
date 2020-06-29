
source(here::here('R','make_ROMS_files_2.R'))
# source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_ROMS_files_new_levels.R')

dir.names = 1981:1983
# dir.names = 2010:2014

# ellapsed.t = list()

for(yr in 1:length(dir.names)){
# for(yr in 1:length(dir.names)){
  if(!dir.names[yr] %in% dir('D:/Output')){
    dir.create(paste0('D:/Output/',dir.names[yr]))
  }
  # Set from and to directories
  orig.dir = paste0('D:/NWA_Revised/',dir.names[yr],'/')
  local.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_IN/'
  local.output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/'
  final.output.dir = paste0('D:/Output/',dir.names[yr],'/')
  
  #Copy files from external to local directory
  files2copy.in = list.files(orig.dir,paste0('neusNWA_Cob10_avg_',dir.names[yr],'_*'),full.names = T)
  files2copy.in.short = list.files(orig.dir,paste0('neusNWA_Cob10_avg_',dir.names[yr],'_*'),full.names = F)
  files.in.local = list.files(local.dir,paste0('neusNWA_Cob10_avg_',dir.names[yr],'_*'),full.names = F)
  
  if(!all(files2copy.in.short %in% files.in.local)){
    tictoc::tic()
    file.copy(files2copy.in, local.dir)
    tictoc::toc()
  }
  
  
  if(!dir.names[yr] %in% dir(local.output.dir)){
    dir.create(paste0(local.output.dir,dir.names[yr]))
  }
  
  make_ROMS_files(
    roms.dir = local.dir,
    roms.prefix = paste0('neusNWA_Cob10_avg_',dir.names[yr],'*'),
    roms.files = files2copy.in.short,
    out.dir = paste0(local.output.dir,dir.names[yr],'/'),
    # dz.file = here::here('Geometry','dz.csv'),
    dz.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',
    # bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm'),
    bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm',
    # shp.file = here::here('Geometry','Neus_ll_0p01.shp'),
    shp.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp',
    name.out = 'roms_cobalt_v10_',
    make.hflux = F,
    make.physvars = T,
    make.ltlvars = T,
    make.nutvars = T  
  )
  
  # Roms2Hydro(roms.dir =local.dir,
  #            roms.prefix = paste0('RM_NWA-SZ.HCob05T_avg_',dir.names[yr],'*'),
  #            out.dir = paste0(local.output.dir,dir.names[yr],'/'),
  #            name.out = 'roms_cobalt_')
  
  files2copy.out = list.files(paste0(local.output.dir,dir.names[yr],'/'),full.names = T)
  file.copy(files2copy.out,final.output.dir,overwrite = T)
  
  # ncdf.tools::closeAllNcfiles()
  # closeAllConnections()
  # removeTmpFiles()
  
  gc()
  
  # close.files = dir(local.dir,full.names = T)
  # for(i in 1:length(close.files)){
  #   x =nc_open(close.files[i])
  #   nc_close(x)
  #   rm(x)
  #   unlink(close.files[i])
  #   system(paste0('attrib -r', close.files[i]))
  # }
  file.remove(dir(local.dir,full.names = T))

  # unlink(dir(local.dir,full.names = T),recursive = T)
  # sapply(dir(local.dir,full.names = T),function(x){
  #   system(paste0('attrib -r', x))
  #   file.remove(x)
  # })
  print(paste0('##################  ',yr,'   #################'))
  
}

