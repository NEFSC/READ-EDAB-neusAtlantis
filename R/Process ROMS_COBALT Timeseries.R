
#source(here::here('R','Roms_to_Hydroconstruct.R'))
source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/Roms_to_Hydroconstruct.R')

dir.names = 1980:2014
# ellapsed.t = list()


for(yr in 6){
  if(!dir.names[yr] %in% dir('D:/Output')){
    dir.create(paste0('D:/Output/',dir.names[yr]))
  }
  # Set from and to directories
  orig.dir = paste0('D:/NWA/',dir.names[yr],'/')
  local.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_IN/'
  local.output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/'
  final.output.dir = paste0('D:/Output/',dir.names[yr],'/')
  
  #Copy files from external to local directory
  files2copy.in = list.files(orig.dir,'RM_NWA-SZ*',full.names = T)
  tictoc::tic()
  file.copy(files2copy.in, local.dir)
  tictoc::toc()
  
  if(!dir.names[yr] %in% dir(local.output.dir)){
    dir.create(paste0(local.output.dir,dir.names[yr]))
  }
  
  Roms2Hydro(roms.dir =local.dir,
             roms.prefix = 'RM_NWA-SZ.HCob05T_avg_',
             out.dir = paste0(local.output.dir,dir.names[yr],'/'),
             name.out = 'roms_cobalt_')
  
  files2copy.out = list.files(paste0(local.output.dir,dir.names[yr],'/'),full.names = T)
  file.copy(files2copy.out,final.output.dir)
  
  ncdf.tools::closeAllNcfiles()
  closeAllConnections()
  removeTmpFiles()
  
  gc()
  
  file.remove(dir(local.dir,full.names = T))

  # unlink(dir(local.dir,full.names = T),recursive = T)
  # sapply(dir(local.dir,full.names = T),function(x){
  #   system(paste0('attrib -r', x))
  #   file.remove(x)
  # })
  print(paste0('##################  ',yr,'   #################'))
  
}

