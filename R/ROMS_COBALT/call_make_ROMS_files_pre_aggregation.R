
#source(here::here('R','make_ROMS_files_2.R'))
source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_ROMS_files_output_preaggregation.R')

dir.names = 1981:2014

for(yr in 2:length(dir.names)){

  # Set from and to directories
  roms.dir = paste0('D:/NWA_Revised/',dir.names[yr],'/')
  output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/PreAggregated Temperature/SST and Bottom/'
  
  roms.files = list.files(roms.dir,paste0('neusNWA_Cob10_avg_',dir.names[yr],'_*'),full.names = F)

  make_ROMS_files_output_preaggregation(
    roms.dir = roms.dir,
    roms.prefix = paste0('neusNWA_Cob10_avg_',dir.names[yr],'*'),
    roms.files = roms.files,
    out.dir = output.dir,
    dz.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',
    bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm',
    shp.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp',
    name.out = 'PreAggregated_Temperature_',
    year.id = dir.names[yr]
  )
  

  print(paste0('##################  ',yr,'   #################'))
  
}

