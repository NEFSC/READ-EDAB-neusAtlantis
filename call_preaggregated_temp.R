source(here::here('R','make_ROMS_files_output_preaggregation.R'))

years = 2010:2013
# years = 1984:2009


for(yr in 1:length(years)){
  roms.dir = paste0('D:/NWA_Revised/',years[yr],'/')
  roms.prefix = paste0('neusNWA_Cob10_avg_',years[yr],'*')
  roms.files = list.files(roms.dir,roms.prefix)
  
  make_ROMS_files_output_preaggregation(
    roms.dir =roms.dir,
    roms.prefix = roms.prefix,
    roms.files = roms.files,
    out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/PreAggregated Temperature/',
    dz.file = here::here('Geometry','dz.csv'),
    bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm'),
    shp.file = here::here('Geometry','Neus_ll_0p01.shp'),
    year.id = years[yr]
  )
  
  print(paste0('#############',years[yr],'##############'))
}

