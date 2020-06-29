#Script to pull all temps on epu/depth bin for all ROMS_COBALT output

library(raster)
library(sp)
library(dplyr)
library(ncdf4)

source(here::here('R','make_ROMS_cellindex_epu.R'))
source(here::here('R','get_ROMS_temp_EPU.R'))

depth.bins = seq(0,500,5)

box_z_index = make_ROMS_cellindex_epu(
  grd.file = 'D:/NWA_grd_NEUS_2.nc',
  epu.file = here::here('Geometry','EPU_NOESTUARIES.shp'),
  depth.bins
)




years = 1981:2014
roms.dir = 'D:/NWA_Revised/'

for(yr in 1:length(years)){
  
  year.dir = paste0(roms.dir,years[yr],'/')
  year.files = list.files(year.dir,pattern = 'neusNWA_Cob10*')
  
  year.temp.ls = list()
  for(d in 1:length(year.files)){
    
    year.temp.ls[[d]] = get_ROMS_temp_EPU(
      roms.dir = year.dir,
      roms.file = year.files[d],
      box_z_index = box_z_index
    )
    print(d)
  }
  
  year.temp.all = dplyr::bind_rows(year.temp.ls)
  year.temp.all$year = years[yr]
  
  save(year.temp.all,file = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_Temp_Depth/Annual_Output/ROMS_temp_depth_',years[yr],'.R'))
  
  print(years[yr])
}