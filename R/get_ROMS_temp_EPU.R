#Script to pull raw ROMS data by depth bin, and map to EPUs

# depth.bins = seq(0,500,10)
# 
# source(here::here('R','make_ROMS_cellindex_epu.R'))
# box_z_index = make_ROMS_cellindex_epu(grd.file = 'D:/NWA_grd_NEUS_2.nc',
#                                       epu.file = here::here('Geometry','EPU_NOESTUARIES.shp'),
#                                       depth.bins)
# roms.dir = 'D:/NWA_Revised/1981/'
# roms.file = 'neusNWA_Cob10_avg_1981_1.nc'
# 
# library(raster)
# library(sp)
# library(dplyr)
# library(ncdf4)

get_ROMS_temp_EPU = function(roms.dir, roms.file, box_z_index){
  
  extract_at_level <- function(x, cell_level) {
    ulevel <- unique(cell_level$level)
    values <- numeric(nrow(cell_level))
    for (ul in seq_along(ulevel)) {
      asub <- cell_level$level == ulevel[ul]
      values[asub] <- raster::extract(x[[ulevel[ul]]], 
                                      cell_level$cell[asub])
    }
    values
  }
  
  out.df = box_z_index
  box_cell = dplyr::rename(box_z_index, level = roms_level, cell = cell)
  
  roms.nc = ncdf4::nc_open(paste0(roms.dir,roms.file))
  ocean_time = roms.nc$dim$ocean_time$vals
  ocean_date = as.Date(as.POSIXct(ocean_time,origin = '1900-01-01 00:00:00',tz = 'UTC'))
  
  #Match temp var to cell/depth
  f=file();sink(file = f)
  temp.brk = brick(paste0(roms.dir,roms.file), varname = "temp", lvar = 4, level = 1, ncdf=T)
  sink();close(f)
  r_temp = raster::setExtent(temp.brk,extent(0,ncol(temp.brk),0,nrow(temp.brk)))
  out.df$temp <- extract_at_level(readAll(r_temp), box_cell)
  
  out.df$ocean_time = ocean_time
  out.df$date = ocean_date
  out.df$month = format(ocean_date,'%m')
  
  out.df2 = out.df %>% group_by(ocean_time,date,month,epu,depth.bin) %>%
    summarize(temp = mean(temp,na.rm=T))
  
  ncdf4::nc_close(roms.nc)
  return(out.df2)
  }