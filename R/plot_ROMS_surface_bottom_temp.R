
# Script to read in ROMS_COBALT daily output, get surface and bottom temperature, assign values to ecoregions, and export as timeseries

roms.file = 'D:/NWA_Revised/2012/neusNWA_Cob10_avg_2012_227.nc'
depths = c(1,40)
epu.file =here::here('Geometry','EPU_NOESTUARIES.shp')



gather_temp = function(roms.file, depths, epu.file){
  # shp.file = raster::shapefile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/EPU_NOESTUARIES.shp')
  # epu_shp = sf::read_sf(dsn = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/',layer = 'EPU_NOESTUARIES')
  epu_shp = rgdal::readOGR(epu.file)
  
  
  roms.nc = ncdf4::nc_open(roms.file)
  roms.temp = ncdf4::ncvar_get(roms.nc,'temp')
  lat.rho = ncdf4::ncvar_get(roms.nc,'lat_rho')
  lon.rho = ncdf4::ncvar_get(roms.nc,'lon_rho')
  lon.rho = ((lon.rho+180) %% 360) - 180
  
  ((roms_ll_rho$longitude.of.RHO.points+180) %% 360) - 180
  
  lat.box = lat.rho > 42 & lat.rho < 44
  lon.box = lon.rho > -70 & lon.rho < -68
  temp.flag = lat.box*lon.box
  
  
  sub.temp = roms.temp[,,40]*temp.flag
  sub.temp[sub.temp == 0] =NA
  
  mean(sub.temp,na.rm=T)
  filled.contour(sub.temp)
  ncdf4::nc_close(roms.nc)
  
  
}
