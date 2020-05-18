#' creates surface plots of specified variable from ROMS_COBALT raw output
#' 
#' For diagnostic purposes. Creates plots of surface conditions of specified 
#' variable from the ROMS_COBALT raw output. Only works for one day at a time.
#' 
#' @roms.file string. Full path to roms file
#' @var.name string. Name of plotting variable
#' @xlim vector. Min and max longitude
#' @ylim vector. Min and max latitude
#' @plot.dir string. path where plot to be saved
#' 
#' @return heat map and coast contours 
#' 
#' Author: J. Caracappa
#' 

# roms.file = 'D:/NWA/1981/RM_NWA-SZ.HCob05T_avg_1981-04-01.nc'
# var.name = 'salt'
# # xlim = c(-70,-78)
# # ylim = c(35,45)
# xlim = c(0,250)
# ylim = c(25,100)

plot_ROMS_2D = function(roms.file, var.name, xlim, ylim, plot.dir){
  
  # roms.nc = ncdf4::nc_open(roms.file)
  # var.dat = ncdf4::ncvar_get(roms.nc, var.name)[,,40]
  
  roms.ll = angstroms::romscoords(roms.file,transpose = T,spatial = c('lon_rho','lat_rho'))
  # var.dat = angstroms::rawdata(roms.file,varname = var.name)
  var.dat = angstroms::romsdata(roms.file,var.name,slice = c(1L,1L),transpose = T)
  var.dat2 = raster::crop(var.dat,raster::extent(xlim[1],xlim[2],ylim[1],ylim[2]))
  
  # roms.ll = as.data.frame(raster::values(angstroms::romscoords(roms.file,transpose = T, spatial = c('lon_rho','lat_rho'))))
  # colnames(roms.ll) = c('lon.rho','lat.rho')
  # roms.ll$var = rev(as.vector(var.dat))
  # roms.ll$lon.rho = ((roms.ll$lon.rho+180) %% 360) - 180
  # rm(var.dat)
  # lon.rho = sort(unique(roms.ll$lon.rho))
  # lat.rho = sort(unique(roms.ll$lat.rho))
  
  # filled.contour(lon.rho,lat.rho,var.dat)
  raster::plot(var.dat2)
  # ggplot2::ggplot(roms.ll,ggplot2::aes(x=lon.rho,y=lat.rho,color=var))+
  #   ggplot2::geom_point()
  
  
}

# plot.roms.timeslice(
#   roms.file = 'C:/Users/joseph.caracappa/Downloads/RM_neusNWA-SZ.HCob10T_avg_1981-08-30T01_00_00.nc',
#   var.name = 'temp',
#   xlim = c(0,250),
#   ylim = c(25,100),
# )
# plot.roms.timeslice(
#   roms.file = 'D:/NWA/1981/RM_NWA-SZ.HCob05T_avg_1981-08-01.nc',
#   var.name = 'temp',
#   xlim = c(0,250),
#   ylim = c(25,100),
# )
