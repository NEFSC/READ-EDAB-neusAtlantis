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

plot_ROMS_2D = function(roms.file, var.name, xlim, ylim, depth,plot.dir,plot.raster = T){
  
  if(plot.raster == F){
    roms.nc = ncdf4::nc_open(roms.file)
    var.dat = ncdf4::ncvar_get(roms.nc, var.name)[,,depth]
    roms.ll = angstroms::romscoords(roms.file,transpose = T,spatial = c('lon_rho','lat_rho'))
    lons = raster::values(roms.ll$longitude.of.RHO.points)
    lats = raster::values(roms.ll$latitude.of.RHO.points)
    filled.contour(y = seq(min(lons),max(lons),length.out = ncol(var.dat)),
                   x = seq(min(lats),max(lats),length.out = nrow(var.dat)),
                   z = var.dat)
  }else{
    var.dat = angstroms::romsdata(roms.file,var.name,slice = c(as.integer(depth),1L),transpose = T)
    var.dat2 = raster::crop(var.dat,raster::extent(xlim[1],xlim[2],ylim[1],ylim[2]))
    
    raster::plot(var.dat2)
  }
}

plot_ROMS_2D(
  roms.file = 'D:/NWA_Revised/1981/neusNWA_Cob10_avg_1981_300.nc',
  var.name = 'nlg',
  xlim = c(0,250),
  ylim = c(25,100),
  depth = 40,
  plot.raster = F
)
# plot.roms.timeslice(
#   roms.file = 'D:/NWA/1981/RM_NWA-SZ.HCob05T_avg_1981-08-01.nc',
#   var.name = 'temp',
#   xlim = c(0,250),
#   ylim = c(25,100),
# )
