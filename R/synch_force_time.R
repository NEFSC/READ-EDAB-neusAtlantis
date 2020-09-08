#Script to synch GLORYS and ECCO-based time vars

library(ncdf4)

statevar.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/statevars/'
trans.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Atlantis_Format/'
trans.prefix = 'GLORYS_Atlantis_Transport_'
statevar.prefix = 'Obs_Hindcast_statevars_'
years = 1993:2017

yr=1
for(yr in 1:length(years)){
  trans.nc = nc_open(paste0(trans.dir,years[yr],'/',trans.prefix,years[yr],'.nc'))
  trans.time = trans.nc$dim$time$vals
  nc_close(trans.nc)
  
  statevar.nc = nc_open(paste0(statevar.dir,statevar.prefix,years[yr],'.nc'),write = T)
  statevar.time = statevar.nc$dim$time$vals
  
  time.df = data.frame(trans.time,statevar.time)
  time.df$trans.date = as.POSIXct(time.df$trans.time,origin = '1964-01-01 00:00:00',tz = 'UTC')
  time.df$statevar.date = as.POSIXct(time.df$statevar.time,origin = '1964-01-01 00:00:00',tz = 'UTC')
  
  #overwrite trans.time onto statevar.nc time
  timedim = ncdf4::ncdim_def('time','',1:length(trans.time),unlim = T,create_dimvar = F)
  var.time = ncdf4::ncvar_def('time',paste0('seconds since 1964-01-01 00:00:00 +10'),timedim,prec='double')
  ncdf4::ncatt_put(statevar.nc,'time','units',paste0('seconds since 1964-01-01 00:00:00 +10'))
  ncdf4::ncvar_put(statevar.nc,var.time,trans.time)
  nc_close(statevar.nc)
}

