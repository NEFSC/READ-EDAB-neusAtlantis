#' Function to align timesteps in forcing files
#' 
#' If there is a mismatch in the hour along the ROMS 
#' data, it creates an uneven timestamp interval. This
#' function ensures that the time variables are recorded 
#' properly (for timee 00:00:00). This is generally meant 
#' to fix aggregated ROMS netCDF files. Although it should 
#' work on Atlantis files.
#' 
#' @force.dir string. Path to location of forcing files
#' @force.file string. Filename
#' 
#' @return Overwrites forcing file to have correct time keeping.
#' 
#' Author: J. Caracappa
#' 

# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/transport/'
# force.file = 'roms_output_transport_tohydro_1981.nc'


fix_force_time = function(force.dir,force.file,is.romsfile = F){
  force.nc =   ncdf4::nc_open(paste0(force.dir,force.file),write = T)
  
  file.year = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",force.file)))
  if(is.romsfile){
    orig.time = ncdf4::ncvar_get(force.nc,'time')  
  }else{
    orig.time = ncdf4::ncvar_get(force.nc,'t')
  }
  
  x=as.POSIXct(orig.time,origin = '1964-01-01',tz = 'UTC')
    
  end.day = as.Date(x[length(x)])
  
  t1 = seq.Date(as.Date(paste0(file.year,'-01-01 00:00:00')),as.Date(paste0(end.day,' 00:00:00')),'days')
  time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct('1964-01-01 00:00:00',tz = 'UTC'),units = 'secs')
  # as.POSIXct(as.numeric(time.vals),origin = '1964-01-01',tz = 'UTC')
  
  if(is.romsfile){
    timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
    var.time = ncdf4::ncvar_def('time',paste0('seconds since 1964-01-01 00:00:00 +10'),timedim,prec='double')
    ncdf4::ncatt_put(force.nc,'time','units',paste0('seconds since 1964-01-01 00:00:00 +10'))
  } else {
    timedim = ncdf4::ncdim_def('t','',1:length(time.vals),unlim = T,create_dimvar = F)
    var.time = ncdf4::ncvar_def('t',paste0('seconds since 1964-01-01 00:00:00 +10'),timedim,prec='double')
    ncdf4::ncatt_put(force.nc,'t','units',paste0('seconds since 1964-01-01 00:00:00 +10'))
    
  }

  ncdf4::ncvar_put(force.nc,var.time,time.vals)
  ncdf4::nc_close(force.nc)
  
}
