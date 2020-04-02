#' Checks that timesteps are consistent within and between annual forcing files
#' 
#' When forcing is split amongst multiple files, there's a possibility of errors
#' in the timekeeping within and between files. This function checks to make sure
#' that the time interval remains constant over all forcing files.
#' 
#' @force.dir string. path to forcing files location
#' @force.pattern string. pattern to recognize desired forcing files
#' @plot.time logical. plot time indices to look for discontinuity?
#' @dt integer. number of seconds inbetween timesteps
#' 
#' @return Summary of whether data passes checks
#' 
#' Author: J. Caracappa
#' 

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/'
# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/transport/'
force.pattern = '^flow.*\\.nc$'
# force.pattern = '^roms_output_transport_tohydro.*\\.nc$'
plot.time = T
dt = 86400

check.force.timestep = function(force.dir,force.pattern,plot.time){
  
  file.names = sort(list.files(force.dir,pattern = force.pattern))
  # years = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",file.names)))
  time.ls = list()
  for(i in 1:length(file.names)){
    dat = ncdf4::nc_open(paste0(force.dir,file.names[i]))
    time.ls[[i]] =dat$dim$t$vals
    # time.ls[[i]] =dat$dim$time$vals
    ncdf4::nc_close(dat)
  }
  time.long = data.frame(secs = unlist(time.ls), date = NA,diff = NA)
  time.long$date = as.POSIXct(time.long$secs,origin = '1964-01-01 00:00:00',tz = 'UTC')  
  time.long$diff[2:nrow(time.long)] = diff(time.long$secs)
  time.long$hour = format(time.long$date,format = '%H')
  
  # plot(as.Date(time.long$date),time.long$diff,type='l')
  # plot(as.Date(time.long$date),time.long$hour,type='l')
  not.match = which(!is.na(time.long$diff) & time.long$diff != dt)
  
  # x= data.frame(s = time.ls[[17]],date = as.POSIXct(time.ls[[17]],origin = '1964-01-01 00:00:00',tz = 'UTC') )
  # x2= data.frame(s = time.ls[[18]],date = as.POSIXct(time.ls[[18]],origin = '1964-01-01 00:00:00',tz = 'UTC') )
  
  if(length(not.match) == 0){
    print('All timesteps spaced by dt value')
  } else {
    print("Some timesteps don't match dt value")
  }
  
}
