#' Converts aggregated ROMS data to long format
#' 
#' Uses roms netcdf files with dimensions [level,box/face/,time] to 
#' flat format. This can be useful for further analysis/summary of data,
#' especially for plotting
#' 
#' @nc.file string. Full path of netcdf file
#' @is.hflux logical. Is the input data horizontal fluxes
#' 
#' @return long-format dataframe with columns: level, box/face, time, val1...valn
#' 
#' Author: J. Caracappa

# nc.file = transport.file
# nc.file = statevars.file
# is.hflux = T

flatten_ROMS = function(nc.file, is.hflux = T){
  
  dat = ncdf4::nc_open(nc.file)
  ocean_time = as.POSIXct(ncdf4::ncvar_get(dat,'time'),origin = '1964-01-01',tz = 'GMT')
  
  if(is.hflux){
    transport = ncdf4::ncvar_get(dat,'transport')
    dest_b = ncdf4::ncvar_get(dat,'dest_boxid')
    source_b = ncdf4::ncvar_get(dat,'source_boxid')
    pt1_x = ncdf4::ncvar_get(dat,'pt1_x')
    pt2_x = ncdf4::ncvar_get(dat,'pt2_x')
    pt1_y = ncdf4::ncvar_get(dat,'pt1_y')
    pt2_y = ncdf4::ncvar_get(dat,'pt2_y')
    nlev = dim(transport)[1]
    nface = dim(transport)[2]
    
    transport.all=dplyr::bind_rows(lapply(1:dim(transport)[3],function(t) {
      x= as.data.frame(transport[,,t])
      # colnames(x) = paste0('.f',0:(nface-1))
      colnames(x)= 0:(nface-1)
      x$level = 1:nlev
      x$time = ocean_time[t]
      x=tidyr::gather(x,face,hflux,'0':'150',factor_key=T)
      x$dest_b = rep(dest_b,each = nlev)
      x$source_b = rep(source_b,each = nlev)
      x$pt1_x = rep(pt1_x,each = nlev)
      x$pt1_y = rep(pt1_y,each = nlev)
      x$pt2_x = rep(pt2_x,each = nlev)
      x$pt2_y = rep(pt2_y,each = nlev)
      return(x)
      }))
    return(transport.all)
  } else{
    var.names = names(dat$var)
    var.df.ls = list()
    for(i in 1:length(var.names)){
      var = ncdf4::ncvar_get(dat,var.names[i])
      nbox = dim(var)[2]
      nlev = dim(var)[1]
      var.all = dplyr::bind_rows(lapply(1:dim(var)[3],function(t){
        x = as.data.frame(var[,,t])
        # colnames(x) = paste0('.bx',0:(nbox-1))
        colnames(x) = 0:(nbox-1)
        x$level = 1:nlev
        x$time = ocean_time[t]
        x = tidyr::gather(x,box,measurement,'0':'29',factor_key=T)
        return(x)
      }))
      colnames(var.all)[4] = var.names[i]
      var.df.ls[[i]] = var.all
      names(var.df.ls)[i]= var.names[i]
    }
    return(var.df.ls)
  }
}
  
