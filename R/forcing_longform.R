#' Converts Atlantis forcing files into long (i.e. flat) format
#' 
#' Takes forcing netCDF files and converts it to a flat format. 
#' Produces a list with one element for each named variable in 
#' forcing file. 
#' 
#' @force.dir string. Path to forcing file directory
#' @force.file string. name of forcing file
#' 
#' @return list. List with on element for each variable.
#' 
#' Author: J. Caracappa


# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'
# force.file = 'roms_ltl_force_1964.nc'

make.force.long = function(force.dir, force.file){
    
  force.nc = ncdf4::nc_open(paste0(force.dir,force.file)) 
  
  var.names = names(force.nc$var)
  dims = dim(ncdf4::ncvar_get(force.nc,var.names[1]))
  # levels = 1:dims[1]
  boxes = 0:(dims[2]-1)
  tsteps = 1:dims[3]
  
  time.vals = force.nc$dim$t$vals
  time.date = as.POSIXct(time.vals,origin = '1964-01-01 00:00:00',tz = 'UTC')
  time.year = format(time.date, format = '%Y')
  time.month = format(time.date,format = '%m')
  
  out.ls = list()
  for(var in 1:length(var.names)){
    
    var.vals = ncdf4::ncvar_get(force.nc,var.names[var])
    var.ls = list()
    for(t in 1:length(tsteps)){
      
      dat = var.vals[,,t]
      dat.ls = list()
      for(b in 1:length(boxes)){
        vals = as.numeric(na.omit(dat[,b]))
        vals = vals[vals!=0]
        if(length(vals) == 0){
          dat.ls[[b]] = NULL
          next
        }
        levs = 0:(length(vals)-1)
        alt.levs = rev(levs)+1
        if(b %in% c(24,25)){
          dat.ls[[b]] = NULL
        }else{
          dat.ls[[b]] =data.frame(box = (b-1),level = levs,alt.level = alt.levs,
                            time = t,value = vals, var.name = var.names[var],time.s = time.vals[t],
                            time.date = time.date[t],year = time.year[t],month = time.month[t],
                            stringsAsFactors = F )  
        }
      }
      
      var.ls[[t]] = dplyr::bind_rows(dat.ls)
    }
    
    out.ls[[var]] = dplyr::bind_rows(var.ls)
    print(var)
  }
  names(out.ls) = var.names
  return(dplyr::bind_rows(out.ls))
  
}
#Test
 # make.force.long(
 #   force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/',
 #   force.file = 'roms_ltl_force_1964.nc'
 #   
 # )
 