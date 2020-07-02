#' Function that manually edits the values for a variable in a forcing file
#' 
#'@force.dir String. Path to forcing files
#'@force.file String. Full name of forcing file 
#'@var.name string. Variable name
#'@overwrite logical. TRUE if force file should be overwritten
#'@new.file string. Name for new forcing file (Only if overwrite = FALSE)
#'@edit.array array. An array with modifications for the new data. This is the same dimension as the original variable array
#'@edit.method string. 'add' means addative modifications (neg vals for subtraction), 'scale' means multiplicative modifications


# force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis2/currentVersion/tsfiles/Annual_Files/'
# force.file = 'roms_tempsalt_force_1964.nc'
# var.name = 'temperature'
# edit.array = array(1.01,dim = c(5,30,366))
# edit.method = 'scale'
# new.file = 'roms_tempsalt_force_lowtemp_1964.nc'
# overwrite = F



edit_force_var = function(force.dir, force.file,var.name,edit.array,edit.method,new.file, overwrite){
  
  if(overwrite == F){
    file.copy(paste0(force.dir,force.file),paste0(force.dir,new.file))
    force.nc = ncdf4::nc_open(paste0(force.dir,new.file),write = T)
  }else{
    force.nc = ncdf4::nc_open(paste0(force.dir,force.file),write =T)  
  }
  orig.vals = ncdf4::ncvar_get(force.nc,var.name)
  # var.dims = dim(var.vals)
  
  if(edit.method == 'add'){
    new.vals = orig.vals + edit.array
    }else if(edit.method == 'scale'){
    new.vals = orig.vals * edit.array
    } else {
      stop("Must specify 'add' or 'scale' for edit.method")
    }
  
  ncdf4::ncvar_put(force.nc,varid = var.name,vals = new.vals)
  
  ncdf4::nc_close(force.nc)
  # 
  # plot(orig.vals[1,10,],type='l')
  # lines(new.vals[1,10,],col=2)
  }
# 
# edit_force_var(force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis2/currentVersion/tsfiles/Annual_Files/',
#                force.file = 'roms_tempsalt_force_lowtemp_1964.nc',
#                var.name = 'temperature',
#                edit.array = array(2,dim = c(5,30,366)),
#                edit.method = 'add',
#                new.file = 'roms_tempsalt_force_lowtemp_1964.nc',
#                overwrite = T)
