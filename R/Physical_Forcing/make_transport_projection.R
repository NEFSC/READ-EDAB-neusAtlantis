#Read in climatology, apply delta, format time
library(dplyr)
library(ncdf4)

years = 2021:2100

start.year = 1964

out.dir = 'C:/Users/joseph.caracappa/Documents/Cobia/Atlantis_Format/transport/'

transport.base.file = here::here('currentVersion','tsfiles','Annual_Files','flow_2020.nc')
transport.base = nc_open(transport.base.file,write = F)

var.names = names(transport.base$var)
var.units = sapply(var.names,function(x) return(ncdf4::ncatt_get(transport.base,x,'units')$value))
var.longname = sapply(var.names,function(x) return(ncdf4::ncatt_get(transport.base,x,'long_name')$value))

y=1
for(y in 1:length(years)){
  
  #make new copy of file
  new.transport.file = paste0(out.dir,'flow_',years[y],'.nc')
  
  #Adjust time marker
  t1 = seq.Date(as.Date(paste0(years[y],'-01-01 00:00:00')),as.Date(paste0(years[y],'-12-31 00:00:00')),'days')
  time.vals =as.numeric(difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0('1964-01-01 00:00:00'),tz = 'UTC'),units = 'secs'))
  
  #define netCDF variables
  t.dim = ncdf4::ncdim_def('t','',1:length(time.vals),unlim = T,create_dimvar = F)
  var.t = ncdf4::ncvar_def('t',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),t.dim,prec='double')
  
  z.dim = ncdf4::ncdim_def('z','',1:4,create_dimvar = F)
  b.dim = ncdf4::ncdim_def('b','',1:30,create_dimvar = F)
  dest.dim = ncdf4::ncdim_def('dest','',1:32,create_dimvar = F)
  
  #If leap year, append values to add extra day
  new.var.dat.ls = list()

  for(v in 1:length(var.names)){
    var.dat = ncdf4::ncvar_get(transport.base,var.names[v])
    # var.def.ls[[v+3]] = ncdf4::ncvar_def(var.names[v],var.units[v],list(leveldim, boxesdim, timedim),-999,longname=var.longname[v],prec="float")
    
    dims = dim(var.dat)
    dims[4] = length(time.vals)

    if(!lubridate::leap_year(years[y])){
      new.var.dat = var.dat
    } else {
      new.var.dat = var.dat[,,,1:365]
    }
    new.var.dat.ls[[v]] = new.var.dat
  }
  # ncdf4::nc_close(transport.base)
  
  #Build new NC File
  #call new netCDF file
  nc.file = RNetCDF::create.nc(new.transport.file)
  
  RNetCDF::dim.def.nc(nc.file, "t", unlim=TRUE)
  RNetCDF::dim.def.nc(nc.file, "b", 30)
  RNetCDF::dim.def.nc(nc.file, "z", 5)
  RNetCDF::dim.def.nc(nc.file, "dest", 32)
  
  RNetCDF::var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
  for(v in 1:length(var.names)){
    #Define Variables
    RNetCDF::var.def.nc(nc.file, var.names[v], 'NC_DOUBLE', c('dest','z','b','t'))
    #Assign Fill Value
    RNetCDF::att.put.nc(nc.file, var.names[v], '_FillValue', "NC_DOUBLE", -999)
    #Assign 
    RNetCDF::att.put.nc(nc.file, var.names[v], 'missing_value', 'NC_DOUBLE',-999)
    #Assign valid_min
    RNetCDF::att.put.nc(nc.file, var.names[v], 'valid_min', 'NC_DOUBLE', -999)
    #Assing valid_max
    RNetCDF::att.put.nc(nc.file, var.names[v], 'valid_max', 'NC_DOUBLE', 99999)
    #Assign units
    RNetCDF::att.put.nc(nc.file, var.names[v], 'units','NC_CHAR', var.units[v])  
    #Assign long_name
    RNetCDF::att.put.nc(nc.file,var.names[v],'long_name','NC_CHAR',var.longname[v])
    
    #Put variable values
    RNetCDF::var.put.nc(nc.file,var.names[v],new.var.dat.ls[[v]])
  }
  
  RNetCDF::att.put.nc(nc.file, "t", "units", "NC_CHAR", 'seconds since 1964-01-01 00:00:00 UTC')
  RNetCDF::att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'NEUS_Atlantis_Obs_Hindcast')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  RNetCDF::var.put.nc(nc.file, "t", as.numeric(time.vals))
  
  
  RNetCDF::close.nc(nc.file)
  
}
