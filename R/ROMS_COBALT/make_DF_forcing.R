#Function that splits "Diatom" forcing (nlg) into Diatom and Dinoflagellates based on SiO4 saturation constant

library(ncdf4)
library(dplyr)
library(RNetCDF)

force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis2/currentVersion/tsfiles/Annual_Files/'

ltl.files = list.files(force.dir,'roms_ltl*')
nut.files = list.files(force.dir,'roms_nut*')

k.sio4 = 184.2 

years = 1964:2014

var.names = c('Diatom_N','Diatom_S','DinoFlag_N')
var.units = c('mg N m-3','mg Si m-3','mg N m-3')
valid.min = rep(0,3)
valid.max = rep(9999,3)
fill.vals = miss.vals = rep(0,3)

for(yr in 1:length(ltl.files)){
  
  ltl.nc = nc_open(paste0(force.dir,ltl.files[yr]))
  nut.nc = nc_open(paste0(force.dir,nut.files[yr]))
  
  nlg = ncvar_get(ltl.nc,'Diatom_N')
  silg = ncvar_get(ltl.nc,'Diatom_S')
  sio4 = ncvar_get(nut.nc,'Si')
  
  time.vals = ltl.nc$dim$t$vals
  
  nc_close(ltl.nc)
  nc_close(nut.nc)
  
  F.PL = sio4/(k.sio4+sio4)
  F.DF = 1-F.PL
  
  PL.new = nlg*F.PL
  DF.new = nlg*F.DF
  silg.new = silg*F.PL
  
  var.result.ls = list(PL.new,silg.new,DF.new)
  
  nc.file = create.nc(paste0(force.dir,'roms_largephyto_force_',years[yr],'.nc'))
  
  dim.def.nc(nc.file,'t',unlim = T)
  dim.def.nc(nc.file,'b',30)
  dim.def.nc(nc.file,'z',5)
  
  var.def.nc(nc.file,'t','NC_DOUBLE','t')
  
  for(v in 1:length(var.names)){
    var.def.nc(nc.file,var.names[v],'NC_DOUBLE',c('z','b','t'))
    att.put.nc(nc.file,var.names[v],'_FillValue','NC_DOUBLE',fill.vals[v])
    att.put.nc(nc.file,var.names[v],'missing_value','NC_DOUBLE',miss.vals[v])
    att.put.nc(nc.file,var.names[v],'valid_min','NC_DOUBLE',valid.min[v])
    att.put.nc(nc.file,var.names[v],'valid_max','NC_DOUBLE',valid.max[v])
    att.put.nc(nc.file,var.names[v],'units','NC_CHAR',var.units[v])
    
    var.put.nc(nc.file,var.names[v],var.result.ls[[v]])
  }
  
  att.put.nc(nc.file,'t','units','NC_CHAR',"seconds since 1964-01-01 00:00:00 +10")
  att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
  att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'ROMS_COBALT')
  att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
  att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc.file, "t", time.vals)
  
  close.nc(nc.file)
  
  
}