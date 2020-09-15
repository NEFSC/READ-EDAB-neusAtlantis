
library(ncdf4)

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/'
roms.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/CurrentVersion/tsfiles/Annual_Files/'
phys.files = list.files(roms.dir,'^roms_tempsalt.*\\.nc$')

for( i in 1:length(phys.files)){
  
  file.nc = nc_open(paste0(roms.dir,phys.files[i]),write=T)
  names(file.nc$var)
  ncvar_rename(file.nc,'Temp','temperature')
  ncvar_rename(file.nc,'salt','salinity')
  nc_close(file.nc)
  
  print(i)
}


# test2 = nc_open('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars/temp1964.nc')
# names(test2$var)
# test3 = nc_open('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars/salt_1964.nc')
# names(test3$var)
