#Script generates the force.prm calls for each ltl forcing file

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/nut_statevars/'
ts.dir = "\\tsfiles\\Annual_Files\\"
years = 1964:2014

nut.files = list.files(force.dir,pattern = '^roms_nut_force.*\\.nc$',full.names = F)

nut.nc = ncdf4::nc_open(paste0(force.dir,nut.files[1]))
var.names = names(nut.nc$var)
var.names = var.names[var.names != 'ndi']
ncdf4::nc_close(nut.nc)

nut.full = matrix(NA,nrow = length(nut.files),ncol = length(var.names))

for( i in 1:length(var.names)){
  nut.full[,i] = paste0(var.names[i],'_File',0:(length(years)-1),'.name ',ts.dir,nut.files)
}

write.csv(nut.full,file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/nutrient_force_filenames.csv',row.names = F)
