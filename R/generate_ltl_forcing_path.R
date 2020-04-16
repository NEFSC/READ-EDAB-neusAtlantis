#Script generates the force.prm calls for each ltl forcing file

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'
ts.dir = "\\tsfiles\\Annual_Files\\"
years = 1964:2014

ltl.files = list.files(force.dir,pattern = '^roms_ltl_force.*\\.nc$',full.names = F)

ltl.nc = ncdf4::nc_open(paste0(force.dir,ltl.files[1]))
var.names = names(ltl.nc$var)
var.names = var.names[var.names != 'ndi']
ncdf4::nc_close(ltl.nc)

ltl.full = matrix(NA,nrow = length(ltl.files),ncol = length(var.names))

for( i in 1:length(var.names)){
  ltl.full[,i] = paste0(var.names[i],'_File',0:(length(years)-1),'.name ',ts.dir,ltl.files)
}

write.csv(ltl.full,file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/tl_force_filenames.csv',row.names = F)
