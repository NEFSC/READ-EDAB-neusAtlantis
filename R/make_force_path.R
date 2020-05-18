# Script to generate the paths and formatting needed for atlantis force.prm file

#Physics forcing
#LTL Forcing
force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/'
ts.dir = "\\tsfiles\\Annual_Files\\"
years = 1964:2014

flow.files = list.files(force.dir,pattern = '^flow.*\\.nc$',full.names = F)
temp.files = list.files(force.dir,pattern = '^temp.*\\.nc$',full.names = F)
salt.files = list.files(force.dir,pattern = '^salt.*\\.nc$',full.names = F)


flow.full = paste0('hd',(1:length(years))-1,'.name ',ts.dir,flow.files)
temp.full = paste0('Temperature',(1:length(years))-1,'.name ',ts.dir,temp.files)
salt.full = paste0('Salinity',(1:length(years))-1,'.name ',ts.dir,salt.files)

out.df = data.frame(flow.files = flow.full, temp.files = temp.full, salt.files = salt.full,ltl.files = ltl.full)

write.csv(out.df,file = paste0(force.dir,'force_file_names.csv'),row.names = F)

difftime(as.Date('1964-01-01'),as.Date('2014-12-12'),'days')


#LTL Forcing

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


#Nutrient Forcing

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
