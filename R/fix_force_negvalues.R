#Removes negative values from forcing files


# force.file = ltl.files[1]
# variable = 'Diatom_N'

fix_force_negvalues = function(force.file, variable){
  
  force.nc = ncdf4::nc_open(force.file,write = T)
  force.vals = ncdf4::ncvar_get(force.nc,variable)
  missing.val = ncdf4::ncatt_get(force.nc,variable,'_FillValue')$value
  which.neg = which(force.vals < 0 & force.vals != missing.val)  
  force.vals[which.neg] = missing.val
  ncdf4::ncvar_put(force.nc,variable,force.vals)
  ncdf4::nc_close(force.nc)
 
}

#Physics
# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/'
roms.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/CurrentVersion/tsfiles/Annual_Files/'
phys.files =  list.files(roms.dir,pattern = '^roms_tempsalt_force.*\\.nc$',full.names = T)

var.names = c('Temp','salt')
var.names = c('temperature','salinity')

for(f in 1:length(phys.files)){
  # for(f in 17){
  for(v in 1:length(var.names)){
    fix_force_negvalues(force.file = phys.files[f], variable = var.names[v])
    # print(var.names[v])
  }
  print(f)
}

#LTL values
# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/'
roms.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/CurrentVersion/tsfiles/Annual_Files/'
ltl.files =  list.files(roms.dir,pattern = '^roms_ltl_force.*\\.nc$',full.names = T)

var.names = c('Diatom_N','PicoPhytopl_N','Carniv_Zoo_N','Zoo_N','MicroZoo_N','Pelag_Bact_N','Diatom_S')

for(f in 1:length(ltl.files)){
# for(f in 17){
  for(v in 1:length(var.names)){
    fix_force_negvalues(force.file = ltl.files[f], variable = var.names[v])
    # print(var.names[v])
  }
  print(f)
}

#Nutrient Values
roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/'
roms.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/CurrentVersion/tsfiles/Annual_Files/'
nut.files =  list.files(roms.dir,pattern = '^roms_nut_force.*\\.nc$',full.names = T)

var.names = c('NH3','NO3','Oxygen','Si')

for(f in 1:length(nut.files)){
  # for(f in 17){
  for(v in 1:length(var.names)){
    fix_force_negvalues(force.file = nut.files[f], variable = var.names[v])
    # print(var.names[v])
  }
  print(f)
}


