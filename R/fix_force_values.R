#Removes negative values from forcing files


# force.file = ltl.files[1]
# variable = 'Diatom_N'

remove.neg = function(force.file, variable){
  
  force.nc = ncdf4::nc_open(force.file,write = T)
  force.vals = ncdf4::ncvar_get(force.nc,variable)
  missing.val = ncdf4::ncatt_get(force.nc,variable,'_FillValue')$value
  which.neg = which(force.vals < 0 & force.vals != missing.val)  
  force.vals[which.neg] = missing.val
  ncdf4::ncvar_put(force.nc,variable,force.vals)
  ncdf4::nc_close(force.nc)
 
}


roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars'
ltl.files =  list.files(roms.dir,pattern = '^roms_ltl_force.*\\.nc$',full.names = T)

var.names = c('Diatom_N','PicoPhytopl_N','Carniv_Zoo_N','Zoo_N','MicroZoo_N','Pelag_Bact_N','Diatom_S')

for(f in 1:length(ltl.files)){
  for(v in 1:length(var.names)){
    remove.neg(force.file = ltl.files[f], variable = var.names[v])
    # print(var.names[v])
  }
  print(f)
}
