#Function to rescale values in forcing files

library(ncdf4)

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/'
# file.name = 'ltl_test.nc'
# var.name = 'ndi'
# scale.factor = 0.1978805

fix_force_scalevars = function(roms.dir,file.name,var.name,scale.factor){
  force.nc = ncdf4::nc_open(paste0(roms.dir,file.name),write=T)
  var.vals = ncdf4::ncvar_get(force.nc,var.name)
  # v.orig = summary(var.vals)
  # var.dims = dim(var.vals)
  ncdf4::ncvar_put(force.nc,var.name,var.vals*scale.factor)
  # new.vals = ncdf4::ncvar_get(force.nc,var.name)
  nc_close(force.nc)
}

# fix_force_scalevars(
#   roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/',
#   file.name = 'ltl_test.nc',
#   var.name = 'ndi',
#   scale.factor = 0.1978805
# )


years = 1964:2014
# roms.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/CurrentVersion/tsfiles/Annual_Files/'
roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/'

for( yr in 1:length(years)){
  file.name = paste0('roms_ltl_force_',years[yr],'.nc')  
  var.names = c("ndi", "Diatom_N", "Carniv_Zoo_N", "Zoo_N", "PicoPhytopl_N", "MicroZoo_N", "Diatom_S", "Pelag_Bact_N")
  scale.factors = c(rep(0.1978805,6),0.7887953,0.1978805)
  for(v in 1:length(var.names)){
    fix_force_scalevars(
      roms.dir = roms.dir,
      file.name = file.name,
      var.name = var.names[v],
      scale.factor = scale.factors[v]
    )
    print(var.names[v])
  }
  print(years[yr])
  
  
}

