

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/'
# roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
# roms.file = paste0(roms.dir,'roms_output_ltl_statevars_tohydro_1964.nc')

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/phys_statevars/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981_neus_atl.nc')
force.vars = c('temperature','salinity')
var.units = c('deg C','psu')


tempsalt.files = list.files(roms.dir,'*.nc',full.names = T)
.packages = c("devtools","tidyverse","stringi","RNetCDF", "data.table")
lapply(.packages, require, character.only=TRUE)
source(here::here('R','make_force_statevar.R'))

for(i in 1:length(tempsalt.files)){
  make_force_statevar(roms.dir = roms.dir,
                      roms.file = tempsalt.files[i],
                      out.dir = out.dir,
                      force.vars = force.vars,
                      var.units = var.units,
                      out.prefix = 'roms_tempsalt_force_')
  print(i)
}

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981_neus_atl.nc')
force.vars = c('ndi','nlg','nlgz','nmdz','nsm','nsmz','silg','nbact')
var.units = c(rep('mg N m-3',6),'mg Si m-3','mg N m-3')

ltl.files = list.files(roms.dir,'*.nc',full.names = T)
for(i in 1:length(tempsalt.files)){
  make_force_statevar(roms.dir = roms.dir,
                      roms.file = ltl.files[i],
                      out.dir = out.dir,
                      force.vars = force.vars,
                      var.units = var.units,
                      out.prefix = 'roms_ltl_force_')
  print(i)
}

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/nut_statevars/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981_neus_atl.nc')
force.vars = c('nh4','no3','o2','sio4')
var.units = c('mg N m-3','mg N m-3','mg O2 m-3','mg Si m-3')

nut.files = list.files(roms.dir,'*.nc',full.names = T)
for(i in 1:length(nut.files)){
  make_force_statevar(roms.dir = roms.dir,
                      roms.file = nut.files[i],
                      out.dir = out.dir,
                      force.vars = force.vars,
                      var.units = var.units,
                      out.prefix = 'roms_nut_force_')
  print(i)
}
