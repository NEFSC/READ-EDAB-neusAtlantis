# Fixes the time dim for all force.nc files (transport,statevars,ltlvars)
# Make sure that each day is starting at 00:00:00

source(here::here('R','fix_force_time.R'))

transport.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/transport/'
statevar.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/'
ltlvar.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'

# transport.files = list.files(transport.dir,'^roms_output_transport_tohydro.*\\.nc$')
# statevar.files = list.files(statevar.dir,'^roms_output_statevars_tohydro.*\\.nc$')
# ltlvar.files = list.files(ltlvar.dir,'^roms_output_ltl_statevars_tohydro.*\\.nc$')

transport.files = 'roms_output_transport_tohydro_1980.nc'
statevar.files = 'roms_output_statevars_tohydro_1980.nc'
ltlvar.files = 'roms_output_ltl_statevars_tohydro_1980.nc'

for(i in 1:length(transport.files)){
  fix.force.time(transport.dir,transport.files[i],is.romsfile = T)
  fix.force.time(statevar.dir,statevar.files[i],is.romsfile = T)
  fix.force.time(ltlvar.dir,ltlvar.files[i],is.romsfile = T)
  print(i)
}
