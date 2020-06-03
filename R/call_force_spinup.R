# loop over all years from start year to 1979 (inclusive) to generate forcing files for gap

start.year = 1964

# years = seq(start.year,1980)
years = 1964:1980

source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_force_spinup.R')

for(i in 1:length(years)){
  roms.out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/'
  make_force_spinup(
    roms.prefix = 'roms_cobalt_v10_',
    roms.out.dir = roms.out.dir,
    # transport.file = paste0(roms.out.dir,'transport/roms_cobalt_v10_transport_1981_neus_atl.nc'),
    transport.file = NA,
    # statevar.file =paste0(roms.out.dir,'phys_statevars/roms_cobalt_v10_statevars_1981_neus_atl.nc'),
    statevar.file = NA,
    ltlvar.file = paste0(roms.out.dir,'ltl_statevars/roms_cobalt_v10_ltl_statevars_1981_neus_atl.nc'),
    # nutvar.file = paste0(roms.out.dir,'nut_statevars/roms_cobalt_v10_nutvars_1981_neus_atl.nc'),
    nutvar.file = NA,
    force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/',
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/roms_cobalt_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/hydroconstruct_run_template.bat'
  )
  print(i)
  
}
