# loop over all years from start year to 1979 (inclusive) to generate forcing files for gap

start.year = 1964

# years = seq(start.year,1980)
years = 1980

source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_spinup_forcing.R')

for(i in 1:length(years)){
  roms.out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/'
  duplicate.forcing.year(
    roms.out.dir = roms.out.dir,
    transport.file = paste0(roms.out.dir,'transport/roms_output_transport_tohydro_1981.nc'),
    statevar.file = paste0(roms.out.dir,'statevars/roms_output_statevars_tohydro_1981.nc'),
    ltlvar.file = paste0(roms.out.dir,'ltl_statevars/roms_output_ltl_statevars_tohydro_1981.nc'),
    force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/',
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/roms_cobalt_hydroconstruct_v2.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/hydroconstruct_run_template.bat'
  )
  print(i)
  
}

