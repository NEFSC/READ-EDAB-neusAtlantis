# loop over all years from start year to 1979 (inclusive) to generate forcing files for gap

start.year = 1964
# years = seq(start.year,1980)
years = 1964:1992

source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_force_spinup.R')

for(i in 1:length(years)){

  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'
  
  make_force_spinup(
    out.dir = out.dir,
    trans.prefix = 'GLORYS_Atlantis_Transport_',
    statevar.prefix = 'Obs_Hindcast_statevars_',
    phyto.prefix = NA,
    transport.file = paste0(out.dir,'transport/GLORYS_Atlantis_Transport_1993.nc'),
    statevar.file = paste0(out.dir,'statevars/Obs_Hindcast_statevars_1993.nc'),
    phyto.file = NA,
    force.dir = paste0(out.dir,'Forcing_Files/'),
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
    
  )
  print(i)
  
}

years = 1964:1997
for(i in 1:length(years)){
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/'
  
  make_force_spinup(
    out.dir = out.dir,
    trans.prefix = NA,
    statevar.prefix = NA,
    phyto.prefix = 'Phyto_Forcing_',
    transport.file = NA,
    statevar.file = NA,
    phyto.file = paste0(out.dir,'phyto_statevars/Phyto_Forcing_1998.nc'),
    # force.dir = paste0(out.dir,'Forcing_Files/'),
    force.dir = out.dir,
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
    
  )
}

