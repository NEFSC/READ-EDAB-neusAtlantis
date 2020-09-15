# Wrapper script to fully process GLORYS data
# A) make GLORYS Atlantis Format (make_GLORYS_files) 1993-2017
# B) make transport files w/ hydroconstruct (make_force_annual) 1993-2017
# C) make tempsalt files using alternative method (make_force_statevar) 1993-2017
# D) create spinup forcing w/ alternative tempsalt and hydroconstruct transport (make_force_spinup) 1964-1992

glorys.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/'
glorys.atl.dir = paste0(glorys.dir,'Atlantis_Format/')
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'
final.force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/'
git.force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'

# A) Make GLORYS Atlantis Format (make_GLORYS_files) 1993-2017 ------------

source(here::here('R','GLORYS','make_GLORYS_files.R'))
dir.names = 1993:2018

for(yr in 1:length(dir.names)){
  
  # Set from and to directories
  in.dir = paste0(glorys.dir,'Data/',dir.names[yr],'/')
  out.dir = paste0(glorys.dir,'Atlantis_Format/',dir.names[yr],'/')
  in.files = list.files(in.dir,paste0('GLORYS_REANALYSIS_*'),full.names = F)

  make_GLORYS_files(
    glorys.dir = in.dir,
    glorys.prefix = paste0('GLORYS_REANALYSIS_*'),
    glorys.files = in.files,
    out.dir = out.dir,
    dz.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',
    bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm',
    bgm.ll.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_ll_WGS84.bgm',
    shp.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp',
    name.out = 'GLORYS_Atlantis_',
    make.hflux = T,
    make.physvars = T
  )
  print(paste0('##################  ',yr,'   #################'))
}

# B) run append_vflux from ECCO -------------------------------------------
#moves statevar files from glorys.atl.dir to Obs_Hindcast/statevars/
source(here::here('R','ECCO','append_vflux.R'))

#STOP run 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/append_vflux_ECCO.sh

# C) Make transport files w/ hydroconstruct (make_force_annual) 19 --------
source(here::here('R','synch_force_time.R'))
source(here::here('R','make_force_annual.R'))

# D) make tempsalt files  using alternative method (make_force_statevar)--------
source(here::here('R','make_force_statevar.R'))

.packages = c("devtools","tidyverse","stringi","RNetCDF", "data.table")
lapply(.packages, require, character.only=TRUE)

force.vars = final.vars = c('temperature','salinity')
var.units = c('degrees Celcius','PSU')
long.names = c('Temperature','Salinity')
fill.val = c(15,0)
miss.val = c(15,0)
valid.min = c(-2,0)
valid.max = c(999,999)

years = 1993:2017

for(yr in 1:length(years)){
  in.dir = paste0(obs.dir,'statevars/')
  in.file = paste0(in.dir,'Obs_Hindcast_statevars_',years[yr],'.nc')
  
  make_force_statevar(roms.dir = in.dir,
                      roms.file = in.file,
                      out.dir = paste0(final.force.dir,'phys_statevars_alternate/'),
                      force.vars = force.vars,
                      var.units = var.units,
                      final.vars = final.vars,
                      fill.val = fill.val,
                      long.names = long.names,
                      miss.val = miss.val,
                      valid.min = valid.min,
                      valid.max = valid.max,
                      out.prefix = 'GLORYS_tempsalt_force_',
                      dupe.bottom = T)
  
}

# E) create spinup forcing 1964-1992 --------

start.year = 1964
years = 1964:1992

source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_force_spinup.R')
# obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/'
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'
for(i in 1:length(years)){
  
  make_force_spinup(
    do.hydroconstruct =F,
    out.dir = obs.dir,
    trans.prefix = 'GLORYS_Atlantis_Transport_',
    statevar.prefix = 'Obs_Hindcast_statevars_',
    anyvar.prefix = NA,
    transport.file = paste0(obs.dir,'Forcing_Files/Annual_Output/combined_years/transport_DOY_climatology.nc'),
    statevar.file = paste0(obs.dir,'Forcing_Files/Annual_Output/combined_years/Physics_DOY_climatology.nc'),
    anyvar.file = NA,
    anyvar.out = NA,
    force.dir =paste0(obs.dir,'Forcing_Files/'),
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
  )
  
  # make_force_spinup(
  #   do.hydroconstruct = F,
  #   out.dir = pasteo(obs.dir,'phys_statevars_alternate_DOY_spinup/'),
  #   trans.prefix = NA,
  #   statevar.prefix = NA,
  #   anyvar.prefix = 'GLORYS_tempsalt_force_',
  #   transport.file = NA,
  #   statevar.file = NA,
  #   anyvar.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/Physics_DOY_Climatology.nc',
  #   # anyvar.out = paste0(final.force.dir,'phys_statevars_alternate/'),
  #   anyvar.out = paste0(obs.dir,'phys_statevars_alternate_DOY_spinup/'),
  #   # force.dir = paste0(obs.dir,'Forcing_Files/'),
  #   force.dir = paste0(obs.dir,'phys_statevars_alternate_DOY_spinup/'),
  #   start.year = 1964,
  #   new.year = years[i],
  #   param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
  #   bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
  # )
  print(i)
  
}


# F) Copy files to Git Directory ------------------------------------------
final.transport = list.files(paste0(final.force.dir,'transport/'),'^flow.*\\.nc$',full.names = T)
final.tempsalt = list.files(paste0(final.force.dir,'phys_statevars_alternate/'),'GLORYS_tempsalt_force_*',full.names = T)

file.copy(final.transport,git.force.dir,overwrite = T)
file.copy(final.tempsalt,git.force.dir,overwrite = T)
