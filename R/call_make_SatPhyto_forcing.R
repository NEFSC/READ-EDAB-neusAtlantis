#Script that calls on make_SatPhyto_Climatology

#Read in climatology function and statevar forcing function
source(here::here('R','make_SatPhyto_files.R'))
source(here::here('R','make_force_statevar.R'))
source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_force_spinup.R')

#set.directories
rawdata.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Constant_PLDF/'
force.dir.2 = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Hirata_PLDF/'
atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicate')

#make climatology
# raw.files = list.files(rawdata.dir,'D8-OCCCI-ATLANTIS_*')

years = 1997:2019
raw.files = paste0('D8-OCCCI-ATLANTIS_',years,'.csv')
f=1

#Forcing with Constant Diatom:Dinoflagellate ratio (2.7:1)
phyto.fract.ls = list()
for(f in 1:length(raw.files)){
  if(years[f] %% 4 == 0){
    phyto.fract.ls[[f]] = matrix(2.7/3.7,nrow = 30, ncol = 366)
  }else{
    phyto.fract.ls[[f]] = matrix(2.7/3.7,nrow = 30, ncol = 365)
  }
}

#make the forcing files
make_SatPhyto_files(in.dir = rawdata.dir,
                    in.prefix = 'D8-OCCCI-ATLANTIS_*',
                    out.dir = force.dir,
                    out.prefix =  'Phyto_Forcing_',
                    stat.var = 'MED',
                    bio.vars = c('MICRO','NANO','PICO'),
                    atl.varname = atl.varname,
                    atl.longname = atl.longname,
                    atl.units = c(rep('mg N m-3',3),'mg Si m-3'),
                    phyto.fract.ls = phyto.fract.ls,
                    chl.con = rep(7,3)
                    )

#Make Forcing using Hirata Diatom Proportion
load('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/diatom_proportion_DOY.R')
phyto.fract.ls = list()
for(f in 1:length(raw.files)){
  if(years[f] %% 4 == 0){
    x = matrix(NA,nrow = 30, ncol = 366)
    x[,1:365] = diatom.prop
    x[,366] = x[,365]
    phyto.fract.ls[[f]] = x
  }else{
    phyto.fract.ls[[f]] = diatom.prop
  }
}

make_SatPhyto_files(in.dir = rawdata.dir,
                    in.prefix = 'D8-OCCCI-ATLANTIS_*',
                    out.dir = force.dir.2,
                    out.prefix =  'Phyto_Forcing_Hirata_',
                    stat.var = 'MED',
                    bio.vars = c('MICRO','NANO','PICO'),
                    atl.varname = atl.varname,
                    atl.longname = atl.longname,
                    atl.units = c(rep('mg N m-3',3),'mg Si m-3'),
                    phyto.fract.ls = phyto.fract.ls,
                    chl.con = rep(7,3)
)

#copy to obs hindcast directory
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_constant_PLDF/'
obs.dir.2 = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_Hirata_PLDF/'
from.files = paste0(force.dir,'Phyto_Forcing_',1998:2017,'.nc')
from.files.2 = paste0(force.dir.2,'Phyto_Forcing_Hirata_',1998:2017,'.nc')
file.copy(from.files,obs.dir,overwrite = T)
file.copy(from.files.2,obs.dir.2,overwrite = T)

#Make spinup files
years = 1964:1997
for(i in 1:length(years)){
  # out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/'
  
  make_force_spinup(
    out.dir = obs.dir,
    trans.prefix = NA,
    statevar.prefix = NA,
    phyto.prefix = 'Phyto_Forcing_',
    transport.file = NA,
    statevar.file = NA,
    phyto.file = paste0(obs.dir,'Phyto_Forcing_1998.nc'),
    # force.dir = paste0(out.dir,'Forcing_Files/'),
    force.dir = obs.dir,
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
    )
}

#Copy files into GitHub directory
from.files = paste0(obs.dir,'Phyto_Forcing_',1964:2017,'.nc')
git.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'
file.copy(from.files,git.dir,overwrite = T)

for(i in 1:length(years)){
  # out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/'
  
  make_force_spinup(
    out.dir = obs.dir.2,
    trans.prefix = NA,
    statevar.prefix = NA,
    phyto.prefix = 'Phyto_Forcing_Hirata_',
    transport.file = NA,
    statevar.file = NA,
    phyto.file = paste0(obs.dir.2,'Phyto_Forcing_Hirata_1998.nc'),
    # force.dir = paste0(out.dir,'Forcing_Files/'),
    force.dir = obs.dir.2,
    start.year = 1964,
    new.year = years[i],
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
  )
}

#Copy files to Git Directory
from.files = paste0(obs.dir.2,'Phyto_Forcing_Hirata_',1964:2017,'.nc')
file.copy(from.files,git.dir,overwrite = T)

# for(f in 1:length(raw.files)){
#   if(years[f] %% 4 == 0){
#     phyto.fract = matrix(0.75,nrow = 30, ncol = 366)
#   }else{
#     phyto.fract = matrix(0.75,nrow = 30, ncol = 365)
#   }
# 
#   make_SatPhyto_files(in.dir = rawdata.dir,
#                       in.file = raw.files[f],
#                       out.dir = force.dir,
#                       out.file = paste0('Phyto_Forcing_',years[f]),
#                       stat.var = 'MED',
#                       bio.vars = c('MICRO','NANO','PICO'),
#                       atl.varname = atl.varname,
#                       atl.longname = atl.longname,
#                       atl.units = c(rep('mg N m-3',3),'mg Si m-3'),
#                       phyto.fract = phyto.fract,
#                       chl.conv = rep(7,3))
#   print(f)
# }

#make forcing
# satphyto.files = list.files(processed.dir,'Phyto_Atlantis',full.names = T)
# for(f in 1:length(satphyto.files)){
#   make_force_statevar(roms.dir = processed.dir,
#                       roms.file = satphyto.files[f],
#                       out.dir = force.dir,
#                       force.vars = atl.varname,
#                       var.units = c(rep('mg N m-3',3),'mg Si m-3'),
#                       final.vars = atl.varname,
#                       fill.val = rep(0,4),
#                       long.names = atl.longname,
#                       miss.val = rep(0,4),
#                       valid.min = rep(0,4),
#                       valid.max = rep(99999,4),
#                       dupe.bottom = F,
#                       out.prefix = 'SatPhyto_Forcing_')
#   print(f)
# }

