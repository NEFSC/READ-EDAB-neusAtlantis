#Script that calls on make_SatPhyto_Climatology

#Read in climatology function and statevar forcing function
source(here::here('R','make_SatPhyto_files.R'))
source(here::here('R','make_force_statevar.R'))
source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_force_spinup.R')

#set.directories
rawdata.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_dynamic_lower/'
atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicate')

#make climatology
# raw.files = list.files(rawdata.dir,'D8-OCCCI-ATLANTIS_*')

years = 1997:2019
raw.files = paste0('D8-OCCCI-ATLANTIS_',years,'.csv')
f=1
# 
#Forcing with Constant Diatom:Dinoflagellate ratio (2.7:1)
# phyto.fract.ls = list()
# for(f in 1:length(raw.files)){
#   if(years[f] %% 4 == 0){
#     phyto.fract.ls[[f]] = matrix(2.7/3.7,nrow = 30, ncol = 366)
#   }else{
#     phyto.fract.ls[[f]] = matrix(2.7/3.7,nrow = 30, ncol = 365)
#   }
# }

# #Make Forcing using Hirata Diatom Proportion
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
                    out.dir = force.dir,
                    out.prefix =  'Phyto_Forcing_',
                    stat.var = 'MED',
                    bio.vars = c('MICRO','NANO','PICO'),
                    atl.varname = atl.varname,
                    atl.longname = atl.longname,
                    atl.units = c(rep('mg N m-3',3),'mg Si m-3'),
                    dynamic.mid = T,
                    dynamic.bot = T,
                    phyto.fract.ls = phyto.fract.ls,
                    chl.conv = rep(7,3)
)

#copy to obs hindcast directory
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_dynamic_lower/'
from.files = paste0(force.dir,'Phyto_Forcing_',1998:2017,'.nc')
file.copy(from.files,obs.dir,overwrite = T)

#Make spinup files
years = 1964:1997
for(i in 1:length(years)){
  make_force_spinup(
    out.dir = obs.dir,
    trans.prefix = NA,
    statevar.prefix = NA,
    phyto.prefix = 'Phyto_Forcing_',
    transport.file = NA,
    statevar.file = NA,
    phyto.file = paste0(obs.dir,'Phyto_Forcing_1998.nc'),
    force.dir = obs.dir,
    start.year = 1964,
    new.year = years[i],
    dynamic.mid.layer = T,
    dynamic.bot.layer = T,
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
    )
}

#Copy files into GitHub directory
from.files = paste0(obs.dir,'Phyto_Forcing_',1964:2017,'.nc')
git.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'
file.copy(from.files,git.dir,overwrite = T)


