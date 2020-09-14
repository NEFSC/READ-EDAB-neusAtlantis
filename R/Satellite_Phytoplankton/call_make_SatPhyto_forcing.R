#Script that calls on make_SatPhyto_Climatology
#A) make satphyto_files 1998-2019
#B) make forcing files w/ make_force_statevar 1998-2019
#C) create spinup forcing 1964-1997

#Read in climatology function and statevar forcing function
source(here::here('R','Satellite_Phytoplankton','make_SatPhyto_files.R'))
source(here::here('R','make_force_statevar.R'))
source('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/R/make_force_spinup.R')

#set.directories
satphyto.dir ='C:/Users/joseph.caracappa/Documents/Satellite_Phyto/' 
rawdata.dir = paste0(satphyto.dir,'Data/')
satphyto.atl.dir = paste0(satphyto.dir,'Atlantis_Format/')
satphyto.force.dir = paste0(satphyto.dir,'Forcing_dynamic_lower/')



# make satphyto files Atlantis-format -------------------------------------

atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicate')

years = 1997:2019
raw.files = paste0('D8-OCCCI-ATLANTIS_',years,'.csv')

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
                    out.dir = satphyto.force.dir,
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


# B) make forcing files w/ make_force_statevar ----------------------------
# .packages = c("devtools","tidyverse","stringi","RNetCDF", "data.table")
# lapply(.packages, require, character.only=TRUE)
# 
# var.units = c(rep('mg N m-3',3),'mg Si m-3')
# fill.val = miss.val = rep(-999,4)
# valid.min = rep(-999,4)
# valid.max = rep(9999,4)
# 
# years = 1997:2019
# 
# for(yr in 1:length(years)){
#   
#   make_force_statevar(roms.dir = satphyto.atl.dir,
#                       roms.file = paste0(satphyto.atl.dir,'Phyto_Atlantis_',years[yr],'.nc'),
#                       out.dir = satphyto.force.dir,
#                       force.vars = atl.varname,
#                       var.units = var.units,
#                       final.vars = atl.varname,
#                       fill.val = fill.val,
#                       long.names = atl.longname,
#                       miss.val = miss.val,
#                       valid.min = valid.min,
#                       valid.max = valid.max,
#                       out.prefix = 'Phyto_Forcing_',
#                       dupe.bottom = F)
#   
# }
# 

# C) Make Spinup ----------------------------------------------------------


#copy to obs hindcast directory
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_DOY_spinup/'
from.files = paste0(satphyto.force.dir,'Phyto_Forcing_',1998:2017,'.nc')
file.copy(from.files,obs.dir,overwrite = T)

#Make spinup files
years = 1964:1997
for(i in 1:length(years)){
  make_force_spinup(
    do.hydroconstruct = F,
    out.dir = obs.dir,
    trans.prefix = NA,
    statevar.prefix = NA,
    anyvar.prefix = 'Phyto_Forcing_',
    transport.file = NA,
    statevar.file = NA,
    # anyvar.file = paste0(obs.dir,'Phyto_Forcing_1998.nc'),
    anyvar.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/LTL_DOY_Climatology.nc',
    anyvar.out = obs.dir,
    force.dir = obs.dir,
    start.year = 1964,
    new.year = years[i],
    mid.layer = 'dynamic',
    bot.layer = 'dynamic',
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
    )
}

#Copy files into GitHub directory
from.files = paste0(obs.dir,'Phyto_Forcing_',1964:2017,'.nc')
git.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'
file.copy(from.files,git.dir,overwrite = T)


