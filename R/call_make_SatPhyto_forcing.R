#Script that calls on make_SatPhyto_Climatology

#Read in climatology function and statevar forcing function
source(here::here('R','make_SatPhyto_files.R'))
source(here::here('R','make_force_statevar.R'))

#set.directories
rawdata.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
# processed.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/'
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Files/'
atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicate')

#make climatology
# raw.files = list.files(rawdata.dir,'D8-OCCCI-ATLANTIS_*')

years = 1998:2017
raw.files = paste0('D8-OCCCI-ATLANTIS_',years,'.csv')
f=1
for(f in 1:length(raw.files)){
  if(years[f] %% 4 == 0){
    phyto.fract = matrix(0.75,nrow = 30, ncol = 366)
  }else{
    phyto.fract = matrix(0.75,nrow = 30, ncol = 365)
  }

  make_SatPhyto_files(in.dir = rawdata.dir,
                      in.file = raw.files[f],
                      out.dir = force.dir,
                      out.file = paste0('Phyto_Forcing_',years[f]),
                      stat.var = 'MED',
                      bio.vars = c('MICRO','NANO','PICO'),
                      atl.varname = atl.varname,
                      atl.longname = atl.longname,
                      atl.units = c(rep('mg N m-3',3),'mg Si m-3'),
                      phyto.fract = phyto.fract,
                      chl.conv = rep(7,3))
  print(f)
}

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

