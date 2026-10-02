#Script that calls on make_SatPhyto_Climatology

#Read in climatology function and statevar forcing function
source(here::here('R','make_SatPhyto_climatology.R'))
source(here::here('R','make_force_statevar.R'))

#set.directories
rawdata.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/'
processed.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/'
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Files/'
atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen')

#make climatology
make_SatPhyto_climatology(in.dir = rawdata.dir,
                          in.file = 'DOY-OCCCI-ATLANTIS_NEUS-VER_1.csv',
                          out.dir = processed.dir,
                          out.file = 'Phyto_Climatology',
                          stat.var = 'MED',
                          bio.vars = c('MICRO','NANO','PICO'),
                          atl.groups = c('PL','DF','PS'),
                          atl.varname = atl.varname,
                          atl.longname = atl.longname,
                          phyto.fract = data.frame(PL = c(0.75,0,0),DF = c(0.25,0,0), PS = c(0,1,1)),
                          chl.conv = rep(7,3))

#make forcing
satphyto.files = list.files(processed.dir,'*Climatology.nc',full.names = T)
for(i in 1:length(satphyto.files)){
  make_force_statevar(roms.dir = processed.dir,
                      roms.file = satphyto.files[i],
                      out.dir = force.dir,
                      force.vars = atl.varname,
                      var.units = rep('mg N m-3',3),
                      final.vars = atl.varname,
                      fill.val = rep(0,3),
                      long.names = atl.longname,
                      miss.val = rep(0,3),
                      valid.min = rep(0,3),
                      valid.max = rep(99999,3),
                      dupe.bottom = F,
                      out.prefix = 'SatPhyto_Climatology_')
  print(i)
}

