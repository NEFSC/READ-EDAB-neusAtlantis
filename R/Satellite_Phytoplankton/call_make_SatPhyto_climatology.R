#Script that calls on make_SatPhyto_Climatology

#Read in climatology function and statevar forcing function
source(here::here('R','Satellite_Phytoplankton','make_SatPhyto_climatology_byClass.R'))
source(here::here('R','Physical_Forcing','make_force_statevar.R'))

#set.directories
rawdata.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/'
processed.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/v6/'
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Files/v6/'
atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen')

#make climatology
make_SatPhyto_climatology_byClass(in.dir = rawdata.dir,
                          micro.file = 'DOY-OCCCI-ATLANTIS_NEUS-PSC_MICRO-TURNER.CSV',
                          nanopico.file = 'DOY-OCCCI-ATLANTIS_NEUS-PSC_NANOPICO-TURNER.CSV',
                          out.dir = processed.dir,
                          out.file = 'Phyto_Climatology',
                          stat.var = 'MED',
                          bio.vars =  c('PSC_MICRO','PSC_NANOPICO'),
                          atl.groups = c('PL','DF','PS'),
                          atl.varname = atl.varname,
                          atl.longname = atl.longname,
                          hirata.doy = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/v6/diatom_proportion_DOY_dataframe.rds', 
                          chl.conv = rep(7,3)
                          )

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
                      out.prefix = 'SatPhyto_Climatology')
  print(i)
}

