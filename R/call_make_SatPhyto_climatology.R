#Script that calls on make_SatPhyto_Climatology

source(here::here('R','make_SatPhyto_climatology.R'))


make_SatPhyto_climatology(in.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/',
                          in.file = 'DOY-OCCCI-ATLANTIS_NEUS-VER_1.csv',
                          out.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/',
                          out.file = 'Phyto_Climatology',
                          stat.var = 'MED',
                          bio.vars = c('MICRO','NANO','PICO'),
                          atl.groups = c('PL','DF','PS'),
                          atl.varname = c('Diatom_N','DinoFlag_N','PicoPhytopl_N'),
                          atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen'),
                          phyto.fract = data.frame(PL = c(0.5,0,0),DF = c(0.5,0.5,0), PS = c(0,0.5,1)),
                          chl.conv = rep(7,3))
