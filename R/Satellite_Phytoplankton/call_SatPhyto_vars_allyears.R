#Script to pull euphotic depth, chlorophyll, and primary production for all years

source(here::here('R','get_SatPhyto_var_allyears.R'))

satphyto.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
years = 1997:2019
file.prefix = 'D8-OCCCI-ATLANTIS_'
varnames = c('PPD','ZEU','CHLOR_A')

for(v in 1:length(varnames)){
  get_SatPhyto_var_allyears(satphyto.dir = satphyto.dir,
                             years = years,
                             file.prefix = file.prefix,
                             varname = varnames[v],
                             out.dir = satphyto.dir)
}