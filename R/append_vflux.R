ecco.dir = 'C:/Users/joseph.caracappa/Documents/ECCO/vflux_daily/'
glorys.dir = 'C:/USers/joseph.caracappa/Documents/GLORYS/Atlantis_Format/'
years = 1993:2017
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/statevars/'
ecco.prefix = 'ECCO_vflux_Atlantis_'
glorys.prefix = 'GLORYS_Atlantis_statevars_'
out.prefix = 'Obs_Hindcast_statevars_'

yr = 1
for(yr in 1:length(years)){
  
  ecco.name = paste0(ecco.dir,years[yr],'/',ecco.prefix,years[yr],'.nc')
  glorys.name = paste0(glorys.dir,years[yr],'/',glorys.prefix,years[yr],'.nc')
  
  out.rename = paste0(out.dir,out.prefix,years[yr],'.nc')
  
  file.copy(ecco.name,out.dir,overwrite = T)
  file.copy(glorys.name,out.rename,overwrite=T)
  
}