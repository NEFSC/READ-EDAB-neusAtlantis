roms.nc = ncdf4::nc_open('D:/NWA/1981/RM_NWA-SZ.HCob05T_avg_1981-01-01.nc')
names.df = data.frame(
  short.name = names(roms.nc$var),
  long.name = NA,
  units = NA,
  stringsAsFactors = F
)

for(i in 1:nrow(names.df)){
  names.df$long.name[i] = ncdf4::ncatt_get(roms.nc,names.df$short.name[i],'long_name')$value
  names.df$units[i] = ncdf4::ncatt_get(roms.nc,names.df$short.name[i],'units')$value
}

write.csv(names.df,'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT varnames.csv',row.names = F)
