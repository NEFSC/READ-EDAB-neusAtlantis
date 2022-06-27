#SCript to change fish condition RN:SN in initial conditions
library(ncdf4)

new.scale = 0.5

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

init.nc = ncdf4::nc_open(here::here('currentVersion','neus_init.nc'),write = T)

var.names = names(init.nc$var)

group.names = fgs$Name[which(fgs$NumCohorts == 10)]

i=1
for(i in 1:length(group.names)){
  
  j=1
  for(j in 1:10){
    
    var.sn = paste0(group.names[i],j,'_StructN')
    var.rn = paste0(group.names[i],j,'_ResN')
    
    val.sn = ncatt_get(init.nc,var.sn,'_FillValue')$value
    val.rn = ncatt_get(init.nc,var.rn,'_FillValue')$value
    
    new.rn = val.sn * new.scale
    
    ncatt_put(init.nc,var.rn,'_FillValue',new.rn)
  }
  
}

nc_close(init.nc)

