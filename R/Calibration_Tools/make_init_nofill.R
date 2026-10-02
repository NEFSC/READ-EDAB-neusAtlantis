#Script to create replace blanks spaces with fill value
library(ncdf4)
init.file = here::here('currentVersion','neus_init_rescale_age.nc')
init.new = here::here('currentVersion','neus_init_rescale_age_nofill.nc')

file.copy(init.file,init.new,overwrite = T)

init.nc = nc_open(init.new,write = T)

var.names = names(init.nc$var)

i=1
for(i in 1:length(var.names)){
  
  var.fill = ncatt_get(init.nc,var.names[i],'_FillValue')$value
  if(length(var.fill)== 0){
    next()
  }
  
  var.vals = ncvar_get(init.nc,var.names[i])
  
  var.vals[which(is.na(var.vals))] = var.fill
  
  ncatt_put(init.nc,var.names[i],attname = '_FillValue',attval = -999,prec = 'double')
  ncvar_change_missval(init.nc,var.names[i],-999)
  ncvar_put(init.nc,var.names[i],var.vals)
  nc_sync(init.nc)
}
nc_close(init.nc)
