# function that edits phytoplankton sinking rates from init.nc

# init.file = here::here('currentVersion','neus_init.nc')
# group.names = c('Diatom_N','Diatom_S')
# svel = c(0.5,0.5)
# new.filename = here::here('currentVersion','neus_init_test.nc')
# overwrite = F
# change = 'scalar'

edit_param_phyto_sink = function(init.file,group.names,svel,change = 'scalar',overwrite = F,new.filename){
  
  library(ncdf4)
  
  if(length(group.names)!= length(svel)){
    error('Svel and group.names not equal length')
  }
  
  if(overwrite == T){
    init.nc = nc_open(init.file,write = T)   
  }else{
    file.copy(init.file,new.filename,overwrite = T)
    init.nc = nc_open(new.filename,write = T)   
  }
  
  
  for(i in 1:length(group.names)){
    
    group.svel = ncatt_get(init.nc, group.names[i])$svel
    
    if(change == 'scalar'){
      new.svel = group.svel * svel[i]
    }else{
      new.svel = svel[i]
    }
    
    ncatt_put(init.nc,varid = group.names[i],attname = 'svel',attval = new.svel)
  }
  
  nc_close(init.nc)
  
}