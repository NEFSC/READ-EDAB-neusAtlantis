#SCript to change fish condition RN:SN in initial conditions
library(ncdf4)
library(dplyr)

new.scale =2

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

init.nc = ncdf4::nc_open(here::here('currentVersion','neus_init.nc'),write = T)

var.names = names(init.nc$var)

group.names = fgs$Name[which(fgs$NumCohorts == 10)]
group.change = 'Monkfish'
i=1

init.rn.sn.ls = list()
for(i in 1:length(group.names)){
  
  j=1
  out.df = data.frame(Name = group.names[i],
                      agecl = 1:10,
                      rn = NA,
                      sn = NA)
  for(j in 1:10){
    
    var.sn = paste0(group.names[i],j,'_StructN')
    var.rn = paste0(group.names[i],j,'_ResN')
    
    val.sn = ncatt_get(init.nc,var.sn,'_FillValue')$value
    val.rn = ncatt_get(init.nc,var.rn,'_FillValue')$value
    
    
    new.rn = val.sn * new.scale
    
    if(group.names[i] %in% group.change){
      ncatt_put(init.nc,var.rn,'_FillValue',new.rn)
      
      sn.mat = matrix(val.sn[1],nrow =5, ncol =30)
      rn.mat = matrix(new.rn,nrow =5, ncol =30)
      
      ncvar_put(init.nc,var.sn,sn.mat)
      ncvar_put(init.nc,var.rn,rn.mat)
      
      out.df$rn[j] = new.rn
      out.df$sn[j] = val.sn
    }else{
      
      out.df$rn[j] = val.rn
      out.df$sn[j] = val.sn
      
    }

    

  }
  init.rn.sn.ls[[i]] = out.df
  
}

init.rn.sn = bind_rows(init.rn.sn.ls)

nc_close(init.nc)

write.csv(init.rn.sn, here::here('diagnostics','Initial_Size_Age.csv'),row.names = F)

