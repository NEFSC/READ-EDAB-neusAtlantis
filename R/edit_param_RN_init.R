#Function to change fish condition RN:SN in initial conditions
# new.scale =2
# fgs.file = here::here('currentVersion','neus_groups.csv')
# init.file = here::here('currentVersion','neus_init_test.nc')
# group.change = 'MAK'
# new.filename = here::here('currentVersion','neus_init_test2.nc')
# out.file = here::here('diagnostics','Initial_Size_Age_test.csv')
edit_param_RN_init = function(init.file,fgs.file,group.change = NULL, new.scale = NA,overwrite = F, new.filename,out.file){
  library(ncdf4)
  library(dplyr)
  
  fgs = read.csv(fgs.file)
  
  if(overwrite){
    init.nc = ncdf4::nc_open(init.file,write = T)  
  }else{
    file.copy(init.file,new.filename,overwrite =T)
    init.nc = ncdf4::nc_open(new.filename,write = T)
  }
  
  var.names = names(init.nc$var)
  
  group.names = fgs$Name[which(fgs$NumCohorts == 10)]
  if(is.null(group.change)){
    group.change.long = group.names
  }else{
    group.change.long = fgs$Name[which(fgs$Code %in% group.change)]  
  }
  
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
      
      if(group.names[i] %in% group.change.long){
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
  
  write.csv(init.rn.sn, out.file,row.names = F)
  
}
edit_param_RN_init(
  init.file =  here::here('currentVersion','neus_init.nc'),
  fgs.file =  here::here('currentVersion','neus_groups.csv'),
  group.change = NULL,
  new.scale = 2,
  overwrite = T,
  out.file = here::here('diagnostics','Initial_Size_Age.csv')
)
