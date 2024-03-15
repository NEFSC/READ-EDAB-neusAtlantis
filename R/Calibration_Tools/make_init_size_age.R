#Function to change fish condition RN:SN in initial conditions
# fgs.file = here::here('currentVersion','neus_groups.csv')
# init.file = here::here('currentVersion','neus_init_test.nc')
# new.filename = here::here('currentVersion','neus_init_test2.nc')
# out.file = here::here('diagnostics','Initial_Size_Age_test.csv')

make_init_size_age = function(init.file,fgs.file,new.filename = NA,out.file ){
  library(ncdf4)
  library(dplyr)
  
  init.nc = nc_open(init.file)
  fgs = read.csv(fgs.file)
  
  var.names = names(init.nc$var)
  
  group.names = fgs$Name[which(fgs$NumCohorts == 10)]

  init.rn.sn.ls = list()
  for(i in 1:length(group.names)){
    
    out.df = data.frame(Name = group.names[i],
                        agecl = 1:10,
                        rn = NA,
                        sn = NA)
    for(j in 1:10){
      
      var.sn = paste0(group.names[i],j,'_StructN')
      var.rn = paste0(group.names[i],j,'_ResN')
      
      val.sn = ncatt_get(init.nc,var.sn,'_FillValue')$value
      val.rn = ncatt_get(init.nc,var.rn,'_FillValue')$value
      
      out.df$rn[j] = val.rn
      out.df$sn[j] = val.sn
    }
    init.rn.sn.ls[[i]] = out.df
    
  }
  
  init.rn.sn = bind_rows(init.rn.sn.ls)
  
  nc_close(init.nc)
  
  write.csv(init.rn.sn, out.file,row.names = F)
  
}
