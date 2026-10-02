#Script to compare initial numbers weight between reference and run
library(ncdf4)
dev.init = nc_open('/net/work3/EDAB/atlantis/dev_branch/currentVersion/neus_init.nc')
master.init = nc_open('/net/work3/EDAB/atlantis/master_branch/currentVersion/neus_init.nc')
new.6536.init = nc_open(here::here('currentVersion','neus_init_rescale_age.nc'))
nre.6665.init = nc_open('/net/work3/EDAB/atlantis/Rob_proj/currentVersion/neus_init.nc')


fgs.dev = read.csv('/net/work3/EDAB/atlantis/dev_branch/currentVersion/neus_groups.csv',as.is = T)
fgs.master = read.csv('/net/work3/EDAB/atlantis/master_branch/currentVersion/neus_groups.csv',as.is = T)
fgs.6536 = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)
fgs.6665 = read.csv('/net/work3/EDAB/atlantis/Rob_proj/currentVersion/neus_groups.csv',as.is = T)

vert.names = fgs.dev$Name[which(fgs.dev$NumCohorts >2)]

out.df = data.frame(Name = vert.names, dev = NA,master = NA, new.6536 = NA, new.6665 = NA)

i=1
for(i in 1:length(vert.names)){
  
  ncohort.dev = fgs.dev$NumCohorts[which(fgs.dev$Name == vert.names[i])]
  ncohort.master = fgs.master$NumCohorts[which(fgs.master$Name == vert.names[i])]
  ncohort.6536 = fgs.6536$NumCohorts[which(fgs.6536$Name == vert.names[i])]
  ncohort.6665 = fgs.6665$NumCohorts[which(fgs.6665$Name == vert.names[i])]
  
  out.dev = lapply(1:ncohort.dev,function(x){
    return(sum(ncvar_get(dev.init,paste0(vert.names[i],x,'_Nums')),na.rm=T))
  })
  out.master = lapply(1:ncohort.master,function(x){
    return(sum(ncvar_get(master.init,paste0(vert.names[i],x,'_Nums')),na.rm=T))
  })
  out.6536 = lapply(1:ncohort.6536,function(x){
    return(sum(ncvar_get(new.6536.init,paste0(vert.names[i],x,'_Nums')),na.rm=T))
  })
  out.6665 = lapply(1:ncohort.6665,function(x){
    return(sum(ncvar_get(nre.6665.init,paste0(vert.names[i],x,'_Nums')),na.rm=T))
  })
  
  out.df$dev[i] = sum(unlist(out.dev))
  out.df$master[i] = sum(unlist(out.master))
  out.df$new.6536[i] = sum(unlist(out.6536))
  out.df$new.6665[i] = sum(unlist(out.6665))
  
}
