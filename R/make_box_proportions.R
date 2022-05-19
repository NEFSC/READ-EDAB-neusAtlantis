#Script to make mean (fall spring) box population proportions by species based on initial conditions
library(ncdf4)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

groups = fgs$Name

init.nc = nc_open(here::here('currentVersion','neus_init.nc'))

varnames = names(init.nc$var)

box.prop.df = data.frame(matrix(0,nrow = 30,ncol = length(groups)))
colnames(box.prop.df) = fgs$Code

i=1
for(i in 1:length(groups)){
  
  if(fgs$NumCohorts[i]>2){
    
    j=1
    box.age.prop.mat = matrix(0,nrow = 30, ncol = 10)
    for(j in 1:10){
      var.age = grep( paste0(groups[i],j,'_Nums'),varnames,value = T)    
    
      var.age.box = ncvar_get(init.nc,var.age)[1,]  
      var.age.box[is.na(var.age.box)] = 0
      
      var.age.tot = sum(var.age.box)
      var.age.prop = var.age.box/var.age.tot
      box.age.prop.mat[,j] = var.age.prop
    }
    box.prop.df[,i] = rowMeans(box.age.prop.mat)
  }else if(fgs$NumCohorts[i] == 2){
    
    box.age.prop.mat = matrix(0,nrow = 30, ncol = 2)
    for(j in 1:2){
      var.age = grep( paste0(groups[i],'_N',j),varnames,value = T)    
      
      var.age.box = ncvar_get(init.nc,var.age)[1,]  
      var.age.box[is.na(var.age.box)] = 0
      
      var.age.tot = sum(var.age.box)
      var.age.prop = var.age.box/var.age.tot
      box.age.prop.mat[,j] = var.age.prop   
    }
    box.prop.df[,i] = rowMeans(box.age.prop.mat)
  }else{
    
    var.age = grep( paste0(groups[i],'_N'),varnames,value = T) 
    var.age.box = ncvar_get(init.nc,var.age)
    var.age.box[is.na(var.age.box)] = 0
    
    if(length(dim(var.age.box))>1){
      var.age.box = var.age.box[1,]
    }
    
    var.age.tot = sum(var.age.box)
    var.age.prop = var.age.box/var.age.tot
    box.prop.df[,i] = var.age.prop 
  }
}

write.csv(box.prop.df,file = here::here('diagnostics','Group_Box_Proportions.csv'),row.names = F)
