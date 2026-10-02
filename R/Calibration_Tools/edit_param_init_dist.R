#Function to edit the spatial distribution in the initial conditions file
edit_param_init_dist = function(init.file, fgs.file, group.name, new.props,new.file.name,overwrite,bgm.file){
  
 fgs = read.csv(fgs.file)
 
 if(overwrite){
   init.nc = ncdf4::nc_open(init.file,write = T)
 }else{
   file.copy(init.file,new.file.name,overwrite =T)
   init.nc = ncdf4::nc_open(new.file.name,write =T)
 }
   
 init.names = names(init.nc$var)
 
 spp.info = fgs[which(fgs$Code == group.name),]
 
 age.groups = atlantistools::get_age_groups(fgs.file)
 epi.groups = c('SED_EP_FF','SED_EP_OTHER','MOB_EP_OTHER')
 
 if(spp.info$Name %in% age.groups){
   
   n.age = spp.info$NumCohorts

   i=1
   for(i in 1:n.age){
     
     name.num = paste0(spp.info$Name,i,'_Nums')
     
     num.orig = ncdf4::ncvar_get(init.nc,name.num)
     
     num.tot = sum(num.orig,na.rm=T)
     
     num.new = num.orig
     
     num.new[1,] = num.tot * new.props
     
     ncdf4::ncvar_put(init.nc,name.num,num.new)
     
   }
   ncdf4::nc_close(init.nc)
   
 }else{
    
   name.N = paste0(spp.info$Name,'_N')
   
   N.orig = ncdf4::ncvar_get(init.nc,name.N)
   
   boxes = rbgm::bgmfile(bgm.file)$boxes
   box.l1 = abs(boxes$botz)
   box.l1[box.l1 > 50] = 50
   box.area = boxes$area
   
   if(spp.info$GroupType %in% epi.groups){
     
     bio.orig = N.orig*box.area
     
     bio.tot = sum(bio.orig,na.rm=T)
     
     N.new = (bio.tot * new.props)/box.area
     N.new[which(!is.finite(N.new))] = 0
     
   }else{
     
     box.volume = box.l1 * box.area
     bio.orig = N.orig * box.volume
     
     bio.tot = sum(bio.orig,na.rm=T)
     
     N.new = (bio.tot * new.props)/box.volume
     N.new[which(!is.finite(N.new))] = 0
    }
   
    ncdf4::ncvar_put(init.nc,name.N,N.new)
    ncdf4::nc_close(init.nc)
  }
}

# edit_param_init_dist(
#   init.file = here::here('currentVersion','neus_init.nc'),
#   fgs.file = here::here('currentVersion','neus_groups.csv'),
#   group.name = 'MAK',
#   new.props = rep(1/30,30),
#   overwrite = F,
#   new.file.name = here::here('currentVersion','neus_init_test.nc'),
#   bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
# )
