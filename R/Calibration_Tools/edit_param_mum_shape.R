#Function to rescale mum and C

reshape_mumC = function(bio.prm, groups, mum.shape,overwrite = F, new.file.name,change.mum = T, change.C = F){
  
  source(here::here('R','edit_param_mum_age.R'))
  source(here::here('R','edit_param_C_age.R'))
  
  mum.orig = get_param_mum_age(bio.prm = bio.prm)
  c.orig = get_param_C_age(bio.prm = bio.prm)

  if(overwrite == F){
    file.copy(bio.prm,new.file.name, overwrite = T)
  }
  
  for(i in 1:length(groups)){
    
    base.mum = as.numeric(mum.orig[which(mum.orig$group == groups[i]),2:11])
    base.c = as.numeric(c.orig[which(c.orig$group == groups[i]),2:11])
    
    mum.c.ratio = mean(base.c/base.mum)
    
    mean.mum = mean(base.mum,na.rm=T)
    
    new.mum = signif(mean.mum*mum.shape,4)
    new.c=cumsum(new.mum*mum.c.ratio)
    
    if(overwrite == T){
      
      if(change.mum == T){
        edit_param_mum_age(bio.prm= bio.prm,
                           new.mum = new.mum,
                           single.group = T,
                           group.name = groups[i],
                           overwrite = T)
      }
      if(change.C == T){
        edit_param_C_age(bio.prm= bio.prm,
                           new.C = new.c,
                          single.group = T,
                           group.name = groups[i],
                           overwrite = T)
      }
    }else{
      if(change.mum == T){
        edit_param_mum_age(bio.prm= new.file.name,
                           new.mum = new.mum,
                           single.group = T,
                           group.name = groups[i],
                           overwrite = T)
      }
      if(change.C == T){
        edit_param_C_age(bio.prm= new.file.name,
                         new.C = new.c,
                         single.group = T,
                         group.name = groups[i],
                         overwrite = T)
      }
    }
    
  }
}

# reshape_mumC(bio.prm = here::here('currentVersion','at_biology.prm'),
#              # groups = c('MAK','WHK','MPF','BUT','GOO','COD','SHK','POL','RHK','BSB','SCU','RED','DRM','STB','HAD'),
#              groups = 'HAL',
#              mum.shape = c(0.05,0.3,2,2,1.75,1.5,1,0.75,0.5,0.15),
#              overwrite = T,
#              new.file.name = NA,
#              change.mum = T,
#              change.C = F
#              )
