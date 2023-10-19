#Script to change mum/C parameters based on new number of age classes
library(dplyr)

source(here::here('R','Calibration_Tools','edit_param_mum_age.R'))
source(here::here('R','Calibration_Tools','edit_param_C_age.R'))
source(here::here('R','Calibration_Tools','edit_param_FSPB.R'))
# source(here::here('R','Calibration_Tools','get_age_mat.R'))

bio.orig = here::here('currentVersion','at_biology.prm')
bio.new =  here::here('currentVersion','at_biology_rescale_mumC.prm')
file.copy(bio.orig,bio.new,overwrite = T)

mum.orig = get_param_mum_age(bio.orig)
c.orig = get_param_C_age(bio.orig)    
# fspb.orig = get_param_FSPB(bio.orig)
# age.mat = get_age_mat(bio.orig)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% mutate(max.age = NumAgeClassSize * NumCohorts)
fgs.orig = read.csv(here::here('diagnostics','neus_groups_v2_0_1.csv'),as.is =T) %>% mutate(max.age = NumAgeClassSize * NumCohorts)

spp.names = mum.orig$group

max.cohort = max(fgs$NumCohorts)

i=2
new.mum.ls = new.c.ls = list()

for(i in 1:length(spp.names)){
  
  new.mum = new.c = rep(0,max.cohort)
  
  N.age.orig = fgs.orig$NumCohorts[i]
  N.age.new = fgs$NumCohorts[i]
  
  mum.match = which(mum.orig$group == spp.names[i])
  c.match = which(c.orig$group == spp.names[i])
  
  mum.spp.orig = as.numeric(mum.orig[mum.match,2:(N.age.orig +1)])
  c.spp.orig = as.numeric(c.orig[c.match,2:(N.age.orig +1)])
  
  if(N.age.orig == N.age.new){
    next
  }else{
    
    new.x = seq(1,N.age.orig,length.out = N.age.new)    
    
    mum.spline = splinefun(x = 1:N.age.orig,y = mum.spp.orig,method = 'monoH.FC')
    new.mum = signif(mum.spline(new.x),2)
    edit_param_mum_age(bio.prm = bio.new,
                       new.mum = new.mum,
                       group.name = spp.names[i],
                       single.group = T,
                       overwrite = T
                       )
    
    c.spline = splinefun(x = 1:N.age.orig, y = c.spp.orig, method = 'monoH.FC')
    new.c = signif(c.spline(new.x),2)
    edit_param_C_age(bio.prm = bio.new,
                       new.C = new.c,
                       group.name = spp.names[i],
                       single.group = T,
                       overwrite = T
    )
    
    
    # plot(mum.spp.orig,type='b')  
    # curve(mum.spline,1,10,add =T,lty =2)
    # lines(1:10, mum.spline(1:10), col =2)
    # lines(new.x,new.mum,col = 3,lwd =2)
    
    
    #Replace vector length for each changed mum/C
    bio.new.lines = readLines(bio.new)
    mum.spp.line = grep(paste0('mum_',spp.names[i]),bio.new.lines)
    new.mum.line = paste0('mum_',spp.names[i],' ',N.age.new)
    bio.new.lines[mum.spp.line] = new.mum.line
    
    c.spp.line =  grep(paste0('C_',spp.names[i]),bio.new.lines)
    new.c.line = paste0('C_' ,spp.names[i],' ',N.age.new)
    bio.new.lines[c.spp.line] = new.c.line
    
    writeLines(bio.new.lines, con = bio.new)
    
    
  }

}


