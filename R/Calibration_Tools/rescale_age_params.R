#Script to change mum/C parameters based on new number of age classes
library(dplyr)

source(here::here('R','Calibration_Tools','edit_param_mum_age.R'))
source(here::here('R','Calibration_Tools','edit_param_C_age.R'))
source(here::here('R','Calibration_Tools','edit_param_FSPB.R'))
source(here::here('R','Calibration_Tools','get_age_mat.R'))
source(here::here('R','Calibration_Tools','edit_param_catch_age.R'))

bio.orig = here::here('currentVersion','at_biology.prm')
bio.new =  here::here('currentVersion','at_biology_rescale_mumC.prm')
file.copy(bio.orig,bio.new,overwrite = F)

harvest.orig = here::here('currentVersion','at_harvest.prm')
harvest.new = here::here('currentVersion','at_harvest_rescale_age.prm')
file.copy(harvest.orig,harvest.new, overwrite =T)

mum.orig = get_param_mum_age(bio.orig)
c.orig = get_param_C_age(bio.orig)    
fspb.orig = get_param_FSPB(bio.orig)
age.mat = get_age_mat(bio.orig)
catch.age.orig = get_param_catch_age(harvest.file = harvest.orig)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% mutate(max.age = NumAgeClassSize * NumCohorts)
fgs.orig = read.csv(here::here('diagnostics','neus_groups_v2_0_1.csv'),as.is =T) %>% mutate(max.age = NumAgeClassSize * NumCohorts)

spp.names = mum.orig$group

max.cohort = max(fgs$NumCohorts)

i=2
new.mum.ls = new.c.ls = list()

do.mum.C =F
do.fspb = F
do.catch.age =F

for(i in 1:length(spp.names)){
  
  # new.mum = new.c = rep(0,max.cohort)
  
  fgs.match = which(fgs$Code == spp.names[i])
  
  N.age.orig = fgs.orig$NumCohorts[fgs.match]
  N.age.new = fgs$NumCohorts[fgs.match]
  
  mum.match = which(mum.orig$group == spp.names[i])
  c.match = which(c.orig$group == spp.names[i])
  
  mum.spp.orig = as.numeric(mum.orig[mum.match,2:(N.age.orig +1)])
  c.spp.orig = as.numeric(c.orig[c.match,2:(N.age.orig +1)])
  
  if(N.age.orig == N.age.new){
    next()
  }else{
    
    
    new.x = seq(1,N.age.orig,length.out = N.age.new)    
    
    if(do.mum.C == T){
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
    
    if(do.catch.age==T){
      
      harvest.match = which(catch.age.orig$group.names == spp.names[i])
      catch.age.spp.orig = as.numeric(catch.age.orig[harvest.match,2:(N.age.orig+1)])
      
      catch.age.spline = splinefun(x = 1:N.age.orig, y = catch.age.spp.orig,method = 'monoH.FC')
      new.catch.age = signif(catch.age.spline(new.x),2)
  
      int =0
      while(sum(new.catch.age)!=1){
        new.catch.age = round(new.catch.age/sum(new.catch.age),2)
        print(int)
        int =int+1
        if(int > 3)
          catch.diff = 1 - sum(new.catch.age)
          new.catch.age[N.age.new] =new.catch.age[N.age.new] + catch.diff
      }
      # plot(1:10,catch.age.spp.orig,type='l')
      # curve(catch.spline,1,10,col =2,add =T)
      # lines(new.x,new.catch.age,col =3)
      
      edit_param_catch_age(harvest.file = harvest.new,
                           group = spp.names[i],
                           new.value = new.catch.age,
                           overwrite =T
      )
      harvest.lines = readLines(harvest.new)
      catch.age.spp.line = grep(paste0('CatchTS_agedistrib',spp.names[i]),harvest.lines)
      new.catch.age.line = paste0('CatchTS_agedistrib',spp.names[i],' ',N.age.new)
      harvest.lines[catch.age.spp.line] = new.catch.age.line
      
      writeLines(harvest.lines,con = harvest.new) 
    }
    
    }

    
    if(fgs$max.age[fgs.match] == fgs.orig$max.age[fgs.match] |do.fspb == F){
      next()
    } else{
      
      fspb.spp = fspb.orig[which(fspb.orig$group == spp.names[i]),2:N.age.orig]
      
      age.mat.spp = as.numeric(age.mat$age.mat[which(age.mat$spp == spp.names[i])])
      
      new.fspb = rep(0,N.age.new)
      new.fspb[age.mat.spp:N.age.new] = 1
      
      edit_param_FSPB(bio.prm = bio.new,
                      group.name = spp.names[i],
                      FSPB = new.fspb,
                      overwrite =T)
      
      bio.new.lines = readLines(bio.new)
      fspb.spp.line = grep(paste0('FSPB_',spp.names[i]),bio.new.lines)
      new.fspb.line = paste0('FSPB_',spp.names[i],' ',N.age.new)
      bio.new.lines[fspb.spp.line] = new.fspb.line
      
      writeLines(bio.new.lines,con = bio.new)
    }
    
    
  }

