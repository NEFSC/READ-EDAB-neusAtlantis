#Script to rescale initial conditions to account for changes in NumCohorts
#Includes: StructN, ResN, and Nums
library(dplyr)

source(here::here('R','Calibration_Tools','get_init_agedist.R'))
source(here::here('R','Calibration_Tools','get_init_spatial_dist.R'))


fgs.file = here::here('currentVersion','neus_groups.csv')
fgs.orig.file = here::here('diagnostics','neus_groups_v2_0_1.csv')
fgs = read.csv(fgs.file,as.is = T) %>% mutate(max.age = NumAgeClassSize * NumCohorts)
fgs.orig = read.csv(fgs.orig.file,as.is =T) %>% mutate(max.age = NumAgeClassSize * NumCohorts)

spp.names = fgs$Code

init.orig = here::here('currentVersion','neus_init.nc')
# init.new = here::here('currentVersion','neus_init_rescale_age.nc')
# file.copy(init.orig,init.new,overwrite =T)

i=2
for(i in 1:length(spp.names)){
  
  fgs.match = which(fgs$Code == spp.names[i])
  
  N.age.orig = fgs.orig$NumCohorts[fgs.match]
  N.age.new = fgs$NumCohorts[fgs.match]
  
  new.x = seq(1,N.age.orig,length.out = N.age.new)    
  
  if(N.age.orig == N.age.new){
    next()
  }else{
    
    init.age.orig= get_init_agedist(init.file = init.orig,fgs.file = fgs.orig.file,group.name = spp.names[i])
    init.box.orig = get_init_spatial_dist(init.file = init.orig, fgs.file = fgs.orig.file, group.name = spp.names[i],
                                          run.file = here::here('currentVersion','at_run.prm'))
    
    sn.spline = splinefun(x = 1:N.age.orig,y = init.age.orig$SN,method = 'monoH.FC')
    new.sn = signif(sn.spline(new.x))
    
    rn.spline = splinefun(x = 1:N.age.orig, y = init.age.orig$RN,method = 'monoH.FC')
    new.rn = signif(rn.spline(new.x))
    
    sn.rn.out = data.frame(age = 1:N.age.new,sn = new.sn,rn = new.rn)
    write.csv(sn.rn.out, here::here('Setup_Files','new_age_init',paste0(spp.names[i],'_RN_SN.csv')))
    
    #Calculate the Num total by box then rescale the age composition in each box
    box.tot.orig = apply(init.box.orig,2,sum,na.rm=T)
    # box.prop.orig = box.tot.orig/sum(box.tot.orig)
    nbox = length(box.tot.orig)
    j=1
    init.box.new = matrix(NA,nrow = N.age.new,ncol = nbox)
    for(j in 1:nbox){
      
      if(all(is.na(init.box.orig[,j]))){
        next()
      }
      box.prop.orig = init.box.orig[,j]/box.tot.orig[j]
      box.prop.spline = splinefun(x = 1:N.age.orig,y = box.prop.orig,method = 'monoH.FC')
      
      box.prop.new = signif(box.prop.spline(new.x),2)
      int =0
      while(sum(box.prop.new)!=1){
        new.catch.age = round(box.prop.new/sum(box.prop.new),2)
        # print(int)
        int =int+1
        if(int > 3){
          prop.diff = 1 - sum(box.prop.new)
          box.prop.new[1] =box.prop.new[1] + prop.diff
        }
      }
      box.age.new = box.prop.new * box.tot.orig[j]
      init.box.new[,j] = box.age.new
    }
    
    # write.csv(init.box.new,here::here('Setup_Files','new_age_init',paste0(spp.names[i],'_box_dist.csv')),row.names =F)
    
    out.txt.ls = list()
    for(k in 1:N.age.new){
      box.age.new2 = paste0(round(init.box.new[k,],0),',')
      box.age.new2[which(box.age.new2 == 'NA,')] = '_,'
      
      out.txt = matrix('_,',nrow = 30, ncol = 5)
      out.txt[,1] = box.age.new2
      out.txt[30,5] = '_ ;'  
      
      out.txt.ls[[k]] = rbind(paste0(fgs$Name[fgs.match],'_',k),out.txt)
    }
    out.txt.ls = do.call('rbind',out.txt.ls)
    
    write.csv(out.txt.ls,here::here('Setup_Files','new_age_init',paste0(spp.names[i],'_box_dist_text.csv')),row.names =F)
    
    # plot(SN~age,init.age.orig,type = 'b')
    # lines(1:N.age.orig,sn.spline(1:N.age.orig),lty =2)
    # lines(new.x,sn.spline(new.x),col =2)

  }
}