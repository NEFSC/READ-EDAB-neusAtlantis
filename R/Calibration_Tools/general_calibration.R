#'Master wrapper for calibration routines
#'Requires a setup file in the form of /diagnostics/cloud_calibration_setup_example.csv
#'Run.ID: Numberic code used to group runs together
#'Run.Group: User-defined grouping of runs for sensible run comparisons
#'Code: Atlantis functional group code, or for diet changes the formatted predator-prey pair separated by semicolon (e.g. 1MAK1:ZL)
#'Type: Parameter names; mQ, mL, mum, C, BHalpha, BHbeta, KDENR, diet
#'Unit: supplied value type: value or scalar
#'Value: Value to be written to biology.prm
library(dplyr)

#Read in setup file
setup.df = read.csv(here::here('diagnostics','cloud_calibration_setup_example.csv'),as.is=T)

#Define base files
bio.file.orig = here::here('currentVersion','at_biology.prm')
run.sh.orig = here::here('currentVersion','RunAtlantis_cloud.sh')
sbatch.orig = here::here('currentVersion','sbatch_scenario_array_base.sh')
fgs.file = here::here('currentVersion','neus_groups.csv')

#Read in functional groups and define inverts
fgs = read.csv(fgs.file,as.is = T)
invert.groups = fgs$Code[which(fgs$NumCohorts==1)]

#identify run numbers
run.id = sort(unique(setup.df$Run.ID))

#Read in necessary functions from setup.df
if(any(c('mQj','mQa','mQ','mLj','mLa','mL') %in% setup.df$Type )){
  source(here::here('R','Calibration_Tools','edit_param_mortality_age.R'))
  source(here::here('R','Calibration_Tools','edit_param_invert_mortality.R'))
  mort.age.orig = get_param_mort_age(bio.file.orig,fgs.file )
  mort.invert.orig = get_param_invert_mort(bio.file.orig,fgs.file)
}
if('mum' %in% setup.df$Type | 'C' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_mum_age.R'))
  source(here::here('R','Calibration_Tools','edit_param_C_age.R'))
  source(here::here('R','Calibration_Tools','edit_param_invert_c_mum.R'))
  c.age.orig = get_param_C_age(bio.file.orig)
  mum.age.orig = get_param_mum_age(bio.file.orig)
  mum.c.invert.orig = get_param_invert_c_mum(bio.file.orig,invert.groups)
}
if('BHalpha' %in% setup.df$Type | 'BHbeta' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_BH.R'))
  bh.orig = get_param_BH(bio.file.orig)
}
if('KDENR' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_KDENR.R'))
  kdenr.orig = get_param_KDENR(bio.file.orig)
}
if('diet' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_pprey.R'))
}


#Loop through run id's
i=1
for(i in 1:length(run.id)){
  
  #copy biology.prm with run.id prefix
  bio.file.new = here::here('currentVersion',paste0('at_biology_',run.id[i],'.prm'))
  file.copy(bio.file.orig, bio.file.new,overwrite = T)
  
  #Separate task for run.id
  setup.run = filter(setup.df, Run.ID == run.id[i])
  
  j=1
  for(j in 1:nrow(setup.run)){
    
    is.invert = setup.run$Code[j] %in% invert.groups
    
    #Do mQ or mL tasks
    if(setup.run$Type[j] %in% c('mQa','mQj','mLa','mLj','mL','mQ')){
      
      #Get Original values
      if(setup.run$Type[j] %in% c('mLa','mLj','mL')){
        if(setup.run$Code[j] %in% run.invert){
          mort.group.old = mort.invert.orig$mL[which(mort.invert.orig$group == setup.run$Code[j])]
        }else{
          mort.group.old = mort.age.orig %>%
            filter(group == setup.run$Code[j])%>%
            select(mL.j,mL.a)%>%
            as.numeric()
        }
      }else{
        if(setup.run$Code[j] %in% run.invert){
          mort.group.old = mort.invert.orig$mQ[which(mort.invert.orig$group == setup.run$Code[j])]
        }else{
          mort.group.old = mort.age.orig %>%
            filter(group == setup.run$Code[j])%>%
            select(mQ.j,mQ.a)%>%
            as.numeric()
        } 
      }
      
      #Handle unit cases
      mort.group.new = mort.group.old
      if(setup.run$Unit[j] == 'value'){
        
        if(setup.run$Type[j] %in% c('mLj','mQj')){
          mort.group.new[1] = setup.run$Value[j]
        }else if(setup.run$Type[j] %in% c('mLa','mQa')){
          mort.group.new[2] = setup.run$Value[j]
        }else{
          mort.group.new = setup.run$Value[j]
        }
        
      }else{
        
        if(setup.run$Type[j] %in% c('mLj','mQj')){
          mort.group.new[1] = mort.group.new[1]*setup.run$Value[j]
        }else if(setup.run$Type[j] %in% c('mLa','mQa')){
          mort.group.new[2] = mort.group.new[2]*setup.run$Value[j]
        }else{
          mort.group.new = mort.group.new *setup.run$Value[j]
        }
        
      }
      
      if(is.invert){
        edit_param_invert_mort(bio.file = bio.file.new,
                               group = setup.run$Code[j],
                               type = setup.run$Type[j],
                               value = mort.group.new,
                               new.file = F)
      }else{
        mort.type = ifelse(grepl('mL',setup.run$Type[j]),'mL','mQ')
        edit_param_mort_age(bio.prm = bio.file.new,
                            new.mort = mort.group.new,
                            type = mort.type,
                            single.group = T,
                            group.name = setup.run$Code[j],
                            overwrite = T
                            )
      }
    
  }
  
    if(setup.run$Type[j] == 'mum'){
      
    }
    
    
  
  
  
  
  
  
}
