# run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_Spawn_Debug2/"
make_recruit_output = function(run.dir){
  
  #Make Recruitment diagnostics to estimate values for BH alpha and beta based on initial conditions
  library(ncdf4)
  library(dplyr)
  
  #Read in functions
  source(here::here('R','get_recruit_type.R'))
  source(here::here('R','get_recruit_params.R'))
  source(here::here('R','edit_param_BH.R'))
  
  #Get Recruitment Parameters
  recruit.params = get_recruit_params(bio.prm = here::here('currentVersion','at_biology.prm'))
  
  bh = get_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
    mutate(alpha = as.numeric(as.character(alpha)),
           beta = as.numeric(as.character(beta)))
  
  #Get Species code Index
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%
    mutate(index = Index+1)%>%
    select('index','Code','LongName')%>%
    rename(species = 'LongName', group = 'Code')
  
  #Get BH Recruitment groups
  group.names = get_recruit_type(here::here('currentVersion','at_biology.prm')) %>%
    filter(flagrecruit ==3)
  
  #Read initial conditions
  init.bio = readRDS(paste0(run.dir,'Post_Processed/Data/biomass_age.rds'))%>%
    left_join(fgs)%>%
    filter( group %in% group.names$group)%>%
    rename(cohort = 'agecl',biomass = 'atoutput')%>%
    select(time,group,cohort,biomass)
  
  init.sn =  readRDS(paste0(run.dir,'Post_Processed/Data/SN_age.rds'))%>%
    left_join(fgs)%>%
    filter(group %in% group.names$group)%>%
    rename(cohort = 'agecl',SN = 'atoutput')%>%
    select(time,group,cohort,SN)
  
  init.rn =  readRDS(paste0(run.dir,'Post_Processed/Data/RN_age.rds'))%>%
    left_join(fgs)%>%
    filter(group %in% group.names$group)%>%
    rename(cohort = 'agecl',RN = 'atoutput')%>%
    select(time,group,cohort,RN)
  
  init.nums = readRDS(paste0(run.dir,'Post_Processed/Data/numbers_age.rds'))%>%
    left_join(fgs)%>%
    filter(group %in% group.names$group)%>%
    rename(cohort = 'agecl',number = 'atoutput')%>%
    select(time,group,cohort,number)
  
  #Calculate Spawn and biomass
  init.bio = init.sn %>%
    left_join(init.rn) %>%
    left_join(init.nums)%>%
    left_join(recruit.params$FSPB)%>%
    left_join(recruit.params$FSP)%>%
    left_join(recruit.params$KSPA)%>%
    # filter(group == 'BLF')%>%
    mutate(w.opt = 3.65*SN,
           biomass = (SN+RN)*number,
           spawn = (((w.opt-KSPA)*FSP)-(w.opt-(SN+RN)))*FSPB*number)
  
  return(init.bio)
}
