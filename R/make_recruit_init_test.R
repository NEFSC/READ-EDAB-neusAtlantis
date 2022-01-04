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

run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_Spawn_Debug2/"

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
  filter(time == 0 & group %in% group.names$group)%>%
  rename(cohort = 'agecl',biomass = 'atoutput')%>%
  select(group,cohort,biomass)

init.sn =  readRDS(paste0(run.dir,'Post_Processed/Data/SN_age.rds'))%>%
  left_join(fgs)%>%
  filter(time == 0 & group %in% group.names$group)%>%
  rename(cohort = 'agecl',SN = 'atoutput')%>%
  select(group,cohort,SN)

init.rn =  readRDS(paste0(run.dir,'Post_Processed/Data/RN_age.rds'))%>%
  left_join(fgs)%>%
  filter(time == 0 & group %in% group.names$group)%>%
  rename(cohort = 'agecl',RN = 'atoutput')%>%
  select(group,cohort,RN)

init.nums = readRDS(paste0(run.dir,'Post_Processed/Data/numbers_age.rds'))%>%
  left_join(fgs)%>%
  filter(time == 0 & group %in% group.names$group)%>%
  rename(cohort = 'agecl',number = 'atoutput')%>%
  select(group,cohort,number)

#Calculate Spawn and biomass
init.bio = init.sn %>%
  left_join(init.rn) %>%
  left_join(init.nums)%>%
  left_join(recruit.params$FSPB)%>%
  left_join(recruit.params$FSP)%>%
  left_join(recruit.params$KSPA)%>%
  mutate(w.opt = 3.65*SN,
         biomass = (SN+RN)*number,
         spawn = (((w.opt-KSPA)*FSP)-(w.opt-(SN+RN)))*FSPB*number)

#Get adult aggregates
adult.init.bio = init.bio %>%
  group_by(group)%>%
  summarise(biomass = sum(biomass),
            spawn = sum(spawn))

#Get age 0 aggregates
rec.init.bio = init.bio %>%
  filter(cohort == 1)%>%
  group_by(group)%>%
  summarise(biomass.0 = sum(biomass))

recruit.required = adult.init.bio %>%
  left_join(rec.init.bio)%>%
  mutate(
    biomass.spawn = spawn/biomass,
    alpha = biomass.0/3,
    # beta = ((spawn*alpha)/biomass.0)-biomass,
    beta = biomass/4,
    recruit = (alpha*spawn)/(biomass+beta))

# par(mfrow=c(4,5))
# for(i in 1:nrow(recruit.required)){
#   curve((recruit.required$alpha[i]*recruit.required$biomass.spawn[i]*x)/(recruit.required$beta[i]+x),0,10*recruit.required$biomass[i],
#         ylab = 'recruit',xlab = 'biomass',main = recruit.required$group[i])
#   points(recruit.required$biomass[i],recruit.required$recruit[i])
# }


new.bh = recruit.required%>%
  ungroup()%>%
  left_join(fgs)%>%
  select(index,group,alpha,beta)%>%
  arrange(group)%>%
  filter(group %in% c('COD','GOO','HER','MAK','MPF','RED','STB','SUF','WHK','WIF','YTF'))

edit_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'),
              group.name = new.bh$group,
              alpha = new.bh$alpha,
              beta = new.bh$beta,
              overwrite = T)

