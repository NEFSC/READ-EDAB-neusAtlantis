#Catch spike analysis 
# 1) Recovery time 
# 2) Maximum deviation
library(dplyr)
library(ggplot2)

experiment.id = 'fspike_UnfishedRecovery'

data.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id,'/')

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T) %>%
  filter(experiment_id %in% c('fspike1','fspike2'))


fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

source(here::here('R','Calibration_Tools','get_age_mat.R'))
age.mat = read.csv(here::here('diagnostics','group_mature_age.csv'),as.is =T) %>%
  rename(Code = 'spp')
source(here::here('R','Calibration_Tools','edit_param_catch_age.R'))
catch.age.prop = get_param_catch_age(harvest.file = here::here('currentVersion','at_harvest.prm'))%>%
  rename(Code = 'group.names')%>%
  tidyr::gather(dum,catch.prop,-Code)%>%
  tidyr::separate(dum, c('dum','agecl'))%>%
  select(-dum)%>%
  mutate(agecl = as.numeric(agecl) -1)

biomass = readRDS(paste0(data.dir,'BiomIndx_',experiment.id,'_1_28105_year.rds'))%>%
  left_join(setup.df, by = 'run.id')%>%
  select(Time,Code,Biomass,scalar,run.id)%>%
  mutate(scalar = factor(scalar))

biomass.baseline = biomass %>%
  filter(scalar == 'baseline')

biomass.age= readRDS(paste0(data.dir,'AgeBiomIndx_',experiment.id,'_1_28105_year.rds'))%>%
  left_join(setup.df,by = 'run.id')%>%
  select(Time,Code,agecl,Biomass,scalar,run.id)%>%
  left_join(age.mat)%>%
  mutate(age.group = ifelse(agecl < age.mat, 'juv','adult'))%>%
  group_by(Time,Code,scalar,run.id)%>%
  mutate(Biomass.tot = sum(Biomass,na.rm=T))%>%
  ungroup()%>%
  mutate(Biomass.prop = Biomass/Biomass.tot)%>%
  group_by(Time,Code,age.group,scalar,run.id)%>%
  summarise(Biomass.prop = sum(Biomass.prop,na.rm=T))%>%
  mutate(scalar = factor(scalar))
  

biomass.age.baseline = biomass.age %>%
  filter(scalar == 'baseline')


spp.names = sort(unique(setup.df$target.species))

#' Measurements at 6 critical time periods
#'t1: Biomass at spike start
#'t2: Biomass at spike end
#'t3: Minimum biomass after spike
#'t3: Biomass at proj t+5
#'t4: Biomass at proj t+10
#'t5: biomass at proj t+20
#'

t1 = master.dat$event_start_d[1]/365
t2 = master.dat$event_end_d[2]/365
t3 = t1 + 5
t4 = t1 + 10
t5 = t1 + 19

#Create empty dataframes for stats
bio.base.stats = data.frame(Code = spp.names,  b0.t1 = NA, b0.t2 = NA, b0.t3 = NA , b0.t4 = NA, b0.t5 = NA,stringsAsFactors = F)
bio.run.stats =  data.frame(Code = setup.df$target.species, scalar= setup.df$scalar, b.t1 = NA, b.t2 = NA,b.t3 = NA, b.t4 = NA, b.t5 = NA,stringsAsFactors = F)
bio.age.base.stats = bio.age.run.stats = list()

extract.time = function(data, time, var.name){  
  dum = data %>%
    filter(Time == time)
  return(dum[[var.name]])
}

i=1
#Calculate biomass at key time points for baseline biomass
for(i in 1:length(spp.names)){
  
  bio.base.spp = biomass %>%
    filter(Code == spp.names[i])
  
  if(nrow(bio.base.spp)== 0){next()}
  
  bio.base.stats$b0.t1[i] = extract.time(bio.base.spp,t1,'Biomass')
  bio.base.stats$b0.t2[i] = extract.time(bio.base.spp,t2,'Biomass')
  bio.base.stats$b0.t3[i] = extract.time(bio.base.spp,t3,'Biomass')
  bio.base.stats$b0.t4[i] = extract.time(bio.base.spp,t4,'Biomass')
  bio.base.stats$b0.t5[i] = extract.time(bio.base.spp,t5,'Biomass')
  
  bio.base.spp = bio.base.spp %>% 
    filter(Time > t2)

  
  #Extract Biomass baseline age group proportions for each time point
  bio.age.base.spp = biomass.age %>%
    filter(Code == spp.names[i])%>%
    arrange(Time,Code,age.group,scalar)
  
  if(length(unique(bio.age.base.spp$age.group))==1){
    age.group.names = NA
  }else{
    age.group.names = c('adult','juv')
  }
  
  bio.age.base.stats[[i]] = data.frame(Code = spp.names[i],
             age.group = age.group.names,
             b0.t1 = extract.time(bio.age.base.spp,t1,'Biomass.prop'),
             b0.t2 = extract.time(bio.age.base.spp,t2,'Biomass.prop'),
             b0.t3 = extract.time(bio.age.base.spp,t3,'Biomass.prop'),
             b0.t4 = extract.time(bio.age.base.spp,t4,'Biomass.prop'),
             b0.t5 = extract.time(bio.age.base.spp,t5,'Biomass.prop'))
  
  print(i)
}
bio.age.base.stats = bind_rows(bio.age.base.stats)

#calculate biomass at key time points for scenario runs
for(i in 1:nrow(setup.df)){
  
  bio.spp = biomass %>%
    filter(Code == setup.df$target.species[i] & scalar == setup.df$scalar[i] )
  
  if(nrow(bio.spp)== 0){next()}
  
  early.recover = bio.spp %>% filter(Time >= t1 & Time <= t3)
  t.min = early.recover$Time[which(early.recover$Biomass == min(early.recover$Biomass,na.rm=T))][1]
  
  bio.run.stats$b.t1[i] = extract.time(bio.spp,t1,'Biomass')
  bio.run.stats$b.t2[i] = extract.time(bio.spp,t2,'Biomass')
  bio.run.stats$b.t3[i] = extract.time(bio.spp,t3,'Biomass')
  bio.run.stats$b.t4[i] = extract.time(bio.spp,t4,'Biomass')
  bio.run.stats$b.t5[i] = extract.time(bio.spp,t5,'Biomass')
  bio.run.stats$t.min[i] = t.min
  bio.run.stats$b.t.min[i] = extract.time(bio.spp,t.min,'Biomass')
  
  #Extract Biomass baseline age group proportions for each time point
  bio.age.spp = biomass.age %>%
    filter(Code == setup.df$target.species[i] & scalar == setup.df$scalar[i])
  
  if(length(unique(bio.age.spp$age.group))==1){
    age.group.names = NA
  }else{
    age.group.names = c('adult','juv')
  }
  
  bio.age.run.stats[[i]] = data.frame(Code = setup.df$target.species[i],
                                       age.group = age.group.names,
                                       scalar = setup.df$scalar[i],
                                       b.t1 = extract.time(bio.age.spp,t1,'Biomass.prop'),
                                       b.t2 = extract.time(bio.age.spp,t2,'Biomass.prop'),
                                       b.t3 = extract.time(bio.age.spp,t3,'Biomass.prop'),
                                       b.t4 = extract.time(bio.age.spp,t4,'Biomass.prop'),
                                       b.t5 = extract.time(bio.age.spp,t5,'Biomass.prop'),
                                      b.t.min = extract.time(bio.age.spp,t.min,'Biomass.prop'))
  
  print(i)
}

bio.age.run.stats = bind_rows(bio.age.run.stats)

#Calculate recovery proportion at each time slice
bio.recovery = bio.run.stats %>% 
  left_join(bio.base.stats)%>%
  mutate(db.t2 = b.t2/b0.t2,
         db.t3 = b.t3/b0.t3,
         db.t4 = b.t4/b0.t4,
         db.t5 = b.t5/b0.t5)%>%
  mutate(recovery.5 = (db.t3-db.t2)/(t3-t2),
         recovery.10 = (db.t4-db.t2)/(t4-t2),
         recovery.20 = (db.t5-db.t2)/(t5-t2))
  



saveRDS(bio.recovery,paste0(data.dir,'recovery_stats_', experiment.id,'.rds'))
bio.recovery = readRDS(paste0(data.dir,'recovery_stats_', experiment.id,'.rds'))

# bio.recovery %>% filter(Code == 'BUT')
#Calculate the minimum maximum scalar before recovery is possible
bio.recovery.max.scalar = data.frame(Code = spp.names, max.recovery = NA)

for(i in 1:length(spp.names)){
  
  bio.spp = bio.recovery %>%
    filter(Code == spp.names[i])%>%
    mutate(db.t4 = ifelse(abs(db.t4) < 0.01, 0, db.t4),
           positive.20 = db.t4 > 0)%>%
    filter(positive.20 == TRUE)
  
  if(nrow(bio.spp)== 0){
    bio.recovery.max.scalar$max.recovery[i] = NA 
  }else{
    bio.recovery.max.scalar$max.recovery[i] = max(bio.spp$scalar)  
  }
    
}

saveRDS(bio.recovery.max.scalar, paste0(data.dir,'recovery_threshold_',experiment.id,'.rds'))

#Calculate the slope of recovery rate ~ scalar at each recovery time
bio.recovery.rate = list()
recovery.times = c(5,10,20)

for(i in 1:length(spp.names)){
  
  bio.spp = bio.recovery %>%
    filter(Code == spp.names[i])%>%
    select(Code, scalar,recovery.5,recovery.10,recovery.20)%>%
    tidyr::gather('dum','recovery.rate',-Code,-scalar)%>%
    tidyr::separate(dum, c('dum','recovery.time'))%>%
    filter(scalar > 0)%>%
    mutate(recovery.rate = ifelse(recovery.rate == 0, NA, recovery.rate))
  
  bio.spp.ls = list()
  
  for(j in 1:length(recovery.times)){
    bio.spp.time = bio.spp %>%
      filter(recovery.time == recovery.times[j])

    if(all(!is.finite(bio.spp.time$recovery.rate))){
      bio.spp.ls[[j]] = data.frame(Code = spp.names[i],
                                   recovery.time = recovery.times[j], 
                                   slope = NA,
                                   intercept = NA,
                                   r2 = NA,
                                   p = NA)  
    }else{
      model = lm(recovery.rate~scalar,data = bio.spp.time)  
      model.sum = summary(model)
      
      bio.spp.ls[[j]] = data.frame(Code = spp.names[i],
                                   recovery.time = recovery.times[j], 
                                   slope = as.numeric(model$coefficients[2]),
                                   intercept = as.numeric(model$coefficients[1]),
                                   r2 = model.sum$r.squared,
                                   p = signif(model.sum$coefficients[2,4],2))  
    }
    
  }
  bio.recovery.rate[[i]] = bind_rows(bio.spp.ls)
}
bio.recovery.rate = bind_rows(bio.recovery.rate)
saveRDS(bio.recovery.rate,paste0(data.dir,'recovery_rate_lm_',experiment.id,'.rds'))

#Caculate change in biomass proportion by age group over time

bio.age.prop = bio.age.run.stats %>%
  left_join(bio.age.base.stats)%>%
  filter(age.group == 'juv')%>%
  mutate(delta.age.spike = b.t2-b.t1,
         delta.age.5 = b.t3-b0.t3,
         delta.age.10 = b.t4-b0.t4,
         delta.age.20 = b.t5-b0.t5)

saveRDS(bio.age.prop, paste0(data.dir,'age_stats_',experiment.id,'.rds'))
bio.age.prop = readRDS( paste0(data.dir,'age_stats_',experiment.id,'.rds'))

spp.names2 = as.character(sort(unique(bio.age.prop$Code)))
bio.age.prop.lm = data.frame(Code = spp.names2, slope = NA, intercept = NA, r2 = NA, p = NA)

for(i in 1:length(spp.names2)){
  
  bio.age.spp = bio.age.prop %>%
    filter(Code == spp.names2[i])%>%
    filter(scalar > 0)
  
  model = lm(delta.age.spike ~ scalar, data = bio.age.spp)
  model.sum = summary(model)
  
  bio.age.prop.lm$slope[i] = as.numeric(model$coefficients[2])
  bio.age.prop.lm$intercept[i] = as.numeric(model$coefficients[1])
  bio.age.prop.lm$r2[i] = model.sum$r.squared
  bio.age.prop.lm$p[i] = signif(model.sum$coefficients[2,4],2)

}

saveRDS(bio.age.prop.lm, paste0(data.dir,'age_stats_lm_',experiment.id,'.rds'))

#Exploitable age
isFished = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>%
  select(Code,isFished)

exploitable.age = read.table('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/neus_outputAgeBiomIndx.txt',header = T, stringsAsFactors = F)%>%
  tidyr::gather('dum','Biomass',-Time)%>%
  tidyr::separate(dum,c('Code','agecl'),sep = '\\.')%>%
  filter(Code %in% fgs$Code)%>%
  mutate(Time = floor(Time/365),
         agecl = as.numeric(agecl))%>%
  group_by(Time,Code,agecl)%>%
  summarise(Biomass = mean(Biomass,na.rm=T))%>%
  filter(Time == t1)%>%
  left_join(catch.age.prop)%>%
  mutate(isCaught = ifelse(catch.prop == 0,0,1),
         Biomass.exploitable = isCaught * Biomass)%>%
  group_by(Code)%>%
  mutate(Biomass.total = sum(Biomass,na.rm=T))%>%
  summarise(Biomass.exploitable = sum(Biomass.exploitable,na.rm=T),
            Biomass.total = mean(Biomass.total,na.rm=T))%>%
  left_join(isFished)%>%
  mutate(invert.fished = isFished == 1 & Biomass.exploitable == 0,
         Biomass.exploitable.prop = (Biomass.exploitable/Biomass.total)+invert.fished)

saveRDS(exploitable.age,paste0(data.dir,'exploitable_age_',experiment.id,'.rds'))
