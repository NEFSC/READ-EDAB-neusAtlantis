#Catch spike analysis 
# 1) Recovery time 
# 2) Maximum deviation
library(dplyr)
library(ggplot2)

experiment.id = 'fspike1'

data.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/',experiment.id,'/')

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T)
master.dat = master.dat[which(master.dat$experiment_id == experiment.id),]

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

biomass.baseline = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_run/fishing_sensitivity_baseline/neus_outputBiomIndx.txt',header = T)%>%
  select(Time, all_of(fgs$Code))%>%
  mutate(Time = floor(Time/365))%>%
  tidyr::gather('Code','Biomass',-Time)%>%
  group_by(Code,Time)%>%
  summarise(Biomass = mean(Biomass,na.rm=T))%>%
  mutate(scalar = 1,
         run.id = 'baseline')

biomass = readRDS(paste0(data.dir,'BiomIndx_fspike1_1_28105_year.rds'))%>%
  left_join(setup.df)%>%
  select(Time,Code,Biomass,scalar,run.id)%>%
  bind_rows(biomass.baseline)

spp.names = sort(unique(setup.df$target.species))

#' Measurements at 6 critical time periods
#'t1: Biomass at spike start
#'t2: Biomass at spike end
#'t3: Minimum biomass after spike
#'t4: Biomass at proj t+5
#'t5: Biomass at proj t+10
#'t6: biomass at proj t+20
#'

t1 = master.dat$event_start_d/365
t2 = master.dat$event_end_d/365
t4 = t1 + 5
t5 = t1 + 10
t6 = t1 + 19

#Create empty dataframes for stats
bio.base.stats = data.frame(Code = spp.names,  b0.t1 = NA, b0.t2 = NA, b0.t4 = NA , b0.t5 = NA, b0.t6 = NA,stringsAsFactors = F)
bio.run.stats =  data.frame(Code = setup.df$target.species, scalar= setup.df$scalar, b.t1 = NA, b.t2 = NA, t3 = NA, b.t3 = NA, b.t4 = NA , b.t5 = NA, b.t6 = NA,stringsAsFactors = F)

i=1
#Calculate biomass at key time points for baseline biomass
for(i in 1:length(spp.names)){
  
  bio.base.spp = biomass.baseline %>%
    filter(Code == spp.names[i])
  
  if(nrow(bio.base.spp)== 0){next()}
  
  bio.base.stats$b0.t1[i] = bio.base.spp$Biomass[which(bio.base.spp$Time == t1)]
  bio.base.stats$b0.t2[i] = bio.base.spp$Biomass[which(bio.base.spp$Time == t2)]
  bio.base.stats$b0.t4[i] = bio.base.spp$Biomass[which(bio.base.spp$Time == t4)]
  bio.base.stats$b0.t5[i] = bio.base.spp$Biomass[which(bio.base.spp$Time == t5)]
  bio.base.stats$b0.t6[i] = bio.base.spp$Biomass[which(bio.base.spp$Time == t6)]
  
  bio.base.spp = bio.base.spp %>% 
    filter(Time > t2)
  
  # bio.base.stats$t3[i] = bio.base.spp$Time[which.min(bio.base.spp$Biomass)]
  # bio.base.stats$b0.t3[i] = bio.base.spp$Biomass[which.min(bio.base.spp$Biomass)]
  
}
#calculate biomass at key time points for scenario runs
for(i in 1:nrow(setup.df)){
  
  bio.spp = biomass %>%
    filter(Code == setup.df$target.species[i] & scalar == setup.df$scalar[i] )
  
  if(nrow(bio.spp)== 0){next()}
  
  bio.run.stats$b.t1[i] = bio.spp$Biomass[which(bio.spp$Time == t1)]
  bio.run.stats$b.t2[i] = bio.spp$Biomass[which(bio.spp$Time == t2)]
  bio.run.stats$b.t4[i] = bio.spp$Biomass[which(bio.spp$Time == t4)]
  bio.run.stats$b.t5[i] = bio.spp$Biomass[which(bio.spp$Time == t5)]
  bio.run.stats$b.t6[i] = bio.spp$Biomass[which(bio.spp$Time == t6)]
  
  bio.spp = bio.spp %>% 
    filter(Time > t2)
  
  bio.run.stats$t3[i] = bio.spp$Time[which.min(bio.spp$Biomass)]
  bio.run.stats$b.t3[i] = bio.spp$Biomass[which.min(bio.spp$Biomass)]
  
}


bio.recovery = bio.run.stats %>% 
  left_join(bio.base.stats)%>%
  mutate(db.t2 = b.t2/b0.t2,
         db.t4 = b.t4/b0.t4,
         db.t5 = b.t5/b0.t5,
         db.t6 = b.t6/b0.t6,
         recovery.5 = (db.t4-db.t2)/(t4-t2),
         recovery.10 = (db.t5-db.t2)/(t5-t2),
         recovery.20 = (db.t6-db.t2)/(t6-t2))

saveRDS(bio.recovery,paste0(data.dir,'recovery_stats_', experiment.id,'.rds'))
