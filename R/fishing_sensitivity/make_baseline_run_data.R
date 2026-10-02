library(dplyr)

source(here::here('R','edit_param_FSPB.R'))

ref.run.dir = 'C:/Users/Joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_Run/fishing_sensitivity_baseline/'
start.time = 365*20
stop.time = 365*57

fgs.file = here::here('currentVersion','neus_groups.csv')
fgs = read.csv(fgs.file,as.is =T) %>%select(Code,LongName,IsTurnedOn)

data.dir = paste0(ref.run.dir,'/Post_Processed/Data/')

fspb = get_param_FSPB(here::here('currentVersion', 'at_biology.prm'))%>%
  tidyr::gather('dum','fspb',-group)%>%
  rename(Code = 'group')%>%
  tidyr::separate(dum, c('dum','agecl'))%>%
  filter(!is.na(agecl))%>%
  mutate(agecl = as.numeric(agecl),
         fspb = as.numeric(fspb))

#Read in reference biomass and numbers data from post-processed RDS files
x = read.table(paste0(ref.run.dir,'neus_outputBiomIndx.txt'),header = T)
biomass.ref = readRDS(paste0(ref.run.dir,'/Post_Processed/Data/biomass.rds'))%>%
  filter(time >= (start.time/365) & time <= (stop.time/365))%>%
  group_by(species)%>%
  summarise(atoutput = mean(atoutput,na.rm=T))%>%
  rename(biomass.ref = 'atoutput')


biomass.age.ref = readRDS(paste0(ref.run.dir,'/Post_Processed/Data/biomass_age.rds'))%>%
  # filter(time >= (start.time/365) & time <= (stop.time/365))%>%
  left_join(fspb)%>%
  mutate(ssb= fspb*atoutput)%>%
  group_by(time,species,agecl)%>%
  summarise(biomass = mean(atoutput,na.rm=T),
            ssb.age = mean(ssb,na.rm=T))

ssb.tot = biomass.age.ref %>%
  mutate(year = floor(time))%>%
  group_by(year,time,species)%>%
  summarise(ssb = sum(ssb.age))
  
  

ssb.ref = biomass.age.ref %>%
  group_by(species)%>%
  summarise(ssb = sum(ssb.age,na.rm=T))
  
age.prop.ref = biomass.age.ref %>%
  group_by(species)%>%
  mutate(biomass.total= sum(biomass))%>%
  ungroup()%>%
  mutate(age.prop = biomass/biomass.total,
         age.wgt = agecl * age.prop)%>%
  group_by(species)%>%
  summarise(biomass.age.mean = sum(age.wgt))
 
number.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/numbers.rds')) %>%
  filter(time >= (start.time/365) & time <= (stop.time/365))%>%
  group_by(species)%>%
  summarise(atoutput = mean(atoutput,na.rm=T))%>%
  rename(number.ref = 'atoutput')

catch.year = read.table(paste0(ref.run.dir,'/neus_outputCatch.txt'),header = T)%>%
  select(Time:ZG)%>%
  tidyr::gather('Code','catch.ref',-Time)%>%
  mutate(year = floor(Time/365))%>%
  group_by(Code,year)%>%
  summarise(Catch = sum(catch.ref,na.rm=T))%>%
  left_join(fgs)%>%
  select(-IsTurnedOn)%>%
  rename(species = 'LongName')
  
catch.ref = read.table(paste0(ref.run.dir,'/neus_outputCatch.txt'),header = T)%>%
  select(Time:ZG)%>%
  filter(Time >= start.time & Time <= stop.time)%>%
  tidyr::gather('Code','catch.ref',-Time)%>%
  group_by(Code)%>%
  summarise(catch.ref= mean(catch.ref,na.rm=T))%>%
  left_join(fgs)%>%
  rename(species = 'LongName')

source(here::here('R','edit_param_BH.R'))
bh.param = get_param_BH(here::here('currentVersion','at_biology.prm'))%>%
  mutate(alpha = as.numeric(as.character(alpha)),
         beta = as.numeric(as.character(beta)))%>%
  left_join(fgs, by = c('group' = 'Code'))%>%
  rename(Code = 'group',
         species = 'LongName')

recruit.ref = read.table(paste0(ref.run.dir,'/neus_outputYOY.txt'),header =T)%>%
  filter(Time >= start.time & Time <= stop.time)%>%
  tidyr::gather('ID','recruit.ref',-Time)%>%
  tidyr::separate('ID',c('Code','agecl'),sep = '\\.')%>%
  group_by(Code)%>%
  summarise(recruit.ref = mean(recruit.ref,na.rm=T))%>%
  left_join(fgs)%>%
  rename(species = 'LongName')%>%
  select(-IsTurnedOn)

data.ref = biomass.ref %>% 
  left_join(number.ref) %>%
  left_join(catch.ref)%>%
  select(-IsTurnedOn)%>%
  left_join(recruit.ref)%>%
  left_join(bh.param)%>%
  select(-IsTurnedOn)%>%
  left_join(age.prop.ref)%>%
  left_join(ssb.ref)%>%
  left_join(fgs, by = c('species' = 'LongName','Code' = 'Code')) %>%
  # select(-species) %>%
  mutate(exploit.prop = catch.ref/ssb,
         start.time = start.time,
         stop.time = stop.time)%>%
  ungroup()%>%
  arrange(species,Code,start.time,stop.time,biomass.ref,ssb,number.ref,catch.ref,recruit.ref,exploit.prop,biomass.age.mean,alpha,beta)

saveRDS(data.ref,file = paste0(data.dir,'ref_run_summary_',start.time,'_',stop.time,'.rds'))

out2.df = ssb.tot %>%
  left_join(catch.year)%>%
  select(Code,species,year,Catch,ssb)
saveRDS(out2.df,paste0(data.dir,'ref_run_SSB_catch.rds'))
