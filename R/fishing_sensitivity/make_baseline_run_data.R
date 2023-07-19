library(dplyr)

ref.run.dir = 'C:/Users/Joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_Run/fishing_sensitivity_baseline/'
ref.time = 20805

fgs.file = here::here('currentVersion','neus_groups.csv')
fgs = read.csv(fgs.file,as.is =T) %>%select(Code,LongName,IsTurnedOn)

data.dir = paste0(ref.run.dir,'/Post_Processed/Data/')

#Read in reference biomass and numbers data from post-processed RDS files
x = read.table(paste0(ref.run.dir,'neus_outputBiomIndx.txt'),header = T)
biomass.ref = readRDS(paste0(ref.run.dir,'/Post_Processed/Data/biomass.rds'))%>%
  filter(time >= (ref.time/365))%>%
  group_by(species)%>%
  summarise(atoutput = mean(atoutput,na.rm=T))%>%
  rename(biomass.ref = 'atoutput')

biomass.age.ref = readRDS(paste0(ref.run.dir,'/Post_Processed/Data/biomass_age.rds'))%>%
  filter(time >= ref.time/365)%>%
  group_by(species,agecl)%>%
  summarise(biomass = mean(atoutput,na.rm=T))%>%
  group_by(species)%>%
  mutate(biomass.total= sum(biomass))%>%
  ungroup()%>%
  mutate(age.prop = biomass/biomass.total,
         age.wgt = agecl * age.prop)%>%
  group_by(species)%>%
  summarise(biomass.age.mean = sum(age.wgt))
 
number.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/numbers.rds')) %>%
  filter(time >= (ref.time/365))%>%
  group_by(species)%>%
  summarise(atoutput = mean(atoutput,na.rm=T))%>%
  rename(number.ref = 'atoutput')

catch.ref = read.table(paste0(ref.run.dir,'/neus_outputCatch.txt'),header = T)%>%
  select(Time:ZG)%>%
  filter(Time >= ref.time & Time <=ref.time+365)%>%
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
  filter(Time >= ref.time & Time <= ref.time + 365)%>%
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
  left_join(recruit.ref)%>%
  left_join(bh.param)%>%
  left_join(biomass.age.ref)%>%
  left_join(fgs, by = c('species' = 'LongName','Code' = 'Code')) %>%
  select(-species) %>%
  mutate(exploit.prop = catch.ref/biomass.ref)%>%
  ungroup()

saveRDS(data.ref,file = paste0(data.dir,'ref_run_summary.rds'))
