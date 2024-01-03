#Script to compare weight at age for species between reference data and run
#Determine if mum needs to be rescaled
library(ggplot2)
library(dplyr)

#get run info
run.name = '6536_new_age_param_init_rescale_2'
run.dir = here::here('Atlantis_Runs',run.name,'')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>%
  select(Code,LongName,NumCohorts,NumAgeClassSize)
fgs.old = read.csv(here::here('diagnostics','neus_groups_v2_0_1.csv'),as.is = T)%>%
  select(Code,LongName,NumCohorts,NumAgeClassSize)
fgs$spp.change= fgs.old$NumCohorts != fgs$NumCohorts

#Read in reference survey length weigth age: indwt = kg, len = cm
ref.stats.survdat = readRDS(here::here('data','survey_lenagewgt.rds'))%>%
  group_by(Code,AGE)%>%
  summarise(LENGTH = mean(LENGTH,na.rm=T),
            WEIGHT = mean(INDWT,na.rm=T))%>%
  rename(ref.len = 'LENGTH',
         ref.wgt = 'WEIGHT')

ref.stats.fishbase = read.csv(here::here('diagnostics','fishbase_len_wgt_ref.csv'),as.is = T)%>%
  mutate(ref.wgt = Weight_g/1000)%>%
  rename(ref.len = 'Length',
         AGE = 'Age')%>%
  select(Code,AGE,ref.len,ref.wgt)

ref.stats.all = ref.stats.survdat %>%
  bind_rows(ref.stats.fishbase)

ref.spp = unique(ref.stats.all$Code)
i=1
ref.stats.spp.ls = list()
for(i in 1:length(ref.spp)){
  
  fgs.match = which(fgs$Code == ref.spp[i])  
  
  spp.agecl = 1:fgs$NumCohorts[fgs.match]
  spp.age = c(0,(1:fgs$NumCohorts[fgs.match])*fgs$NumAgeClassSize[fgs.match],Inf)
  
  ref.stats.spp = ref.stats.all %>%
    filter(Code == ref.spp[i] & !is.na(AGE))
  
  spp.age.match = cut(ref.stats.spp$AGE+1,spp.age,labels = F,right =F)
  spp.age.match[spp.age.match>fgs$NumCohorts[fgs.match]] = fgs$NumCohorts[fgs.match]
  
  ref.stats.spp$agecl = spp.age.match
  ref.stats.spp.ls[[i]] = ref.stats.spp
}
ref.stats.all2 = bind_rows(ref.stats.spp.ls)


#Get run length weight stats
run.wgt = readRDS(paste0(run.dir,'Post_Processed/Data/max_weight.rds'))%>%
  group_by(species,agecl)%>%
  summarise(maxMeanWeight = max(maxMeanWeight,na.rm=T)/1000)
run.len = readRDS(paste0(run.dir,'Post_Processed/Data/length_age.rds'))%>%
  group_by(species,agecl)%>%
  summarise(length = mean(atoutput,na.rm=T))

run.stats = run.wgt %>% 
  left_join(run.len)%>%
  left_join(fgs,by = c(species = 'LongName'))%>%
  # mutate(AGE = agecl * NumAgeClassSize)%>%
  rename(run.wgt = 'maxMeanWeight',
         run.len = 'length')

#Combine and create scalar based on length and weight
combined.stats = run.stats%>%
  left_join(ref.stats.all2)%>%
  select(Code,species,spp.change,agecl,AGE,run.len,ref.len,run.wgt,ref.wgt)%>%
  mutate(len.scale = ref.len / run.len,
         wgt.scale = ref.wgt / run.wgt)

mean.scale.wgt = combined.stats %>%
  group_by(Code,spp.change,agecl)%>%
  summarise(mum.scale = mean(wgt.scale,na.rm=T))%>%
  # group_by(Code,spp.change)%>%
  # summarise(mum.scale = mean(mum.scale,na.rm=T))%>%
  filter(spp.change == T)
  # filter(spp.change == T & is.na(mum.scale))

source(here::here('R','Calibration_Tools','edit_param_mum_age.R'))
mum.orig = get_param_mum_age(bio.prm = here::here('currentVersion','at_biology_rescale_mumC.prm'))
new.mum = mum.orig

spp.names = sort(unique(mean.scale.wgt$Code))

j = 1
for(j in 1:length(spp.names)){
  
  which.mum = which(mum.orig$group == spp.names[j] )
  
  mum.age.scale = filter(mean.scale.wgt,Code == spp.names[j]) %>% arrange(agecl)
  
  mean.scale = mean(mum.age.scale$mum.scale,na.rm=T)
  
  mum.scale.spp = mum.age.scale$mum.scale
  mum.scale.spp[is.na(mum.scale.spp)] = mean.scale
  
  mum.orig.spp = as.numeric(mum.orig[which.mum,2:ncol(mum.orig)])
  
  new.mum[which.mum,2:ncol(new.mum)] = signif(mum.orig.spp * mum.scale.spp,2)
  
}

edit_param_mum_age(bio.prm = here::here('currentVersion','at_biology_rescale_mumC.prm'),
                   single.group = F,
                   new.mum = new.mum,
                   overwrite = T)
