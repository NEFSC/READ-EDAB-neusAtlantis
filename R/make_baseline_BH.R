#Script to set BH alpha/beta baseline for groups with flagrecruit = 1

library(dplyr)

source(here::here('R','get_recruit_type.R'))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% select(Code,LongName,NumAgeClassSize,GroupType)

recruit.type = get_recruit_type(here::here('currentVersion','at_biology.prm'))%>%
  filter(flagrecruit == 1)%>%
  left_join(fgs,by = c('group' = 'Code'))

ref.bio.file = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230213/Post_Processed/Data/biomass_age.rds'

ref.bio = readRDS(ref.bio.file) %>%
  left_join(fgs, by = c('species' = 'LongName'))%>%
  filter(species %in% recruit.type$LongName & time == 0& GroupType == 'FISH')%>%
  mutate(biomass.mgN = atoutput / 20 / 5.7 * 1E9)
         
ref.alpha = ref.bio %>%
  filter(agecl == 1)%>%
  mutate(alpha = biomass.mgN/NumAgeClassSize)%>%
  select(Code,alpha)

#get maturity at age
mat.age = read.csv(here::here('R','group_mature_age.csv'),as.is = T)

ref.beta = ref.bio %>%
  left_join(mat.age, by = c('Code' = 'spp'))%>%
  mutate(is.mature = agecl >= age.mat)%>%
  filter(is.mature)%>%
  group_by(Code)%>%
  summarise(beta = sum(biomass.mgN))

combined = left_join(ref.alpha,ref.beta)

saveRDS(combined,here::here('data','reference_BH_params.rds'))
