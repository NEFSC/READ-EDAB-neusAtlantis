#Script to generate mum-at-age scalar to correct length-distribution issues
#Determine if classes are within +-20% of expected values
#If not determine an age-based scalar to mum/C
#Apply scalar to biology.prm
library(dplyr)

##Scale mum and/or C based on difference from length-at-age reference
scale.mum = F
scale.C = F

##

source(here::here('R','edit_param_mum_age.R'))
source(here::here('R','edit_param_C_age.R'))

run.name = 'all_recruit12_6'
run.dir = here::here('Atlantis_Runs',run.name,'')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%select(Code,LongName)%>%rename(species = 'LongName')

#The threshold for flagging around reference values
ref.length.bound = 0.2

#Read in reference values for length.age and determine upper and lower bounds
age.key = data.frame(variable = paste0('X',1:10),agecl = 1:10)

length.age.ref = read.csv(here::here('currentVersion','vertebrate_init_length_cm_Adjusted.csv')) %>%
  select(Code,agecl,new.length.ref)%>%
  mutate(length.ref.min = new.length.ref*(1-ref.length.bound),
         length.ref.max = new.length.ref*(1+ref.length.bound))%>%
  left_join(fgs)

#Read in post-processed length.age data
#Determine whether output values fall within bounds of reference values
length.age = readRDS(paste0(run.dir,'Post_Processed/Data/length_age.rds'))%>%
  group_by(species,agecl)%>%
  summarise(length = mean(atoutput,na.rm=T))%>%
  left_join(length.age.ref)%>%
  mutate(length.high = ifelse(length > length.ref.max,1,0),
         length.low = ifelse(length < length.ref.min,1,0),
         length.scalar = ifelse((length.high == 0&length.low == 0)|is.na(length.high),1,new.length.ref/length))
  
length.age.check = length.age %>%
  group_by(Code)%>%
  summarise(length.high = sum(length.high,na.rm=T),
            length.low = sum(length.low,na.rm=T))
            
#Pull mum-age and C-age data
current.mum = get_param_mum_age(bio.prm = here::here('currentVersion','at_biology.prm'),write.output = F) %>% as.data.frame()
current.mum[,2:11] = apply(current.mum[,2:11],2,as.numeric)
current.C = get_param_C_age(bio.prm = here::here('currentVersion','at_biology.prm'),write.output = F) %>% as.data.frame()
current.C[,2:11] = apply(current.C[,2:11],2,as.numeric)

length.age.scalars.long = length.age %>%
  ungroup()%>%
  left_join(fgs)%>%
  select(Code,agecl,length.scalar)

write.csv(length.age.scalars.long, paste0(run.dir,'Length_Scalar.csv'),row.names = F)

length.age.scalars = length.age.scalars.long %>%
  tidyr::spread(agecl,length.scalar)

length.age.scalars = length.age.scalars[match(current.mum$group,length.age.scalars$Code),]


if(scale.mum == T){
  new.mum = current.mum
  new.mum[,2:11] = signif(current.mum[,2:11] * length.age.scalars[,2:11],3)
  edit_param_mum_age(bio.prm = here::here('currentVersion','at_biology.prm'),
                     new.mum = new.mum,
                     overwrite = T)
}

if(scale.C == T){
  new.C = current.C
  new.C[,2:11] = signif(current.C[,2:11] * length.age.scalars[,2:11] ,3)
  edit_param_C_age(bio.prm = here::here('currentVersion','at_biology.prm'),
                     new.C = new.C,
                     overwrite = T)
}


