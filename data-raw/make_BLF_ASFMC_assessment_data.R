# location of pdf
library(tabulizer)
library(tidyverse)
options(warn = F)

#### Extract data and calculate biomass for ASFMC SUmmer Flounder assessment ####

BLF.file = here::here('data-raw','/Bluefish_ASMFC_2015.pdf')

#numbers-at-age

# number.age.areas = locate_areas(BLF.file,pages = 158) #(000s)
number.age.areas = c(88,34,615,485)

number.age.raw = extract_tables(BLF.file,pages = 158,output = 'data.frame')[[1]]
number.age.raw = number.age.raw[-c(1,2),]
for(i in 1:ncol(number.age.raw)){ number.age.raw[,i] = as.numeric(str_replace_all(number.age.raw[,i],',',''))}
colnames(number.age.raw) = c('year',paste0('age',0:6),'Total') #Age 6+ is final age

number.age = number.age.raw %>%
  select(-Total)%>%
  gather('variable','numbers',-year)%>%
  separate(variable,c('dum','agecl'),sep='age')%>%
  mutate(numbers = as.numeric(numbers),
         year = as.numeric(year),
         agecl = as.numeric(agecl))%>%
  select(-dum)

# Weight-at-age (kg)

weight.age.areas = locate_areas(BLF.file,pages = 133)

weight.age.areas = c(110,29,601,307)

weight.age.raw = extract_tables(BLF.file,pages = 133,output = 'data.frame')[[1]]

weight.age.raw = weight.age.raw[-1,]

colnames(weight.age.raw) = c('year', paste0('age.',0:6))

weight.age = weight.age.raw %>%
  gather(variable,weight.age,-year)%>%
  separate(variable,c('dum','agecl'))%>%
  mutate(year = as.numeric(year),
         agecl= as.numeric(agecl))%>%
  select(-dum)
  
#calculate biomass estimate
biomass = number.age%>%
  mutate(numbers = numbers*1000)%>%
  left_join(weight.age)%>%
  mutate(biomass.mt = numbers*weight.age/1000)%>%
  group_by(year)%>%
  summarise(biomass = sum(biomass.mt))

saveRDS(biomass,here::here('data-raw','BLF_ASFMC_biomass.rds'))
