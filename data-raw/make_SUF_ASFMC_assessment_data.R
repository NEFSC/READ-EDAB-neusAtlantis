# location of pdf
library(tabulizer)
library(tidyverse)
options(warn = F)

#### Extract data and calculate biomass for ASFMC SUmmer Flounder assessment ####

SUF.file = here::here('data-raw','/SummerFlounder_ASFMC_2019.pdf')

#numbers-at-age

# number.age.areas = locate_areas(SUF.file,pages = 243) (000s)
number.age.areas = c(95,37,706,620)
number.age.raw = extract_tables(SUF.file,pages = 243,output = 'matrix')[[1]]
for(i in 1:ncol(number.age.raw)){ number.age.raw[,i] = str_replace_all(number.age.raw[,i],',','')}
number.age.raw = data.frame(number.age.raw,stringsAsFactors = F)
colnames(number.age.raw) = c('year',paste0('age',0:7),'Total') #Age 7+ is final age

number.age = number.age.raw %>%
  select(-Total)%>%
  gather('variable','numbers',-year)%>%
  separate(variable,c('dum','agecl'),sep='age')%>%
  mutate(numbers = as.numeric(numbers),
         year = as.numeric(year))%>%
  select(-dum)

# Weight-at-age (kg)
# weight.age.comm.land.areas.1 = locate_areas(SUF.file,pages = 176)
# weight.age.comm.land.areas.2 = locate_areas(SUF.file,pages = 177)

# weight.age.comm.disc.areas.1 = locate_areas(SUF.file,pages = 142)
# weight.age.comm.disc.areas.2 = locate_areas(SUF.file,pages = 143)

# weight.age.rec.land.areas.1 = locate_areas(SUF.file,pages = 161)
# weight.age.rec.land.areas.2 = locate_areas(SUF.file,pages = 162)

# weight.age.rec.disc.areas.1 = locate_areas(SUF.file,pages = 168)
# weight.age.rec.disc.areas.2 = locate_areas(SUF.file,pages = 169)

weight.age.comm.land.areas.1 = c(92,70,490,708)
weight.age.comm.land.areas.2 = c(92,78,230,714)

weight.age.comm.disc.areas.1 = c(98,151,417,654)
weight.age.comm.disc.areas.2 = c(108,151,250,661)

weight.age.rec.land.areas.1 = c(93,106,476,674)
weight.age.rec.land.areas.2 = c(92,108,227,684)

weight.age.rec.disc.areas.1 = c(93,94,512,681)
weight.age.rec.disc.areas.2 = c(88,89,247,723)

weight.age.comm.land.raw.1 = extract_tables(SUF.file,pages = 176,output = 'data.frame')[[1]]
weight.age.comm.land.raw.2 = extract_tables(SUF.file,pages = 177,output = 'data.frame')[[1]]

weight.age.comm.disc.raw.1 = extract_tables(SUF.file,pages = 142,output = 'data.frame')[[1]]
weight.age.comm.disc.raw.2 = extract_tables(SUF.file,pages = 143,output = 'data.frame')[[1]]

weight.age.rec.land.raw.1 =  extract_tables(SUF.file,pages = 161,output = 'data.frame')[[1]]
weight.age.rec.land.raw.2 =  extract_tables(SUF.file,pages = 162,output = 'data.frame')[[1]]

weight.age.rec.disc.raw.1 =  extract_tables(SUF.file,pages = 168,output = 'data.frame')[[1]]
weight.age.rec.disc.raw.2 =  extract_tables(SUF.file,pages = 169,output = 'data.frame')[[1]]

colnames(weight.age.comm.land.raw.1) = c('year', paste0('age.',0:10),'total','7+')
colnames(weight.age.comm.land.raw.2) = c('year', paste0('age.',0:10),'total','7+')

colnames(weight.age.comm.disc.raw.1) =c('year', paste0('age.',0:10),'mean')
colnames(weight.age.comm.disc.raw.2) =c('year', paste0('age.',0:10),'mean')

colnames(weight.age.rec.land.raw.1) = c('year', paste0('age.',0:10),'total')
colnames(weight.age.rec.land.raw.2) = c('year', paste0('age.',0:10),'total')

colnames(weight.age.rec.disc.raw.1) =c('year', paste0('age.',0:10),'total')
colnames(weight.age.rec.disc.raw.2) =c('year', paste0('age.',0:10),'total')

weight.age.comm.land = bind_rows(weight.age.comm.land.raw.1,weight.age.comm.land.raw.2) %>%
  select(-total, -'7+')%>%
  gather(variable,weight.age,-year)%>%
  separate(variable,c('dum','agecl'))%>%
  select(-dum)%>%
  mutate(source = 'comm landings')

weight.age.comm.disc = bind_rows(weight.age.comm.disc.raw.1,weight.age.comm.disc.raw.2) %>%
  select(-mean)%>%
  gather(variable,weight.age,-year)%>%
  separate(variable,c('dum','agecl'))%>%
  select(-dum)%>%
  mutate(source = 'comm discards')

weight.age.rec.land = bind_rows(weight.age.rec.land.raw.1,weight.age.rec.land.raw.2)%>%
  select(-total)%>%
  gather(variable,weight.age,-year)%>%
  separate(variable,c('dum','agecl'))%>%
  select(-dum)%>%
  mutate(source = 'rec landings')

weight.age.rec.disc = bind_rows(weight.age.rec.disc.raw.1,weight.age.rec.disc.raw.2)%>%
  select(-total)%>%
  gather(variable,weight.age,-year)%>%
  separate(variable,c('dum','agecl'))%>%
  select(-dum)%>%
  mutate(source = 'rec discards')

weight.age = bind_rows(weight.age.comm.land,weight.age.comm.disc,weight.age.rec.land,weight.age.rec.disc)%>%
  group_by(year,agecl)%>%
  summarise(weight.age = mean(weight.age,na.rm=T))%>%
  ungroup()

#calculate biomass estimate
biomass = number.age%>%
  mutate(numbers = numbers*1000)%>%
  left_join(weight.age)%>%
  mutate(biomass.mt = numbers*weight.age/1000)%>%
  group_by(year)%>%
  summarise(biomass = sum(biomass.mt))

saveRDS(biomass,here::here('data-raw','SUF_ASFMC_biomass.rds'))
