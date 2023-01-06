# location of pdf
library(tabulizer)
library(tidyverse)
options(warn = F)

#### Extract data and calculate biomass for ASFMC SUmmer Flounder assessment ####

MEN.file = here::here('data-raw','/Menhaden_ASFMC_2020.pdf')

#biomass-at-age

# biomass.age.areas.1 = locate_areas(MEN.file,pages = 201) #(1000 mt)
# biomass.age.areas.2 = locate_areas(MEN.file,pages = 202)

biomass.age.areas.1 = c(77,88,721,490)
biomass.age.areas.2 = c(82,95,469,467)

biomass.age.raw.1 = extract_tables(MEN.file,pages = 201,output = 'data.frame')[[1]]
biomass.age.raw.2 = extract_tables(MEN.file,area = list(biomass.age.areas.2),guess = F,pages = 202,output = 'data.frame')[[1]]
biomass.age.raw.2 = biomass.age.raw.2[-1,]
biomass.age.raw.2 = biomass.age.raw.2 %>% mutate_if(is.character,as.numeric)

colnames(biomass.age.raw.1) = c('year',paste0('age.',1:6)) #age 6+ is the final age
colnames(biomass.age.raw.2) = c('year',paste0('age.',1:6))

biomass.age = bind_rows(biomass.age.raw.1,biomass.age.raw.2) %>%
  gather('variable','biomass',-year)%>%
  separate(variable,c('dum','agecl'))%>%
  mutate(biomass = as.numeric(biomass),
         year = as.numeric(year))%>%
  select(-dum)

#calculate biomass estimate convert to mT
biomass = biomass.age%>%
  group_by(year)%>%
  summarise(biomass = sum(biomass)*1000)

saveRDS(biomass,here::here('data-raw','MEN_ASFMC_biomass.rds'))
