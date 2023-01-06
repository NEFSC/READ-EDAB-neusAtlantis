# location of pdf
library(tabulizer)
library(tidyverse)
options(warn = F)

#### Extract data and calculate biomass for ASFMC SUmmer Flounder assessment ####

STB.file = here::here('data-raw','/StripedBass_ASFMC_2022.pdf')

#biomass-at-age

biomass.age.areas = locate_areas(STB.file,pages = 16) #(mt)

biomass.age.areas = c(89,75,477,587)

biomass.age.raw = extract_tables(STB.file,pages = 16,output = 'data.frame')[[1]]
for(i in 1:ncol(biomass.age.raw)){ biomass.age.raw[,i] = str_replace_all(biomass.age.raw[,i],',','')}

colnames(biomass.age.raw) = c('year',paste0('age.',1:15),'Total') #age 15+ is the final age

biomass = biomass.age.raw %>%
  select(year,Total)%>%
  rename(biomass = 'Total')
  
saveRDS(biomass,here::here('data-raw','STB_ASFMC_biomass.rds'))
