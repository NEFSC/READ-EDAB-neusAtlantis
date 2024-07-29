#Script to rescale the spatial distributions for groundfish and scallopsa in
#A) Initial conditions
#B) Seasonal movement parameters
library(dplyr)

source(here::here('R','Calibration_Tools','edit_param_init_dist.R'))
survdat.ref = readRDS(here::here('data','spatial_reference_survdat_2011_2021.rds'))       

groundfish.spp =c('RED','PLA','COD','HAL','HAD','OPT','POL','WHK','WPF','WIF','WTF','WOL','YTF')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

ref.years = 2011:2021
#Do Initial Conditions
i=1
for(i in 1:length(groundfish.spp)){
  
  spp.ref = survdat.ref %>%
    filter(species == fgs$LongName[which(fgs$Code == groundfish.spp[i])] & statistic == 'proportion' & var.name == 'biomass')%>%
    arrange(polygon)
  
  edit_param_init_dist(init.file = here::here('currentVersion','neus_init.nc'),
                       fgs.file = here::here('currentVersion','neus_groups.csv'),
                       group.name = groundfish.spp[i],
                       new.props = spp.ref$ref.value,
                       overwrite =T,
                       bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
                      )  
  
}

#Seasonal Movement
source(here::here('R','Calibration_Tools','edit_param_seasonal_movement.R'))
bboxes = atlantistools::get_boundary(load_box(here::here('Geometry','neus_tmerc_RM2.bgm')))

blank.prop = rep(0,30)
boxes = 0:29

survey = readRDS(here::here('data','sweptAreaBiomassNEUSBoxSpringandFall.RDS'))%>%
  filter(Code %in% groundfish.spp & YEAR %in% ref.years & variable == 'tot.biomass')%>%
  arrange(Code,YEAR,season,box)%>%
  mutate(value = ifelse(box %in% bboxes,0,value))

s=1
for(s in 1:length(groundfish.spp)){
  survey.spring = survey %>%
    filter(season == 'SPRING' & Code == groundfish.spp[s])%>%
    mutate(box = as.numeric(box))%>%
    group_by(box)%>%
    summarise(biomass = mean(value,na.rm=T))%>%
    arrange(box)
  
  survey.fall = survey %>%
    filter(season == 'FALL' & Code == groundfish.spp[s])%>%
    mutate(box = as.numeric(box))%>%
    group_by(box)%>%
    summarise(biomass = mean(value,na.rm=T))%>%
    arrange(box)
  
  spring.tot = sum(survey.spring$biomass,na.rm=T)
  fall.tot = sum(survey.fall$biomass,na.rm=T)
  
  survey.spring$biomass.prop = survey.spring$biomass/spring.tot
  survey.fall$biomass.prop = survey.fall$biomass/fall.tot
  
  spring.prop = fall.prop = blank.prop
  
  #calculate spring vectors
  spring.prop[match(survey.spring$box,boxes)] = round(survey.spring$biomass.prop,3)
  spring.corr = 1-sum(spring.prop)
  spring.prop[which.max(spring.prop)] = spring.prop[which.max(spring.prop)]+spring.corr
  # sum(spring.prop)

  #calculate fall vectors
  fall.prop[match(survey.fall$box,boxes)] = round(survey.fall$biomass.prop,3)
  fall.corr = 1-sum(fall.prop)
  fall.prop[which.max(fall.prop)] = fall.prop[which.max(fall.prop)]+fall.corr
  # sum(fall.prop)
  
  #calculate interpolated summer/winter vector
  inter.prop = round((spring.prop+spring.prop)/2,3)
  inter.corr = 1-sum(inter.prop)
  inter.prop[which.max(inter.prop)] = inter.prop[which.max(inter.prop)]+inter.corr
  # sum(inter.prop)
  
  #Do winter
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = inter.prop,
    Code = groundfish.spp[s],
    phase = 'adult',
    season = 1,
    overwrite =T,
    rescale =T
  )
  
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = inter.prop,
    Code = groundfish.spp[s],
    phase = 'juv',
    season = 1,
    overwrite =T,
    rescale =T
  )
  
  #Do Spring
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = spring.prop,
    Code = groundfish.spp[s],
    phase = 'adult',
    season = 2,
    overwrite =T,
    rescale =T
  )
  
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = spring.prop,
    Code = groundfish.spp[s],
    phase = 'juv',
    season = 2,
    overwrite =T,
    rescale =T
  )
  
  #Do Summer
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = inter.prop,
    Code = groundfish.spp[s],
    phase = 'adult',
    season = 3,
    overwrite =T,
    rescale =T
  )
  
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = inter.prop,
    Code = groundfish.spp[s],
    phase = 'juv',
    season = 3,
    overwrite =T,
    rescale =T
  )
  
  #Do Fall
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = fall.prop,
    Code = groundfish.spp[s],
    phase = 'adult',
    season = 4,
    overwrite =T,
    rescale =T
  )
  
  edit_param_seasonal_movement(
    bio.file = here::here('currentVersion','at_biology.prm'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    move.val = fall.prop,
    Code = groundfish.spp[s],
    phase = 'juv',
    season = 4,
    overwrite =T,
    rescale =T
  )
  
  
}
