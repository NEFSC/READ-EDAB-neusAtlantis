#Script to rescale the spatial distributions for groundfish and scallopsa in
#A) Initial conditions
#B) Seasonal movement parameters
library(dplyr)

source(here::here('R','Calibration_Tools','edit_param_init_dist.R'))
survdat.ref = readRDS(here::here('data','spatial_reference_survdat_2011_2021.rds'))       

groundfish.spp =c('RED','PLA','COD','HAL','HAD','OPT','POL','WHK','WPF','WIF','WTF','WOL','YTF')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

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
