#Script to generate starting values for minimum sizes per species
library(dplyr)

source(here::here('R','Calibration_Tools','edit_param_discard.R'))

catch.ref = readRDS(here::here('data-raw','gfTripLengths.rds'))

catch.min.l = catch.ref %>%
  group_by(Code)%>%
  summarise(min.l = min(LENGTH,na.rm=T))

fisheries.file = here::here('currentVersion','neus_fisheries.csv')

fisheries = read.csv(fisheries.file)  

fleetCodes = grep('gf*',fisheries$Code,value =T)

sppCodes =  catch.min.l$Code

#Set all groundfish fleets to discard type = 2

edit_param_discard(harvest.prm = here::here('currentVersion','at_harvest.prm'), 
                   fisheries.file = fisheries.file,
                   fgs.file = here::here('currentVersion','neus_groups.csv'),
                   sppCodes = sppCodes,
                   fleetCodes = fleetCodes,
                   Type = 'flagdiscard',
                   Value = 2,
                   overwrite =T)

for( s in 1:nrow(catch.min.l)){
  edit_param_discard(harvest.prm = here::here('currentVersion','at_harvest.prm'), 
                     fisheries.file = fisheries.file,
                     fgs.file = here::here('currentVersion','neus_groups.csv'),
                     sppCodes = catch.min.l$Code[s],
                     fleetCodes = fleetCodes,
                     Type = 'FCthreshli',
                     Value = catch.min.l$min.l[s],
                     overwrite =T)
}

