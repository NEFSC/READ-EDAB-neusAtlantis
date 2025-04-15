#Script to correct fleet q's based on reference data

library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleet_calibration_4_q'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

source(here::here('R','Calibration_Tools','edit_param_q.R'))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%
  select(Code,LongName)%>%
  rename(species = 'LongName')
fleets = read.csv(here::here('currentVersion','neus_fisheries.csv'))%>%
  select(Code,Index)%>%
  rename(fleet = 'Code')%>%
  mutate(Index = Index+1)

q.corr = read.csv(paste0(figure.dir,'data/fleet_calibration_4_q_corrections.csv')) %>%
  filter(!(fleet %in% c('catchall','SCAcapemay','SCAnewbedford','SCAnewportnews')))%>%
  left_join(fgs)%>%
  left_join(fleets)
  
for(i in 1:nrow(q.corr)){
  
  edit_param_q(harvest.file = here::here('currentVersion','at_harvest.prm'),
               Code = q.corr$Code[i],
               Fleet = q.corr$fleet[i],
               fleets.file = here::here('currentVersion','neus_fisheries.csv'),
               Value = q.corr$corr.mean[i],
               overwrite = T
              )
}
