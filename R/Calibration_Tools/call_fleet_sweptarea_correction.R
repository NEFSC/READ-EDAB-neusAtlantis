#Script to correct fleet q's based on reference data

library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleet_calibration_8_revesc'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

source(here::here('R','Calibration_Tools','edit_param_fleet.R'))

fleets = read.csv(here::here('currentVersion','neus_fisheries.csv'))%>%
  select(Code,Index)%>%
  rename(fleet = 'Code')%>%
  mutate(Index = Index+1)

area.corr.sa = read.csv(here::here('Setup_Files',paste0(run.name,'_sweptarea_corrections.csv'))) %>%
  filter(!(fleet %in% c('catchall','SCAcapemay','SCAnewbedford','SCAnewportnews','SCAother')))%>%
  left_join(fleets)
  
area.corr.q = read.csv(here::here('Setup_Files',paste0(run.name,'_q_corrections.csv'))) %>%
  filter(!(fleet %in% c('catchall','SCAcapemay','SCAnewbedford','SCAnewportnews','SCAother')))%>%
  left_join(fleets)%>%
  filter(is.finite(corr.mean)) %>%
  group_by(fleet)%>%
  summarise(sweptarea.corr = quantile(corr.mean,0.9,na.rm=T))

file.copy(here::here('currentVersion','at_harvest.prm'),here::here('currentVersion','at_harvest_8_sacorr.prm'))
file.copy(here::here('currentVersion','at_harvest.prm'),here::here('currentVersion','at_harvest_8_qcorr.prm'))

for(i in 1:nrow(area.corr.sa)){
  
  edit_param_fleet(harvest.file = here::here('currentVersion','at_harvest_8_sacorr.prm'),
                   VarName = 'sweptarea',
                   Fleet = area.corr.sa$fleet[i],
                   Unit = 'scalar',
                   Value = area.corr.sa$sweptarea.corr[i],
                   overwrite = T
  )
}

for(i in 1:nrow(area.corr.q)){
  
  edit_param_fleet(harvest.file = here::here('currentVersion','at_harvest_8_qcorr.prm'),
                   VarName = 'sweptarea',
                   Fleet = area.corr.q$fleet[i],
                   Unit = 'scalar',
                   Value = area.corr.q$sweptarea.corr[i],
                   overwrite = T
  )
}
