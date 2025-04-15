#Script to correct fleet q's based on reference data

library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'gffleets_minsize_ref'
run.dir = here::here('Atlantis_Runs',run.name)
figure.dir = paste0(run.dir,'/Post_Processed/')

source(here::here('R','Calibration_Tools','edit_param_fleet.R'))

fleets = read.csv(here::here('currentVersion','neus_fisheries.csv'))%>%
  select(Code,Index)%>%
  rename(fleet = 'Code')%>%
  mutate(Index = Index+1)

# area.corr = read.csv(paste0(figure.dir,'data/fleet_calibration_4_sweptarea_corrections.csv')) %>%
area.corr = read.csv(here::here('Setup_Files',paste0(run.name,'_q_corrections.csv'))) %>%
  filter(!(fleet %in% c('catchall','SCAcapemay','SCAnewbedford','SCAnewportnews','SCAother')))%>%
  left_join(fleets)%>%
  group_by(fleet)%>%
  summarise(sweptarea.corr = max(corr.mean,na.rm=T))

for(i in 1:nrow(area.corr)){
  
  edit_param_fleet(harvest.file = here::here('currentVersion','at_harvest.prm'),
                   VarName = 'sweptarea',
                   Fleet = area.corr$fleet[i],
                   Unit = 'scalar',
                   Value = area.corr$sweptarea.corr[i],
                   overwrite = T
  )
}
