#Script to correct fleet q's based on reference data

library(dplyr)
library(ggplot2)
library(mapdata)

run.name = 'fleet_calibration_6_qcorr'
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

q.corr = read.csv(here::here('Setup_Files',paste0(run.name,'_q_corrections.csv'))) %>%
  filter(!(fleet %in% c('catchall','SCAcapemay','SCAnewbedford','SCAnewportnews','SCAother')))%>%
  left_join(fgs)%>%
  left_join(fleets)%>%
  mutate(corr.max = ifelse(corr.max>1|!is.finite(corr.max),1,corr.max))
  
for(i in 1:nrow(q.corr)){
  
  edit_param_q(harvest.file = here::here('currentVersion','at_harvest.prm'),
               Code = q.corr$Code[i],
               Fleet = q.corr$fleet[i],
               fleets.file = here::here('currentVersion','neus_fisheries.csv'),
               Value = signif(q.corr$corr.max[i],2),
               overwrite = T
              )
}
