# Template to make radial plot sets for run scenarios
library(dplyr)
library(fmsb)
library(ggplot2)
library(gridExtra)

source(here::here('R','make_radar_plot.R'))
#Define time period for aggregation
start.time = 365*30
end.time = 19724

#Define Run sets
set.names = paste0('Run_Set_',1:3)

set.dirs = c(
  here::here('Atlantis_Runs/HER_CatchSpinup_1'),
  here::here('Atlantis_Runs/HER_BHalpha_1'),
  here::here('Atlantis_Runs/HER_mumC_1')
)

#Define Guild definitions
spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),stringsAsFactors = F)%>%
  select(Code,FuncGroup)

#Within each run set read in biology data and aggregate over guild

#Read in base run
base.dir = here::here('Atlantis_Runs','misc_BHalpha_2b')
bio.base = read.table(paste0(base.dir,'/neus_outputBiomIndx.txt'),header = T) %>%
  reshape2::melt(id.vars = 'Time',variable.name = 'Code',value.name = 'Value.base')%>%
  filter(Time >= start.time & Time <= end.time)%>%
  group_by(Code)%>%
  summarise(Value.base = mean(Value.base,na.rm=T))

bio.base.guild = bio.base %>%
  left_join(spp2guild)%>%
  group_by(FuncGroup)%>%
  summarise(Value.base = sum(Value.base))

#Loop through sets
i=1
bio.all.ls = list()
for(i in 1:length(set.names)){
  
  #Get runs within set
  run.dirs = list.dirs(set.dirs[i],recursive = F)
  run.names = list.dirs(set.dirs[i], recursive = F,full.names = F)
  
  #Loop through runs
  j=1
  set.bio.ls = list()
  for(j in 1:length(run.dirs)){
    
    run.bio = read.table(paste0(run.dirs[j],'/neus_outputBiomIndx.txt'),header =T)
    
    col.match = which(colnames(run.bio) %in% c('Time',spp2guild$Code))
    
    set.bio.ls[[j]] = run.bio[,col.match] %>%
      reshape2::melt(id.vars = 'Time',variable.name = 'Code',value.name = 'Value')%>%
      filter(Time >= start.time & Time <= end.time)%>%
      group_by(Code)%>%
      summarise(Value = mean(Value,na.rm=T))%>%
      mutate(set.name = set.names[i],
           run.name = run.names[j])
  }
  bio.all.ls[[i]] = bind_rows(set.bio.ls)
}
bio.all = bind_rows(bio.all.ls)
saveRDS(bio.all,here::here('diagnostics','run_set_test_data_raw.RDS'))

bio.diff = bio.all %>%
  left_join(spp2guild)%>%
  group_by(set.name,run.name,FuncGroup)%>%
  summarise(Value = sum(Value,na.rm=T))%>%
  left_join(bio.base.guild) %>%
  mutate(Value.diff = Value-Value.base)

saveRDS(bio.diff,here::here('diagnostics','run_set_test_data_diff.RDS'))

# par(mfrow = c(1,3),oma = c(0,0,0,0))
p.list = list()
i = 1
for(i in 1:length(set.names)){
  bio.diff.set = filter(bio.diff,set.name == set.names[i]) %>%
    ungroup()%>%
    select(run.name,FuncGroup,Value.diff)%>%
    rename(Group = 'run.name')
  
  p.list[[i]] = make_radar_plot(
    data = bio.diff.set,
    plot.max = 2,
    plot.min = 0,
    ngrid = 3,
    plot.cols = RColorBrewer::brewer.pal(4,'Set2')
  )
}
png(here::here('Figures','Radar_Plot_Test.png'),width = 24, height = 9,units = 'in',res = 300)
do.call('grid.arrange',c(p.list,nrow = 1))
dev.off()
