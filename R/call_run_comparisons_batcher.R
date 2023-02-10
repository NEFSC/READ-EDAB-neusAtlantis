#Script to run multiple batcher run comparisons
library(dplyr)
library(ggplot2)

source(here::here('R','plot_run_comparisons_batcher.R'))

batch.dir = here::here('Atlantis_Runs','batcher_test')
# set.dirs = list.dirs(batch.dir,recursive = F)
set.dirs = batch.dir
# set.names = list.dirs(batch.dir,full.names = F,recursive = F)
set.names = 'batcher_test'

x = read.csv(here::here('currentVersion','neus_groups.csv'),header = T, as.is = T) %>% filter(IsTurnedOn == T)
group.names = sort(x$Code)

for(i in 1:length(set.names)){
  
  run.names = list.dirs(set.dirs[i],full.names = F,recursive = F)
  run.index = data.frame(dir.name = run.names, ID = 0:(length(run.names)-1))
  
  plot_run_comparisons_batcher(
    fig.dir = here::here('Figures/'),
    run.name = set.names[i],
    run.dir = set.dirs[i],
    run.index = run.index,
    param.dir = here::here('currentVersion'),
    group.names = group.names,
    plot.log = F,
    plot.rel = T,
    plot.raw = F
    )
}

#Set Project Name

# #Define run name
# run.name = 'misc_BHalpha_3'
# #Define run set directory
# run.dir = here::here('Atlantis_Runs','misc_mum_C_BHalpha_1',run.name,'')
# #Define run setup file (long version with actual scalars)
# run.index = read.csv(here::here('Setup_Files',paste0(run.name,'_long.csv')),as.is = T)%>%
#   select(ID,dir.name)
# #Define Parameter Files Director
# param.dir = here::here('currentVersion')
# #Define groups you want plotted
# x = read.csv(here::here('currentVersion','neus_groups.csv'),header = T, as.is = T) %>% filter(IsTurnedOn == T)
# group.names = sort(x$Code)