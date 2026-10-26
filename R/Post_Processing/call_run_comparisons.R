
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

run.set.names = c('SCA_redist_init_4','SCA_redist_init_3')
dev = 'Dev_6681_20240905'

# dev.dir = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_6681_20240905/'
dev.dir = here::here("Atlantis_Runs","dev_07152025",'')
run.set.dirs =paste0('Atlantis_Runs/',run.set.names,'/')
# master = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/master_2_2_0/'

figure.dir = here::here('Figures','Run_Comparisons','')

# plot_run_catch_comparisons(model.dirs = c(master,fleet), model.names = c('master','fleet'),
#                            plot.out = paste0(figure.dir,'master_fleet_comparison'),
#                            plot.diff = F,plot.raw = T)
plot_run_comparisons(
  model.dirs = c(dev.dir,run.set.dirs),
  model.names = c(dev,run.set.names),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'SCA_redist_init_4'), 
  table.out = F,
  groups = NULL,
  remove.init = F
)


plot_run_catch_comparisons(
  model.dirs = c(dev.dir,run.set.dirs),
  model.names = c(dev,run.set.names),
  plot.diff = F,
  plot.out = paste(figure.dir,'fleet_calibration_2'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
