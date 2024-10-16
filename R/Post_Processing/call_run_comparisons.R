
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

sca1 = paste0('SCA_newdist_1_',1:16)
dev = 'Dev_6681_20240905'

dev.dir = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_6681_20240905/'
sca.dir = here::here('Atlantis_Runs',sca1,'')
# master = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/master_2_2_0/'

figure.dir = here::here('Figures','Run_Comparisons','')

# plot_run_catch_comparisons(model.dirs = c(master,fleet), model.names = c('master','fleet'),
#                            plot.out = paste0(figure.dir,'master_fleet_comparison'),
#                            plot.diff = F,plot.raw = T)
plot_run_comparisons(
  model.dirs = c(dev.dir,sca.dir),
  model.names = c(dev,sca1),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'SCA_newdist1'),
  table.out = F,
  groups = NULL,
  remove.init = F
)


plot_run_comparisons(
  model.dirs = c(dev.6536,v6681.6b),
  model.names = c('Dev_6536','v6681_Calib_6b'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6681_Calib_6'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
