
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

fleet = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/gfimposezerotsfull/'
master = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/master_2_2_0/'


figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_catch_comparisons(model.dirs = c(master,fleet), model.names = c('master','fleet'),
                           plot.out = paste0(figure.dir,'master_fleet_comparison'),
                           plot.diff = F,plot.raw = T)
plot_run_comparisons(
  model.dirs = c(dev.6536,v6681.4m,v6681.5,v6681.6,v6681.6b,v6681.6c),
  model.names = c('Dev_6536','v6681_Calib_4_merge','v6681_Calib_5','v6681_Calib_6','v6681_Calib_6b','v6681_Calib_6c_HER'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6681_Calib_6c'),
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
