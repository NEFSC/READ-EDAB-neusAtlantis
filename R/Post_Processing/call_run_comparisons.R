
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

dev.6536 = here::here('Atlantis_Runs','Dev_6536_20240109','/')
dev.6681 = dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/v6681_base/'
v6681.1 = here::here('Atlantis_Runs','v6681_Calib_1')
v6681.1b = here::here('Atlantis_Runs','v6681_Calib_1b')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev.6536,dev.6681,v6681.1,v6681.1b),
  model.names = c('Dev_6536','Dev_6681','v6681_Calib_1','v6681_Calib_1b'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6681_Calib_1b'),
  table.out = F,
  groups = NULL,
  remove.init = F
)


