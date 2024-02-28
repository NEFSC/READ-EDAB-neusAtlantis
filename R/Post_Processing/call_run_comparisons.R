
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

dev.6536 = here::here('Atlantis_Runs','Dev_6536_20240109','/')
dev.6681 = dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/v6681_base/'
# v6681.1 = here::here('Atlantis_Runs','v6681_Calib_1','')
v6681.1b = here::here('Atlantis_Runs','v6681_Calib_1b','')
v6681.2 = here::here('Atlantis_Runs','v6681_Calib_2','')
v6681.2b = here::here('Atlantis_Runs','v6681_Calib_2b','')
v6681.3 = here::here('Atlantis_Runs','v6681_Calib_3','')
v6681.3c = here::here('Atlantis_Runs','v6681_Calib_3c','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev.6536,v6681.2b,v6681.3,v6681.3c),
  model.names = c('Dev_6536','v6681_Calib_2b','v6681_Calib_3','v6681_Calib_3c'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6681_Calib_3c'),
  table.out = F,
  groups = NULL,
  remove.init = F
)


plot_run_comparisons(
  model.dirs = c(dev.6536,v6681.2),
  model.names = c('Dev_6536','v6681_Calib_2'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6681_Calib_2_dev'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
