
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
v6681.3d = here::here('Atlantis_Runs','v6681_Calib_3d_no_mort','')
v6681.4 = here::here('Atlantis_Runs','v6681_Calib_4','')
v6681.4m = here::here('Atlantis_Runs','v6681_Calib_4_merge','')
v6681.5 = here::here('Atlantis_Runs','v6681_Calib_5','')
v6681.6 = here::here('Atlantis_Runs','v6681_Calib_6','')
v6681.6b = here::here('Atlantis_Runs','v6681_Calib_6b','')
v6681.6c = here::here('Atlantis_Runs','v6681_Calib_6c_HER','')

figure.dir = here::here('Figures','Run_Comparisons','')

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
