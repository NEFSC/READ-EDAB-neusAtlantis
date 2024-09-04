
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

spatial1 = here::here('Atlantis_Runs','NE_groundfish_new_movement_2_WOL','/')
spatial2 = here::here('Atlantis_Runs','rescale_spatial_1','/')
spatial3 = here::here('Atlantis_Runs','rescale_spatial_1b','/')
# master = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/master_2_2_0/'
dev = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_6681_20240620/'


figure.dir = here::here('Figures','Run_Comparisons','')

# plot_run_catch_comparisons(model.dirs = c(master,fleet), model.names = c('master','fleet'),
#                            plot.out = paste0(figure.dir,'master_fleet_comparison'),
#                            plot.diff = F,plot.raw = T)
plot_run_comparisons(
  model.dirs = c(dev,spatial1,spatial2,spatial3),
  model.names = c('Dev_6681','NE_groundfish_new_movement_2_WOL','rescale_spatial_1','rescale_spatial_1b'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'rescale_spatial_1b'),
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
