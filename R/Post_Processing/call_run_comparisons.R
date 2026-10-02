
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

run.set.names = paste0('fleet_calibration_4_q')
dev = 'Dev_6681_20240905'
base = paste0(here::here('output','calib_setup_9_3_26_1'),'/')
scen1 = paste0(here::here('output','calib_setup_9_3_26_22'),'/')
scen2 = paste0(here::here('output','calib_setup_9_3_26_23'),'/')
scen3 = paste0(here::here('output','calib_setup_9_3_26_24'),'/')
scen4 = paste0(here::here('output','calib_setup_9_3_26_25'),'/')
scen5 = paste0(here::here('output','calib_setup_9_3_26_26'),'/')
scen6 = paste0(here::here('output','calib_setup_9_3_26_27'),'/')
scen7 = paste0(here::here('output','calib_setup_9_3_26_28'),'/')
scen8 = paste0(here::here('output','calib_setup_9_3_26_29'),'/')
scen9 = paste0(here::here('output','calib_setup_9_3_26_30'),'/')
scen10 = paste0(here::here('output','calib_setup_9_3_26_31'),'/')


dev.dir = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_6681_20240905/'
base.dir = here::here('output',base)


run.set.dirs = here::here('Atlantis_Runs',paste0('fleet_calibration_4_q'),'')
# master = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/master_2_2_0/'

figure.dir = here::here('Figures','Run_Comparisons','')

# plot_run_catch_comparisons(model.dirs = c(master,fleet), model.names = c('master','fleet'),
#                            plot.out = paste0(figure.dir,'master_fleet_comparison'),
#                            plot.diff = F,plot.raw = T)
plot_run_comparisons(
  model.dirs = c(base,scen1,scen2,scen3,scen4,scen5,scen6,scen7,scen8,scen9,scen10),
  model.names = c('base','22','23','24','25','26','27','28','29','30','31'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'scens_9_3_26_22-31'), 
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
