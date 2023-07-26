
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev.old = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
dev = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230523/'
CM.deltaT = here::here('Atlantis_Runs','cm2_6_2100_dev_deltaT','')
CM.base = here::here('Atlantis_Runs','cm2_6_2100_dev_baseline','')


figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(CM.deltaT,CM.base),
  model.names = c('CM2.6 w/ Temp','baseline'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'CM2_6'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model.dirs = c(dev.old,dev,zl.7),
  model.names = c('Dev_20230313','Dev_20230327','ZL_restore_7'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'ZL_restore_7_v_dev'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

# plot_run_catch_comparisons(
#   model.dirs = c(dev, new.clams, new.clams.2),
#   model.names = c('Dev_11032022','Dev_New_Clams','Dev_New_Clams_2'),
#   plot.raw = T,
#   plot.diff = F,
#   plot.out = paste(figure.dir,'Dev_New_Clams','Dev_New_Clams_2'),
#   table.out = F,
#   groups = NULL,
#   remove.init = F
# )

