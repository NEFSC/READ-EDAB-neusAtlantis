
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev.old = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
# dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/dev_4_14_23/'
dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/MAK_pers_37/'
v6665.4 = here::here('Atlantis_Runs','v6665_MiscCalib_4','')
v6665.5 = here::here('Atlantis_Runs','v6665_MiscCalib_5','')
v6665.5b = here::here('Atlantis_Runs','v6665_MiscCalib_5b','')
v6665.6 = here::here('Atlantis_Runs','v6665_MiscCalib_6','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,v6665.4,v6665.6),
  model.names = c('dev','v6665_MiscCalib_4','v6665_MiscCalib_6'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6665_MiscCalib_6'),
  table.out = F,
  groups = NULL,
  remove.init = F
)


plot_run_comparisons(
  model.dirs = c(dev.old,dev,ZL.revert,ZL.revert2,ZL.revert3),
  model.names = c('dev_old','dev','ZL_revert_1','ZL_revert_2','ZL.revert3'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'ZL.revert3'),
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

