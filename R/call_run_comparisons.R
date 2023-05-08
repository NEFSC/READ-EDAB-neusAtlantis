
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev.old = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
# dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/dev_4_14_23/'
dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/MAK_pers_37/'
ZL.revert = here::here('Atlantis_Runs','ZL_revert_1','')
ZL.revert2 = here::here('Atlantis_Runs','ZL_revert_2','')
ZL.revert2b = here::here('Atlantis_Runs','ZL_revert_2b','')
recruit.12 = here::here('Atlantis_Runs','all_recruit_12','')
ZL.revert3 = here::here('Atlantis_Runs','ZL_revert_3','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev.old,dev,ZL.revert,recruit.12),
  model.names = c('dev_old','dev','ZL_revert_1','all_recruit_12'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'all_recruit_12'),
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

