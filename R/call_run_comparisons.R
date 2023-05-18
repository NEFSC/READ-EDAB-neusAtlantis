
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev.old = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
dev = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230327/'
zl.1 = here::here('Atlantis_Runs','ZL_restore_1','')
zl.2 = here::here('Atlantis_Runs','ZL_restore_2','')
zl.2b = here::here('Atlantis_Runs','ZL_restore_2b','')
zl.3 = here::here('Atlantis_Runs','ZL_restore_3','')
zl.4 = here::here('Atlantis_Runs','ZL_restore_4','')
zl.5 = here::here('Atlantis_Runs','ZL_restore_5','')
zl.6 = here::here('Atlantis_Runs','ZL_restore_6','')
zl.6b = here::here('Atlantis_Runs','ZL_restore_6b','')
zl.7 = here::here('Atlantis_Runs','ZL_restore_7','')
zl.8 = here::here('Atlantis_Runs','ZL_restore_8','')
mak1 = here::here('Atlantis_Runs','MAK_fix_1','')
mak2 = here::here('Atlantis_Runs','MAK_fix_2','')
mak2.pred = here::here('Atlantis_Runs','MAK_fix_2_redDiet','')
mak23 = here::here('Atlantis_Runs','MAK_pers_23','')
mak25 = here::here('Atlantis_Runs','MAK_pers_25','')
mak37 = here::here('Atlantis_Runs','MAK_pers_37','')
dev = here::here('Atlantis_Runs','MAK_pers_37','')
BHalpha_90 = here::here('Atlantis_Runs','BHalpha_90','')
BH_1 = here::here('Atlantis_Runs','BH_1','')
BH_2 = here::here('Atlantis_Runs','BH_2','')
BH_3 = here::here('Atlantis_Runs','BH_3','')
BH_4 = here::here('Atlantis_Runs','BH_4','')
BH_5 = here::here('Atlantis_Runs','BH_5','')
BH_6 = here::here('Atlantis_Runs','BH_6','')
BH_7 = here::here('Atlantis_Runs','BH_7','')
BH_8 = here::here('Atlantis_Runs','BH_8','')
BH_9 = here::here('Atlantis_Runs','BH_9','')
BH_10 = here::here('Atlantis_Runs','BH_10','')
BH_11 = here::here('Atlantis_Runs','BH_11','')
BH_12 = here::here('Atlantis_Runs','BH_12','')
BH_13 = here::here('Atlantis_Runs','BH_13','')
BH_14 = here::here('Atlantis_Runs','BH_14','')
BH_15 = here::here('Atlantis_Runs','BH_15','')
BH_16 = here::here('Atlantis_Runs','BH_16','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,BH_13,BH_14, BH_15,BH_16),
  model.names = c('dev', 'BH_13','BH_14','BH_15', 'BH_16'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'dev_BH13-16_'),
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

