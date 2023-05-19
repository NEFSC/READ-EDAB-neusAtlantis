
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
BH_17 = here::here('Atlantis_Runs','BH_17','')
BH_18 = here::here('Atlantis_Runs','BH_18','')
BH_19 = here::here('Atlantis_Runs','BH_19','')
BH_20 = here::here('Atlantis_Runs','BH_20','')
BH_21 = here::here('Atlantis_Runs','BH_21','')
BH_22 = here::here('Atlantis_Runs','BH_22','')
BH_23 = here::here('Atlantis_Runs','BH_23','')
BH_24 = here::here('Atlantis_Runs','BH_24','')
BH_25 = here::here('Atlantis_Runs','BH_25','')
BH_26 = here::here('Atlantis_Runs','BH_26','')
BH_27 = here::here('Atlantis_Runs','BH_27','')
BH_28 = here::here('Atlantis_Runs','BH_28','')
BH_29 = here::here('Atlantis_Runs','BH_29','')
BH_30 = here::here('Atlantis_Runs','BH_30','')
BH_31 = here::here('Atlantis_Runs','BH_31','')
BH_32 = here::here('Atlantis_Runs','BH_32','')
BH_33 = here::here('Atlantis_Runs','BH_33','')
BH_34 = here::here('Atlantis_Runs','BH_34','')
BH_35 = here::here('Atlantis_Runs','BH_35','')
BH_36 = here::here('Atlantis_Runs','BH_36','')
BH_37 = here::here('Atlantis_Runs','BH_37','')
BH_38 = here::here('Atlantis_Runs','BH_38','')
BH_39 = here::here('Atlantis_Runs','BH_39','')
BH_40 = here::here('Atlantis_Runs','BH_40','')
BH_41 = here::here('Atlantis_Runs','BH_41','')
BH_42 = here::here('Atlantis_Runs','BH_42','')
BH_43 = here::here('Atlantis_Runs','BH_43','')
BH_44 = here::here('Atlantis_Runs','BH_44','')
BH_combined = here::here('Atlantis_Runs','BH_combined','')
figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
#  model.dirs = c(dev,BH_35,BH_36, BH_37,BH_38,BH_39,BH_40,BH_41,BH_42,BH_43,BH_44),
#  model.names = c('dev', '35','36','37','38','39','40','41','42','43','44'),
  model.dirs = c(dev,BH_35,BH_41),
  model.names = c('dev', '35','41'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'dev_BH35_44_'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model.dirs = c(dev,BH_combined),
  model.names = c('dev', 'BH_combined'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'dev_BH_combined_'),
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

