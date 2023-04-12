
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230327/'
<<<<<<< HEAD
v6665 = here::here('Atlantis_Runs','v6665_test','')
v6665.nm = here::here('Atlantis_Runs','v6665_test_nomig','')
v6665.cal = here::here('Atlantis_Runs','v6665_calibrated','')
v6665.noQ = here::here('Atlantis_Runs','v6665_cal_noQ10','')
=======
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
mak3 = here::here('Atlantis_Runs','MAK_fix_3','')
>>>>>>> dev_branch

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
<<<<<<< HEAD
  model.dirs = c(dev,v6665,v6665.cal,v6665.noQ),
  model.names = c('Dev_20230327','v6665_test','v6665_calibrated','v6665_cal_noQ10'),
  plot.rel = F,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6536_v_v6665_cal_noQ10_'),
=======
  model.dirs = c(mak1,mak2,mak3),
  model.names = c('MAK_fix_1','MAK_fix_2','MAK_fix_3'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'MAK_fix_3_'),
  table.out = F,
  groups = "MAK",
  remove.init = F
)


plot_run_comparisons(
  model.dirs = c(dev.old,dev,zl.7),
  model.names = c('Dev_20230313','Dev_20230327','ZL_restore_7'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'ZL_restore_7_v_dev'),
>>>>>>> dev_branch
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

