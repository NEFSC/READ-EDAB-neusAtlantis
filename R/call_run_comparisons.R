
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

# dev.old = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
# dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/dev_4_14_23/'
dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/MAK_pers_37/'
phyto.update = here::here('Atlantis_Runs','phyto_update_2022','')
phyto.update.2 = here::here('Atlantis_Runs','phyto_update_2022_newDiatomPct','')
phyto.update.3 = here::here('Atlantis_Runs','phyto_update_2022_lowVflux','')
phyto.update.4 = here::here('Atlantis_Runs','phyto_update_2022_lowSink','')
phyto.update.5 = here::here('Atlantis_Runs','phyto_update_2022_midSink','')
phyto.update.6 = here::here('Atlantis_Runs','phyto_update_2022_midSink_2','')
phyto_svel_0 =  here::here('Atlantis_Runs','phyto_svel_1','phyto_svel_1_0','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,phyto_svel_0),
  model.names = c('dev','phyto_svel_1_0'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'phyto_update_2022_sinkVel'),
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

# plot_run_comparisons(
#   model.dirs = c(dev.old,dev,zl.7),
#   model.names = c('Dev_20230313','Dev_20230327','ZL_restore_7'),
#   plot.rel = T,
#   plot.diff = F,
#   plot.out = paste(figure.dir,'ZL_restore_7_v_dev'),
#   table.out = F,
#   groups = NULL,
#   remove.init = F
# )

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

