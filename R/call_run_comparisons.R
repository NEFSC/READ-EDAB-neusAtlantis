
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev.old = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
dev = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230523/'
RED1 = here::here('Atlantis_Runs','BLF_RED_1','')
RED2 = here::here('Atlantis_Runs','BLF_RED_2','')
RED3 = here::here('Atlantis_Runs','BLF_RED_3','')
RED4 = here::here('Atlantis_Runs','BLF_RED_4','')
RED5 = here::here('Atlantis_Runs','BLF_RED_5','')
HER1 = here::here('Atlantis_Runs','HER_LOB_1','')


figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(RED5,HER1),
  model.names = c('BLF_RED_5','HER_LOB_1'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'HER_LOB_1'),
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

