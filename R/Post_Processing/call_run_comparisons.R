
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

master = '/net/work3/EDAB/atlantis//Shared_Data/Release_Runs/Master_2_0_1/'
dev1 = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230731/'
dev2 = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230801/'


figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(master,dev1,dev2),
  model.names = c('v2.0.1','Dev_222','Dev_223'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Update_2_1_0'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model.dirs = c(dev,mak2),
  model.names = c('Dev_20230327','MAK_fix_2'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'PR204_v_dev_'),
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

