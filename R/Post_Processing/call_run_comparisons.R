
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))


dev.old = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/v6665_10_13_23/'
new.age = here::here('Atlantis_Runs','new_age_param','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,new.age),
  model.names = c('Dev','New Ages'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'new_age'),
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

