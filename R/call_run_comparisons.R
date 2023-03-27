
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230324/'

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,bh.g,bh.h),
  model.names = c('Dev_20230324','BH_convert_2_final_g','BH_convert_2_final_h'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'BH_convert_2_final_h_'),
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

