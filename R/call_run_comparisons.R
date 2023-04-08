
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230327/'
v6665 = here::here('Atlantis_Runs','v6665_test','')
v6665.nm = here::here('Atlantis_Runs','v6665_test_nomig','')
v6665.cal = here::here('Atlantis_Runs','v6665_calibrated','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,v6665,v6665.cal),
  model.names = c('Dev_20230327','v6665_test','v6665_calibrated'),
  plot.rel = F,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6536_v_v6665_calibrated_'),
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

