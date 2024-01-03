
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

# dev = '/home/jcaracappa/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/'
dev = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20231101/'
master = '/home/jcaracappa/atlantis/Shared_Data/Release_Runs/Master_2_0_1/'
new.age = here::here('Atlantis_Runs','6536_new_age_param_init_rescale','')
new.age.2 = here::here('Atlantis_Runs','6536_new_age_param_init_rescale_2','')
new.age.revert = here::here('Atlantis_Runs','6536_new_age_param_init_rescale_revert','')
new.mum.1 = here::here('Atlantis_Runs','6536_new_age_param_mum_rescale_1','')
new.mum.2 = here::here('Atlantis_Runs','6536_new_age_param_mum_rescale_2','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,new.age.2,new.mum.2),
  model.names = c('Dev','New Ages rescale 2','New mum 2'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'new_mum_2'),
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

