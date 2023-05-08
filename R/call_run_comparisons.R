
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

# dev = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_20230327/'
dev = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/dev_4_14_23/'
# dev = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/v6536dev_52yr/'
# v6665 = here::here('Atlantis_Runs','v6665_dev_10yr','')
v6665.notemp = here::here('Atlantis_Runs','v6665_dev_10yr_notempsens','')
v6665.diag = here::here('Atlantis_Runs','v6665_dev_TempSense_Flags','')
v6665.pHoff= here::here('Atlantis_Runs','v6665_pHFlags','')
v6665.tempfix= here::here('Atlantis_Runs','v6665_TempFix','')
v6665.fix1= here::here('Atlantis_Runs','v6665_MiscCalib_1','')
v6665.fix2= here::here('Atlantis_Runs','v6665_MiscCalib_2','')
v6665.fix1.k= here::here('Atlantis_Runs','v6665_MiscCalib_1_kconst2','')
v6665.fix3= here::here('Atlantis_Runs','v6665_MiscCalib_3','')
v6665.fix3.diffS= here::here('Atlantis_Runs','v6665_MiscCalib_3_diffStart','')
v6665.fix4= here::here('Atlantis_Runs','v6665_MiscCalib_4','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,v6665.fix4),
  model.names = c('v6536dev','v6665_MiscCalib_4'),
  plot.rel = F,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6536_v_v6665_MiscCalib_4'),
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

