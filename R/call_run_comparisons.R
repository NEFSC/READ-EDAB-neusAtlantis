# model1.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/'
# model2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/'
# plot.raw = T
# plot.diff = T
# plot.out = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/Figures/'
# table.out = T
# # groups = c('HER','CLA','LOB')
# groups = NULL

source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/'
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'

roms.cobalt = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_DinoFlag/')

new.obs = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_NewForcing/')
master = paste0(obs.dir,'Atlantis_Runs/Master_10202020/')
master2 = paste0(obs.dir,'Atlantis_Runs/Master_12152020/')
benth.pred = paste0(obs.dir,'Atlantis_Runs/ReduceBenthicPred_1/')
benth.pred2 = paste0(obs.dir,'Atlantis_Runs/ReduceBenthicPred_2/')
benth.pred3 = paste0(obs.dir,'Atlantis_Runs/ReduceBenthicPred_3/')
benth.pred4 = paste0(obs.dir,'Atlantis_Runs/ReduceBenthicPred_4/')
benth.pred5 = paste0(obs.dir,'Atlantis_Runs/ReduceBenthicPred_5/')
benth.pred6 = paste0(obs.dir,'Atlantis_Runs/ReduceBenthicPred_6/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')


plot_run_comparisons(
  model1.dir = master2,
  model2.dir = benth.pred6,
  model1.name = 'master 12152020',
  model2.name = 'ReduceBenthicPred_6',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'master_v_ReduceBenthicPred_6'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model1.dir = benth.pred4,
  model2.dir = benth.pred6,
  model1.name = 'ReduceBenthicPred_4',
  model2.name = 'ReduceBenthicPred_6',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'ReduceBenthicPred_4_v_6'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
