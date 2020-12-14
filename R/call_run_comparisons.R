# model1.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/'
# model2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/'
# plot.raw = T
# plot.diff = T
# plot.out = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/Figures/'
# table.out = T
# # groups = c('HER','CLA','LOB')
# groups = NULL

source(here::here('R','plot_run_comparisons.R'))

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/'
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'

roms.cobalt = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_DinoFlag/')

new.obs = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_NewForcing/')
master = paste0(obs.dir,'Atlantis_Runs/Master_10202020/')
pred_mort = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_addMort/')
pred9 = paste0(obs.dir,'Atlantis_Runs/ReducePred9/')
pred10 = paste0(obs.dir,'Atlantis_Runs/ReducePred10/')
pred11 = paste0(obs.dir,'Atlantis_Runs/ReducePred11/')
pred12 = paste0(obs.dir,'Atlantis_Runs/ReducePred12/')
pred12b = paste0(obs.dir,'Atlantis_Runs/ReducePred12b/')
pred13 = paste0(obs.dir,'Atlantis_Runs/ReducePred13/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')


plot_run_comparisons(
  model1.dir = master,
  model2.dir = pred13,
  model1.name = 'master',
  model2.name = 'pred13',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'master_v_pred13'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model1.dir = pred12,
  model2.dir = pred13,
  model1.name = 'pred12',
  model2.name = 'pred13',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'pred12_v_pred13'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

