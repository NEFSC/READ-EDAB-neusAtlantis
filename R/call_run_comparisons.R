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
zoo.fix = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_ZooFix_OldParams2/')
upzl2 = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_ZooFix_UpZL2/')
upzl3 = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_ZooFix_UpZL3/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')

plot_run_comparisons(
  model1.dir = upzl2,
  model2.dir = upzl3,
  model1.name = 'UpZL2',
  model2.name = 'UpZL3',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'UpZL2_v_UpZL3'),
  table.out = T,
  groups = NULL,
  remove.init = T
)

plot_run_comparisons(
  model1.dir = new.obs,
  model2.dir = upzl3,
  model1.name = 'Obs_Base',
  model2.name = 'UpZL3',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Obs_Base_v_UpZL3'),
  table.out = T,
  groups = NULL,
  remove.init = T
)

