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

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/'
roms.dir = 'E:/Atlantis_Backup/ROMS_COBALT/'
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'

roms.cobalt = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_Base_06232020/')

# new.obs = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_NewForcing/')
master = paste0(obs.dir,'Atlantis_Runs/Master_12172021/')
phase1 = paste0(obs.dir,'Atlantis_Runs/Phase_1_5day/')
# rg.test = paste0(obs.dir,'Atlantis_Runs/Output_Rob_noFishing_10_8_21/')
all.bh = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_Spawn_Debug/')
bh.ab2 = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_RescaleAlphaBeta_2/')
bh.ab3 = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_RescaleAlphaBeta_3/')
bh.ab4 = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_RescaleAlphaBeta_4/')
bh.ab5 = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_RescaleAlphaBeta_5/')
bh.newcatch = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_NewCatchSpinup/')
bh.newcatch.revert = paste0(obs.dir,'Atlantis_Runs/BH_NEUSv1_NewCatch_Reverted/')

v6536.nfnm = paste0(obs.dir,'Atlantis_Runs/dev07052022_165_6536_noMigration_noFishing/')
v6645.nfnm = paste0(obs.dir,'Atlantis_Runs/dev07052022_165_6645_noMigration_noFishing/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')

plot_run_comparisons(
  model.dirs = c(v6536.nfnm,v6645.nfnm),
  model.names = c('dev07052022_165_6536_noMigration_noFishing','dev07052022_165_6645_noMigration_noFishing'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'v6645_v6536_nfnm'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model.dirs = c(phase1,bh.newcatch.revert),
  model.names = c('Phase1','NewCatch_BH'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Phase1_v_NewCatcH_BH'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_catch_comparisons(
  model1.dir = bh.ab5,
  model2.dir = bh.newcatch,
  model1.name = 'BH5',
  model2.name = 'New Catch',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'All_BH_NewSpinup_Catch'),
  table.out = F,
  groups = NULL,
  remove.init = F
)


plot_run_comparisons(
  model1.dir = roms.cobalt,
  model2.dir = master3,
  model1.name = 'ROMS_COBALT',
  model2.name = 'Master',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'ROMS_COBALT_v_Master'),
  table.out = F,
  groups = NULL,
  remove.init = F
)




