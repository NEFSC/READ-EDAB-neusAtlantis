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

new.obs = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_NewForcing/')
master = paste0(obs.dir,'Atlantis_Runs/Master_05242021/')
master2 = paste0(obs.dir,'Atlantis_Runs/NewHerInit/')
master3 = paste0(obs.dir,'Atlantis_Runs/Master_06032021/')
persist1 = paste0(obs.dir,'Atlantis_Runs/PersistCheck_1/')
persist2 = paste0(obs.dir,'Atlantis_Runs/PersistCheck_2/')
persist3 = paste0(obs.dir,'Atlantis_Runs/PersistCheck_3/')
orig = paste0(obs.dir,'Atlantis_Runs/Atlantis_Output/')


figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')
plot_run_comparisons(
  model1.dir = persist2,
  model2.dir = persist3,
  model1.name = 'PersistCheck_2',
  model2.name = 'PersistCheck_3',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'PersistCheck_2_v_3_'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model1.dir = master3,
  model2.dir = persist3,
  model1.name = 'master_06032021',
  model2.name = 'PersistCheck_3',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Master_v_Persist3'),
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




