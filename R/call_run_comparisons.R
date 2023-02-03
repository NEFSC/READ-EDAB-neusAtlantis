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
dev = '/home/jcaracappa/Doucments/GitHub/neus-atlantis/Atlantis_Runs/Dev_11032022/'
new.clams = here::here('Atlantis_Runs','Dev_New_Clams','')

v6536.nfnm = paste0(obs.dir,'Atlantis_Runs/dev07052022_165_6536_noMigration_noFishing/')
v6645.nfnm = paste0(obs.dir,'Atlantis_Runs/dev07052022_165_6645_noMigration_noFishing/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')

plot_run_comparisons(
  model.dirs = c(dev,new.clams),
  model.names = c('dev_11032022','Dev_New_Clams'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Dev_New_Clams'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_catch_comparisons(
  model.dirs = c(dev,new.clams),
  model.names = c('dev_11032022','Dev_New_Clams'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Dev_New_Clams'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

