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
new.init.catch6 = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS_6/')
master2 = paste0(obs.dir,'Atlantis_Runs/Master_01252021/')
master3 = paste0(obs.dir,'Atlantis_Runs/Master_03192021/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')


plot_run_comparisons(
  model1.dir = master2,
  model2.dir = master3,
  model1.name = 'master 12-15-2020',
  model2.name = 'master 03-19-2021',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'master_v_new_master'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model1.dir = master,
  model2.dir = master2,
  model1.name = 'master',
  model2.name = 'master 12-15-2020',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'master_v_new_master'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
