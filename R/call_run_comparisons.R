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
catch.6490 = paste0(obs.dir,'Atlantis_Runs/New_CatchTS_6490/')
catch.6536 = paste0(obs.dir,'Atlantis_Runs/New_CatchTS_6536/')
catch.6536.3 = paste0(obs.dir,'Atlantis_Runs/New_CatchTS_6536_3/')
new.init.catch = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS/')
new.init.catch.revert = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS_Revert/')
new.init.catch2 = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS_2/')
new.init.catch3 = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS_3/')
new.init.catch4 = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS_4/')
new.init.catch5 = paste0(obs.dir,'Atlantis_Runs/New_Init_CatchTS_5/')
rg.benthic = paste0(obs.dir, 'Atlantis_Runs/Benthic_Fix_RG/')

figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')


plot_run_comparisons(
  model1.dir = new.init.catch5,
  model2.dir = rg.benthic,
  model1.name = 'new init catch 5',
  model2.name = 'new benthic',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'newinitcatch_5_v_benthic'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_comparisons(
  model1.dir = master,
  model2.dir = rg.benthic,
  model1.name = 'master',
  model2.name = 'RG Benthic',
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'master_v_RGBenthic'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
