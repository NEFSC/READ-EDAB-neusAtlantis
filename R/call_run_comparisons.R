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

# new.obs = paste0(obs.dir,'Atlantis_Runs/Obs_Hindcast_NewForcing/')
dev = here::here('Atlantis_Runs','Dev_11032022','')
dev = '/home/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/Dev_11032022/'
const.f = here::here('Atlantis_Runs','Constant_Catch_Test','')
extend.catch = here::here('Atlantis_Runs','Extended_Constant_Catch','')
new.clams.extended = here::here('Atlantis_Runs','Dev_New_Clams_extended','')
new.clams =  '/home/jcaracappa/Documents/GitHub/neus-atlantis_2/Atlantis_Runs/Dev_New_Clams/'
new.clams.2 = '/home/jcaracappa/Documents/GitHub/neus-atlantis_2/Atlantis_Runs/Dev_New_Clams_2/'

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev, new.clams,new.clams.2),
  model.names = c('Dev_11032022','Dev_New_Clams','Dev_New_Clams_2'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Dev_New_Clams','Dev_New_Clams_2'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

plot_run_catch_comparisons(
  model.dirs = c(dev, new.clams, new.clams.2),
  model.names = c('Dev_11032022','Dev_New_Clams','Dev_New_Clams_2'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Dev_New_Clams','Dev_New_Clams_2'),
  table.out = F,
  groups = NULL,
  remove.init = F
)

