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

dev.old = here::here('Atlantis_Runs','Dev_11032022','')
dev = here::here('Atlantis_Runs','Dev_02062023','')
dev.extended = here::here('Atlantis_Runs','Dev_02062023_extended','')

figure.dir = here::here('Figures','Run_Comparisons','')

# repo.dir = '/net/work3/EDAB/atlantis/Joe_Proj/'
# source(paste0(repo.dir,'R/plot_run_comparisons.R'))
# source(paste0(repo.dir,'R/plot_run_catch_comparisons.R'))
# 
# dev.old = paste0(repo.dir,'Atlantis_Runs/Dev_11032022/')
# dev = paste0(repo.dir,'Atlantis_Runs/Dev_02062023/')
# dev.extended = paste0(repo.dir,'Atlantis_Runs/Dev_02062023_extended/')
# 
# figure.dir = paste0(repo.dir,'Figures/Run_Comparisons/')

plot_run_comparisons(
  model.dirs = c(dev.old,dev.extended),
  model.names = c('Dev_11032022','Dev_02062023_extended'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Dev_02062023_extended'),
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

