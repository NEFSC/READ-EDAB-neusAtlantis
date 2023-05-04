
source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

dev.old = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230313/'
dev = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20230327/'
zl.1 = here::here('Atlantis_Runs','ZL_restore_1','')
zl.2 = here::here('Atlantis_Runs','ZL_restore_2','')
zl.2b = here::here('Atlantis_Runs','ZL_restore_2b','')
zl.3 = here::here('Atlantis_Runs','ZL_restore_3','')
zl.4 = here::here('Atlantis_Runs','ZL_restore_4','')
zl.5 = here::here('Atlantis_Runs','ZL_restore_5','')
zl.6 = here::here('Atlantis_Runs','ZL_restore_6','')
zl.6b = here::here('Atlantis_Runs','ZL_restore_6b','')
zl.7 = here::here('Atlantis_Runs','ZL_restore_7','')
zl.8 = here::here('Atlantis_Runs','ZL_restore_8','')
mak1 = here::here('Atlantis_Runs','MAK_fix_1','')
mak2 = here::here('Atlantis_Runs','MAK_fix_2','')
mak2.pred = here::here('Atlantis_Runs','MAK_fix_2_redDiet','')
mak3 = here::here('Atlantis_Runs','MAK_fix_3','')
mak23 = here::here('Atlantis_Runs','MAK_pers_23','')
dev = here::here('Atlantis_Runs','dev_4_14_23','')

figure.dir = here::here('Figures','Run_Comparisons','')

plot_run_comparisons(
  model.dirs = c(dev,mak23),
  model.names = c('dev','MAK_pers_23'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'MAK_pers_23_'),
  table.out = F,
  groups = c('MAK','HER','WHK','BLF','WPF','SUF','WIF','WTF','FOU','HAL','PLA','FLA','BFT','TUN','BIL','MPF','BUT','BPF','ANC','GOO','MEN','FDE','COD','SHK','OHK','NSH','OSH','LSQ','ISQ','ZL','ZM','ZS','ZG'),
  remove.init = F
)


plot_run_comparisons(
  model.dirs = c(dev.old,dev,zl.7),
  model.names = c('Dev_20230313','Dev_20230327','ZL_restore_7'),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'ZL_restore_7_v_dev'),
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

