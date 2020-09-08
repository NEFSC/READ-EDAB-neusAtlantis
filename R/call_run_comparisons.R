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
# orig.model = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/'
# new.physics = paste0(roms.dir,'Atlantis_Output_2/')
# new.ltl = paste0(roms.dir,'Atlantis_Output_LTLForce_1/')
# new.ltl.fill1980 = paste0(roms.dir,'Atlantis_Output_LTLForce_1980Fill/')
# ltl.noscale = paste0(roms.dir,'Atlantis_Output_noLTLscale/')
# force.nutrients = paste0(roms.dir,'Atlantis_Output_NutrientForcing/')
roms.cobalt = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_DinoFlag/')
GLORYS = paste0(obs.dir,'Atlantis_Runs/GLORYS_Physics_1/')
Satphyto = paste0(obs.dir,'Atlantis_Runs/SatPhyto_Forcing_1/')
# satphyto.const.diatom = paste0(obs.dir,'Atlantis_RUns/SatPhyto_Forcing_Const_Diatom_Prop/')
# satphyto.hirata.diatom = paste0(obs.dir,'Atlantis_RUns/SatPhyto_Forcing_Hirata_Diatom_Prop/')
satphyto.dyn.low = paste0(obs.dir,'Atlantis_Runs/SatPhyto_Forcing_Dynamic_Lower_with_DL/')
satphyto.fast.sink = paste0(obs.dir,'Atlantis_Runs/SatPhyto_Forcing_Fast_PL_Sink_3/')
figure.dir = paste0(obs.dir,'Diagnostic_Figures/Run_Comparisons/')

#ROMS_COBALT vs GLORYS
plot_run_comparisons(
  model1.dir = satphyto.dyn.low,
  model2.dir = satphyto.fast.sink,
  model1.name = 'Dynamic_Lower',
  model2.name = 'Faster_Sinking',
  plot.raw = T,
  plot.diff = T,
  plot.out = paste(figure.dir,'DynLow_v_FastSink'),
  table.out = T,
  groups = NULL
)

plot_run_comparisons(
  model1.dir = GLORYS,
  model2.dir = satphyto.fast.sink,
  model1.name = 'GLORYS',
  model2.name = 'Faster_Sinking',
  plot.raw = T,
  plot.diff = T,
  plot.out = paste(figure.dir,'GLORYS_v_FastSink'),
  table.out = T,
  groups = NULL
)


