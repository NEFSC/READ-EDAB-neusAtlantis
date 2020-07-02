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
orig.model = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_DinoFlag/')
temp.p2 = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_HighTempSensitivity/')
temp.m2 = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_LowTempSensitivity/')
temp.lower.p2 = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_Lower_HighTempSensitivity/')
temp.lower.m2 = paste0(roms.dir,'Atlantis_Runs/Atlantis_Output_Lower_LowTempSensitivity/')
figure.dir = paste0(roms.dir,'Diagnostic_Figures/Run_Comparisons/')

#High temp vs orig
plot_run_comparisons(
  model.dirs = c(orig.model,temp.p2,temp.m2,temp.lower.p2,temp.lower.m2),
  model.names = c('original','Temp +2 degC','Temp -2 degC','Temp +2 degC - Lower Only','Temp -2 degC - Lower Only'),
  plot.raw = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'Temperature_Sensitivity_V2_'),
  table.out = T,
  groups = NULL
)


#Original vs. new physics
# comp.model.groups(
#   model1.dir = orig.model,
#   model2.dir = new.physics,
#   model1.name = 'original model',
#   model2.name = 'new physics',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'orig_v_newphys_'),
#   table.out = T,
#   groups = NULL
# )
# 
# #Original vs. new ltl
# comp.model.groups(
#   model1.dir = orig.model,
#   model2.dir = new.ltl,
#   model1.name = 'original model',
#   model2.name = 'new ltl',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'orig_v_newltl_'),
#   table.out = T,
#   groups = NULL
# )
# 
# #new physics vs. new LTL
# comp.model.groups(
#   model1.dir = new.physics,
#   model2.dir = new.ltl,
#   model1.name = 'new physics',
#   model2.name = 'new ltl',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'newphys_v_newltl_'),
#   table.out = T,
#   groups = NULL
# )
# 
# #new LTL vs. new LTL 1980 fill
# comp.model.groups(
#   model1.dir = new.ltl,
#   model2.dir = new.ltl.fill1980,
#   model1.name = 'new ltl',
#   model2.name = 'new ltl - filled 1980',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'newltl_v_newltl1980_'),
#   table.out = T,
#   groups = NULL
# )
# 
# #LTL 1980 fill vs original
# comp.model.groups(
#   model1.dir = new.ltl.fill1980,
#   model2.dir = original.model,
#   model1.name = 'new ltl - filled 1980',
#   model2.name = 'original model',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'newltl1980_v_orig_'),
#   table.out = T,
#   groups = NULL
# )
# 
#Ltl 1980 fill vs new physics
# comp.model.groups(
#   model1.dir = new.ltl.fill1980,
#   model2.dir = new.physics,
#   model1.name = 'new ltl - filled 1980',
#   model2.name = 'new physics',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'newltl1980_v_newphys_'),
#   table.out = T,
#   groups = NULL
# )

# #Ltl no init scale vs new LTL fill 1980
# comp.model.groups(
#   model1.dir = new.ltl.fill1980,
#   model2.dir = ltl.noscale,
#   model1.name = 'new ltl - filled 1980',
#   model2.name = 'ltl no init scale',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'newltl1980_v_noLTLscale_'),
#   table.out = T,
#   groups = NULL
# )
# 
# comp.model.groups(
#   model1.dir = ltl.noscale  ,
#   model2.dir = force.nutrients,
#   model1.name = 'Forced LTL',
#   model2.name = 'Forced Nutrients',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = paste(figure.dir,'newltl_v_newnutrients_'),
#   table.out = T,
#   groups = NULL
# )
