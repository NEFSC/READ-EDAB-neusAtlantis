#Script makes geophysical variables timeseries

source(here::here('R','plot_atlvar_box.R'))
source(here::here('R','plot_atlvar_domain.R'))
source(here::here('R','plot_atlvar_chemphys_comparison.R'))

nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/neus_output_test.nc'
variable.names = c('NH3','NO3','Oxygen','Temp','salt','Light','Chl_a')
plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/Figures/geophysical_output/'
plot.names = paste0(variable.names,'_Timeseries')

for(f in 1:length(variable.names)){
  
  # plot.box.biophys(
  #   nc.file = nc.file,
  #   variable.name = variable.names[f],
  #   plot.dir = plot.dir,
  #   plot.name = plot.names[f]
  # )
  
  # plot.domain.biophys(
  #   nc.file = nc.file,
  #   variable.name = variable.names[f],
  #   plot.dir = plot.dir,
  #   plot.name = plot.names[f]
  # )
  
  plot.geochem.comps(
    output.files = c(
      'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/atneus_v15_01272020.nc',
      'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_noLTLscale/neus_output_test.nc',
      'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/neus_output_test.nc'
    ),
    model.names = c('original','LTL_Forced','LTL_and_Nutrients'),
    var.name = variable.names[f],
    plot.dir = plot.dir,
    plot.name = paste0(variable.names[f],'_original_v_LTL_Forced')
  )
  
  print(f)
}
