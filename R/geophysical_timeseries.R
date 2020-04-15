#Script makes geophysical variables timeseries

source(here::here('R','plot_box_biophys_output.R'))
source(here::here('R','plot_domain_biophys_output.R'))

nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/neus_output_test.nc'
variable.names = c('NH3','NO3','Oxygen','Temp','salt','Light','Chl_a')
plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/Figures/geophysical_output/'
plot.names = paste0(variable.names,'_Timeseries')

for(f in 1:length(variable.names)){
  
  plot.box.biophys(
    nc.file = nc.file,
    variable.name = variable.names[f],
    plot.dir = plot.dir,
    plot.name = plot.names[f]
  )
  
  plot.domain.biophys(
    nc.file = nc.file,
    variable.name = variable.names[f],
    plot.dir = plot.dir,
    plot.name = plot.names[f]
  )
  plot(f)
}