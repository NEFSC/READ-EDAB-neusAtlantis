#Script to plot Satellite Phytoplankton Variables after running in Atlantis

source(here::here('R','plot_atlvar_box.R'))

var.names = c('Diatom_N','Diatom_S','DinoFlag_N','PicoPhytopl_N','Lab_Det_N','Ref_Det_N')
# var.names = c('Lab_Det_N','Ref_Det_N')

for(v in 1:length(var.names)){
  plot_atlvar_box(
    nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/SatPhyto_Forcing_Fast_PL_Sink_2/neus_output.nc',
    # nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output_DinoFlag/neus_output.nc',
    variable.name = var.names[v],
    plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/SatPhyto_Forcing_Fast_PL_Sink_2/Figures/',
    # plot.dir =  'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output_DinoFlag/Figures/',
    plot.name = var.names[v])  
}
# for(v in 1:length(var.names)){
#   plot_atlvar_box(
#     nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/SatPhyto_Forcing_Dynamic_Lower_with_DL/neus_output.nc',
#     # nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output_DinoFlag/neus_output.nc',
#     variable.name = var.names[v],
#     plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/SatPhyto_Forcing_Dynamic_Lower_with_DL/Figures/',
#     # plot.dir =  'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output_DinoFlag/Figures/',
#     plot.name = var.names[v])  
# }
