#Script to plot Satellite Phytoplankton Variables after running in Atlantis

source(here::here('R','plot_atlvar_box.R'))

var.names = c('Diatom_N','Diatom_S','DinoFlag_N','PicoPhytopl_N','Lab_Det_N','Ref_Det_N','Temp')
# var.names
run.name = 'Obs_Hindcast_LeapYearFix'
run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/'

for(v in 1:length(var.names)){
  plot_atlvar_box(
    nc.file = paste0(run.dir,run.name,'/neus_output.nc'),
    variable.name = var.names[v],
    plot.dir = paste0(run.dir,run.name,'/Figures/'),
    plot.name = var.names[v],
    remove.bot = T,
    remove.init = T)
}


