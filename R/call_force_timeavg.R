source(here::here('R','plot_force_timeavg.R'))

file.patterns = c('roms_tempsalt_force_*','roms_ltl_force_*','roms_nut_force_*')
folders = c('phys_statevars_alternate','ltl_statevars','nut_statevars')

for(i in 1:length(file.patterns)){
  plot_force_timeavg(
    force.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/',folders[i],'/'),
    file.pattern = file.patterns[i],
    plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing Aggregated Timeseries/',
    time.group = 'ym',
    plot.region = T
  )
  
}

for(i in 1:length(file.patterns)){
  plot_force_timeavg(
    force.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/',folders[i],'/'),
    file.pattern = file.patterns[i],
    plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing Aggregated Timeseries/',
    time.group = 'm',
    plot.region = T
  )
  
}



