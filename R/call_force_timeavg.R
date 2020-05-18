source(here::here('R','plot_force_timeavg.R'))

forcing.climatology(
  force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/',
  file.pattern = 'salt_force_*',
  plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/alt_',
  time.group = 'ym',
  plot.region = T
  
)

forcing.climatology(
  force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/',
  file.pattern = 'temp_force_*',
  plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/alt_',
  time.group = 'ym',
  plot.region = T
  
)

forcing.climatology(
  force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/',
  file.pattern = file.pattern = 'salt_force_*',
  plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/alt_',
  time.group = 'ym',
  plot.region = T
  
)