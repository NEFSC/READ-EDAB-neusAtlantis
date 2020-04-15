source(here::here('R','Forcing_Climatology.R'))

forcing.climatology(
  force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars',
  file.pattern = file.pattern = 'roms_ltl_force_*',
  plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/',
  time.group = 'ym',
  plot.region = T
    
)
