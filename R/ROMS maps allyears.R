source(here::here('R','ROMS_map_diagnostics.R'))

roms_maps(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing Files/',
          plot.transport = F,
          plot.statevars = T,
          plot.ltlvars = F,
          plot.yearly = T,
          plot.seasonal = F,
          plot.monthly = F,
          plot.prefix = 'Allyears',
          plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/',
          bgm.file = 'neus_ll_WGS84.bgm')

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing Files/'
# plot.transport = T
# plot.statevars = F
# plot.ltlvars = F
# plot.yearly = T
# plot.seasonal = F
# plot.monthly = F
# plot.prefix = 'Allyears'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/'
# bgm.file = 'neus_ll_WGS84.bgm'
