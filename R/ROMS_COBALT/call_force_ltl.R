

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'
# roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
# roms.file = paste0(roms.dir,'roms_output_ltl_statevars_tohydro_1964.nc')


ltl.files = list.files(roms.dir,'roms_output_ltl_statevars_tohydro*',full.names = T)
# ltl.files = paste0(roms.dir,'roms_output_ltl_statevars_tohydro_1980.nc')
# .packages = c("devtools","tidyverse","stringi","RNetCDF", "data.table")
lapply(.packages, require, character.only=TRUE)
source(here::here('R','make_force_ltl.R'))

for(i in 1:length(ltl.files)){
  make.ltl.force(roms.dir = roms.dir,
                 roms.file = ltl.files[i])
  print(i)
}
