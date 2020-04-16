

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/'
# roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
# roms.file = paste0(roms.dir,'roms_output_ltl_statevars_tohydro_1964.nc')


tempsalt.files = list.files(roms.dir,'roms_output_statevars_tohydro*',full.names = T)
.packages = c("devtools","tidyverse","stringi","RNetCDF", "data.table")
lapply(.packages, require, character.only=TRUE)
source(here::here('R','make_statevar_forcing.R'))

for(i in 1:length(tempsalt.files)){
  make.tempsalt.force(roms.dir = roms.dir,
                 roms.file = tempsalt.files[i])
  print(i)
}
