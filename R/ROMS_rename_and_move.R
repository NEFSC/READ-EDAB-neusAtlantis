#' Script pulls ROMS-aggregated data (output of ROMS_to_Hydroconstruct) and moves and renames them. 

setwd('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT')

year.dirs = 1981:2014

for(i in 1:length(year.dirs)){
  old.ltl = paste0('ROMS_OUT/',year.dirs[i],'/roms_cobalt_ltl_statevars_2hydro.nc')
  new.ltl = paste0('ROMS_COBALT output/ltl_statevars/roms_output_ltl_statevars_tohydro_',year.dirs[i],'.nc')
  
  old.nut = paste0('ROMS_OUT/',year.dirs[i],'/roms_cobalt_nutvars_2hydro.nc')
  new.nut = paste0('ROMS_COBALT output/nut_statevars/roms_output_nut_statevars_tohydro_',year.dirs[i],'.nc')
  
  file.copy(old.nut,new.nut,overwrite = T)
  file.copy(old.ltl,new.ltl,overwrite=T)
}
