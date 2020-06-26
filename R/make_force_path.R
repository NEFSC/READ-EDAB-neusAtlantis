# Script to generate the paths and formatting needed for atlantis force.prm file

make_force_path = function(force.dirs, ts.dir, years,var.names, pattern.names,out.dir,out.name){
  
  file.ls = list()
  for(v in 1:length(var.names)){
    file.names = list.files(force.dirs[v],pattern = pattern.names[v],full.names = F)
    var.file.full = paste0(var.names[v],'_File',(1:length(years))-1,'.name ',ts.dir,file.names)
    file.ls[[v]] = var.file.full
  }
  out.df = dplyr::bind_cols(file.ls)
  colnames(out.df) = var.names
  write.csv(out.df,file = paste0(out.dir,out.name,'.csv'),row.names = F)
}

#All windows variables
make_force_path(
  var.names = c('hd','Temperature','Salinity',
                "Carniv_Zoo_N","Zoo_N","PicoPhytopl_N","MicroZoo_N","Pelag_Bact_N",
                'Diatom_N','Diatom_S','DinoFlag_N',
                "NH3","NO3","Oxygen","Si"  
                ),
  force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/transport/',
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/',2),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/',5),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/largephyto_statevars/',3),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/',4)
  ),
  years = 1964:2014,
  ts.dir = "\\tsfiles\\Annual_Files\\",
  pattern.names = c('^flow.*\\.nc$',
                    rep('^roms_tempsalt.*\\.nc$',2),
                    rep('^roms_ltl_force.*\\.nc$',5),
                    rep('^roms_largephyto_force.*\\.nc$',3),
                    rep('^roms_nut_force.*\\.nc$',4)
  ),
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/',
  out.name = 'cobalt_v10_forcing_paths_WINDOWS'
)

#All LINUX variables
make_force_path(
  var.names = c('hd','Temperature','Salinity',
                "Carniv_Zoo_N","Zoo_N","PicoPhytopl_N","MicroZoo_N","Pelag_Bact_N",
                'Diatom_N','Diatom_S','DinoFlag_N',
                "NH3","NO3","Oxygen","Si"  
  ),
  force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/transport/',
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevar_alternate/',2),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/',5),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/largephyto_statevars/',3),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/',4)
  ),
  years = 1964:2014,
  ts.dir = "/tsfiles/Annual_Files/",
  pattern.names = c('^flow.*\\.nc$',
                    rep('^roms_tempsalt.*\\.nc$',2),
                    rep('^roms_ltl_force.*\\.nc$',5),
                    rep('^roms_largephyto_force.*\\.nc$',3),
                    rep('^roms_nut_force.*\\.nc$',4)
  ),
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/',
  out.name = 'cobalt_v10_forcing_paths_LINUX'
)

#All windows variables (Hydroconstruct temp/salt)
# make_force_path(
#   var.names = c('hd','Temperature','Salinity',
#                 "Diatom_N","Carniv_Zoo_N","Zoo_N","PicoPhytopl_N","MicroZoo_N","Diatom_S","Pelag_Bact_N",
#                 "NH3","NO3","Oxygen","Si"  
#   ),
#   force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/transport/',
#                  rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars/',2),
#                  rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/',7),
#                  rep('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/',4)
#   ),
#   years = 1964:2014,
#   ts.dir = "\\tsfiles\\Annual_Files\\",
#   pattern.names = c('^flow.*\\.nc$',
#                     '^temp.*\\.nc$','^salt.*\\.nc$',
#                     rep('^roms_ltl_force.*\\.nc$',7),
#                     rep('^roms_nut_force.*\\.nc$',4)
#   ),
#   out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/',
#   out.name = 'cobalt_v10_forcing_paths_WINDOWS_alt'
# )