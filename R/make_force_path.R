# Script to generate the paths and formatting needed for atlantis force.prm file

make_force_path = function(force.dirs, ts.dir, years,var.names, pattern.names,out.dir,out.name){
  
  file.ls = list()
  for(v in 1:length(var.names)){
    file.names = list.files(force.dirs[v],pattern = pattern.names[v],full.names = F)
    if(var.names[v] %in% c('hd','Temperature','Salinity')){
      var.file.full = paste0(var.names[v],(1:length(years))-1,'.name ',ts.dir,file.names)
    }else{
      var.file.full = paste0(var.names[v],'_File',(1:length(years))-1,'.name ',ts.dir,file.names)  
    }
    file.ls[[v]] = var.file.full
  }
  out.df = dplyr::bind_cols(file.ls)
  colnames(out.df) = var.names
  write.csv(out.df,file = paste0(out.dir,out.name,'.csv'),row.names = F)
}

### For Hirata Model
#All windows variables
make_force_path(
  var.names = c('hd','Temperature','Salinity',
                'Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S',
                'Lab_Det_N'),
  force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/transport/',
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phys_statevars_alternate/',2),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_dynamic_lower/',4),
                 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/labile_detritus/'
  ),
  years = 1964:2017,
  ts.dir = "\\tsfiles\\Annual_Files\\",
  pattern.names = c('^flow.*\\.nc$',
                    rep('GLORYS_tempsalt_force_*',2),
                    rep('^Phyto_Forcing_.*\\.nc$',4),
                    '^Satphyto_Forcing_DL_.*\\.nc$'),
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/',
  out.name = 'obs_hindcast_forcing_paths_WINDOWS'
)

#All LINUX variables
make_force_path(
  var.names = c('hd','Temperature','Salinity',
                'Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S',
                'Lab_Det_N'),
  force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/transport/',
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phys_statevars_alternate/',2),
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_dynamic_lower/',4),
                 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/labile_detritus/'
  ),
  years = 1964:2017,
  ts.dir = "/tsfiles/Annual_Files/",
  pattern.names = c('^flow.*\\.nc$',
                    rep('GLORYS_tempsalt_force_*',2),
                    rep('^Phyto_Forcing_Hirata_.*\\.nc$',4),
                    '^Satphyto_Forcing_DL_.*\\.nc$'),
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/',
  out.name = 'obs_hindcast_forcing_paths_LINUX'
)
