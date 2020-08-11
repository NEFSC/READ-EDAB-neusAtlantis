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

#All windows variables
make_force_path(
  var.names = c('hd','Temperature','Salinity'),
  force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/transport/',
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phys_statevars/',2)
                 ),
  years = 1964:2017,
  ts.dir = "\\tsfiles\\Annual_Files\\",
  pattern.names = c('^flow.*\\.nc$',
                    '^temp_.*\\.nc$',
                    '^salt_.*\\.nc$'),
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/',
  out.name = 'obs_hindcast_forcing_paths_WINDOWS'
)

#All LINUX variables
make_force_path(
  var.names = c('hd','Temperature','Salinity'),
  force.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/transport/',
                 rep('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phys_statevars/',2)
  ),
  years = 1964:2017,
  ts.dir = "/tsfiles/Annual_Files/",
  pattern.names = c('^flow.*\\.nc$',
                    '^temp_.*\\.nc$',
                    '^salt_.*\\.nc$'),
  out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/',
  out.name = 'obs_hindcast_forcing_paths_LINUX'
)
