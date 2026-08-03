#' Functions to change min/max move paramters for temperature and salinity
#' 

edit_param_env_move = function(bio.file, group.name,min.val = NA, max.val = NA, var.name, overwrite = T, new.file.name){
  
  bio.lines = readLines(bio.file)
  
  if(tolower(var.name) == 'temperature'){
    
    if(!is.na(min.val)){
      #change min.temp
      which.min.temp = grep(paste0(group.name,'_min_move_temp'),bio.lines)
      bio.lines[which.min.temp] = paste0(group.name,'_min_move_temp ',min.val)
    }
    
    if(!is.na(max.val)){
      #change max.temp
      which.max.temp = grep(paste0(group.name,'_max_move_temp'),bio.lines)
      bio.lines[which.max.temp] = paste0(group.name,'_max_move_temp ',max.val)
    }
    
  }else if(tolower(var.name) == 'salinity'){
    
    if(!is.na(min.val)){
      #change min.salt
      which.min.salt = grep(paste0(group.name,'_min_move_salt'),bio.lines)
      bio.lines[which.min.salt] = paste0(group.name,'_min_move_salt ',min.val)  
    }
    
    if(!is.na(max.val)){
      #change max.salt
      which.max.salt = grep(paste0(group.name,'_max_move_salt'),bio.lines)
      bio.lines[which.max.salt] = paste0(group.name,'_max_move_salt ',max.val)  
    }
  
  }else{
    stop('var.name needs to be either "temperature" or "salinity"')
  }
  
  if(overwrite == T){
    writeLines(bio.lines,bio.file)
  }else{
    file.copy(bio.file,new.file.name,overwrite = T)
    writeLines(bio.lines,new.file.name)
  }
}

edit_param_env_move(  bio.file = here::here('currentVersion','at_biology.prm'),
                      new.file.name = here::here('currentVersion','at_biology_test.prm'),
                      min.val = 1,
                      max.val = 1,
                      group.name = 'GOO',
                      var.name = 'temperature',
                      overwrite = F)
