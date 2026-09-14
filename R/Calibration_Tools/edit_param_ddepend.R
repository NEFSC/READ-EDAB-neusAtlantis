#'Functions to edit densitiy dependent parameters
#'@var.name parameter name either: ddepend, enviro.kill, enviro.displace, speed, roc.wgt, or k.roc.food

edit_param_ddepend = function(bio.file,group.name = NA,var.name, value,overwrite,new.file.name){
  
  bio.lines = readLines(bio.file)
  
  if(var.name == 'ddepend'){
    #change ddepend
    which.ddepend = grep(paste0(group.name,'_ddepend_move'),bio.lines)
    bio.lines[which.ddepend]  = paste0(group.name,'_ddepend_move ',value)
  }
  if(var.name == 'roc.wgt'){
    #change roc.wgt
    which.roc.wgt = grep('roc_wgt',bio.lines)
    bio.lines[which.roc.wgt] = paste0('roc_wgt ',value)
    
  }
  if(var.name == 'k.roc.food'){
   
    #change k.roc.food
    which.k.roc.food = grep('k_roc_food',bio.lines)
    bio.lines[which.k.roc.food] = paste0('k_roc_food ',value)
    
  }
  
  if(var.name == 'enviro.kill'){
    
    #change enviro.kill
    which.enviro.kill = grep('flagenviro_kill',bio.lines)
    bio.lines[which.enviro.kill] = paste0('flagenviro_kill ',value)
    
  }
  
  if(var.name == 'enviro.displace'){
    
    #change enviro.kill
    which.enviro.displace = grep('flagenviro_displace',bio.lines)
    bio.lines[which.enviro.displace] = paste0('flagenviro_displace ',value)
    
  }
  
  if(var.name == 'speed'){
    
    which.speed = grep(paste0('Speed_',group.name),bio.lines)
    bio.lines[which.speed] = paste0('Speed_',group.name,' ',value)
  }
  
  if(overwrite == T){
    writeLines(bio.lines,bio.file)
  }else{
    file.copy(bio.file,new.file.name,overwrite = T)
    writeLines(bio.lines,new.file.name)
  }
}
  
#test
# edit_param_ddepend(
#   bio.file = here::here('currentVersion','at_biology.prm'),
#   new.file.name = here::here('currentVersion','at_biology_test.prm'),
#   value = 1,
#   group.name = 'GOO',
#   var.name = 'ddepend',
#   overwrite = F
# )
