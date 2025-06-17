#Function to edit fleet-related parameters: selcurve, tstart, mindepth, maxdepth, and sweptarea

# harvest.file = here::here('currentVersion','at_harvest.prm')
# Code = 'COD'
# Fleet = 'gfgloucester'
# fleets.file = here::here('currentVersion','neus_fisheries.csv')
# overwrite = F
# new.file.name = here::here('currentVersion','at_harvest_test.prm')
# VarName = 'sweptarea'
# Value = 2

edit_param_fleet = function(harvest.file, Fleet, VarName, Value, overwrite, new.file.name){
  
  harvest.lines = readLines(harvest.file)
  
  which.var = grep(paste0(Fleet,'_',VarName),harvest.lines)
  
  new.val = paste0(Fleet,'_',VarName,' ',Value)
  
  harvest.lines[which.var] = new.val
  
  if(overwrite == T){
    writeLines(harvest.lines, harvest.file)
  }else{
    file.copy(harvest.file, new.file.name,overwrite =T)
    writeLines(harvest.lines,new.file.name)
  }
}