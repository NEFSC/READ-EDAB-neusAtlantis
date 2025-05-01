
# harvest.file = here::here('currentVersion','at_harvest.prm')
# Code = 'COD'
# Fleet = 'gfgloucester'
# fleets.file = here::here('currentVersion','neus_fisheries.csv')
# overwrite = F
# new.file.name = here::here('currentVersion','at_harvest_test.prm')
# Value = 2
# VarName = 'flagescapement'

edit_param_escape = function(harvest.file, Code, Fleet,VarName,fleets.file,Value,overwrite,new.file.name){
  
  fisheries = read.csv(fleets.file,as.is =T)
  
  harvest.lines = readLines(harvest.file)
  
  var.str = paste0(VarName,'_',Code)
  which.var = grep(var.str,harvest.lines)
  
  which.fleet = match(Fleet, fisheries$Code)
  
  orig.vals = strsplit(harvest.lines[which.var+1],' |\t')[[1]]
  
  new.vals = orig.vals
  new.vals[which.fleet] = Value
  new.vals=paste(new.vals,collapse = ' ')
  
  harvest.lines[which.var+1] = new.vals
  
  if(overwrite == T){
    writeLines(harvest.lines,harvest.file)
  }else{
    file.copy(harvest.file,new.file.name,overwrite =T)
    writeLines(harvest.lines,new.file.name)
  }
}