#Function to change the q_XXX parameters in at_harvest.prm

# harvest.file = here::here('currentVersion','at_harvest.prm')
# Code = 'COD'
# Fleet = 'gfgloucester'
# fleets.file = here::here('currentVersion','neus_fisheries.csv')
# overwrite = F
# new.file.name = here::here('currentVersion','at_harvest_test.prm')
# Value = 2

edit_param_q = function(harvest.file, Code, Fleet,fleets.file,Value,overwrite,new.file.name){
  
  fisheries = read.csv(fleets.file,as.is =T)
  
  harvest.lines = readLines(harvest.file)
  
  q.name = paste0('q_',Code)
  which.q = grep(q.name,harvest.lines)
  
  which.fleet = which(fisheries$Code == Fleet)
  
  orig.vals = strsplit(harvest.lines[which.q+1],' |\t')[[1]]
  
  new.vals = orig.vals
  new.vals[which.fleet] = Value
  new.vals=paste(new.vals,collapse = ' ')
  
  harvest.lines[which.q+1] = new.vals
  
  if(overwrite == T){
    writeLines(harvest.lines,harvest.file)
  }else{
    file.copy(harvest.file,new.file.name,overwrite =T)
    writeLines(harvest.lines,new.file.name)
  }
}