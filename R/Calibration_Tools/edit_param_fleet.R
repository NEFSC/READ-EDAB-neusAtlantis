#Function to edit fleet-related parameters: selcurve, tstart, mindepth, maxdepth, and sweptarea

# harvest.file = here::here('currentVersion','at_harvest.prm')
# Code = 'COD'
# Fleet = 'gfgloucester'
# fleets.file = here::here('currentVersion','neus_fisheries.csv')
# overwrite = F
# new.file.name = here::here('currentVersion','at_harvest_test.prm')
# VarName = 'sweptarea'
# Value = 2

edit_param_fleet = function(harvest.file, Fleet, VarName, Value, Unit, overwrite, new.file.name){
  
  harvest.lines = readLines(harvest.file)
  
  which.var = grep(paste0('\\b',Fleet,'_',VarName,'\\b',"|\\b",Fleet,VarName,'\\b'),harvest.lines)
  var.full = strsplit(harvest.lines[which.var],split = '\t| ')[[1]][1]
  
  if(Unit == 'value'){
    new.val = paste0(var.full,' ',Value)  
  }else{
    old.line =grep(paste0('\\b',var.full,'\\b'),harvest.lines, value = T)
    old.line.val = strsplit(old.line,split= ' |\t')[[1]]
    old.line.val = as.numeric(old.line.val[which(old.line.val != '')][2])
    new.val = paste0(Fleet,'_',VarName,' ',signif(Value * old.line.val,2))
  }
  
  harvest.lines[which.var] = new.val
  
  if(overwrite == T){
    writeLines(harvest.lines, harvest.file)
  }else{
    file.copy(harvest.file, new.file.name,overwrite =T)
    writeLines(harvest.lines,new.file.name)
  }
}