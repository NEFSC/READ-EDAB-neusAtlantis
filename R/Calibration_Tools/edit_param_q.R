#Function to change the q_XXX parameters in at_harvest.prm

# harvest.file = here::here('currentVersion','at_harvest.prm')
# Code = 'COD'
# Fleet = 'gfgloucester'
# fleets.file = here::here('currentVersion','neus_fisheries.csv')
# overwrite = F
# new.file.name = here::here('currentVersion','at_harvest_test.prm')
# Value = 2

edit_param_q = function(harvest.file, Code, Fleet,fleets.file,Type,Value,overwrite,new.file.name){
  
  fisheries = read.csv(fleets.file,as.is =T)
  
  harvest.lines = readLines(harvest.file)
  
  q.name = paste0('q_',Code)
  which.q = grep(q.name,harvest.lines)
  
  which.fleet = which(fisheries$Code == Fleet)
  
  orig.vals = strsplit(harvest.lines[which.q+1],' |\t')[[1]]
  
  new.vals = as.numeric(orig.vals)
  
  if(Type == 'scalar'){
    new.vals[which.fleet] = Value * new.vals[which.fleet]
  }else{
    new.vals[which.fleet] = Value
  }
  vals.1 = which(new.vals>1)
  if(length(vals.1)>0){
    new.vals[vals.1] = 1
  }
  
  new.vals=paste(new.vals,collapse = ' ')
  
  harvest.lines[which.q+1] = new.vals
  
  if(overwrite == T){
    writeLines(harvest.lines,harvest.file)
  }else{
    file.copy(harvest.file,new.file.name,overwrite =T)
    writeLines(harvest.lines,new.file.name)
  }
}

get_param_q = function(harvest.file,fleets.file, fgs.file ){

  fleets = read.csv(fleets.file)  
  fgs = read.csv(fgs.file)
  
  harvest.lines = readLines(harvest.file)
  
  fleet.names = fleets$Code
  
  out.ls = list()
  for(i in 1:length(fgs$Code)){
  
    q.line = harvest.lines[grep(paste0('^q_',fgs$Code[i]),harvest.lines)+1]
    q.vals = strsplit(q.line,split = ' |\t')[[1]] |> as.numeric()
    
    out.ls[[i]] = data.frame(Code = fgs$Code[i],fleet = fleet.names, q  = q.vals)
    
  }
  
  return(dplyr::bind_rows(out.ls))
  
}
