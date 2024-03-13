#Changes FSP parameter in at_biology.prm


get_param_FSP = function(bio.prm, group.name = NA){
  
  bio.lines = readLines(bio.prm)
  
  FSP.line = grep(paste0('^FSP_'),bio.lines)
  
  groups = sapply(bio.lines[FSP.line],function(x) return(strsplit(x,'\t| |_')[[1]][2]),USE.NAMES = F)
  
  group.vals = sapply(bio.lines[FSP.line],function(x) strsplit(x,' |\t')[[1]][2], USE.NAMES = F)
  
  if(is.na(group.name)){
    
    return(data.frame(Code = groups, FSP = group.vals))
    
  }else{
    
    return(group.vals[which(groups == group.name)])
  }
  
  return(out.df)
}

edit_param_FSP = function(bio.prm,group.name,value,unit = 'value',overwrite = F, new.file.name){
  
  bio.lines = readLines(bio.prm)
  
  FSP.line = grep(paste0('FSP_',group.name),bio.lines)
  
  if(unit == 'value'){
    new.val = value
  }else{
    old.val = as.numeric(strsplit(bio.lines[FSP.line],' |\t')[[1]][2])
    new.val = old.val * value
  }
  
  bio.lines[FSP.line] = paste0('FSP_',group.name,' ',new.val)

  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}
