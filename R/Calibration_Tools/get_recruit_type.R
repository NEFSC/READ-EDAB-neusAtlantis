get_recruit_type = function(bio.prm){
  
  bio.lines = readLines(bio.prm)
  
  flag.lines = grep('^flagrecruit*',bio.lines)
  
  flag.group = sapply(bio.lines[flag.lines],function(x) strsplit(x,'flagrecruit| |\t')[[1]][2],USE.NAMES = F)
  
  flag.vals = sapply(bio.lines[flag.lines],function(x) strsplit(x,'flagrecruit| |\t')[[1]][3],USE.NAMES = F)
  
  out.df = data.frame(group = flag.group,flagrecruit = flag.vals)
  return(out.df)
}
