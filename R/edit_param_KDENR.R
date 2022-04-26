#Function to edit KDENR fixed recruitment parameters in biology.prm
#group.names and KDENR are vectorized

edit_param_KDENR = function(bio.prm,group.name,KDENR,overwrite = F, new.file.name){
  
  bio.lines = readLines(bio.prm)
  for( i in 1:length(group.name)){
    
    kdenr.line = grep(paste0('KDENR_',group.name[i]),bio.lines)
    
    bio.lines[kdenr.line+1] = KDENR[i]
    
  }
  
  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}

get_param_KDENR = function(bio.prm){
  
  bio.lines = readLines(bio.prm)
  
  kdenr.line = grep(paste0('^KDENR_'),bio.lines)
  
  groups = sapply(bio.lines[kdenr.line],function(x) return(strsplit(x,'\t| |_')[[1]][2]),USE.NAMES = F)
  is.invert = sapply(bio.lines[kdenr.line],function(x) return(ifelse(strsplit(x,'\t| |_')[[1]][3]==2,T,F)),USE.NAMES = F)
  
  out.df = data.frame(group = groups,KDENR = NA,KDENR.2 = NA)
  
  for(i in 1:length(groups)){
    
    vals = strsplit(bio.lines[kdenr.line[i]+1],' |\t')[[1]]
    out.df$KDENR[i] = vals[1]
    
    if(is.invert[i] == T & !is.na(is.invert[i])){
      
      out.df$KDENR.2[i] = vals[2]
      
    }
    
  }

  return(out.df)
}


#Example

# edit_param_KDENR(bio.prm = here::here('currentVersion','at_biology.prm'),
#               group.name = c('MAK','HER'),
#               KDENR = c(0,0),
#               overwrite = F,
#               new.file.name = here::here('currentVersion','at_biology_test.prm'))

# get_param_KDENR(bio.prm = here::here('currentVersion','at_biology.prm'))

