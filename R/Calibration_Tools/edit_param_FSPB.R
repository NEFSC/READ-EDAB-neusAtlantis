#Function to edit FSPB (proportion mature at age) parameters in biology.prm

get_param_FSPB = function(bio.prm){
  
  bio.lines = readLines(bio.prm)
  
  FSPB.line = grep(paste0('^FSPB_'),bio.lines)
  
  groups = sapply(bio.lines[FSPB.line],function(x) return(strsplit(x,'\t| |_')[[1]][2]),USE.NAMES = F)
  
  out.df = as.data.frame(matrix(NA,nrow = length(groups), ncol = 11))
  colnames(out.df) = c('group',paste0('age.',1:10))
  out.df$group = groups
  
  for(i in 1:length(groups)){
       vals = strsplit(bio.lines[FSPB.line[i]+1],' |\t')[[1]]
    out.df[i,2:(length(vals)+1)] = vals
  }
  
  return(out.df)
}

edit_param_FSPB = function(bio.prm,group.name,FSPB,overwrite = F, new.file.name){
  
  bio.lines = readLines(bio.prm)
  for( i in 1:length(group.name)){
    
    FSPB.line = grep(paste0('FSPB_',group.name[i]),bio.lines)
    
    if(length(group.name) == 1){
      new.val = FSPB
    }else{
      new.val = FSPB[i,]
    }
    bio.lines[FSPB.line+1] = paste0(new.val,collapse= ' ')
    
  }
  
  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}



#Example

# edit_param_FSPB(bio.prm = here::here('currentVersion','at_biology.prm'),
#               group.name = c('MAK','HER'),
#               FSPB = matrix(c(rep(1,10),rep(1,10)),nrow = 2, ncol = 10),
#               overwrite = F,
#               new.file.name = here::here('currentVersion','at_biology_test.prm'))

# get_param_KDENR(bio.prm = here::here('currentVersion','at_biology.prm'))

