#Function to edit C/mum parameters from biology.prm
get_param_invert_c_mum = function(bio.file,groups){
  
  bio.lines = readLines(bio.file)
  data.out = data.frame(Code = groups,mum = NA,c = NA)
  
  for(i in 1:length(groups)){
    mum.line = grep(paste0('mum_',groups[i]),bio.lines)
    C.line = grep(paste0('C_',groups[i]),bio.lines)
    
    if(length(mum.line)>0){data.out$mum[i] = strsplit(bio.lines[mum.line],' |\t')[[1]][2]}
    if(length(C.line)>0){data.out$c[i] = strsplit(bio.lines[C.line],' |\t')[[1]][2]}
  }

  return(data.out)
}

edit_param_invert_c_mum = function(bio.file,group,type, value = NA, new.file = F, new.name = NA){
  
  bio.lines = readLines(bio.file)
  
  if(type == 'mum'){
    val.line = grep(paste0('mum_',group),bio.lines) 
    new.val = paste0('mum_',group,'_T15 ',value)
  }else{
    val.line = grep(paste0('C_',group),bio.lines)  
    new.val = paste0('C_',group,'_T15 ',value)
  }

  bio.lines[val.line] = new.val
  
  if(new.file == F){
    writeLines(bio.lines, con = bio.file)
  }else{
    file.copy(bio.file, new.name, overwrite = T)
    writeLines(bio.lines, con = new.name )
  }
  
}

# edit_param_invert_c_mum(bio.file = here::here('currentVersion','at_biology.prm'),
#                         group = 'ZL',
#                         C = NA,
#                         mum = 999,
#                         scalar = 100,
#                         new.file = T,
#                         new.name = here::here('currentVersion','test_bio.prm'))
