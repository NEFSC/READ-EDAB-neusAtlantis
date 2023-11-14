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

edit_param_invert_c_mum = function(bio.file,group,C = NA, mum = NA, scalar = 10,new.file = F, new.name = NA){
  
  bio.lines = readLines(bio.file)
  mum.line = grep(paste0('mum_',group),bio.lines)
  C.line = grep(paste0('C_',group),bio.lines)
  
  if(!is.na(C) & !is.na(mum)){
    new.mum = paste0('mum_',group,'_T15 ',mum)
    new.C = paste0('C_',group,'_T15 ',C)
  }else if(!is.na(C) & is.na(mum)){
    new.C = paste0('C_',group,'_T15 ',C)
    new.mum = paste0('mum_',group,'_T15 ',C*scalar)
  }else if(is.na(C) & !is.na(mum)){
    new.C = paste0('C_',group,'_T15 ',mum/scalar)
    new.mum = paste0('mum_',group,'_T15 ',mum)
  }
  
  bio.lines[mum.line] = new.mum
  bio.lines[C.line] = new.C
  
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
