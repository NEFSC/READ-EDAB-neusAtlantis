#Function to edit C/mum parameters from biology.prm
get_param_invert_c_mum = function(bio.file,group){
  
  bio.lines = readLines(bio.file)
  mum.line = grep(paste0('mum_',group),bio.lines)
  C.line = grep(paste0('C_',group),bio.lines)
  
  mum.out = strsplit(bio.lines[mum.line],' |\t')[[1]][2]
  c.out = strsplit(bio.lines[C.line],' |\t')[[1]][2]
  
  return(data.frame(mum = as.numeric(mum.out), c = as.numeric(c.out),stringsAsFactors = F))
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
