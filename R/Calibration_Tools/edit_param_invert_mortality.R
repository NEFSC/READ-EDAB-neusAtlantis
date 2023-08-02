#Function to edit invertebrate mortality in biology.prm

edit_param_invert_mort = function(bio.file,group, type, value, new.file = F, new.name = NA){
  
  bio.lines = readLines(bio.file)
  mort.line = grep(paste0(group,'_',type),bio.lines)
  
  new.mort = paste0(group,'_',type,' ',value)

  bio.lines[mort.line] = new.mort
  
  if(new.file == F){
    writeLines(bio.lines, con = bio.file)
  }else{
    file.copy(bio.file, new.name, overwrite = T)
    writeLines(bio.lines, con = new.name )
  }

}

get_param_invert_mort = function(bio.file,fgs){
  
  bio.lines = readLines(bio.file)
  fgs.invert = dplyr::filter(read.csv(fgs), NumCohorts == 1 & IsPredator == 1)
  
  mort.out = data.frame(group = fgs.invert$Code, mL = NA, mQ = NA)
  
  for(i in 1:nrow(mort.out)){
    
    ml = bio.lines[grep(paste0(mort.out$group[i],'_mL'),bio.lines)]
    ml.split = strsplit(ml,'\t| ')
    mort.out$mL[i] = ml.split[[1]][2]
    
    mq = bio.lines[grep(paste0(mort.out$group[i],'_mQ'),bio.lines)]
    mq.split = strsplit(mq,'\t| ')
    mort.out$mQ[i] = mq.split[[1]][2]
    
  }
  
  return(mort.out)
}

# edit_param_invert_mort(
#   bio.file = here::here('currentVersion','at_biology.prm'),
#   group = 'CLA',
#   type = 'mQ',
#   value = 999,
#   new.file = T,
#   new.name = here::here('currentVersion','at_biology_test.prm')
# )

