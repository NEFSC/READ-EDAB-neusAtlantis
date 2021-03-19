#Function to edity mortality for age-structured groups in Biology.prm

edit_param_mort_age = function(bio.prm, new.mort,type, overwrite = F,new.file.name, single.group=F, group.name = NA ){
  
  #Get mum_XXX bio.prm lines
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep(paste0('_',type),bio.lines)
  bio.lines.vals = bio.lines[bio.lines.id]
  group.names =unname(sapply(bio.lines.vals,function(x) strsplit(x,paste0('_',type))[[1]][1]))
  
  if(single.group){
    ind = which(group.name == group.names)
    mort.string = paste(new.mort,collapse='\t')
    bio.lines[bio.lines.id[ind]+1] = mort.string
  }else{
    for(i in 1:nrow(new.mort)){
      
      ind = which(new.mort[i,1] == group.names)
      mort.string = paste(new.mort[i,2:3],collapse = '\t')
      bio.lines[bio.lines.id[ind]+1] = mort.string
    }
    
  }
  #overwrite or make copy of biology file
  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}

# edit_param_mort_age(
#   bio.prm = here::here('currentVersion','at_biology.prm'),
#   single.group = T,
#   new.mort = c(111,111),
#   group.name = 'HER',
#   # new.mort = data.frame(group = c('MAK','HER'), val1 = c(999,999),val2 = c(999,999)),
#   type = 'mL',
#   overwrite =F, 
#   new.file.name = here::here('currentVersion','at_biology_test.prm')
# )


