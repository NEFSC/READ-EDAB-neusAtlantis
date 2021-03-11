#Function to edit invertebrate mortality in biology.prm

edit_param_invert_mort = function(bio.file,group, type, value, new.file = F, new.name = NA){
  
  bio.lines = readLines(bio.file)
  mort.line = grep(paste0(group,'_',type),bio.lines)
  
  new.mort = paste0(group,'_',type,' ',value)

  bio.lines[mort.line] = new.mort
  
  if(new.file == F){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.file, new.name, overwrite = T)
    writeLines(bio.lines, con = new.name )
  }

}

# edit_param_invert_mort(
#   bio.file = here::here('currentVersion','at_biology.prm'),
#   group = 'CLA',
#   type = 'mQ',
#   value = 999,
#   new.file = T,
#   new.name = here::here('currentVersion','at_biology_test.prm')
# )

