#Function to edit Beverto-Holt Alpha and Beta parameters in biology.prm
#group.names, alpha, and beta are vectorized

edit_param_BH = function(bio.prm,group.name,alpha,beta,overwrite = F, new.file.name){
  
  bio.lines = readLines(bio.prm)
  for( i in 1:length(group.name)){
    
    alpha.line = grep(paste0('BHalpha_',group.name[i]),bio.lines)
    beta.line = grep(paste0('BHbeta_',group.name[i]),bio.lines)
    
    new.alpha = paste0('BHalpha_',group.name[i],' ',alpha[i])
    new.beta = paste0('BHbeta_',group.name[i],' ',beta[i])
    
    bio.lines[alpha.line] = new.alpha
    bio.lines[beta.line] = new.beta
  }
  
  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
  
  
}

#Example

# edit_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'),
#               group.name = c('MAK','HER'),
#               alpha = c(3.7E9,3E10),
#               beta = c(7.56E13,3E10),
#               overwrite = F,
#               new.file.name = here::here('currentVersion','at_biology_test.prm'))