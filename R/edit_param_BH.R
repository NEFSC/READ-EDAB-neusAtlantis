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

get_param_BH = function(bio.prm){
  
  bio.lines = readLines(bio.prm)
  
  alpha.line = bio.lines[grep(paste0('BHalpha_'),bio.lines)]
  beta.line = bio.lines[grep(paste0('BHbeta_'),bio.lines)]
  
  alpha.group = sapply(alpha.line,function(x) strsplit(x,'_| ')[[1]][2],USE.NAMES = F)
  alpha.vals =  sapply(alpha.line,function(x){
    dum = strsplit(x,'_| ')[[1]]
    return(dum[2+which(dum[-c(1:2)] != '')])
  },USE.NAMES = F) 
  
  alpha.df = data.frame(group = alpha.group,alpha = alpha.vals)
  
  beta.group = sapply(beta.line,function(x) strsplit(x,'_| ')[[1]][2],USE.NAMES = F)
  beta.vals =  sapply(beta.line,function(x){
    dum = strsplit(x,'_| ')[[1]]
    return(dum[2+which(dum[-c(1:2)] != '')])
  },USE.NAMES = F) 
  
  beta.df = data.frame(group = beta.group,beta = beta.vals)
  
  out.df = dplyr::left_join(alpha.df,beta.df)
  
  return(out.df)
}


#Example

# edit_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'),
#               group.name = c('MAK','HER'),
#               alpha = c(3.7E9,3E10),
#               beta = c(7.56E13,3E10),
#               overwrite = F,
#               new.file.name = here::here('currentVersion','at_biology_test.prm'))

# get_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'))
