#Script to edit catch-at-age parameters

edit_param_catch_age = function(harvest.file,group,new.value=NA,max.prop=NA, min.prop = NA,overwrite = F, new.file.name = NA){
  
  if(!is.na(new.value) & sum(new.value) != 1){
    stop('Catch at age proportions must sum to 1')
  }  

  harvest.lines = readLines(harvest.file)
  
  catchTS.line = grep(paste0('CatchTS_agedistrib',group),harvest.lines)
  
  ages = 0:9
  
  if(is.na(new.value)){
      if(!is.na(max.prop) & !is.na(min.prop)){
      stop('Specify min OR max catch-at-age. Not both')
    }else if(!is.na(max.prop)){
      if(max.prop > 0.632){
        warning('Maximum catch at age for exponential curve is 0.63')
      }
      a = (1-10*max.prop)/-68212
      b = max.prop - (a*exp(9))
    }else {
      a = (1-(10*min.prop))/12808
      b = min.prop - a
    }
    
    if(b < 0){
      new.catch.prop = a*exp(ages)+b+abs(b)
      new.catch.prop = signif(new.catch.prop/sum(new.catch.prop),2)
    }else{
      new.catch.prop = signif(a*exp(ages)+b,2)
      new.catch.prop = signif(new.catch.prop/sum(new.catch.prop),2)  
    }
  }else{
    new.catch.prop = new.value
  }
  
  harvest.lines[catchTS.line+1] = paste0(new.catch.prop,collapse = '\t')
  
  #overwrite or make copy of biology file
  if(overwrite){
    writeLines(harvest.lines, con = harvest.file)
  }else{
    file.copy(harvest.file, new.file.name, overwrite = T)
    writeLines(harvest.lines, con = new.file.name )
  }
  
  # plot(0,0,xlim=c(0,10),ylim = c(0,1),type = 'n')
  # max.prop.ls = min.prop.ls = numeric()
  # for(i in 1:10000){
  #   max.prop = runif(1,0,1)
  #   a = (1-10*max.prop)/-68212
  #   b = max.prop - (a*exp(9))
  #   # if(is.na(b)){
  #     new.prop = a*exp(ages)+b+(abs(b))
  #     new.prop = new.prop/sum(new.prop)
  #     lines(ages,new.prop,col = 'red')
  #   # }else{
  #   #   new.prop = a*exp(ages)+b
  #   #   lines(ages,new.prop,col = ifelse(b<0,'red', 'blue'))
  #   # }
  #   max.prop.ls[i] = max(new.prop)
  #   min.prop.ls[i] = min(new.prop)
  # }
  # max(max.prop.ls)
  # min(min.prop.ls)
  # hist(max.prop.ls)

}

get_param_catch_age = function(harvest.file){
  
  harvest.lines = readLines(harvest.file)
  
  catchTS.line = grep('CatchTS_agedistrib',harvest.lines)
  
  catchTS.names = harvest.lines[catchTS.line]
  catchTS.line.vals = harvest.lines[catchTS.line+1]
  
  group.names = sapply(catchTS.names,function(x) return(strsplit(x,'CatchTS_agedistrib|\t| ')[[1]][2]),USE.NAMES = F)
  vals.mat = do.call('rbind',lapply(catchTS.line.vals,function(x) return(as.numeric(strsplit(x,'\t| ')[[1]]))))
  out.df = as.data.frame(vals.mat)
  colnames(out.df) = paste0('age.',1:10)
  out.df = cbind(group.names,out.df)
  return(out.df)
}

# edit_param_catch_age(harvest.file = here::here('currentVersion','at_harvest.prm'),
#                      group = 'COD',
#                      new.value = c(0,0,.05,.05,.1,.1,.15,.15,.2,.2),
#                      min.prop = NA,
#                      max.prop = NA,
#                      overwrite = F, 
#                      new.file.name = here::here('currentVersion','at_harvest_test.prm')
#                      
#                      
# )
