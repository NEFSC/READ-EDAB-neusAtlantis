#' Functions to pull assimilation efficiency and compare across models
#' 
#' bio.file = biology.prm file
#' type = Assimilation effiency type (E, EPlant, EDL, EDR)
#' unit = 'scalar' or 'value'



get_param_assim = function(bio.file,spp.names){
  
  con = file(bio.file)
  bio.lines = readLines(con)
  close.connection(con)
  
  assim.prefixes = c('E','EPlant','EDL','EDR')
  assim.df.ls = list()
  for(i in 1:length(assim.prefixes)){
    
    assim.params = paste0(assim.prefixes[i], '_',spp.names)
    assim.lines = sapply(assim.params,function(x) grep(paste0('^\\b',x,'\\b'),bio.lines),USE.NAMES = F)
    assim.vals = bio.lines[assim.lines]
    assim.df.ls[[i]] = data.frame(spp = spp.names,
                          param = assim.prefixes[i],
                          value = sapply(assim.vals,function(x) strsplit(x,' |\t')[[1]][2],USE.NAMES = F),
                          stringsAsFactors = F)
  }
  assim.df = dplyr::bind_rows(assim.df.ls)
  return(assim.df)
}

#Function to compare assimilation parameters between two models

compare_param_assim = function(bio.1,bio.2,spp.names,rm.same=F){
  
  library(dplyr)
  
  #Get model 1 assimilation efficiencies
  model.1.assim = get_param_assim(bio.file = bio.1,
                                  spp.names = spp.names)
  
  #Get model 2 assimilation efficiencies
  model.2.assim = get_param_assim(bio.file = bio.2,
                                  spp.names = spp.names)
  
  #Change column names
  colnames(model.1.assim)[3] = 'value.1'
  colnames(model.2.assim)[3] = 'value.2'
  
  #Combine datasets
  both.model.df = model.1.assim %>%
    left_join(model.2.assim, by =c('spp','param')) %>%
    mutate(value.1 = as.numeric(value.1),
           value.2 = as.numeric(value.2),
           diff = value.1-value.2)
  
  if(rm.same){
    both.model.df = both.model.df %>% 
      filter(diff !=0)
  }
  
  return(both.model.df)
}

# bio.file  = here::here('currentVersion','at_biology.prm')
# spp.names = c('HER','MAK')
# type = 'E'
# value = 10
# unit = 'scalar'
# overwrite = F
# new.file.name = here::here('currentVersion','at_biology_test.prm')
edit_param_assim_eff = function(bio.file,spp.names,type,value,unit ='value',overwrite = F,new.file.name){
  
  bio.lines = readLines(bio.file)
  if(unit == 'scalar'){
    assim.orig = get_param_assim(bio.file = bio.file,spp.names = spp.names)
  }
  
  i=1
  for(i in 1:length(spp.names)){
    param.str = paste0(type,'_',spp.names[i])
    
    line.match = grep(param.str,bio.lines)  
    
    if(unit == 'scalar'){
      old.val = as.numeric(assim.orig$value[which(assim.orig$spp == spp.names[i] & assim.orig$param == type)])
      new.val = old.val * value
    }else{
      new.val = value
    }
    new.str = paste0(param.str,' ',new.val)
    
    bio.lines[line.match] = new.str
  }
  
  if(overwrite){
    writeLines(bio.lines, con = bio.file)
  }else{
    file.copy(bio.file, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}
  
#Example
# fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
#   dplyr::filter(NumCohorts >2)
# 
# compare_param_assim(model.1.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/',
#   model.2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/',
#   bio.1 = 'at_biology.prm',
#   bio.2 = 'at_biol_neus_v15_scaled_diet_20181126_3.prm',
#   rm.same = T,
#   spp.names = fgs$Code)
