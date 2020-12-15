#' Functions to pull assimilation efficiency and compare across models
#' 
#' @atl.dir
#' @bio.file
#' @spp.names

# atl.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
# bio.file = 'at_biology.prm'
# spp.names = c('MEN','MAK')
# 
# 
# model.1.dir = atl.dir
# model.2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/'
# bio.1 = bio.file
# bio.2 = 'at_biol_neus_v15_scaled_diet_20181126_3.prm'

get_param_assim = function(atl.dir,bio.file,spp.names){
  
  con = file(paste0(atl.dir,bio.file))
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

compare_param_assim = function(model.1.dir,model.2.dir,bio.1,bio.2,spp.names,rm.same=F){
  
  library(dplyr)
  
  #Get model 1 assimilation efficiencies
  model.1.assim = get_param_assim(atl.dir = model.1.dir,
                                  bio.file = bio.1,
                                  spp.names = spp.names)
  
  #Get model 2 assimilation efficiencies
  model.2.assim = get_param_assim(atl.dir = model.2.dir,
                                  bio.file = bio.2,
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

#Example
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  dplyr::filter(NumCohorts >2)

compare_param_assim(model.1.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/',
  model.2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/',
  bio.1 = 'at_biology.prm',
  bio.2 = 'at_biol_neus_v15_scaled_diet_20181126_3.prm',
  rm.same = T,
  spp.names = fgs$Code)
