#script to get seasonal migration parameters FXXX_SY

get.move = function(bio.file,fgs,nbox = 30){
  library(dplyr)
  
  fgs = read.csv(fgs)
  
  #Get groups and age class
  groups = fgs$Code
  is.age = fgs$NumCohorts > 1 
  
  seasons = 1:4
  
  #read in lines
  bio.lines = readLines(bio.file)
  
  find.line = function(x){
    line.match = grep(paste0('^',x,'\\b'),bio.lines,ignore.case = T)
    if(length(line.match) == 0){
      return(NULL)
    }else{
      line.vals = as.numeric(strsplit(bio.lines[line.match + 1],'\t| ')[[1]])[1:nbox]  
    }
    return(line.vals)
  }
  
  #loop over species
  spp.ls = list()
  for(i in 1:length(groups)){
    
    #construct parameters
    if(is.age[i] == 1){
      param.names = c(c(sapply(seasons, function(x) paste0('F',groups[i],'_S',x))),
                      c(sapply(seasons, function(x) paste0('F',groups[i],'_S',x,'juv'))))
      season.id = rep(1:4,2)
    }else{
      param.names = c(sapply(seasons, function(x) paste0('F',groups[i],'_S',x)))
      season.id = rep(1:4)
    }
    
    spp.vals =  sapply(param.names,find.line)
    
    if(all(sapply(spp.vals,is.null))){
      spp.ls[[i]] = NULL
    }else{
      spp.ls[[i]] = spp.vals %>%
        as.data.frame() %>%
        mutate(box = 0:(nbox-1))%>%
        reshape2::melt(id.vars = 'box')%>%
        mutate(group = groups[i],
               phase = ifelse(grepl('juv$',variable)&is.age[i]==1,'juv','adult'),
               season = rep(season.id,each = nbox))
    }
    # print(groups[i])
  }
  return(dplyr::bind_rows(spp.ls))
}

move.params = get.move(fgs = here::here('currentVersion','neus_groups.csv'),
                       bio.file = here::here('currentVersion','at_biology.prm')
                       )
write.csv(move.params, here::here('data-raw','seasonal_movements.csv'),row.names = F)

