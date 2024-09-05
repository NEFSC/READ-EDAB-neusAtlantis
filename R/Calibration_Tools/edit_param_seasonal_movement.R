#script to get seasonal migration parameters FXXX_SY

get_param_seasonal_movement = function(bio.file,fgs.file,nbox = 30){
  library(dplyr)
  
  fgs = read.csv(fgs.file)
  
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
        mutate(Code = groups[i],
               phase = ifelse(grepl('juv$',variable)&is.age[i]==1,'juv','adult'),
               season = rep(season.id,each = nbox))
    }
    # print(groups[i])
  }
  return(dplyr::bind_rows(spp.ls))
}

edit_param_seasonal_movement = function(bio.file,fgs.file,move.val,Code,phase,season,overwrite = F,rescale = F, new.file.name){
  
  bio.lines = readLines(bio.file)
  
  if(phase == 'adult'){
    param.name = paste0('F',Code,'_S',season)
  }else if(phase == 'juv'){
    param.name = paste0('F',Code,'_S',season,'juv')
  }else{
    stop('phase needs to be either "adult" or "juv"')
  }
  
  param.line = grep(param.name,bio.lines)
  
  nbox = as.numeric(strsplit(bio.lines[param.line],split = ' |\t')[[1]][2])
  
  if(nbox != length(move.val)){
    stop('move.val needs to match number of boxes')
  }
  
  if(sum(move.val)!= 1 & rescale == F){
    warning('move.val must sum to 1. Try turning rescale to T')
  }
  if(rescale ==T){
      move.val = round(move.val/sum(move.val),3)
      corr.factor = round(1- sum(move.val),3)
      move.val[length(move.val)] = move.val[length(move.val)]+corr.factor
  }
  
  new.move.val = paste(move.val,collapse = ' ')
  bio.lines[param.line+1] = new.move.val
  
  if(overwrite ==T){
    writeLines(bio.lines,bio.file)
  }else{
    file.copy(bio.file,new.file.name,overwrite =T)
    writeLines(bio.lines,new.file.name)
  }
  
}

move.params = get_param_seasonal_movement(fgs = here::here('currentVersion','neus_groups.csv'),
                                          bio.file = here::here('currentVersion','at_biology.prm')
)

edit_param_seasonal_movement(
  bio.file = here::here('currentVersion','at_biology.prm'),
  fgs = here::here('currentVersion','neus_groups.csv'),
  Code = 'GOO',
  season = 1,
  phase = 'juv',
  move.val = runif(30),
  rescale = T,
  overwrite = F,
  new.file.name = here::here('currentVersion','at_biology_test.prm')
  
)

write.csv(move.params, here::here('data-raw','seasonal_movements.csv'),row.names = F)

