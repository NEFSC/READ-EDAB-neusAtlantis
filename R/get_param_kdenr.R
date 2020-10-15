# Script to get table of fixed recruitment (KDENR) values




get_param_kdenr = function(bio.file){
  
  #Read lines
  bio.lines = readLines(bio.file)
  closeAllConnections()
  
  #get kdenr lines
  kdenr.line.id = grep('^KDENR',bio.lines)
  kdenr.line.name = bio.lines[kdenr.line.id]
  kdenr.line.vals = bio.lines[grep('^KDENR',bio.lines)+1]
  
  #Get group names, repeat out inverts (juv,adult)
  group.names = unlist(lapply(kdenr.line.name,function(x){
    char.split = strsplit(x,' ')[[1]]
    group = strsplit(char.split[1],'KDENR_')[[1]][2]
    if(length(char.split) == 2){
      return(c(paste0(group,'j'),paste0(group,'a')))
    }else{
      return(group)
    }
  }))
  
  #Get group values
  group.vals = unlist(lapply(kdenr.line.vals,function(x){
    val.split = strsplit(x,' ')[[1]]
    return(val.split)
  }))
  
  return(data.frame(code = group.names,KDENR = group.vals))
}

#Test
orig.kdner = get_param_kdenr(bio.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/at_biol_neus_v15_scaled_diet_20181126_3.prm')
removePred2.kdner = get_param_kdenr(bio.file = here::here('currentVersion','at_biology.prm'))

                                    