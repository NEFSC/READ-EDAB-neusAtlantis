# Script to get table of fixed recruitment (KDENR) values

get_param_mort = function(bio.file){
  
  #Read lines
  bio.lines = readLines(bio.file)
  closeAllConnections()
  
  #get mL lines
  ml.line.id = grep('*_mL',bio.lines)
  ml.line.name = bio.lines[ml.line.id]
  which.age = sapply(ml.line.name,function(x) return(ifelse(strsplit(x,' ')[[1]][2] == 2,T,F)))
  group.name = sapply(ml.line.name, function(x) {
    dum= strsplit(x,' ')[[1]][1]
    dum2 = strsplit(x,'_')[[1]][1]
    return(dum2)
  },USE.NAMES = F)
  ml.df = data.frame(group = group.name,var = 'mL',val.1=NA, val.2=NA)
  for(i in 1:length(ml.line.id)){
    if(which.age[i]){
      dum = strsplit(bio.lines[ml.line.id[i]+1],' ')[[1]]
      ml.df$val.1[i] = dum[1]
      ml.df$val.2[i] = dum[2]
    }else{
      ml.df$val.1[i] = strsplit(ml.line.name[i],' ')[[1]][2]
    }
  }
  
  #get mQ lines
  mq.line.id = grep('*_mQ',bio.lines)
  mq.line.name = bio.lines[mq.line.id]
  which.age = sapply(mq.line.name,function(x) return(ifelse(strsplit(x,' ')[[1]][2] == 2,T,F)))
  group.name = sapply(mq.line.name, function(x) {
    dum= strsplit(x,' ')[[1]][1]
    dum2 = strsplit(x,'_')[[1]][1]
    return(dum2)
  },USE.NAMES = F)
  mq.df = data.frame(group = group.name,var = 'mQ',val.1=NA, val.2=NA)
  for(i in 1:length(mq.line.id)){
    if(which.age[i]){
      dum = strsplit(bio.lines[mq.line.id[i]+1],' ')[[1]]
      mq.df$val.1[i] = dum[1]
      mq.df$val.2[i] = dum[2]
    }else{
      mq.df$val.1[i] = strsplit(mq.line.name[i],' ')[[1]][2]
    }
  }
  
  mort.out = rbind(ml.df,mq.df)

  return(mort.out)
}

#Test
orig.kdner = get_param_mort(bio.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/at_biol_neus_v15_scaled_diet_20181126_3.prm')
currentversion = get_param_mort(bio.file = here::here('currentVersion','at_biology.prm'))
write.csv(currentversion, file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/RemovePred9.csv',row.names = F)

