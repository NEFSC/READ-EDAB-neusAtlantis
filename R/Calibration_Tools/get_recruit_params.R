#Function to get recruitment parameters from biology.prm (FSP, KSPA, FSPB)

get_recruit_params = function(bio.prm){

  bio.lines = readLines(bio.prm)  
  
  #match parameter lines
  kspa.lines = grep('^KSPA_',bio.lines)
  fsp.lines = grep('^FSP_',bio.lines)
  fspb.lines = grep('^FSPB_',bio.lines)
  kwsr.lines = grep('^KWSR_',bio.lines)
  kwrr.lines = grep('^KWRR_',bio.lines)
  
  #pull KSPA into dataframe
  kspa.group = sapply(bio.lines[kspa.lines],function(x) strsplit(x,'_|\t| ')[[1]][2],USE.NAMES = F)
  kspa.val  = as.numeric(sapply(bio.lines[kspa.lines],function(x){
    dum = strsplit(x,'_| |\t')[[1]]
    return(dum[2+which(dum[-c(1:2)] != '')])
  },USE.NAMES = F))
  kspa.df = data.frame(group = kspa.group,KSPA = kspa.val)
  
  #pull FSP into dataframe
  fsp.group =  sapply(bio.lines[fsp.lines],function(x) strsplit(x,'_|\t| ')[[1]][2],USE.NAMES = F)
  fsp.val  = as.numeric(sapply(bio.lines[fsp.lines],function(x){
    dum = strsplit(x,'_| |\t')[[1]]
    return(dum[2+which(dum[-c(1:2)] != '')[1]])
  },USE.NAMES = F))
  fsp.df = data.frame(group = fsp.group,FSP = as.numeric(fsp.val))
  
  #pull FSPB into dataframe
  fspb.group = sapply(bio.lines[fspb.lines],function(x) strsplit(x,'_|\t| ')[[1]][2],USE.NAMES = F)
  fspb.ls = list()
  for(i in 1:length(fspb.group)){
    fspb.val = as.numeric(strsplit(bio.lines[fspb.lines[i]+1],' |\t')[[1]])
    fspb.ls[[i]] = data.frame(group = fspb.group[i],cohort = 1:length(fspb.val),FSPB = fspb.val)
  }
  fspb.df = dplyr::bind_rows(fspb.ls)
  
  #pull KSWR values
  kwsr.group = sapply(bio.lines[kwsr.lines],function(x) strsplit(x,'_|\t| ')[[1]][2],USE.NAMES = F)
  kwsr.val  = as.numeric(sapply(bio.lines[kwsr.lines],function(x){
    dum = strsplit(x,'_| |\t')[[1]]
    return(dum[2+which(dum[-c(1:2)] != '')])
  },USE.NAMES = F))
  kwsr.df = data.frame(group = kwsr.group,KWSR = kwsr.val)
  
  #pull KSRR values
  kwrr.group = sapply(bio.lines[kwrr.lines],function(x) strsplit(x,'_|\t| ')[[1]][2],USE.NAMES = F)
  kwrr.val  = as.numeric(sapply(bio.lines[kwrr.lines],function(x){
    dum = strsplit(x,'_| |\t')[[1]]
    return(dum[2+which(dum[-c(1:2)] != '')])
  },USE.NAMES = F))
  kwrr.df = data.frame(group = kwrr.group,KWRR = kwrr.val)
  
  
  out.ls = list(KSPA = kspa.df,FSP = fsp.df,FSPB = fspb.df,KWSR = kwsr.df,KWRR = kwrr.df)
  return(out.ls)
}