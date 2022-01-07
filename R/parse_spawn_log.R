
#Function to parse spawn debugging
parse_spawn_log = function(file,spp = NULL){
  
  log.lines = readLines(file)  
  spawn.lines  = grep('*TotSpawn*',log.lines)
  
  spawn.dat =log.lines[spawn.lines]
  
  dat.df = read.table(text = gsub(':| ',',',spawn.dat),sep = ',')[,c(3,5,9,12,15,19,23,27,31,35,39,43,47,50,53)]
  colnames(dat.df) = c('Time','Code','Cohort','Box','Layer','TotSpawn','IndSpawn','DEN','FSPB','SN','RN','FSP','KSPA','step1','RSprop')
  
  if(!is.null(spp)){
    dat.df= subset(dat.df,Code %in% spp)
  }
  
  return(dat.df)
}
