
#Function to parse spawn debugging
parse_spawn_log = function(run.dir,spp = NULL){
  
  log.files = list.files(run.dir,'^log*',full.names = T)
  dat.df.ls = list()
  
  for(i in 1:length(log.files)){
    
    log.lines = readLines(log.files[i])  
    spawn.lines  = grep('*TotSpawn*',log.lines)
    
    spawn.dat =log.lines[spawn.lines]
    
    dat.df = read.table(text = gsub(':| ',',',spawn.dat),sep = ',')[,c(3,5,9,12,15,19,23,27,31,35,39,43,47,50)]
    colnames(dat.df) = c('Time','Code','Cohort','Box','Layer','TotSpawn','IndSpawn','DEN','FSPB','SN','RN','FSP','KSPA','step1')
    
    if(!is.null(spp)){
      dat.df= subset(dat.df,Code %in% spp)
    }
    
    dat.df.ls[[i]] = dat.df
  }

  return(dplyr::bind_rows(dat.df.ls))
}
