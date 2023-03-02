library(dplyr)
batch.prefix = 'BH_convert_1'

batch.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix)

run.index = readRDS(here::here('data',paste0(batch.prefix,'_index.rds')))

#Check if files are located in each run directory

output.check = run.index
output.check$has.output = NA

for(i in 1:length(dir.names)){ output.check$has.output[i] = length(list.files(paste0(batch.dir,'/',run.index$name[i])))>0}


log.df = run.index %>% mutate(expected.file = NA,actual.file = NA, file.match = NA)

for(i in 1:nrow(run.index)){
  
  run.name = run.index$name[i]
  expected.file = paste0('at_biology_',run.name,'.prm')
  
  log.file = paste0(batch.dir,'/',run.name,'/log.txt')
  
  if(file.exists(log.file)){
    log.lines = readLines(log.file)
    run.str = grep('atlantisMerged',log.lines,value = T)
    actual.file = strsplit(run.str,' ')[[1]][14]
    
    log.df$expected.file[i] = expected.file
    log.df$actual.file[i] = actual.file
    log.df$file.match[i] = expected.file == actual.file
  }else{
    log.df$expected.file[i] = expected.file
    log.df$actual.file[i] = NA
    log.df$file.match[i] = expected.file == actual.file
  }
  
}
