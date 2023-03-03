library(dplyr)

source(here::here('R','edit_param_BH.R'))

batch.prefix = 'BH_convert_1'

batch.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix)

run.index = readRDS(here::here('data',paste0(batch.prefix,'_index.rds')))

base.BH = readRDS(here::here('data','reference_BH_params.rds'))

#Check if files are located in each run directory

output.check = run.index %>%
  mutate(has.output = NA,
         alpha.check = NA,
         beta.check = NA,
         maxT = NA)

i=1
for(i in 1:nrow(run.index)){
  output.check$has.output[i] = length(list.files(paste0(batch.dir,'/',run.index$name[i])))>0
  
  bio.file =  here::here('currentVersion',paste0('at_biology_',run.index$name[i],'.prm'))
  
  ref.bh = base.BH %>% 
    filter(Code == run.index$Code[i])
  run.bh= get_param_BH(bio.prm = bio.file)%>%
    filter(group == run.index$Code[i])
  
  real.alpha.scale = as.numeric(run.bh$alpha[1])/ref.bh$alpha[1]
  real.beta.scale = as.numeric(run.bh$beta[1])/ref.bh$beta[1]
    
  output.check$alpha.check[i] = all.equal(real.alpha.scale,output.check$BHalpha[i])
  output.check$beta.check[i] = all.equal(real.beta.scale, output.check$BHbeta[i])
  
  biom.file = paste0(batch.dir,'/',run.index$name[i],'/neus_outputBiomIndx.txt')
  if(file.exists(biom.file) & file.size(biom.file)>0){
    biom = read.table(biom.file,as.is = T,header = T)
    output.check$maxT[i] = max(biom$Time,na.rm=T)  
  }
  
  
  }


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
