#Script to generate randomized catch scenarios
set.seed(13)
library(dplyr)

batch.prefix = 'random_catch_1'

N = 100

#Read in base catch file
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)

base.catch.file = here::here('currentVersion','CatchFiles','total_catch.ts')
base.catch = read.table(base.catch.file,header = T)
colnames(base.catch) = c('Time',fgs$Code)
catch.lines = readLines(base.catch.file)
header = grep('#',catch.lines,value = T)

#Original Forcing File
force.file.orig = here::here('currentVersion','at_force_LINUX.prm')
force.lines = readLines(force.file.orig)
catch.file.line = grep('Catchts0.data',force.lines)

#Set values related to shuffling period
n.spinup = 20
base.time = floor(base.catch$Time/365)
base.shuffle = which(base.time>=n.spinup)
base.keep = which(base.time < n.spinup)
shuff.years = (n.spinup+1):max(base.time)


#create directory for new catch files
catch.dir = here::here('currentVersion','CatchFiles',batch.prefix)
dir.create(catch.dir)

force.files.new = vector()

#Loop through N, shuffle active years, save as new catch file, and create corresponding forcing.prm
for(i in 1:N){
  
  ##shuffle all years after n.spinup
  new.years = sample(shuff.years)
  #re-orders groups based on permuatation in (new.years) while perserving groups (i.e. years)
  shuffle.time = c(base.keep,base.shuffle[order(factor(base.time[base.shuffle],levels = new.years))])
  new.catch = base.catch
  new.catch[,2:ncol(base.catch)] = base.catch[shuffle.time,2:ncol(base.catch)]
  
  #gut check plots
  # plot(base.catch$Time,base.catch$MAK,'l')
  # lines(1:nrow(new.catch),new.catch$MAK,col =2)
  
  #write new catch data to file
  new_catch_file = here::here('currentVersion','CatchFiles',batch.prefix,paste0('total_catch_rand_',i,'.ts'))
  file.create(new_catch_file)
  con = file(new_catch_file)
  writeLines(header,con)
  close(con)
  write.table(new.catch,row.names = F, col.names = F, file = new_catch_file,append = T)
  
  #Write new forcing file
  force.file.new = here::here('currentVersion',paste0('at_force_LINUX_rand_',i,'.prm'))
  
  file.copy(force.file.orig, force.file.new,overwrite = T)
  
  force.file.new.lines = readLines(force.file.new)
  catch.file.line.new = paste0('Catchts0.data CatchFiles/',batch.prefix,'/',paste0('total_catch_rand_',i,'.ts'))
  force.file.new.lines[catch.file.line] = catch.file.line.new
  
  writeLines(force.file.new.lines, con = force.file.new )
  force.files.new[i] = force.file.new
}

#### Create Batcher Setup ####
setup.df = data.frame(
  Run = paste0('rand_',1:N),
  OutputDir = paste0(batch.prefix,'/rand_',1:N,'/'),
  BiolPrm = 'at_biology.prm',
  RunPrm = 'at_run.prm',
  HarvestPrm = 'at_harvest.prm',
  InitNC = 'neus_init.nc',
  ForcePrm = paste0('at_force_LINUX_rand_',1:N,'.prm')
)


