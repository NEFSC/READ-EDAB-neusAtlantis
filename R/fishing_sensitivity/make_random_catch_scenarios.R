#Script to generate randomized catch scenarios
set.seed(13)
library(dplyr)

#Set the project directory
# proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
proj.dir = here::here('/')

batch.prefix = 'random_catch_1'

N = 10

#Read in base catch file
fgs = read.csv(paste0(proj.dir,'currentVersion/neus_groups.csv'),as.is = T)

#Need to run baseline catch with projection (total_catch_projected_mean.ts)
proj.duration.yr = 5

source(paste0(proj.dir,'/R/fishing_sensitivity/make_catch_file_projected_mean.R'))
new.base.catch.file =  paste0(proj.dir,'currentVersion/CatchFiles/total_catch_',batch.prefix,'.ts')

make_catch_file_projected_mean(
  fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
  original_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts'),
  start.time = 19724-(365*10),
  end.time = 19724,
  duration = proj.duration.yr*365,
  new_catch_file = new.base.catch.file,
  overwrite = F
)

base.catch.file = new.base.catch.file
base.catch = read.table(base.catch.file,header = T)
colnames(base.catch) = c('Time',fgs$Code)
catch.lines = readLines(base.catch.file)
header = grep('#',catch.lines,value = T)

#Original Forcing File
force.file.orig = paste0(proj.dir,'currentVersion/at_force_LINUX.prm')
force.lines = readLines(force.file.orig)
catch.file.line = grep('Catchts0.data',force.lines)

#Set values related to shuffling period
start.year = 21
stop.year = 57
base.time = floor(base.catch$Time/365)
base.shuffle = which(base.time>=start.year & stop.year)
keep.spinup = which(base.time < start.year)
keep.proj = which(base.time > stop.year)
shuff.years = start.year:stop.year


#create directory for new catch files
catch.dir = paste0(proj.dir,'currentVersion/CatchFiles/',batch.prefix,'/')
dir.create(catch.dir)
dir.create(paste0(proj.dir,'currentVersion/',batch.prefix,'/'))
run.sh.orig = paste0(proj.dir,'currentVersion/runAtlantis.sh')
run.sh.lines = readLines(run.sh.orig)
run.command.line = grep('atlantisMerged',run.sh.lines)

force.files.new = vector()

#Loop through N, shuffle active years, save as new catch file, and create corresponding forcing.prm
i=1
test.catch = matrix(NA, nrow = nrow(base.catch),ncol = N)
for(i in 1:N){
  
  ##shuffle all years after n.spinup
  new.years = sample(shuff.years)
  #re-orders groups based on permuatation in (new.years) while perserving groups (i.e. years)
  shuffle.time = c(keep.spinup,base.shuffle[order(factor(base.time[base.shuffle],levels = new.years))],keep.proj)
  new.catch = base.catch
  new.catch[,2:ncol(base.catch)] = base.catch[shuffle.time,2:ncol(base.catch)]
  
  test.catch[,i] = new.catch[,2]
  
  #gut check plots
  # plot(base.catch$Time,base.catch$MAK,'l')
  # lines(1:nrow(new.catch),new.catch$MAK,col =2)
  
  #write new catch data to file
  new_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/',batch.prefix,paste0('/total_catch_rand_',i,'.ts'))
  file.create(new_catch_file)
  con = file(new_catch_file)
  writeLines(header,con)
  close(con)
  write.table(new.catch,row.names = F, col.names = F, file = new_catch_file,append = T)
  
  #Write new forcing file
  force.file.new = paste0(proj.dir,'currentVersion/',batch.prefix,paste0('/at_force_LINUX_rand_',i,'.prm'))
  force.file.new.short = paste0('/at_force_LINUX_rand_',i,'.prm')
  
  file.copy(force.file.orig, force.file.new,overwrite = T)
  
  force.file.new.lines = readLines(force.file.new)
  catch.file.line.new = paste0('Catchts0.data CatchFiles/',batch.prefix,'/',paste0('total_catch_rand_',i,'.ts'))
  force.file.new.lines[catch.file.line] = catch.file.line.new
  
  writeLines(force.file.new.lines, con = force.file.new )
  force.files.new[i] = force.file.new
  
  #Duplicate run.sh
  run.file.new = paste0(proj.dir,'currentVersion/',batch.prefix,'/',paste0('runAtlantis_rand_',i,'.sh'))
  
  file.copy(run.sh.orig, run.file.new,overwrite=T)
  
  run.file.new.lines = readLines(run.file.new)
  run.command.new =  paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f ',batch.prefix,'/',force.file.new.short,' -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
  run.file.new.lines[run.command.line] = run.command.new
  
  writeLines(run.file.new.lines,con = run.file.new)
  
  print(i)
  
}

# plot(base.catch[,2],type = 'l',col = 'red')
# for(i in 1:N){lines(test.catch[,i],col = 'grey50')}
# lines(base.catch[,2],type = 'l',col = 'red')
# 
# #### Create Batcher Setup ####
setup.df = data.frame(
  Run = paste0('rand_',1:N),
  OutputDir = paste0(batch.prefix,'/rand_',1:N,'/'),
  BiolPrm = 'at_biology.prm',
  RunPrm = 'at_run.prm',
  HarvestPrm = 'at_harvest.prm',
  InitNC = 'neus_init.nc',
  ForcePrm = paste0('at_force_LINUX_rand_',1:N,'.prm')
)

write.csv(setup.df,paste0(proj.dir,'Setup_Files/',paste0(batch.prefix,'.csv')),row.names =F)



