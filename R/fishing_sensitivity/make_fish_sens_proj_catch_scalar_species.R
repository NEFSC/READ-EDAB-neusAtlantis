#Script to generate parameter files for fishing sensitivity scenarios
# 1) Generate Guild and Fishing Scalar combinations
# 2) Create parameter files for each scenario
# 3) Create batcher setup file for each scenario

library(dplyr)
library(ggplot2)

#### Generate Guild and Fishing Scalar Combinations ####

#project directory
#proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
proj.dir = here::here('/')

#Scenario Name
batch.prefix = 'fish_sens_catch_scalar_species_1'

#Read in Functional Group to guild match
fgs = read.csv(paste0(proj.dir,'currentVersion/neus_groups.csv'),as.is = T)

#Need to run baseline catch with projection (total_catch_projected_mean.ts)
proj.duration.yr = 20

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

#Get only caught species
original_catch_file = new.base.catch.file
catch.dat.orig = read.table(original_catch_file,as.is = T)[-1]
groups.fished = fgs$Code[which(colSums(catch.dat.orig)>0)]

#sanity test for mean catch
# catch.dat.orig.long = catch.dat.orig %>% tidyr::gather('Code','Catch',-Time)
# ggplot(data = catch.dat.orig.long,aes(x= Time,y=Catch))+geom_line()+facet_wrap(~Code,scale = 'free_y')

#define fishing levels
fishing.levels = c(0,1.1,1.25,1.5,2,5,10)
fishing.levels.text = c('0','1_1','1_25','1_5','2','5','10')

#Define guild and fishing level combinations
scenario.combs = expand.grid('Code' = groups.fished, 'fishing.levels' = fishing.levels) %>%
  arrange(Code)%>%
  left_join(data.frame(fishing.levels = fishing.levels,fishing.levels.text = fishing.levels.text))

#### Create Parameter Files for each Scenario ####
dir.create(paste0(proj.dir,'currentVersion/CatchFiles/',batch.prefix))

#Functions to make new catch files#Functions to makebatch.prefix new catch files
source(paste0(proj.dir,'R/Fishing_Sensitivity/make_catch_scalar_projected.R'))
new.catch.names = character()
i=1
for(i in 1:nrow(scenario.combs)){

  new.catch.name = paste0(batch.prefix,'_',scenario.combs$Code[i],'_',scenario.combs$fishing.levels.text[i])
  new.catch.names[i] = new.catch.name

  make_catch_scalar_projected(
    proj.dir = proj.dir,
    original_catch_file = original_catch_file,
    fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
    groups = scenario.combs$Code[i],
    new_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/',batch.prefix,'/',paste0(new.catch.name,'.ts')),
    setup.filename = paste0(proj.dir,'currentVersion/CatchFiles/',batch.prefix,'/',paste0(new.catch.name,'.csv')),
    start.time = 19724,
    end.time = 19724 + (365*20),
    type = 'Scalar',
    change = scenario.combs$fishing.levels[i]
  )
}

# Create at_force_LINUX.prm and runAtlantis.sh and put into a new directory
dir.create(paste0(proj.dir,'currentVersion/',batch.prefix))
#specify original run.sh
run.sh.orig = paste0(proj.dir,'currentVersion/runAtlantis.sh')
run.sh.lines = readLines(run.sh.orig)
run.command.line = grep('atlantisMerged',run.sh.lines)

#Specify original force.prm
force.file.orig = paste0(proj.dir,'currentVersion/at_force_LINUX.prm')
force.lines = readLines(force.file.orig)
catch.file.line = grep('Catchts0.data',force.lines)

force.files.new = run.sh.files.new = character()
for(i in 1:length(new.catch.names)){

  #Do force.prm duplication
  force.file.new.short = paste0('at_force_LINUX_',new.catch.names[i],'.prm')
  force.file.new = paste0(proj.dir,'currentVersion/',batch.prefix,'/',force.file.new.short)

  file.copy(force.file.orig, force.file.new,overwrite = T)
  
    force.file.new.lines = readLines(force.file.new)
    catch.file.line.new = paste0('Catchts0.data CatchFiles/',batch.prefix,'/',paste0(new.catch.names[i],'.ts'))
    force.file.new.lines[catch.file.line] = catch.file.line.new
  
    writeLines(force.file.new.lines, con = force.file.new )
  force.files.new[i] = force.file.new
  
  #Do run.sh duplication
  run.file.new = paste0(proj.dir,'currentVersion/',batch.prefix,'/',paste0('runAtlantis_',new.catch.names[i],'.sh'))
  
  file.copy(run.sh.orig, run.file.new,overwrite=T)

  run.file.new.lines = readLines(run.file.new)
  run.command.new =  paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f ',batch.prefix,'/',force.file.new.short,' -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
  run.file.new.lines[run.command.line] = run.command.new

  writeLines(run.file.new.lines,con = run.file.new)
  run.sh.files.new[i] = run.file.new
  
  print(i)
}

#### Create Batcher Setup ####
setup.df = data.frame(
  Run = new.catch.names,
  OutputDir = paste0(batch.prefix,'/',new.catch.names,'/'),
  BiolPrm = 'at_biology.prm',
  RunPrm = 'at_run.prm',
  HarvestPrm = 'at_harvest.prm',
  InitNC = 'neus_init.nc',
  ForcePrm = paste0(batch.prefix,'/at_force_LINUX_',new.catch.names,'.prm')
)

write.csv(setup.df,paste0(proj.dir,'Setup_Files/',paste0(batch.prefix,'.csv')),row.names =F)

