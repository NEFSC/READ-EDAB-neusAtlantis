#Script to generate parameter files for fishing sensitivity scenarios
# 1) Generate Guild and Fishing Scalar combinations
# 2) Create parameter files for each scenario
# 3) Create batcher setup file for each scenario

library(dplyr)

#### Generate Guild and Fishing Scalar Combinations ####

#Define guild names that are used for catch scenarios
guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')
batch.prefix = 'fishing_sensitivity_extended_constant_2'

#Read in Functional Group to guild match
spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code,Guild)%>%
  filter(Guild %in% guild.names)

#define fishing levels c(1.1,1.25,1.33,1.5,2.5,5,10)
fishing.levels = c(0,0.5,1.5,2.5,5,10,15,20,40,60,100)
fishing.levels.text = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100')

#Define guild and fishing level combinations
scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels' = fishing.levels) %>%
  arrange(guild.names)%>%
  left_join(data.frame(fishing.levels = fishing.levels,fishing.levels.text = fishing.levels.text))

#### Create Parameter Files for each Scenario ####
dir.create(here::here('currentVersion','CatchFiles',batch.prefix))

#Functions to make new catch files#Functions to makebatch.prefix new catch files
source(here::here('R','make_catch_scale_scenarios.R'))
new.catch.names = character()
i=1
for(i in 1:nrow(scenario.combs)){
  
  spp.guild = filter(spp2guild,Guild == scenario.combs$guild.names[i])$Code
  new.catch.name = paste0(batch.prefix,'_',scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i])
  new.catch.names[i] = new.catch.name
  
  make_catch_scale_scenarios(
    original_catch_file = here::here('currentVersion','CatchFiles','total_catch_extended_mean.ts'),
    fgs.file = here::here('currentVersion','neus_groups.csv'),
    groups =spp.guild,
    new_catch_file = here::here('currentVersion','CatchFiles',batch.prefix,paste0(new.catch.name,'.ts')),
    setup.filename = here::here('currentVersion','CatchFiles',batch.prefix,paste0(new.catch.name,'.csv')),
    start.time = 19724,
    end.time = 19724 + (365*20),
    type = 'Scalar',
    change = scenario.combs$fishing.levels[i]
  )
}

# Create at_force_LINUX.prm
force.file.orig = here::here('currentVersion','at_force_LINUX.prm')
force.lines = readLines(force.file.orig)
catch.file.line = grep('Catchts0.data',force.lines)

force.files.new = character()
for(i in 1:length(new.catch.names)){
  
  force.file.new = here::here('currentVersion',paste0('at_force_LINUX_',new.catch.names[i],'.prm'))
  
  file.copy(force.file.orig, force.file.new,overwrite = T)
  
  force.file.new.lines = readLines(force.file.new)
  catch.file.line.new = paste0('Catchts0.data CatchFiles/',batch.prefix,'/',paste0(new.catch.names[i],'.ts'))
  force.file.new.lines[catch.file.line] = catch.file.line.new
  
  writeLines(force.file.new.lines, con = force.file.new )
  force.files.new[i] = force.file.new
}

#### Create Batcher Setup ####
setup.df = data.frame(
  Run = new.catch.names,
  OutputDir = paste0(batch.prefix,'/',new.catch.names,'/'),
  BiolPrm = 'at_biology.prm',
  RunPrm = 'at_run.prm',
  HarvestPrm = 'at_harvest.prm',
  InitNC = 'neus_init.nc',
  ForcePrm = paste0('at_force_LINUX_',new.catch.names,'.prm')
)

write.csv(setup.df,here::here('Setup_Files',paste0(batch.prefix,'.csv')),row.names =F)

batch.dir = batch.prefix
dir.create(here::here('Atlantis_Runs',batch.dir))

source(here::here('R','atlantis_batcher_11_30_22.r'))

atlantis_batcher(
  batcherFilename = here::here('Setup_Files',paste0(batch.prefix,'f.csv')),
  userName            = 'jcara',
  CHECK_TIME_INTERVAL = 600,
  NUM_TO_RUN          = 1,
  CONTAINER_TYPE      = 'podman',
  param.dir = here::here('currentVersion',''),
  output.dir = paste0('/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/')
)
