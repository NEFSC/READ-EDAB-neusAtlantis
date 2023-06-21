#Script to generate parameter files for fishing sensitivity scenarios
# 1) Generate Guild and Fishing Scalar combinations
# 2) Create parameter files for each scenario
# 3) Create batcher setup file for each scenario


# proj.dir = here::here('')
# experiment.id = 'fspike1'
# proj.length.d = 365*20
# run.length.d = 28105
# event.start.d = 20805
# event.end.d = 20805+(365*2)
# 
# #define fishing levels
# fishing.levels = c(0,2,5,10,25,50,100)
# fishing.levels.text = c('0','2','5','10','25','50','100')

make_fish_sens_proj_catch_spike_species = function(proj.dir,
                                                    experiment.id,
                                                    proj.length.d,
                                                    run.length.d,
                                                    event.start.d,
                                                    event.end.d,
                                                    fishing.levels,
                                                    fishing.levels.text,
                                                    make.catch.files = T){
  
  
  library(dplyr)
  library(ggplot2)
  
  #### Generate Guild and Fishing Scalar Combinations ####
  
  #Read in Functional Group to guild match
  fgs = read.csv(paste0(proj.dir,'currentVersion/neus_groups.csv'),as.is = T)
  
  
  source(paste0(proj.dir,'/R/fishing_sensitivity/make_catch_file_projected_mean.R'))
  new.base.catch.file =  paste0(proj.dir,'currentVersion/CatchFiles/total_catch_',experiment.id,'.ts')
  
  make_catch_file_projected_mean(
    fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
    original_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts'),
    start.time = event.start.d-(365*10),
    end.time = event.start.d,
    duration = proj.length.d,
    new_catch_file = new.base.catch.file,
    overwrite = F
  )
  
  system(paste0('sudo chmod -R 775 ',proj.dir,'currentVersion'))
  
  #Get only caught species
  original_catch_file = new.base.catch.file
  catch.dat.orig = read.table(original_catch_file,as.is = T)[-1]
  groups.fished = fgs$Code[which(colSums(catch.dat.orig)>0)]
  
  #sanity test for mean catch
  # catch.dat.orig.long = catch.dat.orig %>% tidyr::gather('Code','Catch',-Time)
  # ggplot(data = catch.dat.orig.long,aes(x= Time,y=Catch))+geom_line()+facet_wrap(~Code,scale = 'free_y')
  

  #Define guild and fishing level combinations
  scenario.combs = expand.grid('Code' = groups.fished, 'fishing.levels' = fishing.levels) %>%
    arrange(Code)%>%
    left_join(data.frame(fishing.levels = fishing.levels,fishing.levels.text = fishing.levels.text))
  
  #### Create Parameter Files for each Scenario ####
  system(paste0('sudo mkdir ',proj.dir,'currentVersion/CatchFiles/',experiment.id))
  
  #Functions to make new catch files#Functions to makeexperiment.id new catch files
  source(paste0(proj.dir,'R/fishing_sensitivity/make_catch_spike_projected.R'))
  new.catch.names = character()
  i=1
  for(i in 1:nrow(scenario.combs)){
    
    new.catch.name = paste0(experiment.id,'_',scenario.combs$Code[i],'_',scenario.combs$fishing.levels.text[i])
    new.catch.names[i] = new.catch.name
  
    if(make.catch.files){
      make_catch_spike_projected(
        proj.dir = proj.dir,
        original_catch_file = new.base.catch.file,
        fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
        group.names =scenario.combs$Code[i],
        new_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/',experiment.id,'/',paste0(new.catch.name,'.ts')),
        setup.filename = paste0(proj.dir,'currentVersion/CatchFiles/',experiment.id,'/',paste0(new.catch.name,'.csv')),
        spike.start = event.start.d,
        spike.end = event.end.d,
        time.end = run.length.d,
        type = 'Replace',
        change = scenario.combs$fishing.levels[i]
      )
    }
  }
  
  # Create at_force_LINUX.prm and runAtlantis.sh and put into a new directory
  dir.create(paste0(proj.dir,'currentVersion/',experiment.id))
  #specify original run.sh
  run.sh.orig = paste0(proj.dir,'currentVersion/RunAtlantis_cloud.sh')
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
    force.file.new = paste0(proj.dir,'currentVersion/',force.file.new.short)
    
    file.copy(force.file.orig, force.file.new,overwrite = T)
    
    force.file.new.lines = readLines(force.file.new)
    catch.file.line.new = paste0('Catchts0.data CatchFiles/',experiment.id,'/',paste0(new.catch.names[i],'.ts'))
    force.file.new.lines[catch.file.line] = catch.file.line.new
    
    writeLines(force.file.new.lines, con = force.file.new )
    force.files.new[i] = force.file.new
    
    #Do run.sh duplication
    run.file.new = paste0(proj.dir,'currentVersion/',paste0('runAtlantis_',i,'.sh'))
    
    file.copy(run.sh.orig, run.file.new,overwrite=T)
    
    
    run.file.new.lines = readLines(run.file.new)
    run.command.new =  paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f ',force.file.new.short,' -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
    run.file.new.lines[run.command.line] = run.command.new
    
    writeLines(run.file.new.lines,con = run.file.new)
    run.sh.files.new[i] = run.file.new
    
    print(i)
  }
  
  #### Create Batcher Setup ####
  setup.df = data.frame(
    experiment.id = experiment.id,
    ID = 1:length(new.catch.names),
    run.id = paste0(experiment.id,'_',1:length(new.catch.names)),
    scalar = scenario.combs$fishing.levels,
    target.species = scenario.combs$Code,
    OutputDir = paste0(experiment.id,'/',new.catch.names,'/'),
    sh.script = paste0('/runAtlantis_',experiment.id,'_',1:length(new.catch.names),'.sh')
  )
  
  write.csv(setup.df,paste0(proj.dir,'Setup_Files/',paste0(experiment.id,'_setup.csv')),row.names =F)
  
}