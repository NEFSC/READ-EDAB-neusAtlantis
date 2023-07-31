#Script to generate randomized catch scenarios

# experiment.id = 'random_catch1'
# seed = 13
# N = 250
# proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
# proj.duration.yr = 5
# event.start.d = 20805
# shuffle.start.yr = 21
# shuffle.stop.yr = 57

make_random_catch_scenarios = function(experiment.id,
                                       N,
                                       proj.dir,
                                       proj.duration.yr,
                                       event.start.d,
                                       shuffle.start.yr,
                                       shuffle.stop.yr,
                                       seed,
                                       make.catch.files){
  set.seed(seed)
  library(dplyr)
  
  
  #Read in base catch file
  fgs = read.csv(paste0(proj.dir,'currentVersion/neus_groups.csv'),as.is = T)
  
  #Need to run baseline catch with projection (total_catch_projected_mean.ts)
  
  source(paste0(proj.dir,'/R/fishing_sensitivity/make_catch_file_projected_mean.R'))
  new.base.catch.file =  paste0(proj.dir,'currentVersion/CatchFiles/total_catch_',experiment.id,'.ts')
  
  make_catch_file_projected_mean(
    fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
    original_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts'),
    start.time = event.start.d-(365*10),
    end.time = event.start.d,
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

  base.time = floor(base.catch$Time/365)
  base.shuffle = which(base.time>=shuffle.start.yr & base.time <=shuffle.stop.yr)
  keep.spinup = which(base.time < shuffle.start.yr)
  keep.proj = which(base.time > shuffle.stop.yr)
  shuff.years = shuffle.start.yr:shuffle.stop.yr
  
  
  #create directory for new catch files
  catch.dir = paste0(proj.dir,'currentVersion/CatchFiles/',experiment.id,'/')
  if(!dir.exists(catch.dir)) {dir.create(catch.dir)}
  if(!dir.exists(paste0(proj.dir,'currentVersion/',experiment.id,'/'))){ dir.create(paste0(proj.dir,'currentVersion/',experiment.id,'/'))}
  
  run.sh.orig = paste0(proj.dir,'currentVersion/RunAtlantis_cloud.sh')
  run.sh.lines = readLines(run.sh.orig)
  run.command.line = grep('atlantisMerged',run.sh.lines)
  
  force.files.new = vector()
  
  #Loop through N, shuffle active years, save as new catch file, and create corresponding forcing.prm
  i=1
  shuffle.out = list()
  test.catch = matrix(NA, nrow = nrow(base.catch),ncol = N)
  for(i in 1:N){
    
    ##shuffle all years after n.spinup
    new.years = sample(shuff.years)
    #re-orders groups based on permuatation in (new.years) while perserving groups (i.e. years)
    shuffle.time = c(keep.spinup,base.shuffle[order(factor(base.time[base.shuffle],levels = new.years))],keep.proj)
    shuffle.out[[i]] = shuffle.time
    
    new.catch = base.catch
    new.catch[,2:ncol(base.catch)] = base.catch[shuffle.time,2:ncol(base.catch)]

    test.catch[,i] = new.catch[,2]

    #gut check plots
    # plot(base.catch$Time,base.catch$MAK,'l')
    # lines(1:nrow(new.catch),new.catch$MAK,col =2)

    if(make.catch.files){
    
    #write new catch data to file
    new_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/',experiment.id,paste0('/',experiment.id,'_',i,'.ts'))
    file.create(new_catch_file)
    con = file(new_catch_file)
    writeLines(header,con)
    close(con)
    write.table(new.catch,row.names = F, col.names = F, file = new_catch_file,append = T)

    #Write new forcing file
    force.file.new = paste0(proj.dir,'currentVersion/',paste0('at_force_LINUX_',experiment.id,'_',i,'.prm'))
    force.file.new.short = paste0('at_force_LINUX_',experiment.id,'_',i,'.prm')

    file.copy(force.file.orig, force.file.new,overwrite = T)

    force.file.new.lines = readLines(force.file.new)
    catch.file.line.new = paste0('Catchts0.data CatchFiles/',experiment.id,'/',paste0(experiment.id,'_',i,'.ts'))
    force.file.new.lines[catch.file.line] = catch.file.line.new

    writeLines(force.file.new.lines, con = force.file.new )
    force.files.new[i] = force.file.new

    #Duplicate run.sh
    run.file.new = paste0(proj.dir,'currentVersion/',paste0('RunAtlantis_',experiment.id,'_',i,'.sh'))

    file.copy(run.sh.orig, run.file.new,overwrite=T)

    run.file.new.lines = readLines(run.file.new)
    run.command.new =  paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f ',force.file.new.short,' -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
    run.file.new.lines[run.command.line] = run.command.new

    writeLines(run.file.new.lines,con = run.file.new)

    print(i)
    }
    
  }
  
  saveRDS(shuffle.out,file = here::here('diagnostics',paste0(experiment.id,'_permutations.rds')))
  
  
  
  # plot(base.catch[,2],type = 'l',col = 'red')
  # for(i in 1:N){lines(test.catch[,i],col = 'grey50')}
  # lines(base.catch[,2],type = 'l',col = 'red')
  # 
  # #### Create Batcher Setup ####
  setup.df = data.frame(
    experiment.id = experiment.id,
    ID = 1:N,
    run.id = paste0(experiment.id,'_',1:N),
    OutputDir = paste0(experiment.id,'/',experiment.id,'_',1:N,'/'),
    sh.script = paste0('/RunAtlantis_',experiment.id,'_',1:N,'.sh')
  )
  
  write.csv(setup.df,paste0(proj.dir,'Setup_Files/',paste0(experiment.id,'_setup.csv')),row.names =F)
  
}
