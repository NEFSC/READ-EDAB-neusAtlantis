library(dplyr)

profvis::profvis({
  
  t1 = Sys.time()
  proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
  # proj.dir = here::here('/')
  batch.prefix = 'fish_sens_catch_scalar_species_1'
  
  proj.duration.yr = 20
  fishing.levels = c(0,2,5,10,25,50,100)
  fishing.levels.text = c('0','1','5','10','25','50','100')
  make.catch.files = T
  
  #Read in Functional Group to guild match
  fgs = read.csv(paste0(proj.dir,'currentVersion/neus_groups.csv'),as.is = T)
  
  #Need to run baseline catch with projection (total_catch_projected_mean.ts)
  
  
  source(paste0(proj.dir,'/R/fishing_sensitivity/make_catch_file_projected_mean.R'))
  new.base.catch.file =  paste0(proj.dir,'currentVersion/CatchFiles/total_catch_',batch.prefix,'.ts')
  
  # profvis::profvis(
  # make_catch_file_projected_mean(
  #   fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
  #   original_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts'),
  #   start.time = 19724-(365*10),
  #   end.time = 19724,
  #   duration = proj.duration.yr*365,
  #   new_catch_file = new.base.catch.file,
  #   overwrite = F
  # )
  # )
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
  catch.dir = paste0(proj.dir,'currentVersion/CatchFiles/',batch.prefix)
  if(!dir.exists(catch.dir)){dir.create(catch.dir)}
  
  #Functions to make new catch files#Functions to makebatch.prefix new catch files
  source(paste0(proj.dir,'R/fishing_sensitivity/make_catch_scalar_projected.R'))
  new.catch.names = character()
  i=1
  
  for(i in 1:1){
    
    new.catch.name = paste0(batch.prefix,'_',scenario.combs$Code[i],'_',scenario.combs$fishing.levels.text[i])
    new.catch.names[i] = new.catch.name
    
    if(make.catch.files){
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
    
  }
  t2 = Sys.time()
  print(t2-t1)
})