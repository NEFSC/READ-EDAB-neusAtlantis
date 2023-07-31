#Function to scale catch for scenario testing

make_catch_scale_constant_scenarios = function(original_catch_file,
                                      setup.filename,
                                      groups,
                                      change,
                                      start.time, 
                                      end.time,
                                      type,
                                      overwrite =F,
                                      fgs.file,
                                      new_catch_file ){
  
  #Get header from original 
  catch.lines = readLines(original_catch_file)
  header = grep('#',catch.lines,value = T)
  
  source(here::here('R','get_mean_catch.R'))
  
  mean.catch = get_mean_catch(fgs.file = fgs.file,catch.file = original_catch_file,start.time = start.time, end.time = end.time)%>%
    filter(group %in% groups)
  
  #create setupfile and save
  setup.df = data.frame(
    Group = mean.catch$group,
    Start_Time = start.time,
    End_Time = end.time,
    Change = mean.catch$mean.catch*change,
    Type = type
  )
  
  write.csv(setup.df,file = setup.filename,row.names = F)
  
  source(here::here('R','scale_catch_functions.r'))
  
  new.catch.data = scale_catch(
    filename = setup.filename,
    original_catch_file = original_catch_file,
    fgs.file = fgs.file)
  
  if(overwrite == T){
    
    writeLines(header,new.catch.data)
    write.table(new.catch.data,row.names = F, col.names = F, file = catch.file,append = T)
    
  }else{
    
    file.create(new_catch_file)
    con = file(new_catch_file)
    writeLines(header,con)
    close(con)
    write.table(new.catch.data,row.names = F, col.names = F, file = new_catch_file,append = T)
  }
  
  
}

groups = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)$Code
make_catch_scale_constant_scenarios(
  original_catch_file = here::here('currentVersion','CatchFiles','total_catch.ts'),
  fgs.file = here::here('currentVersion','neus_groups.csv'),
  groups = groups,
  new_catch_file = here::here('currentVersion','CatchFiles','constant_catch_test.ts'),
  setup.filename = here::here('currentVersion','CatchFiles','constant_catch_test.csv'),
  start.time = 365*20,
  end.time = 19724,
  type = 'Replace',
  change = 1
)
