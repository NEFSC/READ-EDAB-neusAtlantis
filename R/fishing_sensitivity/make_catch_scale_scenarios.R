#Function to scale catch for scenario testing

make_catch_scale_scenarios = function(original_catch_file,
                                      setup.filename,
                                      groups,
                                      change,
                                      start.time, 
                                      end.time,
                                      type,
                                      overwrite =F,
                                      fgs.file,
                                      new_catch_file,
                                      repo.dir){
  
  #Get header from original 
  catch.lines = readLines(original_catch_file)
  header = grep('#',catch.lines,value = T)
  
  #create setupfile and save
  setup.df = data.frame(
    Group = groups,
    Start_Time = start.time,
    End_Time = end.time,
    Change = change,
    Type = type
  )
  write.csv(setup.df,file = setup.filename,row.names = F)
  
  source(paste0(repo.dir,'R/scale_catch_functions.r'))
  
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

# make_catch_scale_scenarios(
#   original_catch_file = here::here('currentVersion','CatchFiles','total_catch.ts'),
#   fgs.file = here::here('currentVersion','neus_groups.csv'),
#   groups = c('MAK','HER','MEN','BUT'),
#   new_catch_file = here::here('currentVersion','CatchFiles','total_catch_planktivore_2x.ts'),
#   setup.filename = here::here('currentVersion','CatchFiles','planktivore_2x.csv'),
#   start.time = 365*20,
#   end.time = 19724,
#   type = 'Scalar',
#   change = 2
# )
