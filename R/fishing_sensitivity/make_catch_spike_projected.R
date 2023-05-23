#Function to scale catch for scenario testing
original_catch_file = "C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/CatchFiles/total_catch_project_mean_20yrs.ts"
fgs.file = here::here('currentVersion','neus_groups.csv')
groups = c('MAK','HER')
new_catch_file = "C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/CatchFiles/fish_sens_catch_spike_species_1/test.ts"
spinup.end = 365*20
spike.start = 19724
spike.end = 19724 + (365*2)
time.end = 19724 + (365*20)
type = 'Scalar'
change = 2

make_catch_scale_spike_projected = function(original_catch_file,
                                      setup.filename,
                                      groups,
                                      change,
                                      spinup.end,
                                      start.time, 
                                      end.time,
                                      type,
                                      overwrite =F,
                                      fgs.file,
                                      new_catch_file ){
  
  
  #Get header from original 
  catch.lines = readLines(original_catch_file)
  header = grep('#',catch.lines,value = T)
  
  #Read in original catch and create value to overwrite during spike
  fgs = read.csv(fgs.file,as.is = T)
  
  orig.data = read.table(original_catch_file,as.is = T)
  colnames(orig.data) = c('Time',fgs$Code)
  catch.base = orig.data%>%
    tidyr::gather('groups','catch',-Time)%>%
    filter(groups %in% groups & catch >0 & Time < spike.start & Time > spinup.end)
  
  
  
  #create setupfile and save
  setup.df = data.frame(
    Group = groups,
    Start_Time = spike.start,
    End_Time = spike.end,
    Change = change,
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

# make_catch_scale_spike_projected(
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
