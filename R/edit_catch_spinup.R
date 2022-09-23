#Function to edit catch forcing

edit_catch_spinup = function(catch.file,fgs.file, spinup.years = 20,groups,scalar,overwrite =F, new.filename){
  library(stringr)
  #Get group names
  fgs = read.csv(fgs.file,as.is = T)
  group.names = fgs$Code
  
  #Get header section
  catch.lines = readLines(catch.file)
  header = grep('#',catch.lines,value = T)
  
  catch.orig = read.table(catch.file)
  colnames(catch.orig) = c('Time',group.names)
  new.catch = catch.orig
  
  which.spinup = 1:(365*spinup.years)
  for(i in 1:length(groups)){
    
    which.group = which(group.names == groups[i])
    catch.group = dplyr::select(catch.orig,groups[i])[,1]
    mean.spinup = mean(catch.group[which.spinup],na.rm=T)
    new.spinup = mean.spinup * scalar[i]
    new.catch[which.spinup,(which.group +1)] = new.spinup
  }
  
  if(overwrite == T){
    
    writeLines(header,catch.file)
    write.table(new.catch,row.names = F, col.names = F, file = catch.file,append = T)
    
  }else{
    
    file.create(new.filename)
    con = file(new.filename)
    writeLines(header,con)
    close(con)
    write.table(new.catch,row.names = F, col.names = F, file = new.filename,append = T)
    

  }
  
}

# Usage
edit_catch_spinup(catch.file = here::here('currentVersion','CatchFiles','total_catch.ts'),
                  fgs.file= here::here('currentVersion','neus_groups.csv'),
                  groups = 'HER',
                  scalar = 1,
                  overwrite = F,
                  new.filename = here::here('currentVersion','CatchFiles','total_catch_test.ts'),
                  spinup.years = 20)
