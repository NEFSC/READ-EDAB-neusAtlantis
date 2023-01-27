#Function to extend the catch_ts timeseries by a scalar of the mean catch over a give time period
# fgs.file = atlantis groups filename
# original_catch_file = the current catch TS forcing
# start.time = the beginnning of the reference time for extending timeseries
# end.time = the ending of the reference time for extending timeseries
# duration = number of days to extend catch timeseries

make_catch_file_extended = function(fgs.file,original_catch_file,start.time,end.time,duration,overwrite =F,new_catch_file){
  
  library(dplyr)
  
  group.names = read.csv(fgs.file,as.is = T)$Code
  
  catch.orig = read.table(original_catch_file)
  colnames(catch.orig) = c('Time',group.names)
  catch.orig = catch.orig[-nrow(catch.orig),]
  
  catch.lines = readLines(original_catch_file)
  header = grep('#',catch.lines,value = T)
  
  catch.orig.long = catch.orig %>%
    tidyr::gather(Code,catch,-Time)
  
  mean.catch = catch.orig.long %>%
    filter(Time >= start.time & Time <= end.time)%>%
    group_by(Code)%>%
    summarise(catch = round(mean(catch,na.rm=T),2))
  
  mean.catch  = mean.catch[ match(group.names,mean.catch$Code),]
  
  new.time = seq(max(catch.orig$Time,na.rm=T),max(catch.orig$Time,na.rm=T)+duration,1)
  catch.ext = matrix(rep(mean.catch$catch,length(new.time)),ncol = length(group.names),byrow = T)
  
  catch.ext = cbind(new.time,catch.ext)
  colnames(catch.ext) = colnames(catch.orig)
  
  new.catch.data = rbind(catch.orig,catch.ext)
  
  #Test
  # plot(new.catch$MAK,type='l')
  
  if(overwrite == T){
    
    writeLines(header,new.catch.data)
    write.table(new.catch.data,row.names = F, col.names = F, file = original_catch_file,append = T)
    
  }else{
    
    file.create(new_catch_file)
    con = file(new_catch_file)
    writeLines(header,con)
    close(con)
    write.table(new.catch.data,row.names = F, col.names = F, file = new_catch_file,append = T)
  }

}

make_catch_file_extended(
  fgs.file = here::here('currentVersion','neus_groups.csv'),
  original_catch_file = here::here('currentVersion','CatchFiles','total_catch.ts'),
  start.time = 19724-(365*10),
  end.time = 19724,
  duration = 20*365,
  new_catch_file = here::here('currentVersion','CatchFiles','test_catch_extended.ts'),
  overwrite = F
)
