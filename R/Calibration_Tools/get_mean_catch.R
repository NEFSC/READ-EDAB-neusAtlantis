#Function to get the mean catch from a catch.ts file

get_mean_catch = function(fgs.file,catch.file, start.time, end.time){
  library(dplyr)
  
  fgs = read.csv(fgs.file,as.is = T)
  group.names = fgs$Code
  
  catch.data = read.table(catch.file)
  colnames(catch.data) = c('Time',group.names)
  
  catch.data = catch.data %>%
    tidyr::gather(group,catch,-Time)%>%
    filter(Time >= start.time & Time <= end.time)%>%
    group_by(group)%>%
    summarise(mean.catch = mean(catch,na.rm=T))
  
  return(catch.data)
}
