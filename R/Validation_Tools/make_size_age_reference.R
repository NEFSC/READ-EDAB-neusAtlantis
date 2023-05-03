#Script to generate the correct size-at-age based on the vertebrate_init_length_cm.csv  reference file
#Reference file assumes the year is the last year of the age class. 
# fgs.file = here::here('currentVersion','neus_groups.csv')
# ref.length.age = here::here('currentVersion','vertebrate_init_length_cm.csv')
# out.file = here::here('diagnostics','vertebrate_init_length_cm_Adjusted.csv')
make_size_age_reference = function(fgs.file,ref.length.age,out.file){
  library(dplyr)
  library(ncdf4)
  
  fgs = read.csv(fgs.file)
  
  age.key = data.frame(variable = paste0('X',1:10),agecl = 1:10)
  
  length.age.ref = read.csv(ref.length.age) %>%
    select(Long.Name:X10)%>%
    reshape2::melt(id.vars = 'Long.Name')%>%
    left_join(age.key)%>%
    left_join(fgs, by = c('Long.Name'= 'LongName'))%>%
    rename(species = 'Long.Name',
           length.ref = 'value')%>%
    select(Code,species,NumAgeClassSize,agecl,length.ref)%>%
    arrange(species,agecl)%>%
    mutate(yr.upper = agecl * NumAgeClassSize)
  
  age.groups = unique(length.age.ref$Code)
  
  new.ref.df = list()
  for(i in 1:length(age.groups)){
    
    new.ref.df[[i]] = length.age.ref %>%
      filter(Code == age.groups[i])%>%
      mutate(ref.year = yr.upper - first(NumAgeClassSize)/2,
             new.length.ref = length.ref - (diff(c(0,length.ref))/2))
    
    # plot(length.ref ~yr.upper, group.dat, ylim = c(0, max(group.dat$length.ref)), xlim = c(0,max(group.dat$yr.upper)))
    # points(x = (group.dat$yr.upper - group.dat$NumAgeClassSize[1]/2), y = group.dat$length.ref - (diff(c(0,group.dat$length.ref))/2), col = 2)
    # points(x = group.dat$ref.year, y = group.dat$new.length.ref,col = 3)
    
  }  
  
  new.ref.df = bind_rows(new.ref.df)
  
  write.csv(new.ref.df,out.file,row.names = F)
}
