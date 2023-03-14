#Script to combine Altantis-formatted GLORYS data into one data file
library(dplyr)
library(ggplot2)

data.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Atlantis_Format/'
out.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Summary/'
years = 1993:2017
var.names = c('temperature','salinity')

out.data.ls = list()
yr =1
for(yr in 1:length(years)){
  
  dat.file = paste0(data.dir,years[yr],'/GLORYS_Atlantis_statevars_',years[yr],'.R')
  load(dat.file)
  
  #format temperature
  yr.temp = temperature %>%
    reshape2::melt()%>%
    mutate(value = ifelse(value < 0, NA,value))%>%
    rename(level = 'Var1',
           box = 'Var2',
           time = 'Var3',
           temperature = 'value')%>%
    mutate(date = as.Date(as.POSIXct((time-1)*86400,origin = paste0(years[yr],'-01-01 00:00:00',tz = 'UTC'))),
           box = box-1)%>%
    filter(!is.na(temperature))
    
  yr.salt = salinity %>%
    reshape2::melt()%>%
    mutate(value = ifelse(value < 0, NA,value))%>%
    rename(level = 'Var1',
           box = 'Var2',
           time = 'Var3',
           salinity = 'value')%>%
    mutate(date = as.Date(as.POSIXct((time-1)*86400,origin = paste0(years[yr],'-01-01 00:00:00',tz = 'UTC'))),
           box= box-1)%>%
    filter(!is.na(salinity))
 
  out.data.ls[[yr]] = yr.temp %>%
    left_join(yr.salt)%>%
    select(date,level,box,time,temperature,salinity)
  
  print(years[yr])
  
}

out.data = bind_rows(out.data.ls)

write.csv(out.data,file = paste0(out.dir,'GLORYS_Atlantis_Level_Statevars_all.csv'),row.names = F)
# ggplot(out.data,aes(x=date,y=temperature,color = factor(level)))+
#   geom_line()+
#   facet_wrap(~box)
