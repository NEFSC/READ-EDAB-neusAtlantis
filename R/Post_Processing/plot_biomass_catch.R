# script to make plots of biomass with catch overlayed for diagnostics
library(dplyr)
library(ggplot2)

run.dir = 'C:/Users/joe92/Documents/Atlantis/New_Init_Age_10/'

biomass = read.table(paste0(run.dir,'neus_outputBiomindx.txt'),header = T)
  
catch = read.table(paste0(run.dir,'neus_outputCatch.txt'),header = T)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

plot.name = paste0(run.dir,'Post_Processed/Biomass_Catch.pdf')

pdf(plot.name,onefile = T)
for(i in 1:nrow(fgs)){
  
  if(fgs$IsTurnedOn[i] == 1){
    biomass.group = select(biomass,Time,all_of(fgs$Code[i])) %>%
      mutate(variable = 'Biomass') %>%
      rename(value = fgs$Code[i])  %>%
      mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
             year = as.numeric(format(date,format = '%Y')))%>%
      group_by(year,variable)%>%
      summarise(value = mean(value,na.rm=T))%>%
      ungroup()
    
    
    catch.group = select(catch,Time,all_of(fgs$Code[i])) %>%
      mutate(variable = 'Catch') %>%
      rename(value = fgs$Code[i])%>%
      mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
             year = as.numeric(format(date,format = '%Y')))%>%
      group_by(year,variable)%>%
      summarise(value = mean(value,na.rm=T))%>%
      ungroup()
    
    data.all = bind_rows(biomass.group,catch.group)
    
    p = ggplot(data.all,aes(x= year, y = value, color = variable))+
      geom_line()+
      ggtitle(fgs$Code[i])
    
    gridExtra::grid.arrange(p)
  }else{
    next()
  }

}

dev.off()
