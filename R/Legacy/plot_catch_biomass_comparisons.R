library(ggplot2)
library(dplyr)

run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_08122021/'

bio = read.table(paste0(run.dir,'neus_outputBiomIndx.txt'),header = T)
catch = read.table(paste0(run.dir,'neus_outputCatch.txt'),header = T)

bio = bio[,1:90]
catch = catch[,1:78]

bio.cols=colnames(bio)
catch.cols = colnames(catch)

bio = bio[bio.cols %in% catch.cols]

bio = bio[,colMeans(catch)!=0]
catch = catch[,colMeans(catch)!=0]

bio$variable = 'biomass'
catch$variable = 'catch'

data = bind_rows(bio,catch)

data.long = reshape2::melt(data,id.vars = c('Time','variable'),variable.name = 'group') %>%
  mutate(date = as.Date(as.POSIXct(Time*86400,origin= '1964-01-01 00:00:00',tz = 'UTC')))

groups = sort(as.character(unique(data.long$group)))

data.long$group = factor(data.long$group,levels = groups)

ggplot(data.long, aes(x= date, y = value,col = variable))+
  geom_line()+
  facet_wrap(~group,scale = 'free_y')+
  theme(legend.position = 'bottom')+
  ylab('Value')+
  ggsave(paste0(run.dir,'Post_Processed/catch_biomass_comparison.png'),width =12,height = 8, units = 'in',dpi = 300)


#Do Relative Values
for(i in 2:56){
  bio[,i] = bio[,i]/bio[2,i]
  catch[,i] = catch[,i]/catch[2,i]
}

bio$variable = 'biomass'
catch$variable = 'catch'

data = bind_rows(bio,catch)

data.long.rel = reshape2::melt(data,id.vars = c('Time','variable'),variable.name = 'group') %>%
  mutate(date = as.Date(as.POSIXct(Time*86400,origin= '1964-01-01 00:00:00',tz = 'UTC')))

data.long.rel$group = factor(data.long.rel$group,levels = groups)
ggplot(data.long.rel, aes(x= date, y = value,col = variable))+
  geom_line()+
  facet_wrap(~group,scale = 'free_y')+
  theme(legend.position = 'bottom')+
  ylab('Relative Value')+
  ggsave(paste0(run.dir,'Post_Processed/catch_biomass_relative_comparison.png'),width =12,height = 8, units = 'in',dpi = 300)
