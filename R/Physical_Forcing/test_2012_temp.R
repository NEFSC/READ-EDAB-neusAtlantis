library(dplyr)
library(ggplot2)

temp.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/PreAggregated Temperature/'

years = 2010:2014
files = paste0('PreAggregated_Temperature_',years,'.R')

temp.ls = list()
for(f in 1:length(files)){
  load(paste0(temp.dir,files[f]))
  start = as.Date(paste0(years[f],'-01-01'))
  box_props$date = as.Date(box_props$band_level,origin = start-1)
  temp.ls[[f]] = box_props
  rm(box_props)
}

temp.df = bind_rows(temp.ls)

boxes.gom = c(11,16:20)

temp.gom = temp.df %>% filter(.bx0 %in% boxes.gom & roms_level == 40) %>%
  group_by(date) %>%
  summarize(mean.temp = mean(mean.temp, na.rm=T),
            med.temp = median(med.temp,na.rm=T))

ggplot(temp.gom, aes(x = date, y = mean.temp))+geom_path()
ggplot(temp.gom, aes(x = date, y = med.temp))+geom_path()

temp.gom2 = temp.df %>% filter(.bx0 %in% boxes.gom & neuslevels == 40) %>%
  group_by(date) %>%
  summarize(mean.temp = mean(mean.temp, na.rm=T),
            med.temp = median(med.temp,na.rm=T))


test = filter(temp.df, .bx0 == 3 & date == '2010-01-01')
