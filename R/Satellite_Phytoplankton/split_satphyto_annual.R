#Satellite Phytoplankton data too large to work with as single .csv. Split into annual files and save to directory
library(dplyr)

satphyto.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
full.data = read.csv(paste0(satphyto.dir, 'D8-OCCCI-ATLANTIS_NEUS-VER_1.csv'),as.is = T)

period.df = full.data %>% 
  select(PERIOD) %>%
  # group_by(PERIOD) %>%
  # summarize(N = n()) %>%
  tidyr::separate(PERIOD,c('code','start','stop')) %>%
  # select(-N, -code)%>%
  transmute(start = as.Date(start,format = '%Y%m%d'),
            stop = as.Date(stop,format = '%Y%m%d')) %>%
  mutate(mid = start +3,
         mid.year = format(mid,format = '%Y'),
         start.year = format(start,format = '%Y'),
         stop.year = format(stop,format = '%Y'))
  
years = unique(c(period.df$start.year,period.df$stop.year))


for(y in 1:length(years)){
  out.name = paste0('D8-OCCCI-ATLANTIS_',years[y],'.csv')
  year.match = which(period.df$mid.year == years[y])
  dat = full.data[year.match,]
  dat = cbind(dat, period.df[year.match,c('start','stop','mid','start.year','stop.year','mid.year')])
  write.csv(dat,paste0(satphyto.dir,out.name),row.names = F)
  
}
