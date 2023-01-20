library(ggplot2)
library(dplyr)
catch.data = read.table(here::here('currentVersion','CatchFiles','total_catch_new_spinup.txt'))
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),stringsAsFactors = F)
colnames(catch.data) = c('time',fgs$Code)

catch.new = catch.data %>%
  tidyr::gather('Code','catch.mg.s',-time) %>%
  mutate(date = as.POSIXct(time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')),
         catch.mt.d = catch.mg.s*86400*5.7*20*1E-9
         )%>%
  group_by(year,Code)%>%
  summarise(catch.mt = sum(catch.mt.d,na.rm=T))

plot(catch.mt~year,catch.new,'l')  


saveRDS(catch.new,here::here('diagnostics','NEUSv2_adjusted_catch.rds'))

catch.data.old = read.table(here::here('currentVersion','CatchFiles','total_catch.ts'))
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),stringsAsFactors = F)
colnames(catch.data.old) = c('time',fgs$Code)

catch.old = catch.data.old %>%
  tidyr::gather('Code','catch.mg.s',-time) %>%
  mutate(date = as.POSIXct(time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')),
         catch.mt.d = catch.mg.s*86400*5.7*20*1E-9
  )%>%
  group_by(year,Code)%>%
  summarise(catch.mt.old = sum(catch.mt.d,na.rm=T))


catch.total = catch.new %>% left_join(catch.old)%>%
  tidyr::gather('variable','catch',-year,-Code)

ggplot(catch.total, aes(x=year,y=catch,color = variable))+
  geom_line()+
  xlab('Catch (mt)')+
  facet_wrap(~Code,scales = 'free_y')+
  ggsave(here::here('Figures','catch_comparisons_new_clams.png'),width = 20,height = 12, units = 'in', dpi = 300)

