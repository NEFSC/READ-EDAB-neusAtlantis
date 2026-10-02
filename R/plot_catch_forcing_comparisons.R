library(dplyr)
library(ggplot2)

catch.old = read.table('/home/jcaracappa/atlantis/Joe_Proj/currentVersion/CatchFiles/total_catch.ts')
catch.new = read.table('/home/jcaracappa/atlantis/Joe_Proj_alt/currentVersion/CatchFiles/total_catch.ts')

codes = read.csv(here::here('currentVersion','neus_groups.csv'))$Code

colnames(catch.old) = c('Time',codes)
colnames(catch.new) = c('Time',codes)

catch.old = catch.old %>%
  tidyr::gather(Code,Catch,-Time)%>%
  mutate(source = 'Old Catch')
catch.new = catch.new %>% 
  tidyr::gather(Code,Catch,-Time) %>%
  mutate(source = 'New Catch')

catch.all = bind_rows(catch.old,catch.new)

ggplot(catch.all, aes(x= Time, y= Catch, color = source))+
  geom_line()+
  facet_wrap(~Code,scale = 'free_y')
