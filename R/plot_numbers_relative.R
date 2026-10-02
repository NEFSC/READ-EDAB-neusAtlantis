library(ggplot2)
library(dplyr)
run = '/net/work3/EDAB/atlantis/Rob_proj/Atlantis_Runs/MAK_pers_37/'

data = readRDS(paste0(run,'Post_Processed/Data/numbers.rds'))

numbers = data %>%
  group_by(species)%>%
  mutate(atoutput = atoutput/atoutput[1])

p =ggplot(data = numbers, aes(x= time, y= atoutput))+
  geom_line()+
  facet_wrap(~species, scales = 'free_y')+
  ggsave(filename = here::here('numbers_rel_test.png'), width = 12, height = 12, units = 'in', dpi = 300)
