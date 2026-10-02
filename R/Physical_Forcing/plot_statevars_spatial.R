#Script5 to make plots of temp and salt for Non-spinup years based on output
library(dplyr)
library(ggplot2)

run.name = 'update_physics_2021'

fig.dir = here::here('Atlantis_Runs',run.name,'Post_Processed','')

phys = readRDS(here::here('Atlantis_Runs',run.name,'Post_Processed','Data','physics_statevars.rds')) %>%
  filter(time >= 30 & variable %in% c('Temp','salt'))%>%
  group_by(variable,polygon)%>%
  mutate(max.lev = layer == max(layer))%>%
  filter(max.lev == T)

temp = phys %>% filter(variable == 'Temp')
salt = phys %>% filter(variable == 'salt')

ggplot(temp, aes(x= time+1964,y = atoutput),alpha = 0.5)+
  geom_line()+
  facet_wrap(~polygon)+
  theme_bw()+
  ylab('temperature')+
  ggsave(file = paste0(fig.dir,'temperature_box.png'))
  
ggplot(salt, aes(x= time+1964,y = atoutput),alpha = 0.5)+
  geom_line()+
  facet_wrap(~polygon)+
  theme_bw()+
  ylab('salinity')+
  ggsave(file = paste0(fig.dir,'salt_box.png'))

