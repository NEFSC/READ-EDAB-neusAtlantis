#Figure 6: proportion recovered at 5yrs by species
library(ggplot2)
library(dplyr)

fished.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
unfished.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_UnfishedRecovery/'

out.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))

fished.data = readRDS(paste0(fished.dir,'recovery_stats_fspike_combined.rds'))%>%
  filter(scalar %in% c(2,5,10,25,50,100))%>%
  mutate(source = 'fished_recovery',
         disturb.prop.fished = b.t.min/b.t1)%>%
  rename(recovery.5.fished = 'recovery.5')%>%
  select(Code,scalar,disturb.prop.fished,recovery.5.fished)

unfished.data = readRDS(paste0(unfished.dir,'recovery_stats_fspike_UnfishedRecovery.rds'))%>%
  filter(scalar %in% c(2,5,10,25,50,100))%>%
  mutate(source = 'unfished_recovery',
         disturb.prop.unfished = b.t.min/b.t1)%>%
  rename(recovery.5.unfished = 'recovery.5')%>%
  select(Code,scalar,disturb.prop.unfished,recovery.5.unfished)

data.all = fished.data %>%
  left_join(unfished.data)%>%
  mutate(disturbance = disturb.prop.unfished - disturb.prop.fished,
         recovery.5 = recovery.5.unfished - recovery.5.fished)%>%
  left_join(guild2spp)%>%
  left_join(fgs)%>%
  filter(!is.na(LongName))%>%
  arrange(LongName)%>%
  filter(scalar <= 25)
  

# data.all = fished.data %>%
#   bind_rows(unfished.data)%>%
#   mutate(disturbance = b.t1 - b.t.min)%>%
#   left_join(guild2spp)%>%
#   left_join(fgs)%>%
#   filter(!is.na(LongName))%>%
#   arrange(desc(LongName))%>%
#   filter(scalar <= 25)

ggplot(data = data.all, aes(x=0,xend = disturbance, y = LongName,yend = LongName, color = Guild))+
  geom_vline(xintercept = 0,lty =1,size = 0.25)+
  geom_segment(size = 1.1)+
  scale_y_discrete(limits = rev)+
  xlim(-1,1)+
  guides(color = guide_legend(nrow = 1))+
  facet_wrap(~scalar,ncol = 6)+
  xlab('Disturbance Impact (unfished - fished)')+
  ylab('')+
  theme_bw()+
  theme(legend.position = 'bottom', 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave(paste0(figure.dir,'Figure_8b_fished_effect_disturbance.png'),width = 12, height = 10, units = 'in',dpi = 300)

ggplot(data = data.all, aes(x=0,xend = recovery.5, y = LongName,yend = LongName, color = Guild))+
  geom_vline(xintercept = 0,lty =1,size = 0.25)+
  geom_segment(size = 1.1)+
  scale_y_discrete(limits = rev)+
  guides(color = guide_legend(nrow = 1))+
  facet_wrap(~scalar,ncol = 6)+
  xlab('Recovery Rate (unfished - fished)')+
  ylab('')+
  theme_bw()+
  theme(legend.position = 'bottom', 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave(paste0(figure.dir,'Figure_8c_fished_effect_recovery_rate.png'),width = 12, height = 10, units = 'in',dpi = 300)

