#Figure 8: Recovery Rate by species as a function of disturbance size

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))
guild.color.df = data.frame(Guild = sort(unique(guild2spp$Guild)),plot.color = guild.colors)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>% select(Code,LongName)

bio.run.stats = readRDS(paste0(data.dir,'recovery_stats_fspike_combined.rds')) %>%
  filter(scalar != 0)%>%
  left_join(guild2spp)%>%
  left_join(fgs)%>%
  filter(!is.na(recovery.5))%>%
  filter(scalar %in% c(2,5,10,50,100))%>%
  arrange(LongName)

spp.min = bio.run.stats %>%
  select(Code,LongName,scalar,db.t5)%>%
  mutate(is.zero = ifelse(db.t5 == 0,T,F))%>%
  filter(is.zero == F)%>%
  group_by(Code,LongName)%>%
  summarise(max.scalar = max(scalar,na.rm=T))

bio.run.stats2 =  bio.run.stats %>%
  left_join(guild.color.df)%>%
  filter(Code != 'SG')%>%
  left_join(spp.min)%>%
  filter(scalar <= max.scalar)
  
bio.run.sort = bio.run.stats2 %>%
  select(Code,LongName,Guild,scalar,db.tmin,db.t5,db.t20)%>%
  group_by(Code)%>%
  mutate(min.prop = db.t5 == min(db.t5))%>%
  filter(min.prop == T)%>%
  ungroup()%>%
  distinct(Code,db.t5,.keep_all = T)%>%
  # filter(scalar %in% c(2,100))%>%
  # tidyr::spread(scalar,db.t5)%>%
  # rename(s.2 = '2',s.100 = '100')%>%
  arrange(Guild,db.t5)%>%
  mutate(plot.order.t5 = 1:n())%>%
  arrange(Guild,db.tmin)%>%
  mutate(plot.order.tmin = 1:n())%>%
  arrange(Guild,db.t20)%>%
  mutate(plot.order.t20 = 1:n())%>%
  select(Code,plot.order.tmin,plot.order.t5,plot.order.t20)

bio.run.stats2 = bio.run.stats2%>%
  left_join(bio.run.sort,by = 'Code')%>%
  arrange(Guild,plot.order.t5)

name.col.t5 = bio.run.stats2 %>% 
  filter(scalar == 2)%>%
  select(Code,Guild,plot.color)

ggplot(bio.run.stats2, aes(color= factor(scalar),
                           y  = reorder(LongName,plot.order.t5),
                           yend =  reorder(LongName,plot.order.t5),
                           x = db.t5,xend = 1))+
  geom_segment(color = 'grey70')+
  # geom_point(size = 4,shape = 108,alpha = 0.6)+
  geom_errorbar(aes(ymin = plot.order.t5-.4,ymax = plot.order.t5+.4, x = db.t5),linewidth = 1)+
  # scale_y_discrete(limits=rev)+
  scale_color_manual(name = 'Disturbance Scalar',values = RColorBrewer::brewer.pal(5,'Set1'))+
  geom_hline(yintercept = c(6.5,27.5,33.5,45.5,53.5),lty = 3,color = 'grey30')+
  xlab('Recoved proportion after 5 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_text(color = name.col.t5$plot.color ))
ggsave(paste0(figure.dir,'Figure_8_Recovery_Prop_5yr.png'),width = 7, height = 8, units = 'in',dpi = 300)

bio.run.stats2 = bio.run.stats2 %>%
  group_by(Guild)%>%
  arrange(plot.order.t20)%>%
  ungroup()
  
name.col.t20 = bio.run.stats2 %>% 
  filter(scalar == 2)%>%
  select(Code,Guild,plot.color)

ggplot(bio.run.stats2, aes(color= factor(scalar),
                           y  = reorder(LongName,plot.order.t20),
                           yend =  reorder(LongName,plot.order.t20),
                           x = db.t20,xend = 1))+
  geom_segment(color = 'grey70')+
  # geom_point(size = 4,shape = 108,alpha = 0.6)+
  geom_errorbar(aes(ymin = plot.order.t20-.4,ymax = plot.order.t20+.4, x = db.t20),linewidth = 1)+
  # scale_y_discrete(limits=rev)+
  scale_color_manual(name = 'Disturbance Scalar',values = RColorBrewer::brewer.pal(5,'Set1'))+
  geom_hline(yintercept = c(6.5,27.5,33.5,45.5,53.5),lty = 3,color = 'grey30')+
  xlab('Recoved proportion after 20 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_text(color = name.col.t20$plot.color ))
ggsave(paste0(figure.dir,'Figure_8_Recovery_Prop_20yr.png'),width = 7, height = 8, units = 'in',dpi = 300)

bio.run.stats2 = bio.run.stats2 %>%
  group_by(Guild)%>%
  arrange(plot.order.tmin)%>%
  ungroup()

name.col.tmin = bio.run.stats2 %>% 
  filter(scalar == 2)%>%
  select(Code,Guild,plot.color)

ggplot(bio.run.stats2, aes(color= factor(scalar),
                           y  = reorder(LongName,plot.order.tmin),
                           yend =  reorder(LongName,plot.order.tmin),
                           x = db.tmin,xend = 1))+
  geom_segment(color = 'grey70')+
  # geom_point(size = 4,shape = 108,alpha = 0.6)+
  geom_errorbar(aes(ymin = plot.order.tmin-.4,ymax = plot.order.tmin+.4, x = db.tmin),linewidth = 1)+
  # scale_y_discrete(limits=rev)+
  scale_color_manual(name = 'Disturbance Scalar',values = RColorBrewer::brewer.pal(5,'Set1'))+
  geom_hline(yintercept = c(6.5,27.5,33.5,45.5,53.5),lty = 3,color = 'grey30')+
  xlab('Maximum Disturbance Proportion')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_text(color = name.col.tmin$plot.color ))
ggsave(paste0(figure.dir,'Figure_9_Disturbance_Impact.png'),width = 10, height = 8, units = 'in',dpi = 300)
