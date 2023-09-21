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
  

ggplot(bio.run.stats, aes(color= factor(scalar),y  = LongName,x = recovery.5*100,xend = 0,yend = LongName))+
  geom_segment(color = 'grey50')+
  geom_point()+
  scale_y_discrete(limits=rev)+
  xlab('Recovery Rate (% recovered/year) after 5 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
          legend.position = 'bottom')
ggsave(paste0(figure.dir,'Figure_8_Recovery_Rate_5yr_Scalar.png'),width = 7, height = 8, units = 'in',dpi = 300)

ggplot(bio.run.stats, aes(color= factor(scalar),y  = LongName,x = db.t5,xend = 1,yend = LongName))+
  geom_segment(color = 'grey50')+
  geom_point()+
  scale_y_discrete(limits=rev)+
  xlab('Recover proportion after 5 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom')
ggsave(paste0(figure.dir,'Figure_8_Recovery_Prop_5yr_Scalar.png'),width = 7, height = 8, units = 'in',dpi = 300)

bio.run.sort = bio.run.stats %>%
  select(Code,LongName,Guild,scalar,db.t5)%>%
  filter(scalar %in% c(2,100))%>%
  tidyr::spread(scalar,db.t5)%>%
  rename(s.2 = '2',s.100 = '100')%>%
  arrange(Guild,s.100)%>%
  mutate(plot.order = 1:n())%>%
  select(Code,plot.order)
 
        
bio.run.stats2 =  bio.run.stats %>%
  left_join(bio.run.sort)%>%
  left_join(guild.color.df)%>%
  arrange(plot.order)%>%
  filter(Code != 'SG')

name.col = bio.run.stats2 %>% 
  filter(scalar == 100)%>%
  select(Code,Guild,plot.order,plot.color)
  
ggplot(bio.run.stats2, aes(color= factor(scalar),y  = reorder(LongName,plot.order),x = db.t5,xend = 1,yend =  reorder(LongName,plot.order)))+
  geom_segment(color = 'grey50')+
  geom_point(size = 3,alpha = 0.5)+
  scale_y_discrete(limits=rev)+
  scale_color_manual(name = 'Disturbance Scalar',values = RColorBrewer::brewer.pal(5,'Set1'))+
  geom_hline(yintercept = c(1.5,9.5,21.5,27.5,48.5),lty = 3,color = 'grey30')+
  xlab('Recover proportion after 5 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_text(color = rev(name.col$plot.color )))
ggsave(paste0(figure.dir,'Figure_8_Recovery_Prop_5yr_Scalar_Alt.png'),width = 7, height = 8, units = 'in',dpi = 300)

spp.min = bio.run.stats2 %>%
  select(Code,LongName,scalar,db.t5)%>%
  mutate(is.zero = ifelse(db.t5 == 0,T,F))%>%
  filter(is.zero == F)%>%
  group_by(Code,LongName)%>%
  summarise(max.scalar = max(scalar,na.rm=T))

bio.run.stats3 = bio.run.stats2 %>%
  left_join(spp.min)%>%
  filter(scalar <= max.scalar)%>%
  arrange(Guild,plot.order)
  
ggplot(bio.run.stats3, aes(color= factor(scalar),y  = reorder(LongName,plot.order),x = db.t5,xend = 1,yend =  reorder(LongName,plot.order)))+
  geom_segment(color = 'grey50')+
  geom_point(size = 3,alpha = 0.6)+
  scale_y_discrete(limits=rev)+
  scale_color_manual(name = 'Disturbance Scalar',values = RColorBrewer::brewer.pal(5,'Set1'))+
  geom_hline(yintercept = c(1.5,9.5,21.5,27.5,48.5),lty = 3,color = 'grey30')+
  xlab('Recoved proportion after 5 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_text(color = rev(name.col$plot.color )))
ggsave(paste0(figure.dir,'Figure_8_Recovery_Prop_5yr_Scalar_Alt2.png'),width = 7, height = 8, units = 'in',dpi = 300)


ggplot(bio.run.stats, aes(color= factor(scalar),y  = LongName,x = recovery.20*100,xend = 0,yend = LongName))+
  geom_segment(color = 'grey50')+
  geom_point()+
  scale_y_discrete(limits=rev)+
  xlab('Recovery Rate (% recovered/year) after 20 years')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom')
ggsave(paste0(figure.dir,'Figure_8_Recovery_Rate_20yr_Scalar.png'),width = 7, height = 8, units = 'in',dpi = 300)


ggplot(bio.run.stats,aes(y= LongName, x = db.tmin,label = Code,color = Guild))+
  geom_point()+
  # geom_text_repel(max.overlaps = 50)+
  facet_wrap(~scalar,nrow = 1)+
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),labels = c('0','0.25','0.5','0.75','1'))+
  xlab('Disturbance Impact (proportion)')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom')
ggsave(paste0(figure.dir,'Figure_9_Disturbance_Impact.png'),width = 10, height = 8, units = 'in',dpi = 300)

  
  
# ggplot(bio.run.stats,aes(x= scalar, y= recovery.5*100, color = Guild))+
#   geom_point()+
#   geom_line(size = 1.25,alpha = 0.75)+
#   facet_wrap(~Code)+
#   scale_color_manual(values = guild.colors)+
#   guides(color = guide_legend(nrow =1))+
#   theme_bw()+
#   ylab('Recovery Rate (% recovered/year) after 5 years')+
#   xlab('Disturbance Size (Scalar of Contemporary)')+
#   theme(panel.grid =element_blank(),
#         legend.position = 'bottom')

