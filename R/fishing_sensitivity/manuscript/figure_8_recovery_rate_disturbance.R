#Figure 8: Recovery Rate by species as a function of disturbance size

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))

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

