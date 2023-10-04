#Figure 7: Impact after disturbance by species

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))
guild.color.df = data.frame(Guild = sort(unique(guild2spp$Guild)),plot.color = guild.colors)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>% select(Code,LongName)
multi.spp = read.csv(here::here('diagnostics','multigroup_index.csv'),as.is =T)

bio.run.stats = readRDS(paste0(data.dir,'recovery_stats_fspike_combined.rds')) %>%
  filter(scalar != 0)%>%
  left_join(guild2spp)%>%
  left_join(fgs)%>%
  left_join(multi.spp)%>%
  filter(!is.na(recovery.20) & IsMulti == 0)%>%
  filter(scalar %in% c(2,5,10,50,100))%>%
  arrange(LongName)

spp.min = bio.run.stats %>%
  select(Code,LongName,scalar,db.tmin)%>%
  mutate(is.zero = ifelse(db.tmin == 0,T,F))%>%
  group_by(Code,is.zero)%>%
  mutate(count = n())%>%
  ungroup()%>%
  mutate(dup.zero = is.zero & count > 1)%>%
  group_by(Code,dup.zero)%>%
  mutate(min.scalar = scalar == min(scalar))%>%
  ungroup()%>%
  mutate(trim = dup.zero & !min.scalar)%>%
  filter(trim == F)%>%
  group_by(Code,LongName)%>%
  summarise(max.scalar = max(scalar,na.rm=T))



bio.run.stats2 =  bio.run.stats %>%
  left_join(guild.color.df)%>%
  filter(Code != 'SG')%>%
  left_join(spp.min)%>%
  filter(scalar <= max.scalar)

bio.run.sort = bio.run.stats2 %>%
  select(Code,LongName,Guild,scalar,db.tmin)%>%
  group_by(Code)%>%
  mutate(min.prop = db.tmin == min(db.tmin))%>%
  filter(min.prop == T)%>%
  ungroup()%>%
  distinct(Code,db.tmin,.keep_all = T)%>%
  arrange(Guild,db.tmin)%>%
  mutate(plot.order = 1:n())%>%
  select(Code,plot.order)

bio.run.stats2 = bio.run.stats2%>%
  left_join(bio.run.sort,by = 'Code')%>%
  arrange(Guild,plot.order)

name.col = bio.run.stats2 %>% 
  filter(scalar == 2)%>%
  select(Code,Guild,plot.color)

ggplot(bio.run.stats2, aes(color= factor(scalar),
                           y  = reorder(LongName,plot.order),
                           yend =  reorder(LongName,plot.order),
                           x = db.tmin,xend = 1))+
  geom_segment(color = 'grey70')+
  # geom_point(size = 4,shape = 108,alpha = 0.6)+
  geom_errorbar(aes(ymin = plot.order-.4,ymax = plot.order+.4, x = db.tmin),linewidth = 1)+
  # scale_y_discrete(limits=rev)+
  scale_color_manual(name = 'Disturbance Scalar',values = RColorBrewer::brewer.pal(5,'Set1'))+
  geom_hline(yintercept = c(6.5,27.5,33.5,45.5,53.5),lty = 3,color = 'grey50',linewidth = 0.25)+
  xlab('Disturbance Magnitude')+
  ylab('')+
  theme_bw()+
  theme(panel.grid.minor =element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_text(color = name.col$plot.color ))
ggsave(paste0(figure.dir,'Figure_7b_Initial_Disturbance_Size.png'),width = 7, height = 8, units = 'in',dpi = 300)
