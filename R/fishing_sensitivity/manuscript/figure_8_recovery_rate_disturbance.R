#Figure 8: Recovery Rate by species as a function of disturbance size


out.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))


bio.run.stats = readRDS(paste0(data.dir,'recovery_stats_fspike_combined.rds')) %>%
  filter(scalar != 0)%>%
  left_join(guild2spp)%>%
  filter(!is.na(recovery.5) & recovery.5 >0)

ggplot(bio.run.stats,aes(x= scalar, y= recovery.5*100, color = Guild, group = Code))+
  geom_line(size = 1.25,alpha = 0.75)+
  # facet_wrap(~Guild)+
  scale_color_manual(values = guild.colors)+
  guides(color = guide_legend(nrow =1))+
  theme_bw()+
  ylab('Recovery Rate (% recovered/year) after 5 years')+
  xlab('Disturbance Size (Scalar of Contemporary)')+
  theme(panel.grid =element_blank(),
        legend.position = 'bottom')
ggsave(paste0(figure.dir,'Figure_8_Recovery_Rate_Scalar.png'),width = 10, height = 8, units = 'in',dpi = 300)

