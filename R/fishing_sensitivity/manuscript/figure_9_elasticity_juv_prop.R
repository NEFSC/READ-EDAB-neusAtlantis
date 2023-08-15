#Figure 9: Elasticity as a function of juvenile proportion

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv')) %>%
  select(Code,LongName,Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))

exploitable.age = readRDS(paste0(data.dir,'exploitable_age_fspike_combined.rds'))

# ref.data = readRDS('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/Post_Processed/Data/ref_run_summary.rds')

bio.age.lm = readRDS( paste0(data.dir,'age_stats_lm_fspike_combined.rds'))%>%
  select(Code,slope)%>%
  rename(slope.age = 'slope')

bio.recovery.rate = readRDS(paste0(data.dir,'recovery_rate_lm_fspike_combined.rds'))%>%
  filter(recovery.time == 5)

data.recovery.age = exploitable.age %>%
  left_join(bio.recovery.rate)%>%
  left_join(guild2spp)%>%
  filter(Biomass.exploitable.prop <1)

ggplot(data.recovery.age, aes(x = 1-Biomass.exploitable.prop,y = slope, color = Guild))+
  geom_point()+
  ylab('Elasticity')+
  xlab('Unexploitable Proportion')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'Recoverability_v_Juvenile_',experiment.id,'.png'),width = 12, height = 10, units = 'in', dpi = 300)
