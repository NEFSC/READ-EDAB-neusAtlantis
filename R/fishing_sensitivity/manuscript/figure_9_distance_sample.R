# Figure 10: Deviation from sample distribution of random catch permutations

experiment.id = 'random_catch_combined'

data.dir = 'Z:/Shared_Data/fishing_sensitivity_manuscript/data/'
figure.dir = 'Z:/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'


out.stats.df = readRDS(paste0(data.dir,experiment.id,'/',experiment.id,'_path_dependence.rds'))

ggplot(out.stats.df, aes(x = reorder(Guild,-deviation.max),y = deviation.log))+
  geom_boxplot()+
  xlab('Guild')+
  ylab('Deviation from mean (#stdev) - log transformed')+
  theme_bw()
ggsave(paste0(figure.dir,'Figure_9_deviation_from_mean.png'),width = 8,height =6, units = 'in',dpi =300)

out.stats.sub.df = out.stats.df |> 
  filter(Guild %in% c('Apex_Predator','Benthos','Benthivore','Piscivore','Planktivore','Shrimp'))

ggplot(out.stats.sub.df, aes(x = reorder(Guild,-deviation.max),y = deviation.log, color = Guild))+
  geom_boxplot()+
  xlab('Guild')+
  ylab('Deviation from mean (#stdev) - log transformed')+
  theme_bw()+
  theme(legend.position = 'none')
  
ggsave(paste0(figure.dir,'Figure_9_deviation_from_mean_subset.png'),width = 8,height =6, units = 'in',dpi =300)
