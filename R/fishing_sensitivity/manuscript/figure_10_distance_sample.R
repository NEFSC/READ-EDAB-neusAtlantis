# Figure 10: Deviation from sample distribution of random catch permutations

experiment.id = 'random_catch_combined'

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'


out.stats.df = readRDS(paste0(data.dir,experiment.id,'/',experiment.id,'_path_dependence.rds'))

ggplot(out.stats.df, aes(x = reorder(Guild,-deviation.max),y = deviation.log))+
  geom_boxplot()+
  xlab('Guild')+
  ylab('Deviation from mean (#stdev) - log transformed')+
  theme_bw()
ggsave(paste0(figure.dir,'Figure_10_deviation_from_mean.png'),width = 8,height =6, units = 'in',dpi =300)

