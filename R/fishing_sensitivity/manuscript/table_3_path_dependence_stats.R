experiment.id = 'random_catch_combined'

data.dir = 'Z:/Shared_Data/fishing_sensitivity_manuscript/data/'
figure.dir = 'Z:/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'


out.stats.df = readRDS(paste0(data.dir,experiment.id,'/',experiment.id,'_path_dependence.rds'))
