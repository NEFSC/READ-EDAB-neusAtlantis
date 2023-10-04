experiment.id = 'random_catch_combined'

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'


out.stats.df = readRDS(paste0(data.dir,experiment.id,'/',experiment.id,'_path_dependence.rds'))
