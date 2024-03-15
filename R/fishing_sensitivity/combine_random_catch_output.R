
data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/'

scenario.names = c('random_catch1','random_catch2')

combined.name = 'random_catch_combined'

out.ls = list()
permute.ls = list()

i=1
for(i in 1:length(scenario.names)){
  
  out.ls[[i]] = readRDS(paste0(data.dir,scenario.names[i],'/',scenario.names[i],'_mean_5yr_proj.rds'))
  
  permute.ls[[i]]=readRDS(paste0(data.dir,scenario.names[i],'/',scenario.names[i],'_permutations.rds'))
    
}

out.df = dplyr::bind_rows(out.ls)

permute.out = c(permute.ls[[1]],permute.ls[[2]])

saveRDS(out.df,paste0(data.dir,combined.name,'/',combined.name,'_mean_5yr_proj.rds'))
saveRDS(permute.out,paste0(data.dir,combined.name,'/',combined.name,'_permutations.rds'))
