data.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/'
library(dplyr)
dir.names = c('fspike1','fspike2')

file.names = list(paste0('AgeBiomIndx_',dir.names,'_1_28105_year.rds'),
                     paste0('BiomIndx_',dir.names,'_1_28105_year.rds'),
                     paste0('BoxBiomass_',dir.names,'_1_28105_year.rds'),
                     paste0('Catch_',dir.names,'_1_28105_year.rds'),
                     paste0('DietCheck_',dir.names,'_1_28105_year.rds'))

file.prefixes = c('AgeBiomIndx_1_28105_year_',
                  'BiomIndx_1_28105_year_',
                  'BoxBiomass_1_28105_year_',
                  'Catch_1_28105_year_',
                  'DietCheck_1_28105_year_')

combined.name = 'fspike_combined'
combined.dir = paste0(data.dir,combined.name,'/')

if(!dir.exists(combined.dir)){ dir.create(combined.dir)}

i=1
for(i in 1:length(file.prefixes)){
    
  data.ls = list()
  for(j in 1:length(dir.names)){
    
    data.ls[[j]] = readRDS(paste0(data.dir,dir.names[j],'/',file.names[[i]][j])) %>%
      mutate(experiment.id = dir.names[i])
  }
  data.combined = bind_rows(data.ls)
  
  saveRDS(data.combined, file = paste0(combined.dir,file.prefixes[i],combined.name,'.rds'))
  
}

setup.df.ls = list()
for(i in 1:length(dir.names)){
  
  setup.df.ls[[i]] = read.csv(here::here('diagnostics','scenario_db',paste0(dir.names[i],'_setup.csv')))
}
setup.new = bind_rows(setup.df.ls)

write.csv(setup.new,here::here('diagnostics','scenario_db',paste0(combined.name,'_setup.csv')))
