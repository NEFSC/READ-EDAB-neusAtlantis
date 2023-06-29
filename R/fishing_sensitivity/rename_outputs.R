experiment.id = 'fspike1'
run.dir = paste0('/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/Atlantis_Runs/',experiment.id,'/')


old.prefix = 'out_'
new.prefix = experiment.id

run.folders = list.files(run.dir,pattern = old.prefix)

for(i in 1:length(run.folders)){
  
  run.name = run.folders[i]
  run.id = strsplit(run.name,old.prefix)[[1]][2]
  
  new.name = paste0(new.prefix,'_',run.id)
  
  file.rename(paste0(run.dir,run.name),paste0(run.dir,new.name))  
}


# system(paste0('sudo mkdir ',run.dir,'test_1'))
# file.rename(paste0(run.dir,'TEST_1'),paste0(run.dir,'TEST_2'))
