# Function to remove slurm .out files from dirrectories

rm_slurm_files = function(dir.name, slurm.id){
  slurm.files = list.files(dir.name, pattern = paste0('slurm-',slurm.id),full.name =T)
  file.remove(slurm.files)
}

rm_gz_files = function(dir.name){
  gz.files = list.files(dir.name, recursive = T, pattern = '*.gz', full.names = T)
  file.remove(gz.files)
}

rm_param_files = function(dir.name, prm.prefix){
  prm.pattern = paste0("^",prm.prefix,".*_[0-9]+\\.prm$")
  prm.files = list.files(dir.name,pattern = prm.pattern,full.names = T)
  file.remove(prm.files)
}

rm_slurm_files('/home/Joseph.Caracappa/',slurm.id = 1)
rm_slurm_files(here::here,1981)

rm_gz_files('/atlantisdisk2/eof_targeting_3/')

rm_param_files(here::here('currentVersion'),'at_biology')
