
###########################################
#Checksums for BiomIndx to see if runs are different and completed

#name of run output director
run.dir = '/atlantisdisk2/eof_targeting_4'
dir.list = list.files(run.dir,full.names = T)
base.dir = basename(dir.list)

out.df = data.frame(base.dir = base.dir, maxtime = NA,sumval = NA)
for(i in 1:length(dir.list)){
  this.file = paste0(dir.list[i],'/neus_outputBiomIndx.txt')
  if(file.exists(this.file)){
    this.data = read.table(this.file,header =T, fill = T)
    out.df$maxtime[i] = max(this.data$Time)
    out.df$sumval[i] = sum(this.data)
  }else{
    out.df$maxtime[i] = 0
  }

}

###########################################
# Creates bundles of runs for rerunning if needed
# Needs to be copy-pasted into R/Cloud_Utilities/rerun_failed_runs.sbatch
rerun.df = out.df %>% 
  dplyr::filter(maxtime < 20804, grepl('eof_',base.dir)) %>% 
  dplyr::group_by(base.dir) %>% 
  dplyr::mutate(run.id = as.numeric(strsplit(base.dir,split = 'eof_targeting_4_')[[1]][2])) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(run.id) %>% 
  dplyr::mutate(node = (dplyr::row_number()-1)%/%36 +1) %>% 
  dplyr::arrange(node,run.id)

for(i in 1:length(dir.list)){
  eco.file = list.files(dir.list[i],pattern = 'eco_indicators_mean.rds',recursive = T)
  if(length(eco.file) == 0){
    message('Eco File missing from',basename(dir.list[i]))
  }
}

n.nodes = sort(unique(rerun.df$node))

for( i in n.nodes){
  this.node = dplyr::filter(rerun.df,node == i)
  head.str = paste0(i-1,') sim_ids=')
  run.string = paste(this.node$run.id,collapse = ' ')
  out.string = paste0(head.str, run.string)
  print(out.string)
}