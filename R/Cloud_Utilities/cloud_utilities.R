x = read.table('/atlantisoutput/eof_targeting_1/eof_targeting_1_518/neus_outputBiomIndx.txt',fill = T, header =T)
x$Time

extra.dirs = list.files('/atlantisoutput/eof_targeting_1/','data',include.dirs = T,full.names = T)
file.remove(extra.dirs)

x = list.files(here::here(),'slurm-10493')
file.remove(x)

r= 1:600
gz.files = sapply(r,function(x){
  a=list.files(path = paste0('/atlantisoutput/eof_targeting_1/eof_targeting_1_',x),pattern = '*.gz', full.names = T)
  return(a)
})
gz = unlist(gz.files)
file.remove(gz)


run.dirs = list.files('/atlantisoutput/eof_targeting_1/',include.dirs = T)
run.dirs.long = list.files('/atlantisoutput/eof_targeting_1/',full.names = T, include.dirs = T)

missing.runs = data.frame(run.id = numeric(600), nfiles= NA)
# missing.runs = numeric()
for( i in 1:length(run.dirs)){
  this.run.id = sub(".*_(\\d+)$","\\1", run.dirs[i])
  missing.runs$run.id[i] = this.run.id
  missing.runs$nfiles[i] = length(list.files(run.dirs.long[i]))
  # log.file = list.files(run.dirs.long[i],'log.txt')

  # if(length(log.file) == 0){
  #   missing.runs = c(missing.runs,this.run.id)
  # }
  
}
# Rscript R/EOF/call_process_eco_state.R "A" "B" "C"