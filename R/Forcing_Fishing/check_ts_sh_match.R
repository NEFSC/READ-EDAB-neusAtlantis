proj.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'

source(paste0(proj.dir,'R/Forcing_Fishing/get_forcing_ts.r'))

experiment.id = 'eof_targeting_3'
experiment.dir = paste0(proj.dir,'currentVersion/',experiment.id)

ts.files = list.files(experiment.dir, pattern = 'total_catch_', full.names =T)
sh.files = list.files(paste0(proj.dir,'currentVersion'),pattern = 'runAtlantis_', full.names = T)
force.files = list.files(paste0(proj.dir,'currentVersion'),pattern = 'at_force_LINUX_',full.names = T)

prm_sh_match = function(file){
  li = readLines(file)
  run.li = grep('atlantisMerged',li,value =T)
  run.args = strsplit(run.li,split = ' |/t')[[1]]
  this.force = grep('at_force_LINUX_',run.args,value =T)
  force.run.id = stringr::str_extract(this.force, "\\d+")
  this.file.run.id = stringr::str_extract(basename(file),"\\d+")
  run.match = this.file.run.id == force.run.id
  return(run.match)
}

force_run_match = function(file){
  li= readLines(file)
  catch.line = grep('Catchts0.data',li,value =T)
  catch.path = strsplit(catch.line,split =' |\t')[[1]][2]
  ts.run = stringr::str_extract(basename(catch.path),"\\d+")
  force.run = stringr::str_extract(basename(file),"\\d+")
  run.match = ts.run == force.run
  return(run.match)
}

file.match = sapply(sh.files, prm_sh_match)
which(file.match == F)

force.match = sapply(force.files,force_run_match)
which(force.match == F)



