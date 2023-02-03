# Script to move from ROMS_OUT into by-variable folder ins ROMS_COBALT output

orig.out.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Atlantis_Format/'
new.out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'

years = 1993:2017

for(yr in 1:length(years)){
  
  all.files = list.files(paste0(orig.out.dir,'/',years[yr]),'*.nc')
  
  hflux.file = all.files[which(sapply(all.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]

  file.copy(paste0(orig.out.dir,'/',years[yr],'/',hflux.file),paste0(new.out.dir,'/transport/',hflux.file))
  
  #Same for R files
  # R.files = list.files(paste0(orig.out.dir,'/',years[yr]),'*.R')
  # 
  # hflux.file = R.files[which(sapply(R.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
  # statevars.file = R.files[which(sapply(R.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='statevars') & all(strsplit(x,'[_.]+')[[1]] != 'ltl')))]
  # ltlvars.file = R.files[which(sapply(R.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'ltl')))]
  # nutvars.file = R.files[which(sapply(R.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'nutvars')))]
  # 
  # file.copy(paste0(orig.out.dir,'/',years[yr],'/',hflux.file),paste0(new.out.dir,'/transport/',hflux.file))
  # file.copy(paste0(orig.out.dir,'/',years[yr],'/',statevars.file),paste0(new.out.dir,'/phys_statevars/',statevars.file))
  # file.copy(paste0(orig.out.dir,'/',years[yr],'/',ltlvars.file),paste0(new.out.dir,'/ltl_statevars/',ltlvars.file),overwrite =T)
  # file.copy(paste0(orig.out.dir,'/',years[yr],'/',nutvars.file),paste0(new.out.dir,'/nut_statevars/',nutvars.file))
  
}
