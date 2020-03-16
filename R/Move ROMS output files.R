file.dirs = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_Out/',1980:2014,'/')
new.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/'

for(i in 1:length(file.dirs)){
  
  nc.files = list.files(file.dirs[i],pattern = '*.nc')
  transport.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
  statevars.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='statevars') & all(strsplit(x,'[_.]+')[[1]] != 'ltl')))]
  ltlvars.file = nc.files[which(sapply(nc.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'ltl')))]
  
  file.copy(paste0(file.dirs[i],transport.file),paste0(new.dir,'/transport/',transport.file))
  file.copy(paste0(file.dirs[i],statevars.file),paste0(new.dir,'/statevars/',statevars.file))
  file.copy(paste0(file.dirs[i],ltlvars.file),paste0(new.dir,'/ltl_statevars/',ltlvars.file))
}

#make csv with just file names
transport.files = paste(paste0('trans',0:34,'.name'),list.files(paste0(new.dir,'/transport/'),pattern = '*.nc',full.names = T))
statevars.files = paste(paste0('tempsalt',0:34,'.name'),list.files(paste0(new.dir,'/statevars/'),pattern = '*.nc',full.names = T))
ltlvars.files = list.files(paste0(new.dir,'/ltl_statevars/'),pattern = '*.nc',full.names = T)

write.csv(transport.files,file = paste0(new.dir,'transport_file_names.csv'),row.names = F)
write.csv(statevars.files,file = paste0(new.dir,'statevars_file_names.csv'),row.names = F)
write.csv(ltlvars.files,file = paste0(new.dir,'ltl_statevars_file_names.csv'),row.names = F)
