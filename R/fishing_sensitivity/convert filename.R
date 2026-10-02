
loadRData = function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}
dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/Data/fscale2/'
files = list.files(dir,'*.Rdata')
shortname = sapply(files,function(x) strsplit(x,'.Rdata')[[1]][1])

i =1 
for(i in 1:length(files)){
  x = loadRData(paste0(dir,files[i]))
  saveRDS(x,file = paste0(dir,shortname[i],'.rds'))
  rm(x)
  # print(shortname[i])
}
