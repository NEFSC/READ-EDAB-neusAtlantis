
years = 2020:2022

for(yr in 1:length(years)){
  
  file.dir = paste0('D:/GLORYS/Data_PSY_Raw/',years[yr],'/')
  prefix = paste0('GLORYS_REANALYSIS_PSY_',years[yr])
  new.prefix = paste0('GLORYS_REANALYSIS_PSY_TEMP_',years[yr])
  
  file.names = list.files(file.dir,prefix)
  
  file.suffix = sapply(file.names,function(x) return(strsplit(x,prefix)[[1]][2]),USE.NAMES = F)
  
  file.rename(paste0(file.dir,file.names),
              paste0(file.dir,new.prefix,file.suffix))
}

