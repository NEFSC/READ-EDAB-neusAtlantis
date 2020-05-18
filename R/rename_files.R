#' Function that converts file 
#' 
#' Renames files Based on a prefix and a file type.
#' Works on all files within one or more directories.
#' This assumes all files match the format of "prefix"*".suffix"
#' 
#' @file.dirs character vector. Vector of directories files are located in.
#' @file.types  character vector. Vector of desired filetypes
#' @file.prefix string. The shared file prefix for all files you wish to change
#' @new.names character vector. New prefix for files. If more than one, matches file.dirs
#' 
#' @return renames files in directory
#' 
#' #' Author: J. Caracappa

rename_files = function(file.dirs,file.types,file.prefix,new.names){
  
  if(length(file.dirs)!=length(new.names) & length(new.names)>1){
    warning('Error: new.names must be either length 1 or the same length as file.dirs')
    stop()
    
  }
  
  for(d in 1:length(file.dirs)){
    for(t in 1:length(file.types)){
      file.orig = list.files(file.dirs[d],pattern = paste0(file.prefix,'.*\\',file.types[t]))
      if(length(file.orig)==0){
        next()
      } else if(length(new.names)==1) {
        file.replace = sapply(file.orig,function(x) paste0(new.names,strsplit(x,file.prefix)[[1]][2]))
      } else {
        file.replace = sapply(file.orig,function(x) paste0(new.names[d],strsplit(x,file.prefix)[[1]][2]))
      }
      file.rename(paste0(file.dirs[d],file.orig),paste0(file.dirs[d],file.replace))
      print(file.types[t])
    }
    print(file.dirs[d])
  }
  
}

# file.rename2(file.dirs = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/',1980:2014,'/'),
#              file.prefix = 'roms_cobalt_',
#              new.names = paste0('roms_',1980:2014,'_output_'),
#              file.types = c('.nc','.R')
#              )
