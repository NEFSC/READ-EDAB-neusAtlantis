#Script to remove double underscore "__" from roms files

fix_ROMS_filename = function(roms.dir){
  
  file.names = list.files(roms.dir,'*.nc')  
  for( i in 1:length(file.names)){
   
    if(length(grep('__',file.names[i]))>0){
      old.name = file.names[i]
      name.split = strsplit(old.name,'__')[[1]]
      new.name = paste0(name.split[1],'_',name.split[2])
      file.rename(paste0(roms.dir,old.name),paste0(roms.dir,new.name))
      
    }  
  }
  
}

fix_ROMS_filename(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/transport/')
fix_ROMS_filename(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/phys_statevars/')
fix_ROMS_filename(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/nut_statevars/')
fix_ROMS_filename(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/')
