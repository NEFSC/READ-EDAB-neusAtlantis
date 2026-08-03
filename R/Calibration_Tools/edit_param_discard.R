#Functions to edit the discard parameters (flagdiscard_XXX, FCthreshli_XXX, and k_retain_XXX)
edit_param_discard = function(harvest.prm, fisheries.file, fgs.file, sppCodes, fleetCodes, Type, Value, overwrite =F, new.harvest.file){

  if(!(Type %in% c('k_retain','flagdiscard','FCthreshli'))){
    error("Type needs to be one of 'k_retain', 'flagdiscard', or 'FCthreshli'")
  }  
  
  fisheries = read.csv(fisheries.file,as.is =T)
  fgs = read.csv(fgs.file, as.is =T)
    
  harvest.lines = readLines(harvest.prm)
  
  #If neither sppCodes or fleetCodes are null do for all combinations of fleetCodes and sppCodes
  if(!is.null(fleetCodes) &  !is.null(sppCodes)){
  
    fleet.match = match(fleetCodes,fisheries$Code)
    
    for(s in 1:length(sppCodes)){
      
      which.var.line = grep(paste0(Type,'_',sppCodes[s]),harvest.lines)+1
      var.line = harvest.lines[which.var.line]
      new.var.line = strsplit(var.line,split = ' |/t')[[1]]
      
      new.var.line[fleet.match] = Value  
      
      harvest.lines[which.var.line] = paste(new.var.line,collapse = ' ')
      
    }
    
  #If fleetCodes = NULL, then do for all sppCodes  
  } else if(is.null(fleetCodes) & !is.null(sppCodes)){
    
    for(s in 1:length(sppCodes)){
      
      which.var.line = grep(paste0(Type,'_',sppCodes[s]),harvest.lines)+1
      var.line = harvest.lines[which.var.line]
      new.var.line = strsplit(var.line,split = ' |/t')[[1]]
      
      new.var.line[1:length(new.var.line)] = Value  
      
      harvest.lines[which.var.line] = paste(new.var.line,collapse = ' ')
      
    }
  
  #If sppCodes = NULL, do for all target species in fleetCodes  
  }else if(!is.null(fleetCodes) & is.null(sppCodes)){
  
    target.ls = list()
    for(f in 1:length(fleetCodes)){
      
      which.target.line = grep(paste0('target_',fleetCodes[f]),harvest.lines)+1
      target.line = harvest.lines[which.target.line]
      target.line = as.logical(as.numeric((strsplit(target.line,split = ' |\t')[[1]])))
      
      target.ls[[f]] = data.frame(fleet = fleetCodes[f],
                                  target.spp = fgs$Code[target.line])
      
    }
    target.df = dplyr::bind_rows(target.ls)
    
    target.spp.names = sort(unique(target.df$target.spp))
    
    for(s in 1:length(target.spp.names)){
      
      target.df.spp = target.df %>%
        dplyr::filter(target.spp == target.spp.names[s])
      
      which.fleet = match(target.df.spp$fleet,fisheries$Code)
      
      which.var.line = grep(paste0(Type,'_',target.spp.names[s]),harvest.lines)+1
      var.line = harvest.lines[which.var.line]
      new.var.line = strsplit(var.line,split = ' |/t')[[1]]
      
      new.var.line[which.fleet] = Value  
      
      harvest.lines[which.var.line] = paste(new.var.line,collapse = ' ')
    }
  
  #if fleetCodes = NULL and sppCodes = NULL, error
  }else{
    error("fleetCodes and sppCodes can't both be NULL")
  }
  
  if(overwrite){
    writeLines(harvest.lines,harvest.prm)
  }else{
    
    file.copy(harvest.prm,new.harvest.prm, overwrite = T)
    writeLines(new.harvest.prm)
  }
}
