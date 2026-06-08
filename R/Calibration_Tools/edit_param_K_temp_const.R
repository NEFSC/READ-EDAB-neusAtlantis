#Changes K_temp_const parameter in at_biology.prm


get_param_K_temp_const = function(bio.prm, group.name = NA){

  bio.lines = readLines(bio.prm)

  K_temp_const.line = grep(paste0('K_temp_const'),bio.lines)

  groups = sapply(bio.lines[K_temp_const.line],function(x) return(strsplit(x,'\t| |_')[[1]][1]),USE.NAMES = F)

  group.vals = sapply(bio.lines[K_temp_const.line],function(x) strsplit(x,' |\t')[[1]][2], USE.NAMES = F)

  if(is.na(group.name)){

    return(data.frame(Code = groups, K_temp_const = group.vals))

  }else{

    return(group.vals[which(groups == group.name)])
  }

  return(out.df)
}

edit_param_K_temp_const = function(bio.prm,group.name,value,unit = 'value', overwrite = F,new.file.name="CrapXXX"){

  bio.lines = readLines(bio.prm)
  K_temp_const.line = grep(paste0(group.name,'_K_temp_const'),bio.lines)

  if(unit == 'value'){
    new.val = value
    print(new.val)
  }else{
    old.val = as.numeric(strsplit(bio.lines[K_temp_const.line],' |\t')[[1]][2])
    new.val = old.val * value
  }

  bio.lines[K_temp_const.line] = paste0(group.name,'_K_temp_const ',new.val)

  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}
