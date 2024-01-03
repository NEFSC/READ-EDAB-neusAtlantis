# Functions to output and edit mum values for age-structured groups
# 1) get_param_mum_age()
# 2) edit_param_mum_age()



# 1) get_param_mum_age -------------------------------------------------------

get_param_mum_age = function(bio.prm, write.output = F, output.dir, out.name ){
  
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep('^mum.*',bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  which.invert = grepl('_T15',bio.lines.vals1)
  bio.lines.vals1 = bio.lines.vals1[!which.invert]
  bio.lines.id = bio.lines.id[!which.invert]
  
  group.names =unname(sapply(bio.lines.vals1,function(x) strsplit(x,'mum_|\t10.00|\t| ')[[1]][2]))
  max.age = max(as.numeric(sapply(bio.lines.vals1,function(x) strsplit(x,'mum_|\t| ')[[1]][3])))
  
  mum.mat = matrix(NA, nrow = length(group.names),ncol = max.age)
  colnames(mum.mat) = paste0('mum',1:max.age)
  out.df = data.frame(mum.mat)
  out.df = cbind(data.frame(group = group.names),out.df)
  
  for(i in 1:length(bio.lines.id)){
    mum.group = bio.lines[bio.lines.id[i] + 1 ]
    mum.split = strsplit(mum.group,split = "\t| |  ")[[1]]
    
    if(length(mum.split)>max.age){print(paste0(group.names[i],' has ',length(mum.split)-10,' trailing tabs ',i))}
    
    mum.out = rep(NA,max.age)
    mum.out[1:length(mum.split)] = mum.split
    out.df[i,2:ncol(out.df)] = mum.out
  }
  if(write.output){
    write.csv(out.df, file = paste0(output.dir,out.name,'.csv'),row.names = F)
  }else{
    return(out.df)
  }
}

# edit_param_mum_age ------------------------------------------------------

edit_param_mum_age = function(bio.prm, new.mum, overwrite = F,new.file.name, single.group = F, group.name = NA ){
  
  #Get mum_XXX bio.prm lines
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep('^mum.*',bio.lines)
  bio.lines.vals = bio.lines[bio.lines.id]
  which.invert = grepl('_T15',bio.lines.vals)
  bio.lines.vals = bio.lines.vals[!which.invert]
  bio.lines.id = bio.lines.id[!which.invert]
  
  group.names =unname(sapply(bio.lines.vals,function(x) strsplit(x,'mum_|\t10.00|\t| ')[[1]][2]))
  max.age = max(as.numeric(sapply(bio.lines.vals,function(x) strsplit(x,'mum_|\t| ')[[1]][3])))
  
  if(single.group){
    
    ind = which(group.name == group.names)
    new.mum = new.mum[!is.na(new.mum)]
    mum.string = paste(new.mum,collapse = '\t')
    bio.lines[bio.lines.id[ind]+1] = mum.string
  }else{
    for(i in 1:nrow(new.mum)){
      
      ind = which(group.names == new.mum$group[i])
      
      mum.string = new.mum[i,2:ncol(new.mum)]
      which.na = which(is.na(mum.string))
      if(length(which.na > 0)){mum.string = mum.string[-which.na]}
      mum.string = paste(mum.string,collapse='\t')
      
      bio.lines[bio.lines.id[ind]+1] = mum.string
      
    }
  }
  
  #overwrite or make copy of biology file
  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
  
}

# Example -----------------------------------------------------------------
# bio.prm = here::here('currentVersion','at_biology.prm')
# bio.orig = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/at_biol_neus_v15_scaled_diet_20181126_3.prm'
# output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/'
# out.name = 'New_Init_CatchTS_mum_age'
# # new.mum.df = read.csv(paste0(output.dir,'mum_age_test_new.csv'),stringsAsFactors = F)
# # new.file.name = here::here('currentVersion','at_biology_test.prm')
# 
# get_param_mum_age(bio.prm=bio.prm, write.output = T,output.dir = output.dir, out.name = out.name)
# get_param_mum_age(bio.prm=bio.orig, write.output = T,output.dir = output.dir, out.name = 'Orig_Mum_RM')
# new.mum.df = read.csv(paste0(output.dir,'Reset_Mum_New_Init.csv'),stringsAsFactors = F)
# edit_param_mum_age(bio.prm, new.mum.df,overwrite = T)
