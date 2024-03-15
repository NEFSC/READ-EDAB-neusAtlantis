# Functions to output and edit C values for age-structured groups
# 1) get_param_C_age()
# 2) edit_param_C_age()



# 1) get_param_C_age -------------------------------------------------------

get_param_C_age = function(bio.prm, write.output = F, output.dir, out.name ){
  
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep('^C_.*',bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  which.invert = grepl('_T15',bio.lines.vals1)
  bio.lines.vals1 = bio.lines.vals1[!which.invert]
  bio.lines.id = bio.lines.id[!which.invert]
  
  group.names =unname(sapply(bio.lines.vals1,function(x) strsplit(x,'C_|\t10.00|\t| ')[[1]][2]))
  max.age = max(as.numeric(sapply(bio.lines.vals1,function(x) strsplit(x,'C_|\t| ')[[1]][3])))
  
  C.mat = matrix(NA, nrow = length(group.names),ncol = max.age)
  colnames(C.mat) = paste0('C',1:max.age)
  out.df = data.frame(C.mat)
  out.df = cbind(data.frame(group = group.names),out.df)
  
  for(i in 1:length(bio.lines.id)){
    C.group = bio.lines[bio.lines.id[i] + 1 ]
    C.split = strsplit(C.group,split = "\t| |  ")[[1]]
    
    if(length(C.split)>max.age){print(paste0(group.names[i],' has ',length(C.split)-10,' trailing tabs ',i))}
    
    C.out = rep(NA,max.age)
    C.out[1:length(C.split)] = C.split
    out.df[i,2:ncol(out.df)] = C.out
  }
  if(write.output){
    write.csv(out.df, file = paste0(output.dir,out.name,'.csv'),row.names = F)
  }else{
    return(out.df)
  }
}

# edit_param_C_age ------------------------------------------------------

edit_param_C_age = function(bio.prm, new.C, overwrite = F,new.file.name,single.group = F, group.name = NA ){
  
  #Get C_XXX bio.prm lines
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep('^C_',bio.lines)
  bio.lines.vals = bio.lines[bio.lines.id]
  which.invert = grepl('_T15',bio.lines.vals)
  bio.lines.vals = bio.lines.vals[!which.invert]
  bio.lines.id = bio.lines.id[!which.invert]
  
  group.names =unname(sapply(bio.lines.vals,function(x) strsplit(x,'C_|\t10.00|\t| ')[[1]][2]))
  max.age = max(as.numeric(sapply(bio.lines.vals,function(x) strsplit(x,'C_|\t| ')[[1]][3])))
  
  if(single.group){
    
    ind = which(group.name == group.names)
    new.C = new.C[!is.na(new.C)]
    C.string = paste(new.C,collapse = '\t')
    bio.lines[bio.lines.id[ind]+1] = C.string
  }else{
    for(i in 1:nrow(new.C)){
      
      ind = which(group.names == new.C$group[i])
      
      C.string = new.C[i,2:ncol(new.C)]
      which.na = which(is.na(C.string))
      if(length(which.na > 0)){C.string = C.string[-which.na]}
      C.string = paste(C.string,collapse='\t')
      
      bio.lines[bio.lines.id[ind]+1] = C.string
      
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
# out.name = 'New_Init_CatchTS_C_age'
# # new.C.df = read.csv(paste0(output.dir,'C_age_test_new.csv'),stringsAsFactors = F)
# # new.file.name = here::here('currentVersion','at_biology_test.prm')
# 
# get_param_C_age(bio.prm=bio.prm, write.output = T,output.dir = output.dir, out.name = out.name)
# get_param_C_age(bio.prm=bio.orig, write.output = T,output.dir = output.dir, out.name = 'Orig_C_RM')
# new.C.df = read.csv(paste0(output.dir,'Reset_C_New_Init.csv'),stringsAsFactors = F)
# edit_param_C_age(bio.prm, new.C.df,overwrite = T)
