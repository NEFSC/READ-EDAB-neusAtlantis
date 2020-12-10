# Functions to output and edit C values for age-structured groups
# 1) get_param_C_age()
# 2) edit_param_C_age()



# 1) get_param_C_age -------------------------------------------------------

get_param_C_age = function(bio.prm, write.output = F, output.dir, out.name ){
  
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep('^C_.*10',bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  
  group.names =unname(sapply(bio.lines.vals1,function(x) strsplit(x,'C_|\t|10.00')[[1]][2]))
  out.df = data.frame(group = group.names,
                      C1 = NA, C2 = NA, C3 = NA, C4 = NA, C5 = NA, C6 = NA, C7 = NA, C8 = NA, C9 = NA, C10 = NA)
  for(i in 1:length(bio.lines.id)){
    C.group = bio.lines[bio.lines.id[i] + 1 ]
    out.df[i,2:11] = strsplit(C.group,split = "\t| ")[[1]]
  }
  if(write.output){
    write.csv(out.df, file = paste0(output.dir,out.name,'.csv'),row.names = F)
  }else{
    return(out.df)
  }
}

# edit_param_C_age ------------------------------------------------------

edit_param_C_age = function(bio.prm, new.C.df, overwrite = F,new.file.name ){
  
  #Get C_XXX bio.prm lines
  bio.lines = readLines(bio.prm)
  bio.lines.id = grep('^C.*10.00',bio.lines)
  bio.lines.vals = bio.lines[bio.lines.id]
  group.names =unname(sapply(bio.lines.vals,function(x) strsplit(x,'C_|\t10.00| 10.00')[[1]][2]))
  
  for(i in 1:nrow(new.C.df)){
    
    ind = which(new.C.df$group == group.names[i])
    C.string = paste(new.C.df[ind,2:11],collapse='\t')
    bio.lines[bio.lines.id[ind]+1] = C.string
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
