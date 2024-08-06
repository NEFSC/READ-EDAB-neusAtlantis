#' Functions to:
#' 1) Retrieve init_scalar params
#' 2) Retrieve total initial condition biomass (tonnes)
#' 3) edit init_scalar params
#' 4) Calculate_init_scalar

# get_param_init_scalar ---------------------------------------------------

get_param_init_scalar = function(run.prm,groups.file,write.output = F, output.dir){
  
  #Read in data
  run.lines = readLines(run.prm)
  fgs = read.csv(groups.file,header = T, stringsAsFactors = F)
  
  #Get group names (order is important)
  group.code = fgs$Code
  
  #get init_scalar lines
  init.scalar.string = run.lines[grep('init_scalar',run.lines)+1]
  init.scalar.vals =strsplit(init.scalar.string,'\t')[[1]]
  
  #output dataframe
  out.df = data.frame(group = group.code, init.scalar = init.scalar.vals)
  
  if(write.output==F){
    return(out.df)  
  }else{
    write.csv(out.df, paste0(output.dir,'init_scalar.csv'),row.names = F)
  }
}


# get_init_cond_biomass ---------------------------------------------------

get_init_biomass = function(bio.age.file, groups.file,write.output =F, output.dir){
  library(dplyr)
  
  bio.age = read.table(bio.age.file,header = T,stringsAsFactors = F)
  
  bio.age.init = dplyr::filter(bio.age,Time == 0)
  bio.age.colnames = colnames(bio.age.init)[-1]
  bio.age.df = data.frame(
    code = sapply(bio.age.colnames,function(x) strsplit(x,'\\.')[[1]][1]),
    age = sapply(bio.age.colnames,function(x) strsplit(x,'\\.')[[1]][2]),
    biomass.tonnes = c(t(unname(bio.age.init[1,-1]))),
    row.names =NULL
  ) %>%
    group_by(code) %>%
    summarize(biomass.tonnes = sum(biomass.tonnes,na.rm=T))
  
  if(write.output){
    write.csv(bio.age.df,paste0(output.dir,'initial_biomass_tonnes.csv'),row.names = F,overwrite =T)
  }else{
    return(bio.age.df)
  }
}


# edit_param_init_scalar --------------------------------------------------
# run.prm = here::here('currentVersion','at_run.prm')
# groups.file = here::here('currentVersion','neus_groups.csv')
# group.name = 'MAK'
# unit = 'scalar'
# value = 2
# new.file.name = here::here('currentVersion','at_run_test.prm')
edit_param_init_scalar = function(run.prm,groups.file,group.name,unit,value,overwrite = F, new.file.name){
  
  #Read in groups file and run.prm
  fgs = read.csv(groups.file,stringsAsFactors = F)
  run.prm.lines = readLines(run.prm)
  
  init.scalar.line = grep('init_scalar',run.prm.lines)+1
  
  init.scalar.orig = run.prm.lines[init.scalar.line]
  init.scalar.orig =  as.numeric(strsplit(init.scalar.orig,'\t| ')[[1]])
  
  which.val = which(fgs$Code == group.name)
  
  if(unit == 'value'){
    new.val = value
  }else{
    new.val = init.scalar.orig[which.val] * value
  }
  
  init.scalar.new = init.scalar.orig
  init.scalar.new[which.val] = new.val
  
  init.scalar.new.string = paste(init.scalar.new,collapse = '\t')
  
  run.prm.lines[init.scalar.line] = init.scalar.new.string
  
  # #Read in init_scalar .csv (edited)
  # # new.init.scalar = read.csv(init.scalar.file,stringsAsFactors = F)
  # new.init.scalar$init.scalar = sapply(as.numeric(new.init.scalar$init.scalar),function(x){
  #   if(x<1){
  #     return(signif(x,3))
  #   }else{
  #     return(round(x,3))
  #   }
  # })
  # new.init.scalar$init.scalar = as.character(new.init.scalar$init.scalar)
  # 
  # 
  # #Sort by Group Code Index and join with new data
  # init.scalar.edit = fgs %>% 
  #   select(Code, Index) %>%
  #   arrange(Index) %>%
  #   left_join(new.init.scalar, by = c('Code'= 'group'))
  # 
  # #overwrite line
  # run.prm.lines[grep('init_scalar',run.prm.lines)+1] = paste(init.scalar.edit$init.scalar,collapse ='\t')
  
  #overwrite or make copy of run file
  if(overwrite){
    writeLines(run.prm.lines, con = run.prm)
  }else{
    file.copy(run.prm, new.file.name, overwrite = T)
    writeLines(run.prm.lines, con = new.file.name )
  }
}


# Calculate_init_scalar ---------------------------------------------------


# Example -----------------------------------------------------------------
# 
# run.prm = here::here('currentVersion','at_run.prm')
# init.file = here::here('currentVersion','neus_init.nc')
# init.file.nofill = here::here('currentVersion','neus_init_nofill.nc')
# groups.file = here::here('currentVersion','neus_groups.csv')
# bgm.file = here::here('currentVersion','neus_tmerc_RM2.bgm')
# # bio.age.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_10202020/neus_outputAgeBiomIndx.txt'
# bio.age.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HerZoo_24/neus_outputAgeBiomIndx.txt'
# 
# get_param_init_scalar(run.prm,groups.file,write.output = T,
#                       output.dir = here::here('Setup_Files',''))
# 
# init.scalar.file =here::here('Setup_Files','init_scalar_0330.csv')
# output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Initial_Scalars/'
# # missing.cohorts = read.csv(paste0(output.dir,'missing_survdat_cohorts.csv'),stringsAsFactors = F)
# 
# 
# get_init_biomass(bio.age.file,groups.file,write.output = T,
#                  output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/')
# edit_param_init_scalar(run.prm,groups.file,
#                        new.init.scalar = read.csv(init.scalar.file,stringsAsFactors = F),
#                        overwrite =T,
#                        new.file.name = here::here('currentVersion','at_run_test.prm'))
# calculate_init_scalar(run.prm = run.prm,
#                       groups.file = groups.file,
#                       bio.age.file = bio.age.file,
#                       init.file = init.file,
#                       missing.cohorts = missing.cohorts,
#                       exclude.invert = T,
#                       write.output = T,
#                       output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/')
# 
# edit_param_init_scalar(run.prm,groups.file,
#                        new.init.scalar = read.csv(init.scalar.file,stringsAsFactors = F),
#                        overwrite = T)
