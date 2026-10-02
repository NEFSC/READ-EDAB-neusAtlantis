#Function to change mumC parameters and make new biology.prm files
#' Bracket parameters
#'
#'

###############################
####___Change These Only___####
###############################

# #Choose number of runs
# n.run = 4
# #Choose parameter interval. If TRUE, interval is log-scaled (Better for multiple orders of magnitude)
# log.scaling = F
# #Choose the run set's name
# run.prefix = 'misc_mumC_4'
# #Choose the run set's directory 
# project.dir = here::here('Atlantis_Runs',run.prefix,'')
# #Choose the parameter directory
# param.dir = here::here('currentVersion/')
# #Choose the name of the run index (i.e. the Set up file)
# run.index = read.csv(here::here('Setup_Files',paste0(run.prefix,'.csv')),as.is = T)
# #Choose the name for the expanded run index
# run.index.long = here::here('Setup_Files',paste0(run.prefix,'_long.csv'))

###############################
###############################

make_mumC_parameter_files = function(n.run,
                                     log.scaling,
                                     run.prefix,
                                     project.dir,
                                     param.dir,
                                     run.index,
                                     run.index.long){
  library(dplyr)
  
  #Source in function to edit biology.prm file
  source(file = 'R/edit_param_mum_age.R')
  source(file = 'R/edit_param_C_age.R')
  source(file = 'R/edit_param_invert_c_mum.R')
  source('R/get_recruit_type.R')
  
  #Read in run index as dataframe and reshape to long format
  mum.scale.df = c.scale.df =list()
  if(log.scaling == T){
    mum.scale =as.data.frame(apply(run.index[,2:3],1,function(x) return(10^seq(log10(x[1]),log10(x[2]),length.out = n.run))))
    c.scale = as.data.frame(apply(run.index[,4:5],1,function(x) return(10^seq(log10(x[1]),log10(x[2]),length.out = n.run))))
  }else{
    mum.scale =as.data.frame(apply(run.index[,2:3],1,function(x) return(seq(x[1],x[2],length.out = n.run))))
    c.scale = as.data.frame(apply(run.index[,4:5],1,function(x) return(seq(x[1],x[2],length.out = n.run))))
    
  }
  
  colnames(mum.scale)= colnames(c.scale) = run.index$Species
  mum.scale$param = 'mum'
  c.scale$param = 'C'
  
  mum.scale$ID = c.scale$ID =(1:n.run)-1
  mum.scale$dir.name = c.scale$dir.name = paste0(run.prefix,'_',(1:n.run)-1)
  
  run.index = reshape2::melt(bind_rows(mum.scale,c.scale),id.vars = c('ID','dir.name','param'),variable.name = 'group')
  run.index$ID = as.numeric(run.index$ID)
  run.index = tidyr::spread(run.index,param,value)
  
  write.csv(run.index, file = run.index.long,row.names  = F )
  
  # create folder structure
  # folders = unique(run.index$dir.name)
  
  #Set groups to be calibrated
  group.names = unique(as.character(run.index$group))
  
  #Get at_biology_base mum/C values
  mum.base.age = get_param_mum_age(bio.prm = paste0(param.dir,'at_biology.prm')) %>%
    filter(group %in% group.names)%>%
    mutate_at(vars(mum1:mum10),funs(as.numeric))
  c.base.age = get_param_C_age(paste0(param.dir,'at_biology.prm'))%>%
    filter(group %in% group.names)%>%
    mutate_at(vars(C1:C10),funs(as.numeric))
  
  #Identify which groups are inverts
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)
  invert.groups = fgs$Code[which(fgs$NumCohorts == 1)]
  
  #Create run set directory
  # dir.create(project.dir)
  
  run.ID = unique(run.index$ID)
  new.param.files = character()
  i = 1
  for (i in 1:length(run.ID)){
    # create sensible folder name has parameter in name (or index) documented in run.index
    # dir.create(paste0(project.dir,folders[i]))
    
    #Copy over biology.prm template with new name (one for each run)
    new.bio.name = paste0('at_biology_',run.prefix,'_',run.ID[i],'.prm')
    file.copy(paste0(param.dir,'at_biology.prm'),paste0(param.dir,new.bio.name),overwrite = T)
    
    #Edit Mum and C
    j=1
    for(j in 1:length(group.names)){
      
      run.index.sub = filter(run.index,ID ==(i-1) & group == group.names[j]) 
      
      if(group.names[j] %in% invert.groups){
        
        mum.c.base.invert = get_param_invert_c_mum(paste0(param.dir,'at_biology.prm'),group = group.names[j])
        new.mum = mum.c.base.invert[1] * run.index.sub$mum[1]
        new.C = mum.c.base.invert[2] * run.index.sub$C[1]
        
        edit_param_invert_c_mum(bio.file = paste0(param.dir,new.bio.name),
                                group = group.names[j],
                                mum = new.mum,
                                C = new.C,
                                new.file = F)
        
      }else{
        group.id = which(mum.base.age$group== group.names[j])  
        
        new.mum = mum.base.age[group.id,2:11]*run.index.sub$mum[1]
        new.C = c.base.age[group.id,2:11]*run.index.sub$C[1]
        
        edit_param_mum_age(bio.prm = paste0(param.dir,new.bio.name),
                           overwrite = T,
                           new.mum = new.mum,
                           single.group = T,
                           group.name = group.names[j]
        )
        
        edit_param_C_age(bio.prm = paste0(param.dir,new.bio.name),
                         overwrite = T,
                         new.C = new.C,
                         single.group = T,
                         group.name = group.names[j]
        )
      }
    }
    new.param.files[i] = new.bio.name
  }
  
  return(new.param.files)
  
}
