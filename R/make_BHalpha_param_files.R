#Function to create parameter files for BHalpha adjustments

###############################
####___Change These Only___####
###############################

# #Choose number of runs
# n.run = 4
# #Choose parameter interval. If TRUE, interval is log-scaled (Better for multiple orders of magnitude)
# log.scaling = F
# #Choose the run set's name
# run.prefix = 'misc_BHalpha_3'
# #Choose the run set's directory 
# project.dir = here::here('Atlantis_Runs/',run.prefix,'')
# #Choose the parameter directory
# param.dir = here::here('currentVersion','')
# #Choose the name of the run index (i.e. the Set up file)
# run.index = read.csv(here::here('Setup_Files',paste0(run.prefix,'.csv')),as.is = T)
# #Choose the name for the expanded run index
# run.index.long = here::here('Setup_Files',paste0(run.prefix,'_long.csv'))

###############################
###############################

make_BHalpha_param_files = function(n.run,log.scaling,run.prefix,project.dir,param.dir,run.index,run.index.long){
  library(dplyr)
  #Source in function to edit biology.prm file
  source(file = 'R/edit_param_BH.R')
  source('R/get_recruit_type.R')
  
  #Read in run index
  alpha.scale.df = beta.scale.df =list()
  if(log.scaling == T){
    alpha.scale =as.data.frame(apply(run.index[,2:3],1,function(x) return(10^seq(log10(x[1]),log10(x[2]),length.out = n.run))))
    beta.scale = as.data.frame(apply(run.index[,4:5],1,function(x) return(10^seq(log10(x[1]),log10(x[2]),length.out = n.run))))
  }else{
    alpha.scale =as.data.frame(apply(run.index[,2:3],1,function(x) return(seq(x[1],x[2],length.out = n.run))))
    beta.scale = as.data.frame(apply(run.index[,4:5],1,function(x) return(seq(x[1],x[2],length.out = n.run))))
  }
  
  colnames(alpha.scale)= colnames(beta.scale) = run.index$Species
  
  alpha.scale$param = 'alpha'
  beta.scale$param = 'beta'
  
  alpha.scale$ID = beta.scale$ID =(1:n.run)-1
  alpha.scale$dir.name = beta.scale$dir.name = paste0(run.prefix,'_',(1:n.run)-1)
  
  run.index = reshape2::melt(bind_rows(alpha.scale,beta.scale),id.vars = c('ID','dir.name','param'),variable.name = 'group')
  run.index$ID = as.numeric(run.index$ID)
  run.index = tidyr::spread(run.index,param,value)
  
  write.csv(run.index, file = run.index.long,row.names  = F )
  
  # create folder structure, pulling from run.index
  folders = unique(run.index$dir.name)
  
  #Set groups to be calibrated
  group.names = unique(as.character(run.index$group))
  
  #Get group BH alpha and beta
  base.BH = get_param_BH(bio.prm = paste0(param.dir,'at_biology.prm')) %>%
    filter(group %in% group.names)%>%
    mutate(alpha = as.numeric(as.character(alpha)),beta = as.numeric(as.character(beta)))
  
  #Loop over all IDs in run.index
  run.ID = unique(run.index$ID)
  
  #Create Run set directory
  # dir.create(project.dir)
  
  new.param.files = character()
  
  i = 1
  for(i in 1:length(run.ID)){
    # create sensible folder name has parameter in name (or index) documented in run.index
    # dir.create(paste0(project.dir,folders[i]))
    
    #Copy over biology.prm template with new name (one for each run)
    new.bio.name = paste0('at_biology_',run.prefix,'_',run.ID[i],'.prm')
    file.copy(paste0(param.dir,'at_biology.prm'),paste0(param.dir,new.bio.name),overwrite = T)
    
    # Edit parameter file using functions, etc.
    
    #Do fish BH alpha beta
    j = 1
    for(j in 1:length(group.names)){
      
      run.index.sub = filter(run.index,ID ==(i-1) & group == group.names[j]) 
      
      group.id = which(base.BH$group== group.names[j])  
      
      new.alpha = base.BH$alpha[group.id]*run.index.sub$alpha[1]
      new.beta = base.BH$beta[group.id]*run.index.sub$beta[1]
      
      edit_param_BH(bio.prm = paste0(param.dir,new.bio.name),
                    group.name = group.names[j],
                    alpha = new.alpha,
                    beta = new.beta,
                    overwrite = T)
    }
    new.param.files[i] = new.bio.name
  }
  return(new.param.files)
}
