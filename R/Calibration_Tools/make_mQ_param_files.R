#Function to generate parameter files that manipulate quadratic mortality (mQ)

#Choose number of runs
#n.run = 4
#Choose parameter interval. If TRUE, interval is log-scaled (Better for multiple orders of magnitude)
# log.scaling = F
# #Choose the run set's name
# run.prefix = 'misc_mQ_1'
# #Choose the run set's directory 
# project.dir = here::here('Atlantis_Runs',run.prefix,'')
# #Choose the parameter directory
# param.dir = here::here('currentVersion','')
# #Choose the name of the run index (i.e. the Set up file)
# run.index = read.csv(here::here('Setup_Files',paste0(run.prefix,'.csv')),as.is = T)
# #Choose the name for the expanded run index
# run.index.long = here::here('Setup_Files',paste0(run.prefix,'_long.csv'))
# #Choose a baseline mQ if it's currently zero and you want to change it
# start.mQ = 1E-12

###############################
###############################

make_mQ_param_files = function(n.run,
                               log.scaling,
                               run.prefix,
                               project.dir,
                               param.dir,
                               run.index,
                               run.index.long,
                               start.mQ){
  
  library(dplyr)
  
  #Source in function to edit biology.prm file
  source(file = 'R/edit_param_mortality_age.R')
  source(file = 'R/get_recruit_type.R')
  source(file = 'R/edit_param_invert_mortality.R')
  
  #Read in run index as dataframe and convert to long format
  juv.mQ.scale.df = adult.mQ.scale.df =list()
  juv.mQ.scale =as.data.frame(apply(run.index[,2:3],1,function(x) return(seq(x[1],x[2],length.out = n.run))))
  adult.mQ.scale = as.data.frame(apply(run.index[,4:5],1,function(x) return(seq(x[1],x[2],length.out = n.run))))
  
  colnames(juv.mQ.scale)= colnames(adult.mQ.scale) = run.index$Species
  
  juv.mQ.scale$param = 'juv.mQ'
  adult.mQ.scale$param = 'adult.mQ'
  
  juv.mQ.scale$ID = adult.mQ.scale$ID =(1:n.run)-1
  juv.mQ.scale$dir.name = adult.mQ.scale$dir.name = paste0(run.prefix,'_',(1:n.run)-1)
  
  run.index = reshape2::melt(bind_rows(juv.mQ.scale,adult.mQ.scale),id.vars = c('ID','dir.name','param'),variable.name = 'group')
  run.index$ID = as.numeric(run.index$ID)
  run.index = tidyr::spread(run.index,param,value)
  
  write.csv(run.index,file = run.index.long,row.names = F)
  
  # create folder structure
  folders = unique(run.index$dir.name)
  
  #Set groups to be calibrated
  group.names = unique(as.character(run.index$group))
  
  #Get Mortality
  base_mQ_age = get_param_mort_age(bio.prm =paste0(param.dir,'at_biology.prm'),fgs =paste0(param.dir,'neus_groups.csv'))%>%
    filter(group %in% group.names)
  base_mQ_invert = get_param_invert_mort(bio.file = paste0(param.dir,'at_biology.prm'),fgs = paste0(param.dir,'neus_groups.csv'))
  
  #Identify which groups are inverts
  fgs = read.csv(paste0(param.dir,'neus_groups.csv'),as.is = T)
  invert.groups = fgs$Code[which(fgs$NumCohorts == 1)]
  
  #Create run set directory
  # dir.create(project.dir)
  
  run.ID = unique(run.index$ID)
  new.param.files = character()
  #Loop over all IDs in run.index
  i = 1
  for (i in 1:length(run.ID)){
    # create sensible folder name has parameter in name (or index) documented in run.index
    # dir.create(paste0(project.dir,folders[i]))
    
    #Copy over biology.prm template with new name (one for each run)
    new.bio.name = paste0('at_biology_',run.prefix,'_',run.ID[i],'.prm')
    file.copy(paste0(param.dir,'at_biology.prm'),paste0(param.dir,new.bio.name),overwrite = T)
    
    #Do Age mQ
    j=1
    for(j in 1:length(group.names)){
      
      run.index.sub = filter(run.index,ID ==(i-1) & group == group.names[j]) 
      
      if(group.names[j] %in% invert.groups){
        
        base_mQ = filter(base_mQ_invert,group == group.names[j])$mQ
        
        if(base_mQ == 0 & run.index.sub$adult.mQ != 0){
          base_mQ = start.mQ
        }
        
        edit_param_invert_mort(bio.file =paste0(param.dir,new.bio.name),
                               group = group.names[j],
                               type='mQ',
                               value = as.numeric(base_mQ) * run.index.sub$adult.mQ[1],
                               new.file = F)
      }else{
        group.id = which(base_mQ_age$group== group.names[j])  
        
        if(base_mQ_age$mQ.j[group.id] == 0 & run.index.sub$juv.mQ[1] >1 ){
          new.juv.mQ = start.mQ*run.index.sub$juv.mQ[1]  
        }else{
          new.juv.mQ = as.numeric(base_mQ_age$mQ.j[group.id])*run.index.sub$juv.mQ[1]
        }
        
        if(base_mQ_age$mQ.a[group.id] == 0 & run.index.sub$adult.mQ[1] >1){
          new.adult.mQ = start.mQ*run.index.sub$adult.mQ[1]  
        }else{
          new.adult.mQ = as.numeric(base_mQ_age$mQ.a[group.id])*run.index.sub$adult.mQ[1]
        }
        
        new.mQ = c(new.juv.mQ,new.adult.mQ)
        
        edit_param_mort_age(bio.prm = paste0(param.dir,new.bio.name),
                            new.mort = new.mQ,
                            type= 'mQ',
                            overwrite = T,
                            single.group = T,
                            group.name = group.names[j]
        )
      }
    }
    new.param.files[i] = new.bio.name
  }
  return(new.param.files)
}
