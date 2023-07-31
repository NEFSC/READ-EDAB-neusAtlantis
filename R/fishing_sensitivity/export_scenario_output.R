#Function to extract output data from scenario runs over a time window


extract_scenario_output = function(scenario.dir,
                                   experiment.id,
                                   setup.file,
                                   start.time,
                                   end.time,
                                   do.BiomIndx = T,
                                   do.BoxBiomass = T,
                                   do.AgeBiomIndx = T,
                                   do.Catch = T,
                                   do.DietCheck = T,
                                   out.dir,
                                   agg.level,
                                   only.target = T){
  
  library(dplyr)
  options(dplyr.summarise.inform = FALSE)
  
  setup.df = read.csv(setup.file,as.is = T)
  
  #Loop through each run, extract data, export
  BiomIndx.ls = BoxBiomass.ls  = AgeBiomIndx.ls = Catch.ls = DietCheck.ls = list()
  
  filter.data = function(run.dir){
    out = data.table::fread(run.dir,header=T)%>%
      filter(Time >= start.time & Time <= end.time)%>%
      as.data.frame()
    return(out)
  }
  
  i=1
  for(i in 1:nrow(setup.df)){
    
    run.dir = paste0(scenario.dir,setup.df$run.id[i])
    
    if(do.BiomIndx){
      BiomIndx.run = filter.data(paste0(run.dir,'/neus_outputBiomIndx.txt'))
      
      if(!(setup.df$target.species[i] %in% colnames(BiomIndx.run))){
        BiomIndx.ls[[i]] = NULL
        next()
      }
      if (agg.level == 'year'){
        BiomIndx.run = BiomIndx.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time)%>%
          summarise(across(everything(), mean))
      }
      if(only.target == T){
        BiomIndx.run = BiomIndx.run %>%
          select(Time,all_of(setup.df$target.species[i]))
      }
      BiomIndx.run = BiomIndx.run %>%
        tidyr::gather('Code','Biomass',-Time)%>%
        mutate(run.id = setup.df$run.id[i])
        
      BiomIndx.ls[[i]] = BiomIndx.run
    }
    
    if(do.BoxBiomass){
      BoxBiomass.run = filter.data(paste0(run.dir,'/neus_outputBoxBiomass.txt'))
      
      if(!(setup.df$target.species[i] %in% colnames(BoxBiomass.run))){
        BoxBiomass.ls[[i]] = NULL
        next()
      }
      
      if (agg.level == 'year'){
        BoxBiomass.run = BoxBiomass.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time,Box)%>%
          summarise(across(everything(), mean))
      }
      if(only.target == T){
        BoxBiomass.run = BoxBiomass.run %>%
          select(Time,Box,all_of(setup.df$target.species[i]))
      }
      BoxBiomass.run = BoxBiomass.run %>%
        tidyr::gather('Code','Biomass',-Time, -Box)%>%
        mutate(run.id = setup.df$run.id[i])
      BoxBiomass.ls[[i]] = BoxBiomass.run
    }
    
    if(do.AgeBiomIndx){
      AgeBiomIndx.run = filter.data(paste0(run.dir,'/neus_outputAgeBiomIndx.txt'))
      
      if(length(grep(paste0('\\b',setup.df$target.species[i],'\\b'),colnames(AgeBiomIndx.run)))==0){
        AgeBiomIndx.ls[[i]] = NULL
        next()
      }
      if (agg.level == 'year'){
        AgeBiomIndx.run = AgeBiomIndx.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time)%>%
          summarise(across(everything(), mean))
      }
      if(only.target == T){
        col.match = grep(paste0('\\b',setup.df$target.species[i],'\\b'),colnames(AgeBiomIndx.run),value = T)
        
        AgeBiomIndx.run = AgeBiomIndx.run %>%
          select(Time,all_of(col.match))
      }
      AgeBiomIndx.run = AgeBiomIndx.run %>%
        tidyr::gather('ID','Biomass',-Time)%>%
        tidyr::separate(ID, c('Code','agecl'))%>%
        mutate(run.id = setup.df$run.id[i])
      
      AgeBiomIndx.ls[[i]] = AgeBiomIndx.run
    }
    if(do.Catch){
      Catch.run = filter.data(paste0(run.dir,'/neus_outputCatch.txt'))
      
      if(!(setup.df$target.species[i] %in% colnames(Catch.run))){
        Catch.ls[[i]] = NULL
        next()
      }
      if (agg.level == 'year'){
        Catch.run = Catch.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time)%>%
          summarise(across(everything(),mean))
      }
      if(only.target == T){
        Catch.run = Catch.run %>%
          select(Time,all_of(setup.df$target.species[i]))
      }
      Catch.run = Catch.run %>%
        tidyr::gather('Code','Catch',-Time)%>%
        mutate(run.id = setup.df$run.id[i])
      
      Catch.ls[[i]] = Catch.run
    }
    if(do.DietCheck){
      DietCheck.run = filter.data(paste0(run.dir,'/neus_outputDietCheck.txt'))
      
      if(!(setup.df$target.species[i] %in% (DietCheck.run$Predator))){
        DietCheck.ls[[i]] = NULL
        next()
      }
      
      if(only.target == T){
        DietCheck.run = DietCheck.run %>%
          filter(Predator == setup.df$target.species[i])
      }
      if (agg.level == 'year'){
        DietCheck.run = DietCheck.run %>%
          mutate(Time = floor(Time/365))%>%
          select(-Stock,-Updated)%>%
          group_by(Time,Predator,Cohort)%>%
          summarise(across(everything(),mean))
      }

      which.nonzero = colnames(DietCheck.run)[4:ncol(DietCheck.run)][which(colSums(DietCheck.run[4:ncol(DietCheck.run)]) !=0)]
      DietCheck.run = DietCheck.run %>%
        select(all_of(which.nonzero))%>%
        tidyr::gather('Prey','Proportion',-Time,-Predator)%>%
        mutate(run.id = setup.df$run.id[i])
      
      DietCheck.ls[[i]] = DietCheck.run
    }
    print(setup.df$run.id[i])
  }
  
  if(do.BiomIndx){
    BiomIndx.out = bind_rows(BiomIndx.ls)
    saveRDS(BiomIndx.out,file = paste0(out.dir,'BiomIndx_',experiment.id,'_',start.time,'_',end.time,'_',agg.level,'.rds'))
  }
  if(do.BoxBiomass){
    BoxBiomass.out = bind_rows(BoxBiomass.ls)
    saveRDS(BoxBiomass.out,file = paste0(out.dir,'BoxBiomass_',experiment.id,'_',start.time,'_',end.time,'_',agg.level,'.rds'))
  }
  if(do.AgeBiomIndx){
    AgeBiomIndx.out = bind_rows(AgeBiomIndx.ls)
    saveRDS(AgeBiomIndx.out,file = paste0(out.dir,'AgeBiomIndx_',experiment.id,'_',start.time,'_',end.time,'_',agg.level,'.rds'))
  }
  
  if(do.Catch){
    Catch.out = bind_rows(Catch.ls)
    saveRDS(Catch.out,file = paste0(out.dir,'Catch_',experiment.id,'_',start.time,'_',end.time,'_',agg.level,'.rds'))
  }
  
  if(do.DietCheck){
    DietCheck.out = bind_rows(DietCheck.ls)
    saveRDS(DietCheck.out,file = paste0(out.dir,'DietCheck_',experiment.id,'_',start.time,'_',end.time,'_',agg.level,'.rds'))
  }
  
}

extract_scenario_output(scenario.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/Atlantis_Runs/fscale3/',
                        setup.file = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/Setup_Files/fscale3_setup.csv',
                        experiment.id = 'fscale3',
                        start.time = 1,
                        end.time = 28105,
                        out.dir =  '/contrib/Joseph.Caracappa/fishing_sensitivity/Data/fscale3/',
                        do.BiomIndx = T,
                        do.BoxBiomass =T,
                        do.AgeBiomIndx = T,
                        do.Catch = T,
                        do.DietCheck = T,
                        agg.level = 'year',
                        only.target = T
)

# scenario.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/Atlantis_Runs/fspike2/'
# setup.file = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/Setup_Files/fspike2_setup.csv'
# experiment.id = 'fspike2'
# start.time = 1
# end.time = 28105
# out.dir =  '/contrib/Joseph.Caracappa/fishing_sensitivity/Data/fspike2/'
# do.BiomIndx = F
# do.BoxBiomass = F
# do.AgeBiomIndx = T
# do.Catch = F
# do.DietCheck = F
# agg.level = 'year'
# only.target = T