#Function to extract output data from scenario runs over a time window


extract_scenario_output = function(scenario.dir,
                                   start.time,
                                   end.time,
                                   do.BiomIndx = T,
                                   do.BoxBiomass = T,
                                   do.AgeBiomIndx = T,
                                   do.Catch = T,
                                   do.DietCheck = T,
                                   out.dir,
                                   agg.level){
  
  library(dplyr)
  options(dplyr.summarise.inform = FALSE)
  
  run.names = list.dirs(scenario.dir,full.names = F)
  run.dirs = list.dirs(scenario.dir,recursive = F)
  
  #Loop through each run, extract data, export
  BiomIndx.ls = BoxBiomass.ls  = AgeBiomIndx.ls = Catch.ls = DietCheck.ls = list()
  
  filter.data = function(run.dir){
    out = data.table::fread(run.dir,header=T)%>%
      filter(Time >= start.time & Time <= end.time)%>%
      as.data.frame()
    return(out)
  }
  
  i=1
  for(i in 1:length(run.names)){
    
    if(do.BiomIndx){
      BiomIndx.run = filter.data(paste0(run.dirs[i],'/neus_outputBiomIndx.txt'))
      if (agg.level == 'year'){
        BiomIndx.run = BiomIndx.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time)%>%
          summarise(across(everything(), mean))
      }
      BiomIndx.ls[[i]] = BiomIndx.run
    }
    if(do.BoxBiomass){
      BoxBiomass.run = filter.data(paste0(run.dirs[i],'/neus_outputBoxBiomass.txt'))
      if (agg.level == 'year'){
        BoxBiomass.run = BoxBiomass.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time,Box)%>%
          summarise(across(everything(), mean))
      }
      BoxBiomass.ls[[i]] = BoxBiomass.run
    }
    if(do.AgeBiomIndx){
      AgeBiomIndx.run = filter.data(paste0(run.dirs[i],'/neus_outputAgeBiomIndx.txt'))
      if (agg.level == 'year'){
        AgeBiomIndx.run = AgeBiomIndx.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time)%>%
          summarise(across(everything(), mean))
      }
      AgeBiomIndx.ls[[i]] = AgeBiomIndx.run
    }
    if(do.Catch){
      Catch.run = filter.data(paste0(run.dirs[i],'/neus_outputCatch.txt'))
      if (agg.level == 'year'){
        Catch.run = Catch.run %>%
          mutate(Time = floor(Time/365))%>%
          group_by(Time)%>%
          summarise(across(everything(),mean))
      }
      Catch.ls[[i]] = Catch.run
    }
    if(do.DietCheck){
      DietCheck.run = filter.data(paste0(run.dirs[i],'/neus_outputDietCheck.txt'))
      if (agg.level == 'year'){
        DietCheck.run = DietCheck.run %>%
          mutate(Time = floor(Time/365))%>%
          select(-Stock,-Updated)%>%
          group_by(Time,Predator,Cohort)%>%
          summarise(across(everything(),mean))
      }
      DietCheck.ls[[i]] = DietCheck.run
    }
  }
  
  if(do.BiomIndx){
    BiomIndx.out = bind_rows(BiomIndx.ls)
    save(BiomIndx.out,file = paste0(out.dir,'BiomIndx_',start.time,'_',end.time,'_',agg.level,'.Rdata'))
  }
  if(do.BoxBiomass){
    BoxBiomass.out = bind_rows(BoxBiomass.ls)
    save(BoxBiomass.out,file = paste0(out.dir,'BoxBiomass_',start.time,'_',end.time,'_',agg.level,'.Rdata'))
  }
  if(do.AgeBiomIndx){
    AgeBiomIndx.out = bind_rows(AgeBiomIndx.ls)
    save(AgeBiomIndx.out,file = paste0(out.dir,'AgeBiomIndx_',start.time,'_',end.time,'_',agg.level,'.Rdata'))
  }
  
  if(do.Catch){
    Catch.out = bind_rows(Catch.ls)
    save(Catch.out,file = paste0(out.dir,'Catch_',start.time,'_',end.time,'_',agg.level,'.Rdata'))
  }
  
  if(do.DietCheck){
    DietCheck.out = bind_rows(DietCheck.ls)
    save(DietCheck.out,file = paste0(out.dir,'DietCheck_',start.time,'_',end.time,'_',agg.level,'.Rdata'))
  }
  
}

# extract_scenario_output(scenario.dir = 'D:/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
#                         start.time = 20000,
#                         end.time = 20000+3650,
#                         out.dir =  'D:/fishing_sensitivity_test/',
#                         do.BiomIndx = T,
#                         do.BoxBiomass = T,
#                         do.AgeBiomIndx = T,
#                         do.Catch = T,
#                         do.DietCheck = T,
#                         agg.level = 'year'
# )
