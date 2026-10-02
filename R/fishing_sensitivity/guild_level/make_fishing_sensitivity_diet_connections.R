#Function to genereate the connnectivity of each predator grouped by guild for each scenario


make_fishing_sensitivity_diet_connections = function(batch.dir,
                                                     batch.prefix,
                                                     out.dir,
                                                     run.index.file,
                                                     ref.years = 20){
  scenario.combs = readRDS(run.index.file)
  
  diet.ls = list()
  i=1
  for(i in 1:nrow(scenario.combs)){
    
    tryCatch(
      {
        run.name = paste0(batch.prefix,'_',scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i])  
        
        diet.ls[[i]] = data.table::fread(paste0(batch.dir,run.name,'/neus_outputDietcheck.txt'),header = T) %>%
          select(-Stock,-Updated) %>%
          tidyr::gather('Prey','Proportion',-Time,-Predator,-Cohort)%>%
          group_by(Time,Predator,Prey)%>%
          summarise(Proportion = sum(Proportion,na.rm=T))%>%
          mutate(flag.interaction = Proportion > 0)%>%
          group_by(Time,Predator)%>%
          summarise(n.prey = sum(flag.interaction))
      
      }, error = function(e){}
    )
    
    print(i)
  }
  
  diet.ls = bind_rows(diet.ls)
  
  #set up time window for comparisons
  end.time = max(diet.ls$Time)
  start.time = end.time - (365*ref.years)
  
  
  diet.conn = diet.ls %>%
    filter(Time >= start.time & Time <= end.time) %>%
    group_by(Predator)%>%
    summarise(n.prey = mean(n.prey,na.rm=T))
  
  saveRDS(diet.conn,paste0(out.dir,'/diet_connections.rds'))
  
}

#   
# make_fishing_sensitivity_diet_connections(batch.dir = 'D:/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
#                                           batch.prefix = 'fishing_sensitivity_extended_constant_2',
#                                           out.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
#                                           run.index.file = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'))
 
