#Function to export statistics based on raw scenario output (without needing export)

export_scenario_stats =function(scenario.dir,
                                fgs.file,
                                start.time,
                                end.time,
                                out.dir){
  
  #get run names
  
  library(dplyr)
  options(dplyr.summarise.inform = FALSE)
  
  run.names = list.dirs(scenario.dir,full.names = F,recursive = F)
  run.dirs = list.dirs(scenario.dir,recursive = F)
  
  #create output dataframe space
  biomass.ls = list()
  biomass.age.ls = list()
  biomass.box.ls = list()
  numbers.ls = list()
  numbers.age.ls = list()
  numbers.box.ls = list()
  diet.ls = list()
  
  fgs = read.csv(fgs.file,as.is = T)%>%filter(IsTurnedOn == 1)
   
  i=1
  # for(i in 1:length(run.names)){
  for(i in 1:2){
      
    tictoc::tic()
    #In biomass.ls cacluate:
    # 1) Mean biomass in window by species
    # 2) Slope of biomass during window
    
    biomass.run = data.table::fread(paste0(run.dirs[i],'/neus_outputBiomIndx.txt'),header = T)%>%
      filter(Time >= start.time & Time <= end.time)%>%
      select(Time,all_of(fgs$Code))
    
    biomass.time = biomass.run$Time
    
    biomass.run = select(biomass.run, -Time)
    
    biomass.ls[[i]] = data.frame(
      run.name = run.names[i],
      species = colnames(biomass.run),
      biomass.mean = colMeans(biomass.run),
      biomass.slope = apply(biomass.run,2, function(x) return(lm(x~biomass.time)$coefficients[2]))
    )
    
    #From AgeBiomIndx calculate:
    # 1) Mean Biomass by age over last N years
    # 2) Mean age weighted by biomass
    
    biomass.age = data.table::fread(paste0(run.dirs[i],'/neus_outputAgeBiomIndx.txt'),header = T)%>%
      filter(Time >= start.time & Time <= end.time) 
    
    biomass.age.ls[[i]] = biomass.age %>% 
      select(-Time)%>%
      summarise(across(everything(),mean))%>%
      mutate(run.name = run.names[i])%>%
      tidyr::gather('ID','biomass', -run.name)%>%
      tidyr::separate('ID',c('species','cohort'))%>%
      tidyr::spread(cohort,biomass)
    
    age.mean.biomass= biomass.age %>%
      tidyr::gather('ID','biomass',-Time)%>%
      tidyr::separate('ID',c('species','cohort'))%>%
      group_by(species,cohort)%>%
      summarise(biomass = mean(biomass,na.rm=T))%>%
      group_by(species)%>%
      mutate(biomass.tot = sum(biomass))%>%
      ungroup()%>%
      mutate(cohort = as.numeric(cohort),
             cohort.wgt = biomass*cohort,
             age.wgt = cohort.wgt/biomass.tot)%>%
      group_by(species)%>%
      summarise(age.mean = sum(age.wgt,na.rm=T))%>%
      mutate(run.name = run.names[i])%>%
      select(run.name,species,age.mean)
    
    biomass.ls[[i]]  =     biomass.ls[[i]]  %>% 
      left_join(age.mean.biomass)
    
    #From BoxBiomass calculate:
    # 1) Mean biomass by box
    
    biomass.box.ls[[i]] = data.table::fread(paste0(run.dirs[i],'/neus_outputBoxBiomass.txt')) %>%
      tidyr::gather('species','biomass',-Time,-Box)%>%
      group_by(species,Box)%>%
      summarise(biomass = mean(biomass))%>%
      tidyr::spread(Box,biomass)
    
    
    #From neus_output.nc calculate:
    # 1) Mean Numbers
    # 2) Mean Numbers by age
    # 3) Mean age weighted by numbers
    
    nc.data = ncdf4::nc_open(paste0(run.dirs[i],'/neus_output.nc'))
    nc.time = nc.data$dim$t$vals/86400
    which.time = which(nc.time >= start.time & nc.time <= end.time)
    
    numbers.run = list()
    numbers.age.run = list()
    numbers.box.run = list()
    
    j=1

    for(j in 1:nrow(fgs)){
    
      
      number.group.time = matrix(nrow = 10, ncol = length(which.time))
      number.box.group = array(dim =c(10,22,length(which.time)))
      
      k=1
      
      # profvis::profvis({
      for(k in 1:10){
        
        
        var.name = paste0(fgs$Name[j],k,'_Nums')
        if(var.name %in% names(nc.data$var)){
          
          
          numbers.group = ncdf4::ncvar_get(nc.data,var.name)[,2:23,which.time]
          number.group.time[k,] = apply(numbers.group,3,sum)
          number.box.group[k,,]=apply(numbers.group,c(2,3),sum)
          
        }
      }
      # })
      
      
        numbers.total = apply(number.group.time,2,sum)

        if(any(is.na(number.group.time))){
          numbers.run[[j]] = data.frame(run.name = run.names[i],species = fgs$Code[j]) %>%
            mutate(number.mean = NA,
                   number.slope = NA,
                   age.mean = NA)

          numbers.age.run[[j]] = data.frame(run.name = run.names[i], species = fgs$Code[j], cohort = 1:10, number = NA)%>%
            tidyr::spread(cohort,number)

          numbers.box.run[[j]]=data.frame(run.name = run.names[i],species = fgs$Code[j], box = 1:22, number = NA)%>%
            tidyr::spread(box,number)
        }else{
          numbers.run[[j]] = data.frame(run.name = run.names[i],species = fgs$Code[j]) %>%
            mutate(number.mean = mean(apply(number.group.time,2,sum)),
                   number.slope = lm(apply(number.group.time,2,sum)~nc.time[which.time])$coefficients[2],
                   age.mean = mean(colSums(sweep(number.group.time,2,colSums(number.group.time),'/')* 1:10)))

          numbers.age.run[[j]] = data.frame(run.name = run.names[i], species = fgs$Code[j], cohort = 1:10, number = rowMeans(number.group.time))%>%
            tidyr::spread(cohort,number)

          numbers.box.run[[j]]=data.frame(run.name = run.names[i],species = fgs$Code[j], box = 1:22, number = rowMeans(apply(number.box.group,c(2,3),sum)))%>%
            tidyr::spread(box,number)
        }
        
        print(j)
    }

    
    numbers.ls[[i]] = bind_rows(numbers.run)
    numbers.age.ls[[i]] = bind_rows(numbers.age.run)
    numbers.box.ls[[i]] = bind_rows(numbers.box.run)
    
    #read dietcheck.txt and calculate mean diet proportions
    
    diet.ls[[i]] = data.table::fread(paste0(run.dirs[i],'/neus_outputDietCheck.txt'))%>%
      filter(Time >= start.time & Time <= end.time)%>%
      select(-Stock, -Updated,-Time)%>%
      group_by(Predator,Cohort)%>%
      summarise(across(everything(),mean))
    
    tictoc::toc()
  }
  
  
  
  saveRDS(bind_rows(biomass.ls),paste0(out.dir,'scenario_stats_biomass.rds'))
  saveRDS(bind_rows(biomass.age.ls),paste0(out.dir,'scenario_stats_biomass_age.rds'))
  saveRDS(bind_rows(biomass.box.ls),paste0(out.dir,'scenario_stats_biomass_box.rds'))
  saveRDS(bind_rows(biomass.ls),paste0(out.dir,'scenario_stats_biomass.rds'))
  
  saveRDS(bind_rows(numbers.ls),paste0(out.dir,'scenario_stats_numbers.rds'))
  saveRDS(bind_rows(numbers.age.ls),paste0(out.dir,'scenario_stats_numbers_age.rds'))
  saveRDS(bind_rows(numbers.box.ls),paste0(out.dir,'scenario_stats_numbers_box.rds'))
  
  saveRDS(bind_rows(diet.ls),paste0(out.dir,'scenario_stats_diet.rds'))
  
  
    
}
 
export_scenario_stats(scenario.dir = 'D:/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
                      fgs.file = here::here('currentVersion','neus_groups.csv'),
                      start.time = 20000,
                      end.time = 20000+3650,
                      out.dir =  'D:/fishing_sensitivity_test/')
