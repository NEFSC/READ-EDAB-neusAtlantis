#Function to export statistics based on raw scenario output (without needing export)

scenario.dir = 'D:/Atlantis_Runs/fishing_sensitivity_extended_constant_2/'
fgs.file = here::here('currentVersion','neus_groups.csv')
start.time = 20000
end.time = 20000+3650
out.dir =  'D:/fishing_sensitivity_test/'

export_scenario_stats =function(scenario.dir,
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
  biomass.age.mean.ls = list()
  numbers.ls = list()
  numbers.age.ls = list()
  numbers.age.mean.ls = list()
  numbiomass.box.ls = list()
  diet.ls = list()
  
  fgs = read.csv(fgs.file,as.is = T)%>%filter(IsTurnedOn == 1)
   
  i=1
  for(i in 1:length(run.names)){
      
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
    
    biomass.age = data.table:::fread(paste0(run.dirs[i],'/neus_outputAgeBiomIndx.txt'),header = T)%>%
      filter(Time >= start.time & Time <= end.time) 
    
    biomass.age.ls[[i]] = biomass.age %>% 
      select(-Time)%>%
      summarise(across(everything(),mean))%>%
      mutate(run.name = run.names[i])%>%
      tidyr::gather('ID','biomass', -run.name)%>%
      tidyr::separate('ID',c('species','cohort'))%>%
      tidyr::spread(cohort,biomass)
    
    age.mean.biomass.ls[[i]] = biomass.age %>%
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
    
    #From neus_output.nc calculate:
    # 1) Mean Numbers
    # 2) Mean Numbers by age
    # 3) Mean age weighted by numbers
    
    nc.data = ncdf4::nc_open(paste0(run.dirs[i],'/neus_output.nc'))
    nc.time = nc.data$dim$t$vals/86400
    which.time = which(nc.time >= start.time & nc.time <= end.time)
    
    j=1
    for(j in 1:nrow(fgs)){
      
      k=1
      
      number.tot.group = data.frame(species = fgs$Code[j],Cohort = 1:10,number = NA)
      number.box.group = data.frame(species = fgs$Code[j], Cohort =1:10)
      for(l in 1:30){number.box.group[,l+2] = NA;colnames(number.box.group)[l+2] = as.character(l)}
      for(k in 1:10){
        var.name = paste0(fgs$Name[j],k,'_Nums')
        if(var.name %in% names(nc.data$var)){
          numbers.group = ncdf4::ncvar_get(nc.data,var.name)[,,which.time]
          
          number.tot.group$number[k] = mean(apply(numbers.group,3,sum))
          number.box.group[k,3:32] = rowMeans(apply(numbers.group,c(2,3),sum))
        }
          
        
        
      }
      
    }
      
      
 
run.names}

  
    
}
