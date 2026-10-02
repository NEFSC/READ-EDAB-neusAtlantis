library(dplyr)

experiment.id = 'random_catch1'

experiment.dir = here::here('Atlantis_Runs',experiment.id)
data.dir = paste0('/contrib/Joseph.Caracappa/fishing_sensitivity/Data/',experiment.id,'/')

setup.df = read.csv(here::here('Setup_Files',paste0(experiment.id,'_setup.csv')),as.is = T)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>%
  filter(IsTurnedOn == 1)

out.ls = list()

start.time = 20805/365
end.time = start.time + 5

i=1
for(i in 1:nrow(setup.df)){
  
  run.dir = paste0(experiment.dir,'/',setup.df$run.id[i],'/')  
  
  out.ls[[i]] = data.table::fread(paste0(run.dir,'neus_outputBiomIndx.txt'), header = T) %>%
    mutate(Time = floor(Time/365))%>%
    filter(Time >= start.time & Time <= end.time)%>%
    select(Time, all_of(fgs$Code))%>%
    tidyr::gather('Code','Biomass',-Time)%>%
    group_by(Code)%>%
    summarise(Biomass = mean(Biomass,na.rm = T))%>%
    mutate(experiment.id = experiment.id,
           run.id = setup.df$run.id[i],
           ID = setup.df$ID[i])
    
    
}

out.df = bind_rows(out.ls)

saveRDS(out.df,paste0(data.dir,experiment.id,'_mean_5yr_proj.rds'))
