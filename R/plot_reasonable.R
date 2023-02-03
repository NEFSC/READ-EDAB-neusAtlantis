library(dplyr)
library(ggplot2)

run.name = 'PersistCheck_2'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name)

bio.model = read.table(paste0(run.dir,'/neus_outputBiomIndx.txt'),header = T)[,1:90]

realBiomass <- readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)
surveyBounds = c(0.5,2)
initBioBounds = c(0.5,2)

priority = read.csv(here::here('Diagnostics','neus_atlantis_group_priority.csv'))

load(paste0(run.dir,'/Post_Processed/diagnostics.Rdata'))

reasonable = reasonable %>% 
  left_join(priority)

spp.bio.ls = list()
for(i in 1:nrow(reasonable)){
  
  spp = reasonable$code[i]
  
  spp.bio = select(bio.model,Time,all_of(spp)) %>%
    mutate(Time = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'))%>%
    rename(value = all_of(spp)) %>%
    mutate(code = spp) %>%
    select(code,Time,value)
  
  spp.bio$test = reasonable$test[i]
  
  if(reasonable$test[i] == 'initialBio'){
    
    init.bio = spp.bio$value[which(spp.bio$value != 0)[1]]
    
    spp.bio$low.lim = init.bio * initBioBounds[1]
    spp.bio$up.lim = init.bio * initBioBounds[2]
    
  }else {
    
    surv.bio = realBiomass %>% 
      filter(Code == spp) 
    
    surv.mean = mean(surv.bio$value)
    
    spp.bio$low.lim = surv.mean * surveyBounds[1]
    spp.bio$up.lim = surv.mean * surveyBounds[2]
    
  }
  spp.bio = spp.bio %>% 
    tidyr::gather(key = 'variable',value = 'value', - code, - Time,-test)
  
  spp.bio.ls[[i]] = spp.bio
}
spp.bio.df = bind_rows(spp.bio.ls)

ggplot(spp.bio.df, aes(x= Time, y = value, color = variable))+
  geom_line()+
  scale_color_manual(values = c('red','green','black'))+
  guides(color = F)+
  facet_wrap(~code, scale = 'free_y')+
  theme_bw()+
  ggsave(paste0(run.dir,'/Post_Processed/reasonable_bounds.png'),width = 20, height = 12, units = 'in', dpi = 300)
