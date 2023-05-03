#Check the range of relative slopes in survdat data
#Stability = relativized slope of last x years of timeseries

#Read in survdat
data = readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)

groups = unique(data$Code)

for( i in 1:length(groups)){
  
  
}