#' plot survdat biomass and stocksmart biomass to compare
#' 



library(magrittr)
# read in stock smart data fro attlantis groups
ssBiomass <- readRDS(here::here("data/stockSMARTData.rds")) %>%
  dplyr::select(-units,-Description) %>% 
  dplyr::mutate(variable = ifelse(as.character(variable)=="biomass","stockSmartBio",as.character(variable))) %>%
  dplyr::group_by(YEAR,Code,variable) %>% 
  dplyr::summarize(value = sum(value),.groups="drop")%>%
  dplyr::mutate(source = 'StockSmart')%>%
  dplyr::select(YEAR,Code,value,source)

# read in survdat data
sdBiomass <- readRDS(here::here("data/sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(variable = ifelse(as.character(variable)=="tot.biomass","survdatBio",as.character(variable))) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::group_by(YEAR,Code,variable) %>% 
  dplyr::summarize(value = sum(value),.groups="drop")%>%
  dplyr::ungroup()%>%
  dplyr::mutate(source = 'survdat')%>%
  dplyr::select(YEAR,Code,value,source)

#Read in neus biomass
neus.bio.file = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/PL_DF_SlowSink_4/Post_Processed/Data/biomass.rds')
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% select(Code, LongName)
grps = unique(c(ssBiomass$Code,sdBiomass$Code))

neusBiomass = readRDS(neus.bio.file) %>%
  left_join(fgs, by = c('species' = 'LongName'))%>%
  filter(Code %in% grps)%>%
  mutate(date = as.POSIXct(time*86400*365,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         YEAR = as.numeric(format(date,format = '%Y')))%>%
  group_by(YEAR,Code)%>%
  summarise(value = mean(atoutput,na.rm=T))%>%
  mutate(source = 'NEUS')%>%
  ungroup()

# find functional groups in common
# ssGroups <- ssBiomass %>% dplyr::distinct(Code) %>% dplyr::pull()
# survdatGroups <- realBiomass %>% dplyr::distinct(Code) %>% dplyr::pull()
# grps <- intersect(ssGroups,survdatGroups)

# filter bby common groups and combine
# combined <- rbind(ssBiomass %>% dplyr::filter(Code %in% grps), realBiomass %>% dplyr::filter(Code %in% grps))
combined = dplyr::bind_rows(ssBiomass,sdBiomass,neusBiomass)
  # filter(YEAR >= 1998)

# plot

ggplot2::ggplot(data= combined) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=value,color = source)) + 
  ggplot2::facet_wrap(~Code,scales = "free") + 
  ggplot2::ggtitle("Assessment biomass vs Survdat biomass")+
  ggplot2::ggsave('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/Obs_Comparison2.png',width =24, height = 12, units ='in',dpi = 300)

  



