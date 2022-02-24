#' plot survdat biomass and stocksmart biomass to compare
#' 

library(magrittr)
# read in stock smart data fro attlantis groups
ssBiomass <- readRDS(here::here("data/stockSMARTData.rds")) %>%
  dplyr::select(-units,-Description) %>% 
  dplyr::mutate(variable = ifelse(as.character(variable)=="biomass","stockSmartBio",as.character(variable))) %>%
  dplyr::group_by(YEAR,Code,variable) %>% 
  dplyr::summarize(value = sum(value),.groups="drop")

# read in survdat data
realBiomass <- readRDS(here::here("data/sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(variable = ifelse(as.character(variable)=="tot.biomass","survdatBio",as.character(variable))) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::group_by(YEAR,Code,variable) %>% 
  dplyr::summarize(value = sum(value),.groups="drop")

# find functional groups in common
ssGroups <- ssBiomass %>% dplyr::distinct(Code) %>% dplyr::pull()
survdatGroups <- realBiomass %>% dplyr::distinct(Code) %>% dplyr::pull()
grps <- intersect(ssGroups,survdatGroups)

# filter bby common groups and combine
combined <- rbind(ssBiomass %>% dplyr::filter(Code %in% grps), realBiomass %>% dplyr::filter(Code %in% grps))

# plot

ggplot2::ggplot(data= combined) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=value,color = variable)) + 
  ggplot2::facet_wrap(~Code,scales = "free") + 
  ggplot2::ggtitle("Assessment biomass vs Survdat biomass")
