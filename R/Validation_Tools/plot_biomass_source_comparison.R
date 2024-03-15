#' plot survdat biomass and stocksmart biomass to compare
#' 

run.name = 'Dev_03242022'
  

library(magrittr)
# read in stock smart data fro attlantis groups
ssBiomass <- readRDS(here::here("data/stockSMARTData.rds")) %>%
  dplyr::select(-units,-Description) %>% 
  dplyr::mutate(variable = ifelse(as.character(variable)=="biomass","stockSmartBio",as.character(variable))) %>%
  dplyr::group_by(YEAR,Code,variable) %>% 
  dplyr::summarize(value = sum(value),.groups="drop")%>%
  dplyr::mutate(source = 'StockSmart')%>%
  dplyr::select(YEAR,Code,value,source)

men.bio = read.table(here::here('data','MEN_biomass_2017_assessment.txt'),header = T)%>%
  reshape2::melt(id.vars = 'Year')%>%
  group_by(Year)%>%
  summarise(value = sum(value*1000))%>%
  mutate(Code = 'MEN', source = 'StockSmart')%>%
  rename(YEAR = 'Year')

ssBiomass = bind_rows(ssBiomass,men.bio)

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
neus.bio.file = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/Post_Processed/Data/biomass.rds')
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
data.groups = c('BFT','BIL','BLF','BSB','BUT','CLA','COD','DOG','DRM','FOU','GOO','HAD','HAL','HER','ISQ','LOB','LSQk',
                'LSQ','MAK','MEN','NSH','OHK','PLA','POL','QHG','RED','REP','RHK','SCA','SCU','SK','SMO','SUF','TYL','WHK','WIF','WTF','YTF')

combined.good.data = combined %>%
  filter(Code %in% data.groups)

ggplot2::ggplot(data= combined.good.data) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=value,color = source)) + 
  ggplot2::facet_wrap(~Code,scales = "free",ncol = 6) + 
  ggplot2::ggtitle("Assessment biomass vs Survdat biomass")+
  ggplot2::ggsave(paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/Post_Processed/All_Groups_Data_Comparison.png'),
                  width =24, height = 12, units ='in',dpi = 300)


plank.df = data.frame(Code = c('HER','MEN','BUT','MAK','LSQ'),
                      name = c('Atlantic Herring','Atlantic Menhaden','Butterfish','Atlantis Mackerel','Loligo Squid'),
                      stringsAsFactors = F)

plank.bio = combined %>%
  filter((Code %in% plank.df$Code) & (YEAR >=1998) & source != 'survdat')%>%
  left_join(plank.df)

ggplot2::ggplot(data= plank.bio) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=value,lty = source)) + 
  ggplot2::facet_wrap(~name,scales = "free_y",ncol =1) + 
  scale_y_continuous(limits= c(0,NA))+
  # scale_color_manual(values = c('red3','blue3'))+
  scale_linetype_manual(values = c(1,2))+
  guides(color = 'none',lty = 'none')+
  ylab('Biomass (mT)')+
  xlab('')+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ggplot2::ggsave(paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/Post_Processed/Planktivore_Assessment_comparison.png'),
                  width =5, height = 10, units ='in',dpi = 300)

plank.bio %>%
  group_by(Code,source)%>%
  summarise(value = mean(value,na.rm=T))%>%
  tidyr::spread(source,value)%>%
  mutate(diff = StockSmart-NEUS,
         ratio = StockSmart/NEUS)
