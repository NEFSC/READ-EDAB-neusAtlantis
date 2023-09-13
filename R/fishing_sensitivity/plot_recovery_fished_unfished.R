#Plot unfished and fished recovery biomass timeseries faceted by scalar
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/'

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))

experiment.id.fished = 'fspike_combined'
data.dir.fished = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id.fished,'/')
setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id.fished,'_setup.csv')),as.is = T)
data.fished.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id.fished,'/')
biomass.fished = readRDS(paste0(data.dir.fished,'BiomIndx_1_28105_year_',experiment.id.fished,'.rds'))%>%
  left_join(setup.df, by = 'run.id')%>%
  select(Time,Code,Biomass,scalar,run.id)%>%
  filter(scalar %in% c(2,5,10,25,50,100))%>%
  mutate(scenario = 'Fished Recovery')
  
experiment.id.unfished = 'fspike_UnfishedRecovery'
data.dir.unfished = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id.unfished,'/')
setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id.unfished,'_setup.csv')),as.is = T)
data.unfished.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id.unfished,'/')
biomass.unfished =  readRDS(paste0(data.unfished.dir,'BiomIndx_',experiment.id.unfished,'_1_28105_year.rds'))%>%
  left_join(setup.df, by = 'run.id')%>%
  select(Time,Code,Biomass,scalar,run.id)%>%
  filter(scalar %in% c(2,5,10,25,50,100))%>%
  mutate(scenario = 'Unfished Recovery')

data.all = biomass.fished %>%
 bind_rows(biomass.unfished)%>%
  left_join(fgs)%>%
  left_join(guild2spp)%>%
  filter(Time > 50)

spp.names = sort(unique(data.all$LongName))

i=1
# plot.ls = list()
pdf(paste0(figure.dir,'Fished_v_Unfished_Biomass.pdf'))
for(i in 1:length(spp.names)){
  
  data.spp = data.all %>%
    filter(LongName == spp.names[i])
  
  bio.plot = ggplot(data.spp,aes(x = Time, y= Biomass,color = scenario))+
    geom_line()+
    facet_wrap(~scalar,labeller = label_both)+
    ggtitle(spp.names[i])+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5))
  grid.arrange(bio.plot)
}
dev.off()
