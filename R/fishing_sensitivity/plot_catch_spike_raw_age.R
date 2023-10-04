#Plot catch spike summary figure

library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'fspike_combined'

data.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id,'/')
figure.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/',experiment.id,'/')

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T) %>%
  filter(experiment_id %in% c('fspike1','fspike2'))

t1 = master.dat$event_start_d[1]/365

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

biomass.age.baseline = read.table('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/neus_outputAgeBiomIndx.txt',header = T)%>%
  tidyr::gather('dum','Biomass',-Time)%>%
  tidyr::separate(dum,c('Code','agecl'))%>%
  filter(Code %in% fgs$Code)%>%
  mutate(Time = floor(Time/365))%>%
  group_by(Time,Code,agecl)%>%
  summarise(Biomass = mean(Biomass,na.rm=T))%>%
  mutate(scalar = factor('baseline'),
         run.id = 'baseline')
  

biomass.age = readRDS(paste0(data.dir,'AgeBiomIndx_1_28105_year_',experiment.id,'.rds')) %>%
  left_join(setup.df, by = 'run.id')%>%
  select(Time,Code,Biomass,agecl,scalar,run.id)%>%
  mutate(scalar = factor(scalar))%>%
  bind_rows(biomass.age.baseline)%>%
  filter(scalar %in% c('baseline',2,5,10,25,50,100))

levels(biomass.age$scalar)
spike.start = master.dat$event_start_d
spike.end = master.dat$event_end_d

spp.names = sort(unique(setup.df$target.species))

bio.plot.ls = bio.plot.ls2 = list()
i=6
for(i in 1:length(spp.names)){
  
  dat.spp = biomass.age %>%
    filter(Code == spp.names[i])%>%
    left_join(fgs)%>%
    filter(Time >= (t1-1))
  
  if(nrow(dat.spp)== 0){next()}
  
  bio.plot.ls[[i]] = ggplot(dat.spp, aes(x= Time, y= Biomass,color = agecl))+
    facet_wrap(~scalar,nrow = 2)+
    # scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(dat.spp)),'Set3'),'black'))+
    geom_line(size = 1.2)+
    theme_bw()+
    ggtitle(dat.spp$LongName[1])+
    guides(color = guide_legend(title = 'Age Class', nrow = 2))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = 'bottom')+
    xlab('Year')
  
  bio.plot.ls2[[i]] = ggplot(dat.spp, aes(x= Time, y= Biomass,color = factor(scalar)))+
    facet_wrap(~agecl,nrow = 2)+
    scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(dat.spp))-1,'Paired'),'black'))+
    geom_line(size =1.2)+
    theme_bw()+
    ggtitle(dat.spp$LongName[1])+
    guides(color = guide_legend(title = 'Age Class', nrow = 2))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = 'bottom')+
    xlab('Year')
}

pdf(paste0(figure.dir,experiment.id,'_biomass_age.pdf'),width = 14,height = 10)
for(i in 1:length(bio.plot.ls)){
  if(is.null(bio.plot.ls[[i]])){next()}
  grid.arrange(bio.plot.ls[[i]])}
dev.off()

pdf(paste0(figure.dir,experiment.id,'_biomass_age_2.pdf'),width = 14,height = 10)
for(i in 1:length(bio.plot.ls2)){
  if(is.null(bio.plot.ls2[[i]])){next()}
  grid.arrange(bio.plot.ls2[[i]])}
dev.off()
