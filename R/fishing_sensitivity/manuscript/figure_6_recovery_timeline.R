#Figure 5: Example Recovery scenario with markers

library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'fspike_combined'

data.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/',experiment.id,'/')
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T) %>%
  filter(experiment_id %in% c('fspike1','fspike2'))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

biomass.baseline = read.table('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/neus_outputBiomIndx.txt',header = T)%>%
  select(Time, all_of(fgs$Code))%>%
  mutate(Time = floor(Time/365))%>%
  tidyr::gather('Code','Biomass',-Time)%>%
  group_by(Code,Time)%>%
  summarise(Biomass = mean(Biomass,na.rm=T))%>%
  mutate(scalar = 'baseline',
         run.id = 'baseline')

biomass = readRDS(paste0(data.dir,'BiomIndx_1_28105_year_',experiment.id,'.rds')) %>%
  left_join(setup.df, by = 'run.id')%>%
  select(Time,Code,Biomass,scalar,run.id)%>%
  mutate(scalar = as.character(scalar),
         Time = Time +1)%>%
  bind_rows(biomass.baseline)%>%
  filter(scalar %in% c(1,2,5,10,25,50,100)&Code == 'MEN'&Time >= 50)%>%
  left_join(fgs)%>%
  mutate(scalar = as.factor(scalar))

biomass.init = biomass %>%
  filter(Time == 50)%>%
  mutate(Biomass.init =Biomass)%>%
  select(Code,scalar,Biomass.init)

biomass = biomass %>%
  left_join(biomass.init)%>%
  mutate(Biomass.rel = Biomass/Biomass.init)

biomass$scalar = factor(biomass$scalar,levels = c(2,5,10,25,50,100))

spike.start = master.dat$event_start_d
spike.end = master.dat$event_end_d

spp.names = sort(unique(setup.df$target.species))

plot.colors = c('#AABBD5','#FFCD25','#DE8FAE','#FC6F53','#C6E184','grey50')

t0 = master.dat$event_start_d[1]/365
t1 = master.dat$event_end_d[1]/365
t5 = t1 + 5
t10 = t1 + 9
t20 = t1 + 15
biomass.recover = biomass %>%
  filter(Time > t0 & Time <t5 & scalar == 10)

tmin =biomass.recover$Time[which(biomass.recover$Biomass == min(biomass.recover$Biomass))]

point.df = data.frame(xend = c(t0,t1,tmin,t5,t20),yend =NA)

for(i in 1:nrow(point.df)){point.df$yend[i] = biomass$Biomass.rel[which(biomass$Time == point.df$xend[i] & biomass$scalar == 25)]}

point.df$x = point.df$xend
point.df$y = 1.1
point.df$label = c('Event\nStart',
                   'Event\nEnd',
                   '\n\nMin',
                   'Recovery\n+5',
                   'Recovery\n+15'
                   )
point.df$label.x = point.df$x
point.df$label.y = 1.2

ggplot(biomass, aes(x= Time, y= Biomass.rel,color = factor(scalar)))+
    geom_line()+
    theme_bw()+
    ggtitle(biomass$LongName[1])+
    ylab('Relative Biomass')+
    ylim(0,1.25)+
    guides(color = guide_legend(title = 'Event Magnitude', nrow = 1))+
    scale_color_manual(values = plot.colors)+
    geom_segment(data = point.df,aes(x=x,y=y,xend = xend,yend =yend),color = 'black',
                 arrow = arrow(length = unit(0.25,'cm')))+
    geom_text(data = point.df,aes(x = label.x,y= label.y,label = label),color = 'black',size = 3)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = 'bottom')+
    xlab('Year')

ggsave(paste0(figure.dir,'figure_6_recovery_example.png'),width =9,height =5,units ='in',dpi =300)
