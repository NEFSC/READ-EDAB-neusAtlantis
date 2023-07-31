#Categorize climate impact of species by positive, negative, or neutral
# positive = > 5% difference
# negative = < -5% difference
# netral between

library(dplyr)
library(ggplot2)
deltaT.dir = here::here('Atlantis_Runs','cm2_6_2100_dev_deltaT_TempDependOff','')
base.dir  = here::here('Atlantis_Runs','cm2_6_2100_dev_baseline_TempDependOff','')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>%
  select(Code, LongName)
spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is =T)%>%
  select(Code,Guild)

proj.start = 2021
hind.start = 1984

deltaT.bio = read.table(paste0(deltaT.dir,'neus_outputBiomIndx.txt'),header = T)%>%
  select(Time, MAK:DC)%>%
  tidyr::gather('Code','Biomass.deltaT',-Time)%>%
  mutate(year = floor(Time/365)+1964)%>%
  filter(year >= proj.start)
  
base.bio = read.table(paste0(base.dir,'neus_outputBiomIndx.txt'),header = T)%>%
  select(Time, MAK:DC)%>%
  tidyr::gather('Code','Biomass.base',-Time)%>%
  mutate(year = floor(Time/365)+1964)%>%
  filter(year >= proj.start)

deltaT.mean = deltaT.bio %>%
  group_by(Code)%>%
  summarise(Biomass.deltaT = mean(Biomass.deltaT,na.rm=T))

base.mean = base.bio %>%
  group_by(Code)%>%
  summarise(Biomass.base = mean(Biomass.base,na.rm=T))

impact = function(x){
  if(!is.finite(x)){
    return(NA)
  }else if(x < 0.05){
    return('Negative')
  }else if( x > 0.05){
    return('Positive')
  }else{
    return('Neutral')
  }
}

data.all = deltaT.mean %>%
  left_join(base.mean)%>%
  mutate(Biomass.rel = (Biomass.deltaT-Biomass.base)/Biomass.base)

data.all$impact = sapply(data.all$Biomass.rel , impact)

data.all %>%
  group_by(impact)%>%
  summarise(N = n(),
            pct = N/nrow(data.all))


deltaT.bio2 = read.table(paste0(deltaT.dir,'neus_outputBiomIndx.txt'),header = T)%>%
  select(Time, MAK:DC)%>%
  tidyr::gather('Code','Biomass.deltaT',-Time)%>%
  mutate(year = floor(Time/365)+1964)%>%
  filter(year >= hind.start)%>%
  group_by(Code,year)%>%
  summarise(Biomass = mean(Biomass.deltaT,na.rm=T))%>%
  mutate(run.name = 'CM2.6')

base.bio2 = read.table(paste0(base.dir,'neus_outputBiomIndx.txt'),header = T)%>%
  select(Time, MAK:DC)%>%
  tidyr::gather('Code','Biomass.base',-Time)%>%
  mutate(year = floor(Time/365)+1964)%>%
  filter(year >= hind.start)%>%
  group_by(Code,year)%>%
  summarise(Biomass = mean(Biomass.base,na.rm=T))%>%
  mutate(run.name = 'Baseline')

data.all2 = deltaT.bio2 %>%
  bind_rows(base.bio2)%>%
  left_join(spp2guild)%>%
  filter(!is.na(Guild))
  

guild.bio = data.all2 %>%
  group_by(year,Guild,run.name)%>%
  summarise(Biomass = sum(Biomass,na.rm=T))

init.bio = guild.bio %>%
  ungroup()%>%
  filter(year == hind.start)%>%
  select(Guild,run.name,Biomass)%>%
  rename(Biomass.init = 'Biomass')

guild.bio = guild.bio %>%
  left_join(init.bio)%>%
  mutate(Biomass.rel = Biomass/Biomass.init)

ggplot(guild.bio, aes(x= year, y = Biomass.rel,color = run.name))+
  geom_line()+
  facet_wrap(~Guild,ncol =3,scale = 'free_y')+
  geom_vline(xintercept = 2020)+
  guides(color = guide_legend(title = ''))+
  theme_bw()+
  xlab('')+
  ylab('Relative Biomass (from 1984)')+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures','CM_2_6_v_baseline_biomass.png'),width = 10,height = 10, units = 'in',dpi = 300)
