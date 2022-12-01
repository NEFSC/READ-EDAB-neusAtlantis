# Script to generate diagnostic/publication plot of fishing mortality vs fishing sensitivity

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% select(Code, LongName) %>% arrange(LongName)
spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code,Guild)

# (1) Pull all fishing sensitivity biomass output
batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_scenarios_1/'

batch.prefix = 'fishing_sensitivity_1'
guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore','Other')
fishing.levels.scalar = c(0.1, 0.25, 0.5, 0.75, 1.1,1.25, 1.33, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 10)
fishing.levels.text = c('0_1','0_25','0_5','0_75','1_1','1_25','1_33','1_5','2','2_5','3','4','5','6','7','10')

scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels.scalar' = fishing.levels.scalar) %>%
  arrange(guild.names)%>%
  left_join(data.frame(fishing.levels.scalar = fishing.levels.scalar,fishing.levels.text = fishing.levels.text))

#Read in base run
base.run = 'Dev_11032022'
base.biomass = read.table(here::here('Atlantis_Runs',base.run,'neus_outputBiomIndx.txt'),as.is  =T, header = T)%>%
  select(Time:DC)%>%
  tidyr::gather(Code,Biomass.baseline,-Time)%>%
  mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')))%>%
  group_by(Code,year)%>%
  summarise(Biomass.baseline = mean(Biomass.baseline,na.rm=T))
base.catch = read.table(here::here('Atlantis_Runs',base.run,'neus_outputCatch.txt'),as.is  =T, header = T)%>%
  select(Time:ZG)%>%
  tidyr::gather(Code,Catch.baseline,-Time)%>%
  mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')))%>%
  group_by(Code,year)%>%
  summarise(Catch.baseline = mean(Catch.baseline,na.rm=T))
#Calculate error bars on fishing mortality
f.mort.base = base.biomass %>%
  left_join(base.catch)%>%
  filter(year >= (max(year)-20))%>%
  mutate(F.mort = Catch.baseline/Biomass.baseline)%>%
  group_by(Code)%>%
  summarise(F.mort.mean = mean(F.mort), F.mort.sd = sd(F.mort))

f.mort.all = readRDS(here::here('data','fishing_scenarios_1_mort.RDS'))

spp.names = fgs$Code

i=1
plot.df = data.frame(Code = spp.names,min.collapse = NA, mean.f.mort = NA, mean.bio = NA, mean.catch = NA)
for(i in 1:length(spp.names)){
  
  dat.spp = f.mort.all %>%
    filter(in.guild == T & Code == spp.names[i])%>%
    mutate(below.min = Biomass < (Biomass.baseline*0.1))%>%
    arrange(fishing.scalar)
  
  if(all(dat.spp$below.min == F)){
    plot.df$min.collapse[i] = NA
  }else{
    plot.df$min.collapse[i] = min(dat.spp$fishing.scalar[which(dat.spp$below.min == T)])
  }
  
  plot.df$mean.f.mort[i] = mean(dat.spp$Catch.baseline/dat.spp$Biomass.baseline,na.rm=T)
  plot.df$mean.bio[i] = mean(dat.spp$Biomass.baseline,na.rm=T)
  plot.df$mean.catch[i] = mean(dat.spp$Catch.baseline,na.rm=T)
  
}

plot.df = plot.df %>%
  left_join(spp2guild)%>%
  filter(!is.na(min.collapse))%>%
  left_join(f.mort.base)

ggplot()+
  geom_text(data=plot.df, aes(x = min.collapse,y = mean.f.mort,label = Code,color = Guild))+
  scale_x_continuous(breaks = 1:10,minor_breaks = sort(unique(f.mort.all$fishing.scalar)))+
  xlab('Min Fishing Scalar Resulting in Collapse')+
  ylab('Mean Fishing Mortality')+
  theme(legend.position = 'bottom')+
  ggsave(filename = here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_diag_collapse_v_F.png'),width = 14, height = 6, units = 'in', dpi = 300)

ggplot(data=plot.df, aes(x = mean.catch,y = mean.f.mort,color = Guild,label = Code))+
  # geom_errorbar(data= plot.df, aes(x = min.collapse,ymin = mean.f.mort - F.mort.sd, ymax = mean.f.mort + F.mort.sd))+
  geom_point(aes(size = min.collapse),alpha = 0.5)+
  scale_size(name = 'Fishing Capacity',range = c(1,10),breaks = c(1,1.5,2,5,10), labels = c('1 - 1.5','1.5 - 2','2 - 5','5 - 10','>10'))+
  scale_color_manual(name = 'Guild',values = RColorBrewer::brewer.pal(6,'Set2'))+
  geom_text_repel(max.overlaps = 12)+
  # scale_x_continuous(breaks = 1:10,minor_breaks = sort(unique(f.mort.all$fishing.scalar)))+
  guides(size = guide_legend(title.position = 'top',title.hjust = 0.5, label.position = 'bottom'),
         color = guide_legend(title.position = 'top'))+
  xlab('Mean Catch (mT/year)')+
  ylab('Mean Proportion Caught Annually')+
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank())+
  ggsave(filename = here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_diagnostic_v2.png'),width = 14, height = 6, units = 'in', dpi = 300)


ggplot()+
  geom_text(data=plot.df, aes(x = min.collapse,y = mean.bio,label = Code,color = Guild))+
  ggsave(filename = here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_diag_collapse_v_biomass.png'))

ggplot()+
  geom_text(data=plot.df, aes(x = min.collapse,y = mean.catch,label = Code,color = Guild))+
  ggsave(filename = here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_diag_collapse_v_catch.png'))
