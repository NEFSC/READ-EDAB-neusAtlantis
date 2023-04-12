# Script to generate diagnostic/publication plot of fishing mortality vs fishing sensitivity

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)

#get catch-at-age
source(here::here('R','edit_param_catch_age.R'))
catch.age.prop = get_param_catch_age(here::here('currentVersion','at_harvest.prm'))%>%
  rename(Code = 'group.names')%>%
  tidyr::gather(ID,catch.prop,-Code)%>%
  tidyr::separate(ID,c('dum','agecl'))%>%
  ungroup()%>%
  select(-dum)%>%
  mutate(agecl = as.numeric(agecl) -1,
         exploitable.scalar = ifelse(catch.prop == 0, 0,1))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% select(Code, LongName,NumCohorts) %>% arrange(LongName)
spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code,Guild)

# (1) Pull all fishing sensitivity biomass output
batch.prefix = 'fishing_sensitivity_extended_constant_3'
batch.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/')

guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')
fishing.levels.scalar = c(0,0.1,0.5,1.1,1.25,1.5,2,5,10,25,50,100)
fishing.levels.text = c('0','0_1','0_5','1_1','1_25','1_5','2','5','10','25','50','100')

scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels.scalar' = fishing.levels.scalar) %>%
  arrange(guild.names)%>%
  left_join(data.frame(fishing.levels.scalar = fishing.levels.scalar,fishing.levels.text = fishing.levels.text))

#Read in base run
base.run = 'Dev_02062023_extended'
base.biomass = read.table(here::here('Atlantis_Runs',base.run,'neus_outputBiomIndx.txt'),as.is  =T, header = T)%>%
  select(Time:DC)%>%
  tidyr::gather(Code,Biomass.baseline,-Time)%>%
  mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')))%>%
  group_by(Code,year)%>%
  summarise(Biomass.baseline = mean(Biomass.baseline,na.rm=T))

base.biomass.age = read.table(here::here('Atlantis_Runs',base.run,'neus_outputAgeBiomIndx.txt'),as.is  =T, header = T)%>%
  tidyr::gather(ID,Biomass.baseline,-Time)%>%
  tidyr::separate(ID,c('Code','agecl'))%>%
  mutate(agecl = as.numeric(agecl))%>%
  select(Time,Code,agecl,Biomass.baseline)%>%
  left_join(catch.age.prop)%>%
  mutate(Biomass.exploitable = Biomass.baseline * exploitable.scalar)%>%
  group_by(Time,Code)%>%
  summarise(Biomass.exploitable = sum(Biomass.exploitable,na.rm= T))%>%
  mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')))%>%
  group_by(Code,year)%>%
  summarise(Biomass.exploitable = mean(Biomass.exploitable,na.rm=T))

base.biomass.all = base.biomass %>% 
  left_join(base.biomass.age)%>%
  mutate(Biomass.exploitable.combined = Biomass.exploitable)

missing.bio = which(base.biomass.all$Biomass.exploitable == 0)
base.biomass.all$Biomass.exploitable.combined[missing.bio] = base.biomass.all$Biomass.baseline[missing.bio]
base.biomass.all$Biomass.exploitable.combined[which(base.biomass.all$Biomass.exploitable.combined<1)] = 0
base.biomass.all$Biomass.baseline[which(base.biomass.all$Biomass.baseline<1)] = 0

# base.biomass.all.ls = list()
# i=1
# for(i in 1:length(fgs$Code)){
#   
#   biomass.age.spp = filter(base.biomass.age,Code == fgs$Code[i])
#   
#   if(nrow(biomass.age.spp)==0){
#     next()
#   }else {
#     if(mean(biomass.age.spp$Biomass.exploitable,na.rm=T)==0){
#       print(fgs$Code[i])
#       base.biomass.all.ls[[i]] = filter(base.biomass,Code == fgs$Code[i]) %>% rename(Biomass.exploitable = 'Biomass.baseline')
#     }else{
#       base.biomass.all.ls[[i]] = biomass.age.spp 
#     }
#   }
# }
# base.biomass.all = bind_rows(base.biomass.all.ls)

base.catch = read.table(here::here('Atlantis_Runs',base.run,'neus_outputCatch.txt'),as.is  =T, header = T)%>%
  select(Time:ZG)%>%
  tidyr::gather(Code,Catch.baseline,-Time)%>%
  mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')))%>%
  group_by(Code,year)%>%
  summarise(Catch.baseline = mean(Catch.baseline,na.rm=T))

#Calculate error bars on fishing mortality
f.mort.base = base.biomass.all %>%
  left_join(base.catch)%>%
  filter(year >= (max(year)-20))%>%
  mutate(F.mort = ifelse(Biomass.baseline == 0,NA,Catch.baseline/Biomass.baseline),
         F.mort.exploitable = ifelse(Biomass.exploitable.combined == 0,NA,Catch.baseline/Biomass.exploitable.combined))%>%
  group_by(Code)%>%
  summarise(
    Biomass.baseline = mean(Biomass.baseline,na.rm=T),
    Biomass.exploitable = mean(Biomass.exploitable.combined,na.rm=T),
    Catch.baseline = mean(Catch.baseline,na.rm=T),
    F.mort.mean = mean(F.mort,na.rm=T),
    F.mort.sd = sd(F.mort,na.rm=T),
    F.mort.exploitable.mean = mean(F.mort.exploitable,na.rm=T),
    F.mort.exploitable.sd = sd(F.mort.exploitable,na.rm=T))

f.mort.all = readRDS( paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/post_processed/biomass_baseline_mortality_all.rds'))

test = f.mort.all %>%
  filter(in.guild == T)%>%
  mutate(realized.scalar =  round(Catch/Catch.baseline,2),
         test = realized.scalar == fishing.scalar)%>%
  ungroup()%>%
  select(Code,fishing.scalar,realized.scalar,test,Catch,Catch.baseline)
  
  
spp.names = fgs$Code

i=1
plot.df = data.frame(Code = spp.names,min.collapse = NA, mean.f.mort = NA,mean.f.mort.exploitable = NA, mean.bio = NA, mean.bio.exploitable = NA,mean.catch = NA)
for(i in 1:length(spp.names)){
  
  dat.spp = f.mort.all %>%
    filter(in.guild == T & Code == spp.names[i])%>%
    mutate(below.min = Biomass < (Biomass.baseline*0.1))%>%
    arrange(fishing.scalar)
  dat.base.spp = f.mort.base %>%
    filter(Code == spp.names[i])
  
  if(nrow(dat.base.spp)==0){
    next()
  }
  
  if(all(dat.spp$below.min == F)){
    plot.df$min.collapse[i] = 10
  }else{
    plot.df$min.collapse[i] = min(dat.spp$fishing.scalar[which(dat.spp$below.min == T)])
  }
  
  # plot.df$mean.f.mort[i] = mean(dat.spp$Catch.baseline/dat.spp$Biomass.baseline,na.rm=T)
  # plot.df$mean.bio[i] = mean(dat.spp$Biomass.baseline,na.rm=T)
  # plot.df$mean.catch[i] = mean(dat.spp$Catch.baseline,na.rm=T)
  
  plot.df$mean.f.mort[i] = dat.base.spp$F.mort.mean
  plot.df$mean.f.mort.exploitable[i] = dat.base.spp$F.mort.exploitable.mean
  plot.df$mean.bio[i] = dat.base.spp$Biomass.baseline
  plot.df$mean.bio.exploitable[i] = dat.base.spp$Biomass.exploitable
  plot.df$mean.catch[i] =dat.base.spp$Catch.baseline
  
}

plot.df = plot.df %>%
  left_join(spp2guild)%>%
  filter(!is.na(min.collapse))%>%
  mutate(min.collapse.log = log10(min.collapse))%>%
         # collapse.bins = cut(min.collapse.log, breaks = c(),include.lowest = T))%>%
  filter(mean.catch>0)
  # left_join(f.mort.base)
# levels(plot.df$collapse.bins) = LETTERS[1:4]

used.levels = sort(unique(plot.df$min.collapse))
x = 1:length(used.levels)
plot.levels = seq(8,2,length.out = length(used.levels))
# plot.levels = 8*x^-0.5
# plot(plot.levels)

plot.df.long = plot.df %>%
  select(Code,Guild,min.collapse,mean.f.mort.exploitable,mean.bio,mean.catch)%>%
  tidyr::gather('variable','value',-Code,-Guild,-min.collapse)

fig.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/figures/')

ggplot(data = plot.df.long,aes(x= value,y= min.collapse,label = Code,color = Guild))+
  geom_point()+
  geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 30)+
  facet_grid(Guild~variable,scales = 'free')+
  ylab('Fishing Capacity')+
  xlab('')+
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        strip.text = element_text(size = 18),
        axis.text =element_text(size = 14),
        axis.title = element_text(size = 18))+
  ggsave(filename = paste0(fig.dir,'diagnostic_variables_v_fishing_capacity.png'),width = 16, height = 10, units = 'in', dpi = 300)

  
# ggplot(data = plot.df,aes(y=min.collapse,x= mean.f.mort.exploitable,label = Code))+
#   geom_point()+
#   facet_wrap(~Guild,scales = 'free')+
#   geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 20)+
#   xlab('Mean Proportion of Exploitable Biomass Caught Annually')+
#   ylab('Fishing Capacity')+
#   theme_bw()+
#   theme(legend.position = 'bottom',
#         panel.grid.major.y  = element_blank(),
#         panel.grid.minor.y  = element_blank())+
#   ggsave(filename = here::here('Figures',batch.prefix,paste0(batch.prefix,'_diagnostic_exploitation_v_fishing_capacity.png')),width = 14, height = 7.5, units = 'in', dpi = 300)
# 
# ggplot(data = plot.df,aes(y=min.collapse,x= mean.catch,label = Code))+
#   geom_point()+
#   facet_wrap(~Guild,scales = 'free')+
#   geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 20)+
#   xlab('Mean Proportion of Exploitable Biomass Caught Annually')+
#   ylab('Fishing Capacity')+
#   theme_bw()+
#   theme(legend.position = 'bottom',
#         panel.grid.major.y  = element_blank(),
#         panel.grid.minor.y  = element_blank())+
#   ggsave(filename = here::here('Figures',batch.prefix,paste0(batch.prefix,'_diagnostic_catch_v_fishing_capacity.png')),width = 14, height = 7.5, units = 'in', dpi = 300)
# 
# ggplot(data = plot.df,aes(y=min.collapse,x= mean.bio,label = Code))+
#   geom_point()+
#   facet_wrap(~Guild,scales = 'free')+
#   geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 20)+
#   xlab('Mean Proportion of Exploitable Biomass Caught Annually')+
#   ylab('Fishing Capacity')+
#   theme_bw()+
#   theme(legend.position = 'bottom',
#         panel.grid.major.y  = element_blank(),
#         panel.grid.minor.y  = element_blank())+
#   ggsave(filename = here::here('Figures',batch.prefix,paste0(batch.prefix,'_diagnostic_biomass_v_fishing_capacity.png')),width = 14, height = 7.5, units = 'in', dpi = 300)

ggplot(data=plot.df, aes(x = mean.catch,y = mean.f.mort.exploitable,label = Code))+
  # geom_errorbar(data= plot.df, aes(x = min.collapses,ymin = mean.f.mort - F.mort.sd, ymax = mean.f.mort + F.mort.sd))+
  geom_point(aes(fill = Guild,size = factor(min.collapse.log)),alpha = 0.5,pch = 21,color = 'black')+
  scale_size_manual(name = 'Fishing Capacity',values = plot.levels, labels = fishing.levels.scalar[match(used.levels,fishing.levels.scalar)])+
  scale_color_manual(name = 'Guild',values = RColorBrewer::brewer.pal(6,'Set2'))+
  geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 20)+
  facet_wrap(~Guild,scales = 'free')+
  # scale_x_continuous(breaks = 1:10,minor_breaks = sort(unique(f.mort.all$fishing.scalar)))+
  guides(size = guide_legend(title.position = 'top',title.hjust = 0.5, label.position = 'bottom',nrow =1 ),
         fill = guide_legend(title.position = 'top',title.hjust = 0.5))+
  xlab('Mean Catch (mT/year)')+
  ylab('Mean Proportion of Exploitable Biomass Caught Annually')+
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank())+
  ggsave(filename = paste0(fig.dir,'diagnostic_exploitable_biomass_log.png'),width = 14, height = 7.5, units = 'in', dpi = 300)

ggplot(data=plot.df, aes(x = mean.catch,y = mean.f.mort.exploitable,label = Code))+
  # geom_errorbar(data= plot.df, aes(x = min.collapses,ymin = mean.f.mort - F.mort.sd, ymax = mean.f.mort + F.mort.sd))+
  geom_point(aes(fill = Guild,size = factor(min.collapse.log),shape = Guild),alpha = 0.25,color = 'black')+
  scale_size_manual(name = 'Fishing Capacity',values = seq(10,3,length.out = length(used.levels)), labels = fishing.levels.scalar[match(used.levels,fishing.levels.scalar)])+
  scale_color_manual(name = 'Guild',values = RColorBrewer::brewer.pal(length(used.levels),'Set2'))+
  scale_shape_manual(name = 'Guild',values = c(21,22,23,24,25))+
  geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 30)+
  # geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 30)+
  # scale_x_continuous(breaks = 1:10,minor_breaks = sort(unique(f.mort.all$fishing.scalar)))+
  guides(size = guide_legend(title.position = 'top',title.hjust = 0.5, label.position = 'bottom',nrow =1),
         shape = guide_legend(title.position = 'top',title.hjust = 0.5, label.position = 'bottom',nrow =1),
         fill = guide_legend(title.position = 'top',title.hjust = 0.5, override.aes = list(size =15)))+
  xlab('Mean Catch (mT/year)')+
  # ylab('Mean Proportion of Exploitable Biomass Caught Annually')+
  ylab('Mean Exploitation Rate')+
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_text(size =20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
        )
  # ggsave(filename = paste0(fig.dir,'diagnostic_exploitable_biomass_log_no_facet_no_label.png'),width = 14, height = 7.5, units = 'in', dpi = 300)
  ggsave(filename = paste0(fig.dir,'diagnostic_exploitable_biomass_log_no_facet.png'),width = 14, height = 7.5, units = 'in', dpi = 300)

ggplot(data=plot.df, aes(x = mean.catch,y = mean.f.mort.exploitable,label = Code))+
  # geom_errorbar(data= plot.df, aes(x = min.collapses,ymin = mean.f.mort - F.mort.sd, ymax = mean.f.mort + F.mort.sd))+
  geom_point(aes(col = factor(round(10^min.collapse.log,2)),size = factor(min.collapse.log)),alpha = 1)+
  scale_size_manual(name = 'Fishing Capacity',values = seq(10,3,length.out = length(used.levels)), labels = fishing.levels.scalar[match(used.levels,fishing.levels.scalar)])+
  scale_color_manual(name = 'Guild',values = rev(RColorBrewer::brewer.pal(length(unique(plot.df$min.collapse)),'Reds')))+
  geom_text_repel(color = 'black',family = 'mono',fontface = 'bold',max.overlaps = 20)+
  facet_wrap(~Guild,scales = 'free')+
  # scale_x_continuous(breaks = 1:10,minor_breaks = sort(unique(f.mort.all$fishing.scalar)))+
  guides(size = guide_legend(title.position = 'top',title.hjust = 0.5, label.position = 'bottom',nrow =1 ),
         color = 'none')+
  xlab('Mean Catch (mT/year)')+
  ylab('Mean Proportion of Exploitable Biomass Caught Annually')+
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank())+
  ggsave(filename = paste0(fig.dir,'diagnostic_exploitable_biomass_log_facet_unified_scale.png'),width = 14, height = 7.5, units = 'in', dpi = 300)

