library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'random_catch1'

data.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/'
figure.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/figures/'

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  select(Code, LongName)

start.time = 20805/365
stop.time = start.time + 5

ref.data = readRDS('C:/Users/Joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_Run/fishing_sensitivity_baseline/Post_Processed/Data/biomass.rds')%>%
  mutate(time = floor(time))%>%
  filter(time >= start.time & time <=stop.time)%>%
  left_join(fgs,by = c('species' = 'LongName'))%>%
  group_by(Code,species)%>%
  summarise(Biomass = mean(atoutput,na.rm=T))%>%
  mutate(experiment.id = 'baseline')%>%
  rename(LongName = 'species')
  # filter(Code == 'CLA')
# plot(atoutput~time,ref.data,type = 'l')
# abline(v = c(start.time,stop.time))

ref.F = readRDS('C:/Users/Joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_Run/fishing_sensitivity_baseline/Post_Processed/Data/ref_run_summary.rds')%>%
  select(Code,exploit.prop)%>%
  left_join(fgs)
  

rand.data = readRDS(paste0(data.dir,experiment.id,'/',experiment.id,'_mean_5yr_proj.rds'))%>%
  left_join(fgs)%>%
  select(Code,LongName,experiment.id,Biomass)
  

spp.names = sort(unique(rand.data$LongName))  
plot.ls = list()
out.df = data.frame(Code = spp.names, p2_5 =NA, p97_5 =NA , med = NA)
i = 1
for(i in 1:length(spp.names)){
  
  rand.spp = rand.data %>%
    filter(LongName ==spp.names[i])
  
  ref.spp = ref.data %>%
    filter(LongName ==spp.names[i])
  
  F.spp = signif(ref.F$exploit.prop[which(ref.F$LongName == spp.names[i])],2)
  
  pct.95 = quantile(rand.spp$Biomass,probs = c(0.025,0.975),na.rm=T)
  bio.med = median(rand.spp$Biomass,na.rm=T)
  
  out.df$p2_5[i] = pct.95[1]
  out.df$p97_5[i] = pct.95[2]
  out.df$med[i] = bio.med
  
  plot.ls[[i]] = ggplot(data = rand.spp, aes(x = Biomass),fill = 'grey',alpha = 50)+
    # geom_histogram(binwidth = 1000)+
    geom_density(fill = 'grey')+
    geom_vline(xintercept = ref.spp$Biomass[1],color = 'red')+
    geom_vline(xintercept = pct.95,color = 'blue3', lty =2)+
    geom_vline(xintercept = bio.med,color = 'magenta', lty =2)+
    theme_bw()+
    xlab('Biomass (mT)')+
    ggtitle(paste0(spp.names[i],' : F=',F.spp))+
    theme(plot.title = element_text(hjust = 0.5))
}

pdf(paste0(figure.dir,experiment.id,'/',experiment.id,'_biomass_distribution.pdf'))
for(i in 1:length(plot.ls)){
  if(is.null(plot.ls[[i]])){next()}
  grid.arrange(plot.ls[[i]])}
dev.off()
