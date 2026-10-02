library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'random_catch_combined'

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/'

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  select(Code, LongName)

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code, Guild)

start.time = 20805/365
stop.time = start.time + 5

ref.data = readRDS('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/Post_Processed/Data/biomass.rds')%>%
  mutate(time = floor(time))%>%
  filter(time >= start.time & time <=stop.time)%>%
  left_join(fgs,by = c('species' = 'LongName'))%>%
  group_by(Code,species)%>%
  summarise(Biomass.ref = mean(atoutput,na.rm=T))%>%
  mutate(experiment.id = 'baseline')%>%
  rename(LongName = 'species')
# filter(Code == 'CLA')
# plot(atoutput~time,ref.data,type = 'l')
# abline(v = c(start.time,stop.time))

ref.F = readRDS('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/Post_Processed/Data/ref_run_summary.rds')%>%
  select(Code,exploit.prop)%>%
  left_join(fgs)


rand.data = readRDS(paste0(data.dir,experiment.id,'/',experiment.id,'_mean_5yr_proj.rds'))%>%
  left_join(fgs)%>%
  select(Code,LongName,experiment.id,Biomass)


spp.names = sort(unique(rand.data$LongName))  
plot.ls = list()
out.df = data.frame(LongName = spp.names, rand.p2_5 =NA, rand.p97_5 =NA , rand.med = NA, rand.mean = NA, rand.sd = NA)
i = 1
for(i in 1:length(spp.names)){
  
  rand.spp = rand.data %>%
    filter(LongName ==spp.names[i])
  
  ref.spp = ref.data %>%
    filter(LongName ==spp.names[i])
  
  F.spp = signif(ref.F$exploit.prop[which(ref.F$LongName == spp.names[i])],2)
  
  pct.95 = quantile(rand.spp$Biomass,probs = c(0.025,0.975),na.rm=T)
  bio.med = median(rand.spp$Biomass,na.rm=T)
  bio.mean = mean(rand.spp$Biomass,na.rm=T)
  bio.sd = sd(rand.spp$Biomass,na.rm= T)
  
  out.df$rand.p2_5[i] = pct.95[1]
  out.df$rand.p97_5[i] = pct.95[2]
  out.df$rand.med[i] = bio.med
  out.df$rand.mean[i] = bio.mean
  out.df$rand.sd[i] = bio.sd
  
  plot.ls[[i]] = ggplot(data = rand.spp, aes(x = Biomass),fill = 'grey',alpha = 50)+
    # geom_histogram(binwidth = 1000)+
    geom_density(fill = 'grey')+
    geom_vline(xintercept = ref.spp$Biomass.ref[1],color = 'red')+
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

out.stats.df = out.df %>%
  left_join(ref.data)%>%
  mutate(outside = Biomass.ref <rand.p2_5 | Biomass.ref > rand.p97_5)%>%
  filter(!is.na(Biomass.ref))%>%
  left_join(spp2guild)%>%
  mutate(rand.mean = ifelse(rand.mean == 0, NA, rand.mean),
         rand.sd = ifelse(rand.sd == 0, NA, rand.sd),
         deviation = abs(Biomass.ref - rand.mean)/rand.sd,
         deviation.log = log10(deviation))%>%
  group_by(Guild)%>%
  mutate(deviation.max = max(deviation.log,na.rm=T))

saveRDS(out.stats.df,paste0(data.dir,experiment.id,'/',experiment.id,'_path_dependence.rds'))

sum(out.df$outside)/nrow(out.df)

out.df2 =out.df %>%
  group_by(Guild)%>%
  summarise(N = n(),
            outside.N = sum(outside,na.rm=T))%>%
  mutate(outside.pct = outside.N/N)

ggplot(out.stats.df, aes(x = reorder(Guild,-deviation.max),y = deviation.log))+
  geom_boxplot()+
  xlab('Guild')+
  ylab('Deviation from mean (#stdev) - log transformed')+
  theme_bw()
ggsave(paste0(figure.dir,experiment.id,'/',experiment.id,'_deviation_from_mean.png'),width = 8,height =6, units = 'in',dpi =300)
