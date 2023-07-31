#Plot catch spike summary figure

library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'fspike_combined'

data.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/',experiment.id,'/')
figure.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/figures/',experiment.id,'/')

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T) %>%
  filter(experiment_id %in% c('fspike1','fspike2'))

t1 = master.dat$event_start_d[1]/365
t2 = master.dat$event_end_d[1]/365
t3 = t1 + 5
t4 = t1 + 10
t5 = t1 + 19

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

bio.run.stats = readRDS(paste0(data.dir,'recovery_stats_', experiment.id,'.rds')) %>%
  filter(scalar != 0)
age.run.stats = readRDS(paste0(data.dir,'age_stats_',experiment.id,',.rds')) %>%
  filter(scalar != 0)

ref.data = readRDS('C:/Users/Joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_Run/fishing_sensitivity_baseline/Post_Processed/Data/ref_run_summary.rds')

spp.names = sort(unique(bio.run.stats$Code))

i = 1
plot.recovery.ls = plot.age.ls = list()

for(i in 1:length(spp.names)){
  
  bio.run.spp = bio.run.stats %>%
    filter(Code == spp.names[i])%>%
    left_join(fgs)%>%
    select(LongName,scalar,recovery.5, recovery.10, recovery.20)%>%
    tidyr::gather('dum','Recovery_Rate',-LongName,-scalar)%>%
    tidyr::separate(dum,c('dum2','Recovery_Time'))%>%
    mutate(Recovery_Rate = ifelse(Recovery_Rate == 0,NA,Recovery_Rate))
  
  
  bio.run.spp$Recovery_Time = factor(bio.run.spp$Recovery_Time, levels = c('5','10','20'))
  
  scalars = sort(unique(bio.run.spp$scalar))
  scalars.log = log10(scalars)
  
  plot.recovery.ls[[i]] = ggplot(bio.run.spp, aes(x= log10(scalar), y= Recovery_Rate, color = factor(Recovery_Time)))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = scalars.log,labels = scalars,minor_breaks = NULL)+
      guides(color = guide_legend(title = 'Recovery Time (years)'))+
      # facet_wrap(~scalar,ncol =1 )+
      xlab('Event Magnitude Scalar')+
      ylab('Recovery Rate (%recovered/year)')+
      ggtitle(bio.run.spp$LongName[1])+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom')
  
  # if(spp.names[i] %in% age.run.stats$Code ){
  #   
  #   age.run.spp = age.run.stats %>%
  #     filter(Code == spp.names[i])%>%
  #     left_join(fgs)%>%
  #     select(LongName,scalar,delta.age.spike,delta.age.5,delta.age.10,delta.age.20)%>%
  #     tidyr::gather('variable','value',-LongName, -scalar)
  #   
  #   plot.age.ls[[i]] = ggplot(dat = age.run.spp, aes(x= scalar, y= value, color = variable))+
  #     geom_line()+
  #     geom_point()+
  #     theme_bw()+
  #     xlab('Event Magnitude Scalar')+
  #     ylab('Change in juvenile proportion')+
  #     ggtitle(age.run.spp$LongName[1])+
  #     theme( plot.title = element_text(hjust = 0.5))
  # }
  
}

pdf(paste0(figure.dir,'recovery_rate_species_',experiment.id,'.pdf'))
for(i in 1:length(plot.recovery.ls)){
  if(is.null(plot.recovery.ls[[i]])){next()}
  grid.arrange(plot.recovery.ls[[i]])}
dev.off()



# pdf(paste0(figure.dir,'age_prop_species_',experiment.id,'.pdf'))
# for(i in 1:length(plot.age.ls)){
#   if(is.null(plot.age.ls[[i]])){next()}
#   grid.arrange(plot.age.ls[[i]])}
# dev.off()

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv')) %>%
  select(Code,LongName,Guild)

bio.recovery.max = readRDS(paste0(data.dir,'recovery_threshold_',experiment.id,'.rds'))%>%
  left_join(guild2spp)%>%
  arrange(Guild)%>%
  left_join(ref.data)

ggplot(data = bio.recovery.max,aes(x= reorder(LongName,max.recovery),y=max.recovery,fill = Guild))+
  geom_bar(stat = 'identity')+
  guides(fill= guide_legend(title.position = 'left',nrow = 1))+
  ylab('Max Scalar that Recovers')+
  xlab('')+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'Recovery_Threshold_',experiment.id,'.png'),width = 10, height = 10, units = 'in', dpi = 300)

ggplot(data = bio.recovery.max,aes(x= exploit.prop,y=max.recovery,color = Guild,label = Code))+
  geom_point(size = 5)+
  geom_text_repel()+
  ylab('Max Scalar that Recovers')+
  xlab('Exploitation Rate')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'Recovery_Threshold_vs_F_',experiment.id,'.png'),width = 12, height = 10, units = 'in', dpi = 300)


#Recovery Slope
bio.age.lm = readRDS( paste0(data.dir,'age_stats_lm_',experiment.id,'.rds'))%>%
  select(Code,slope)%>%
  rename(slope.age = 'slope')

bio.recovery.rate = readRDS(paste0(data.dir,'recovery_rate_lm_',experiment.id,'.rds'))%>%
  select(Code,recovery.time,slope)%>%
  rename(slope.bio = 'slope')%>%
  left_join(bio.age.lm)%>%
  left_join(guild2spp)%>%
  left_join(ref.data)%>%
  select(Code,LongName,Guild,recovery.time,slope.bio,exploit.prop)

dum= bio.recovery.rate %>% 
  filter(recovery.time == 5)%>%
  rename(slope.order = slope.bio)%>%
  select(Code,slope.order)

bio.recovery.rate =bio.recovery.rate %>% 
  left_join(dum)%>%
  filter(!is.na(slope.bio))

ggplot(bio.recovery.rate,aes(x=reorder(LongName,slope.order),y=slope.bio,fill = Guild))+
  geom_bar(stat = 'identity')+
  facet_wrap(~recovery.time)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'bottom')+
  ylab('Elasticity (slope of recovery rate vs disturbance size)')+
  xlab('')
ggsave(paste0(figure.dir,'Recoverability_',experiment.id,'.png'),width = 10, height = 10, units = 'in', dpi = 300)

# ggplot(bio.recovery.rate, aes(x = slope.age,y = slope.bio, color = Guild,label = Code))+
#   geom_point()+
#   geom_text_repel()+
#   facet_wrap(~recovery.time,nrow = 3)+
#   ylab('Juvenile proportion slope')+
#   xlab('Biomass recovery slope')+
#   theme_bw()+
#   theme(legend.position = 'bottom')
# ggsave(paste0(figure.dir,'Recoverability_v_Juvenile_',experiment.id,'.png'),width = 12, height = 10, units = 'in', dpi = 300)

ggplot(bio.recovery.rate, aes(x = exploit.prop,y = slope.bio, color = Guild, label = Code))+
  geom_point()+
  geom_text_repel()+
  facet_wrap(~recovery.time,ncol =1)+
  ylab('Recoverability')+
  xlab('Exploitation Rate')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'Recoverability_F_',experiment.id,'.png'),width = 10, height = 10, units = 'in', dpi = 300)

#Recovery Rate vs Juv proportion (time after x scalar)
bio.recovery.age = bio.run.stats %>%
  select(Code,scalar,recovery.5,recovery.10,recovery.20)%>%
  tidyr::gather('var','recoverability',-Code,-scalar)%>%
  tidyr::separate('var',c('dum','recovery.time'))%>%
  left_join(select(age.run.stats,Code,scalar,delta.age.spike))%>%
  left_join(guild2spp)%>%
  filter(recovery.time == 10)%>%
  left_join(select(ref.data,Code,exploit.prop))

ggplot(bio.recovery.age,aes(x= delta.age.spike, y = recoverability,color = Guild))+
  geom_point(size =3)+
  facet_wrap(~scalar,ncol = 1,labeller = 'label_both')+
  theme_bw()+
  xlab('Juvenile Proportion')+
  ylab('Recoverability')
ggsave(paste0(figure.dir,'Recoverability_juv_prop_',experiment.id,'.png'),width = 10, height = 14, units = 'in', dpi = 300)

ggplot(bio.recovery.age,aes(x= exploit.prop, y = recoverability,color = Guild))+
  geom_point(size =3)+
  facet_wrap(~scalar,nrow = 1,labeller = 'label_both')+
  guides(color = guide_legend(nrow = 1, title = 'Guild'))+
  theme_bw()+
  xlab('Exploitation Rate')+
  ylab('Recoverability Rate')+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'Recovery_Rate_F_',experiment.id,'.png'),width = 12, height = 6, units = 'in', dpi = 300)

recov.prop = bio.recovery.max %>% 
  group_by(max.recovery)%>%
  summarise(N = n())%>%
  mutate(tot = nrow(bio.recovery.max),
         prop= N/tot,
         prop.cum = cumsum(prop))
  