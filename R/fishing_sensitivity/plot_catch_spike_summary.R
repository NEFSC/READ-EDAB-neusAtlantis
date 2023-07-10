#Plot catch spike summary figure

library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'fspike1'

data.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/',experiment.id,'/')
figure.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/figures/',experiment.id,'/')

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T)
master.dat = master.dat[which(master.dat$experiment_id == experiment.id),]

t1 = master.dat$event_start_d/365
t2 = master.dat$event_end_d/365
t4 = t1 + 5
t5 = t1 + 10
t6 = t1 + 19

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)

bio.run.stats = readRDS(paste0(data.dir,'recovery_stats_', experiment.id,'.rds')) %>%
  filter(scalar != 0)
# bio.base.stats = readRDS(paste0(data.dir,'recovery_stats_baseline_', experiment.id,'.rds'))

spp.names = sort(unique(bio.run.stats$Code))

i = 1
plot.recovery.ls = plot.recovery.ls2= list()

for(i in 1:length(spp.names)){
  
  bio.run.spp = bio.run.stats %>%
    filter(Code == spp.names[i])%>%
    left_join(fgs)%>%
    select(LongName,scalar,recovery.5, recovery.10, recovery.20)%>%
    tidyr::gather('dum','Recovery_Rate',-LongName,-scalar)%>%
    tidyr::separate(dum,c('dum2','Recovery_Time'))%>%
    mutate(Recovery_Rate = ifelse(Recovery_Rate == 0,NA,Recovery_Rate))
  
  
  bio.run.spp$Recovery_Time = factor(bio.run.spp$Recovery_Time, levels = c('5','10','20'))
  
  plot.recovery.ls[[i]] = ggplot(bio.run.spp, aes(x= scalar, y= Recovery_Rate, color = factor(Recovery_Time)))+
      geom_point()+
      geom_line()+
      # facet_wrap(~scalar,ncol =1 )+
      xlab('Event Magnitude Scalar')+
      ylab('Recovery Rate (%recovered/year)')+
      ggtitle(bio.run.spp$LongName[1])+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
  

  
  # plot.recovery.ls[[i]] = ggplot(bio.run.spp, aes(x= Recovery_Time, y= Recovery_Rate, color = factor(scalar)))+
  #   geom_point()+
  #   geom_line()+
  #   # facet_wrap(~scalar,ncol =1 )+
  #   xlab('Recovery Time (years)')+
  #   ylab('Recovery Rate (mt/year)')+
  #   ggtitle(bio.run.spp$LongName[1])+
  #   theme_bw()+
  #   theme(plot.title = element_text(hjust = 0.5))
  # 
  # plot.recovery.ls2[[i]] = ggplot(bio.run.spp, aes(x= scalar, y= Recovery_Rate, color = factor(Recovery_Time)))+
  #   geom_point()+
  #   geom_line()+
  #   # facet_wrap(~scalar,ncol =1 )+
  #   xlab('Catch Spike Scalar')+
  #   ylab('Recovery Rate (mt/year)')+
  #   ggtitle(bio.run.spp$LongName[1])+
  #   theme_bw()+
  #   theme(plot.title = element_text(hjust = 0.5))
    
}

pdf(paste0(figure.dir,'recovery_rate_species_',experiment.id,'.pdf'))
for(i in 1:length(plot.recovery.ls)){
  if(is.null(plot.recovery.ls[[i]])){next()}
  grid.arrange(plot.recovery.ls[[i]])}
dev.off()

# pdf(paste0(figure.dir,'recovery_rate_species_',experiment.id,'2.pdf'))
# for(i in 1:length(plot.recovery.ls2)){
#   if(is.null(plot.recovery.ls2[[i]])){next()}
#   grid.arrange(plot.recovery.ls2[[i]])}
# dev.off()
