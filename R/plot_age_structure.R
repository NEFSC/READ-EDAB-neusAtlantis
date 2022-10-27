#Function to summarize numbers and biomass at age by guild
library(dplyr)
library(ggplot2)
library(gridExtra)

run.dir = here::here('Atlantis_Runs','misc_BHalpha_2b')
run.bio.age = readRDS(paste0(run.dir,'/Post_Processed/Data/biomass_age.rds'))
run.num.age = readRDS(paste0(run.dir,'/Post_Processed/Data/numbers_age.rds'))

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),stringsAsFactors = F)%>%
  select(LongName,Code,FuncGroup)


#Generate mean biomass at age for last 20 years of run
max.time = max(run.bio.age$time)

mean.bio.age = run.bio.age %>%
  filter(time > max.time-20)%>%
  group_by(species,agecl)%>%
  summarise(value = mean(atoutput,na.rm=T))

bio.spp.tot = mean.bio.age %>%
  group_by(species)%>%
  summarise(value.tot = sum(value,na.rm=T))

bio.age.prop = mean.bio.age %>%
  left_join(bio.spp.tot)%>%
  mutate(value.prop = value/value.tot)%>%
  left_join(spp2guild, by = c('species' = 'LongName'))%>%
  filter(!(FuncGroup %in% c('Other', 'NA','Shrimp')))%>%
  ungroup()

#Generate mean numbers at age for last 20 years of run
mean.num.age = run.num.age %>%
  filter(time > max.time-20)%>%
  group_by(species,agecl)%>%
  summarise(value = mean(atoutput,na.rm=T))

num.spp.tot = mean.num.age %>%
  group_by(species)%>%
  summarise(value.tot = sum(value,na.rm=T))

num.age.prop = mean.num.age %>%
  left_join(num.spp.tot)%>%
  mutate(value.prop = value/value.tot)%>%
  left_join(spp2guild, by = c('species' = 'LongName'))%>%
  filter(!(FuncGroup %in% c('Other', 'NA','Shrimp')))%>%
  ungroup()

guild.names = sort(unique(bio.age.prop$FuncGroup))
bio.plot.bar.ls = num.plot.bar.ls = list()
for(i in 1:length(guild.names)){
  
  bio.prop.guild = bio.age.prop %>%
    filter(FuncGroup == guild.names[i])
  
  num.prop.guild = num.age.prop %>%
    filter(FuncGroup == guild.names[i])
  
  p = ggplot(bio.prop.guild,aes(x=species,y=value.prop,fill = factor(agecl),group = species))+
    geom_bar(stat = 'identity',position = position_stack())+
    scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
    scale_y_continuous(breaks = c(0,0.5,1))+
    ylab('')+
    xlab('')+
    ggtitle(guild.names[i])+
    theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 12),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 13),
          axis.text.y = element_text(size = 10)
          )
  
  q = ggplot(num.prop.guild,aes(x=species,y=value.prop,fill = factor(agecl),group = species))+
    geom_bar(stat = 'identity',position = position_stack())+
    scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
    scale_y_continuous(breaks = c(0,0.5,1))+
    ylab('')+
    xlab('')+
    ggtitle(guild.names[i])+
    theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 12),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 13),
          axis.text.y = element_text(size = 10)
    )
    
  if(i == length(guild.names)){
    p = p +
      guides(fill = guide_legend(nrow =1))+
      theme(legend.position = 'bottom')
    
    q = q +
      guides(fill = guide_legend(nrow =1))+
      theme(legend.position = 'bottom')
      
  }else{
    p = p + 
      guides(fill = 'none')
    
    q = q + 
      guides(fill = 'none')
  }
  
  bio.plot.bar.ls[[i]] = p
  num.plot.bar.ls[[i]] = q
  
  # bio.plot.line.ls[[i]] = ggplot(data=bio.prop.guild,aes(x=agecl,y=value.prop,color = species))+
  #   geom_line()+
  #   scale_x_continuous(breaks = 1:10)+
  #   ylab('Proportion')+
  #   xlab('Age class')+
  #   ggtitle(guild.names[i])+
  #   theme_bw()+
  #   theme(panel.grid = element_blank())
    
}
png(here::here('Figures','biomass_age_prop_guild_bar_test.png'),width = 9, height = 18,units = 'in',res = 300)
do.call('grid.arrange',c(bio.plot.bar.ls,ncol = 1))
dev.off()

png(here::here('Figures','numbers_age_prop_guild_bar_test.png'),width = 9, height = 18,units = 'in',res = 300)
do.call('grid.arrange',c(num.plot.bar.ls,ncol = 1))
dev.off()
# png(here::here('Figures','biomass_age_prop_guild_line_test.png'),width = 24, height = 9,units = 'in',res = 300)
# do.call('grid.arrange',c(bio.plot.line.ls,nrow = 3))
# dev.off()


