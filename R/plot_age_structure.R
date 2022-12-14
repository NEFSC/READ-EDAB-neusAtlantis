#Function to summarize numbers and biomass at age by guild
library(dplyr)
library(ggplot2)
library(gridExtra)

run.dir = here::here('Atlantis_Runs','misc_BHalpha_2b','')
run.bio.age = readRDS(paste0(run.dir,'/Post_Processed/Data/biomass_age.rds'))
run.num.age = readRDS(paste0(run.dir,'/Post_Processed/Data/numbers_age.rds'))

expected.age.prop = read.csv(here::here('diagnostics','expected_age_structure.csv'),as.is = T,header = T)
colnames(expected.age.prop)[3:12] = 1:10
expected.age.prop=reshape2::melt(expected.age.prop,id.vars = c('variable','fishing.level'),variable.name = 'agecl')

fishing.intesity.df = read.csv(here::here('diagnostics','fishing_intensity.csv'),as.is = T)
fishing.intesity.df$fishing.intensity[which(fishing.intesity.df$fishing.intensity %in% c('L','N'))] = 'L'

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),stringsAsFactors = F)%>%
  select(LongName,Code,FuncGroup)%>%
  left_join(fishing.intesity.df)

spp2guild$fishing.intensity = factor(spp2guild$fishing.intensity, levels = c('L','M','H'))

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
  mutate(value.prop = value/value.tot,
         variable = 'Biomass')%>%
  left_join(spp2guild, by = c('species' = 'LongName'))%>%
  filter(!(FuncGroup %in% c('Other', 'NA','Shrimp')))%>%
  ungroup()%>%
  arrange(FuncGroup,fishing.intensity,species,agecl)

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
  mutate(value.prop = value/value.tot,
         variable = 'Numbers')%>%
  left_join(spp2guild, by = c('species' = 'LongName'))%>%
  filter(!(FuncGroup %in% c('Other', 'NA','Shrimp')))%>%
  ungroup()

#Combine
data.age.prop = bind_rows(bio.age.prop,num.age.prop)
## Make header with ideal age structure

header.unfished = expected.age.prop %>% filter(fishing.level == 'unfished')
header.medium = expected.age.prop %>% filter(fishing.level == 'medium')
header.high = expected.age.prop %>% filter(fishing.level == 'high')
header.unfished.plot = ggplot(expected.age.prop,aes(x = variable, y = value, fill = agecl))+
  geom_bar(stat = 'identity',position = position_stack(reverse = T))+
  coord_flip()+
  scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  ggtitle('Minimal Fishing')+
  guides(fill = 'none')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust = 0.5,size = 12),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 16)
  )
header.medium.plot = ggplot(header.medium,aes(x = variable, y = value, fill = agecl))+
  geom_bar(stat = 'identity',position = position_stack(reverse = T))+
  coord_flip()+
  scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  ggtitle('Moderately Fished')+
  guides(fill = 'none')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust = 0.5,size = 12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
header.high.plot = ggplot(header.high,aes(x = variable, y = value, fill = agecl))+
  geom_bar(stat = 'identity',position = position_stack(reverse = T))+
  coord_flip()+
  scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  ggtitle('Highly Fished')+
  guides(fill = 'none')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust = 0.5,size = 12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

guild.names = sort(unique(bio.age.prop$FuncGroup))
plot.ls = list()
for(i in 1:length(guild.names)){
  
  
  # bio.prop.guild = bio.age.prop %>%
  #   filter(FuncGroup == guild.names[i])%>%
  #   ungroup()%>%
  #   arrange(fishing.intensity,species)
  # bio.prop.guild$species = factor(bio.prop.guild$species,levels = unique(bio.prop.guild$species))
  # 
  # spp.guild = unique(bio.prop.guild$species)
  #   
  # bio.fishing.labs = data.frame(species = spp.guild,x = 1:length(spp.guild),y = 1.1) %>%
  #   left_join(select(spp2guild,LongName,fishing.intensity),by = c('species' = 'LongName'))
  # 
  # #Format for numbers
  # num.prop.guild = num.age.prop %>%
  #   filter(FuncGroup == guild.names[i])%>%
  #   ungroup()%>%
  #   arrange(fishing.intensity,species)
  # 
  # num.prop.guild$species = factor(num.prop.guild$species,levels = unique(num.prop.guild$species))
  # 
  # spp.guild = unique(num.prop.guild$species)
  # 
  # num.fishing.labs = data.frame(species = spp.guild,x = 1:length(spp.guild),y = 1.1) %>%
  #   left_join(select(spp2guild,LongName,fishing.intensity),by = c('species' = 'LongName'))
  
  data.age.guild = data.age.prop %>%
    filter(FuncGroup == guild.names[i])
  spp.guild = unique(data.age.guild$species)
  fishing.labs = data.frame(species = spp.guild,x = 1:length(spp.guild),y = 1.1,variable = 'Numbers') %>%
      left_join(select(spp2guild,LongName,fishing.intensity),by = c('species' = 'LongName'))

  p = ggplot()+
    geom_bar(data = data.age.guild,aes(x=species,y=value.prop,fill = factor(agecl),group = species),stat = 'identity',position = position_stack())+
    geom_text(data = fishing.labs,aes(x=x,y=y,label = fishing.intensity))+
    facet_wrap(~variable)+
    scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
    scale_y_continuous(breaks = c(0,0.5,1),labels = c('0','0.5','1'))+
    ylab('')+
    xlab('')+
    ggtitle(guild.names[i])+
    coord_flip()+
    theme(axis.text.x = element_text(hjust = 1,size = 12),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 13),
        axis.text.y = element_text(size = 10),
        )
  if(i  == length(guild.names)){
    plot.ls[[i]] = p +
          guides(fill = guide_legend(nrow =1))+
          theme(legend.position = 'bottom')
  }else{
      plot.ls[[i]] = p +
        guides(fill = 'none')
  }
  
  # p = ggplot()+
  #   geom_bar(data = bio.prop.guild,aes(x=species,y=value.prop,fill = factor(agecl),group = species),stat = 'identity',position = position_stack())+
  #   geom_text(data = bio.fishing.labs,aes(x=x,y=y,label = fishing.intensity))+
  #   scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  # 
  #   scale_y_continuous(breaks = c(0,0.5,1))+
  #   ylab('')+
  #   xlab('')+
  #   ggtitle(guild.names[i])+
  #   coord_flip()
  #   theme(axis.text.x = element_text(hjust = 1,size = 12),
  #         panel.background = element_blank(),
  #         plot.title = element_text(hjust = 0.5,size = 13),
  #         axis.text.y = element_text(size = 10)
  #         )
    
  

  # q = ggplot()+
  #   geom_bar(data = num.prop.guild,aes(x=species,y=value.prop,fill = factor(agecl),group = species),stat = 'identity',position = position_stack())+
  #   geom_text(data = num.fishing.labs,aes(x=x,y=y,label = fishing.intensity))+
  #   scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  #   scale_y_continuous(breaks = c(0,0.5,1))+
  #   ylab('')+
  #   xlab('')+
  #   ggtitle(guild.names[i])+
  #   coord_flip()+
  #   theme(axis.text.x = element_text(hjust = 1,size = 12),
  #       panel.background = element_blank(),
  #       plot.title = element_text(hjust = 0.5,size = 13),
  #       axis.text.y = element_text(size = 10)
  # )
    
  # if(i == length(guild.names)){
  #   p = p +
  #     guides(fill = guide_legend(nrow =1))+
  #     theme(legend.position = 'bottom')
  #   
  #   q = q +
  #     guides(fill = guide_legend(nrow =1))+
  #     theme(legend.position = 'bottom')
  #     
  # }else{
  #   p = p + 
  #     guides(fill = 'none')
  #   
  #   q = q + 
  #     guides(fill = 'none')
  # }
  # 
  # bio.plot.bar.ls[[i]] = p
  # num.plot.bar.ls[[i]] = q
  
  # bio.plot.line.ls[[i]] = ggplot(data=bio.prop.guild,aes(x=agecl,y=value.prop,color = species))+
  #   geom_line()+
  #   scale_x_continuous(breaks = 1:10)+
  #   ylab('Proportion')+
  #   xlab('Age class')+
  #   ggtitle(guild.names[i])+
  #   theme_bw()+
  #   theme(panel.grid = element_blank())
    
}

grob1 = arrangeGrob(header.unfished.plot,header.medium.plot,header.high.plot,ncol = 3)
grob2 = gridExtra::grid.arrange(plot.ls[[1]],plot.ls[[2]],plot.ls[[3]],plot.ls[[4]],plot.ls[[5]], ncol = 1, heights = c(6,13,7,17,8))
png(here::here('Figures','biomass_age_prop_guild_bar_test.png'),width = 9, height = 18,units = 'in',res = 300)
gridExtra::grid.arrange(grob1,grob2,heights = 
                          c(1,8))
dev.off()


png(here::here('Figures','biomass_age_prop_guild_bar_test.png'),width = 9, height = 18,units = 'in',res = 300)
do.call('grid.arrange',c(bio.plot.bar.ls,ncol = 1))
dev.off()

png(here::here('Figures','numbers_age_prop_guild_bar_test.png'),width = 9, height = 18,units = 'in',res = 300)
do.call('grid.arrange',c(num.plot.bar.ls,ncol = 1))
dev.off()
# png(here::here('Figures','biomass_age_prop_guild_line_test.png'),width = 24, height = 9,units = 'in',res = 300)
# do.call('grid.arrange',c(bio.plot.line.ls,nrow = 3))
# dev.off()


