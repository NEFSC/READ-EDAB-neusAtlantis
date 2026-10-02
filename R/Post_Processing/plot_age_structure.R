#Function to summarize numbers and biomass at age by guild
library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)

run.dir = here::here('Atlantis_Runs','misc_BHalpha_2b','')
run.bio.age = readRDS(paste0(run.dir,'/Post_Processed/Data/biomass_age.rds'))
run.num.age = readRDS(paste0(run.dir,'/Post_Processed/Data/numbers_age.rds'))

expected.age.prop = read.csv(here::here('diagnostics','expected_age_structure.csv'),as.is = T,header = T)
colnames(expected.age.prop)[4:13] = 1:10
expected.age.prop=reshape2::melt(expected.age.prop,id.vars = c('variable','fishing.level','fishing.level.label'),variable.name = 'agecl')

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
  mutate(value.prop = value/value.tot,
         variable = 'Numbers')%>%
  left_join(spp2guild, by = c('species' = 'LongName'))%>%
  filter(!(FuncGroup %in% c('Other', 'NA','Shrimp')))%>%
  ungroup()

#Combine
data.age.prop = bind_rows(bio.age.prop,num.age.prop)%>%
  arrange(FuncGroup,fishing.intensity,species,agecl)
## Make header with ideal age structure

header.unfished = expected.age.prop %>% filter(fishing.level == 'unfished')
header.medium = expected.age.prop %>% filter(fishing.level == 'medium')
header.high = expected.age.prop %>% filter(fishing.level == 'high')
header.unfished.plot = ggplot(expected.age.prop,aes(x = variable, y = value, fill = agecl))+
  geom_bar(stat = 'identity',position = position_stack(reverse = T))+
  coord_flip()+
  scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  ggtitle('Minimally Fished (L)')+
  guides(fill = 'none')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust = 0.5,size = 20),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 18)
  )
header.medium.plot = ggplot(header.medium,aes(x = variable, y = value, fill = agecl))+
  geom_bar(stat = 'identity',position = position_stack(reverse = T))+
  coord_flip()+
  scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  ggtitle('Moderately Fished (M)')+
  guides(fill = 'none')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust = 0.5,size = 20),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
header.high.plot = ggplot(header.high,aes(x = variable, y = value, fill = agecl))+
  geom_bar(stat = 'identity',position = position_stack(reverse = T))+
  coord_flip()+
  scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
  ggtitle('Highly Fished (H)')+
  guides(fill = 'none')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    plot.title = element_text(hjust = 0.5,size = 20),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#Try as single facet plot

guild.names = sort(unique(bio.age.prop$FuncGroup))
plot.ls = list()
for(i in 1:length(guild.names)){

  data.age.guild = data.age.prop %>%
    filter(FuncGroup == guild.names[i])
  
  data.age.guild$species = factor(data.age.guild$species, levels = rev(unique(data.age.guild$species)))
  
  spp.guild = unique(data.age.guild$species)
  fishing.labs = data.frame(species = rev(spp.guild),x = 1:length(spp.guild),y = 1.1,variable = 'Numbers') %>%
      left_join(select(spp2guild,LongName,fishing.intensity),by = c('species' = 'LongName'))
  

  p = ggplot()+
    geom_bar(data = data.age.guild,aes(x=species,y=value.prop,fill = factor(agecl),group = species),stat = 'identity',position = position_stack(),width = 0.8)+
    geom_text(data = fishing.labs,aes(x=x,y=y,label = fishing.intensity),size = 6)+
    facet_wrap(~variable)+
    scale_fill_manual(name = 'Age Class',values = RColorBrewer::brewer.pal(10,'Paired'))+
    scale_y_continuous(breaks = c(0,0.5,1),labels = c('0','0.5','1'))+
    ylab('')+
    xlab('')+
    ggtitle(guild.names[i])+
    coord_fixed()+
    coord_flip()+
    theme(axis.text.x = element_text(hjust = 1,size = 15),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 22),
        axis.text.y = element_text(size = 17),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = 'transparent')
        
        )
  if(i  == length(guild.names)){
    plot.ls[[i]] = p +
          guides(fill = guide_legend(nrow =1))+
          theme(legend.position = 'bottom',
                legend.key.size = unit(1, 'cm'), #change legend key size
                legend.key.height = unit(1, 'cm'), #change legend key height
                legend.key.width = unit(1, 'cm'), #change legend key width
                legend.title = element_text(size=18), #change legend title font size
                legend.text = element_text(size=16))
  }else{
      plot.ls[[i]] = p +
        guides(fill = 'none')
  }
  
}

grob1 = arrangeGrob(header.unfished.plot,header.medium.plot,header.high.plot,ncol = 3,widths = c(0.5,0.3,0.3))
# grob2 = gridExtra::grid.arrange(plot.ls[[1]],plot.ls[[2]],plot.ls[[3]],plot.ls[[4]],plot.ls[[5]], ncol = 1, heights = c(5,10,5,13,8))
grob2 = ggplotGrob(plot.ls[[1]]+plot.ls[[2]]+plot.ls[[3]]+plot.ls[[4]]+plot.ls[[5]]+plot_layout(ncol = 1, byrow = FALSE, heights  = c(4,10,7,15,7)))

layout = "ABC
          DDD
          EEE
          FFF
          GGG
          HHH"

png(here::here('Figures','biomass_age_prop_guild_bar_test.png'),width = 15, height = 20,units = 'in',res = 150)
# gridExtra::grid.arrange(grob1,grob2,heights = c(0.75,9))
header.unfished.plot + header.medium.plot + header.high.plot+
  plot.ls[[1]]+plot.ls[[2]]+plot.ls[[3]]+plot.ls[[4]]+plot.ls[[5]]+
  plot_layout(ncol = 1, byrow = FALSE, design = layout,heights = c(3,4,13,7,17,7))
dev.off()



