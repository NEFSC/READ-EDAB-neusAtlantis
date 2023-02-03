#Function to plot biomass composition per guild (by species)

# data.dir = here::here('data','fishing_sensitivity_extended_constant_2','')
# fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','')
# filter.type = 'fished'
# ref.years = 20
# guild.match = here::here('diagnostics','functional_groups_match.csv')
# fgs.file = fgs.file = here::here('currentVersion','neus_groups.csv')

plot_fishing_sensitivity_guild_composition = function(data.dir,
                                                      fig.dir,
                                                      filter.type = 'all',
                                                      ref.years = 20,
                                                      guild.match,
                                                      fgs.file){
  
  spp2guild = read.csv(guild.match,as.is= T) %>%
    select(Code,Guild)
  
  fgs = read.csv(fgs.file,as.is = T)%>%select(Code,LongName)
  
  bio.all = readRDS(paste0(data.dir,'/biomass_baseline_',filter.type,'.rds'))
  
  end.time = max(bio.all$Time)
  start.time = end.time - (365*ref.years)
  
  bio.all = bio.all %>%
    left_join(spp2guild)%>%
    filter(Time >= start.time & Time <= end.time & guild.name == Guild)
  
  bio.baseline = bio.all %>%
    filter(fishing.scalar == 0)%>%
    select(guild.name,Code,Time,Biomass.baseline)%>%
    rename(Biomass = 'Biomass.baseline')%>%
    mutate(fishing.scalar = 1)
  
  bio.all = bio.all %>% 
    select(guild.name:Biomass)%>%
    bind_rows(bio.baseline) 
    
  guild.names = sort(unique(bio.all$guild.name))
  
  plot.cols = c(RColorBrewer::brewer.pal(12,'Paired'),RColorBrewer::brewer.pal(12,'Set3'))
  i=1
  pdf(paste0(fig.dir,'/biomass_guild_composition_',filter.type,'.pdf'),onefile = T,width = 14, height = 10)
  for(i in 1:length(guild.names)){
    bio.guild = bio.all %>%
      filter(guild.name == guild.names[i]) %>%
      left_join(fgs)
    
    p = ggplot(data = bio.guild, aes(x=factor(fishing.scalar), y= Biomass,fill = LongName))+
      geom_bar(position = 'stack',stat = 'identity')+
      scale_fill_manual(values = plot.cols[1:length(unique(bio.guild$Code))],name = '')+
      guides(fill = guide_legend(title.position = 'top'))+
      xlab('Fishing Scalar')+
      ylab('Biomass (mT)')+
      ggtitle(guild.names[i])+
      theme_bw()+
      theme(panel.grid = element_blank(),
            legend.position = 'bottom')
      
    grid.arrange(p)
  }
  dev.off()
  
    
  
  
  
}

# plot_fishing_sensitivity_guild_composition(data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
#                                            fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2',''),
#                                            filter.type = 'fished',
#                                            ref.years = 20,
#                                            guild.match = here::here('diagnostics','functional_groups_match.csv'),
#                                            fgs.file = here::here('currentVersion','neus_groups.csv'))
