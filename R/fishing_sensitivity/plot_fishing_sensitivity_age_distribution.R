#Creates bubble plots of age distribution

plot_fishing_sensitivity_age_distribution = function(data.dir,
                                                     fig.dir,
                                                     filter.type = 'all',
                                                     ref.years,
                                                     guild.match,
                                                     fgs.file){
  
  spp2guild = read.csv(guild.match,as.is = T) %>% select(Code,Guild)
  fgs = read.csv(fgs.file,as.is = T) %>% select(Code,LongName)
  
  bio.age.all = readRDS(paste0(data.dir,'/biomass_age_baseline_',filter.type,'.rds'))
  
  end.time = max(bio.age.all$Time)
  start.time = end.time - (365*ref.years)
  
  fishing.scalars = sort(unique(bio.age.all$fishing.scalar))
  fishing.scalars = fishing.scalars[which(fishing.scalars>1)]
    
  bio.age.baseline = bio.age.all %>%
    left_join(spp2guild)%>%
    filter(Time >= start.time & Time <= end.time & Guild == guild.name & fishing.scalar == 1.5)%>%
    select(Time,guild.name,Code,agecl,fishing.scalar,Biomass.baseline)%>%
    mutate(fishing.scalar = 'baseline')%>%
    rename(Biomass = 'Biomass.baseline')
  
  bio.age.all =bio.age.all %>%
    left_join(spp2guild)%>%
    filter(Time >= start.time & Time <= end.time & Guild == guild.name & fishing.scalar > 1) %>%
    mutate(fishing.scalar = as.character(fishing.scalar))%>%
    bind_rows(bio.age.baseline)%>%
    group_by(guild.name,fishing.scalar,Code,agecl)%>%
    summarise(Biomass = mean(Biomass,na.rm=T))
  
  
  bio.age.all$fishing.scalar = factor(bio.age.all$fishing.scalar, levels = c('baseline',fishing.scalars))
    
  guild.names = sort(unique(bio.age.all$guild.name))
  spp.names = sort(unique(bio.age.all$Code))
  
  pdf(file = paste0(fig.dir,'/age_distributionz_species_',filter.type,'.pdf'),onefile = T)
  for(i in 1:length(spp.names)){
    
    bio.age.spp = bio.age.all %>% 
      filter(Code == spp.names[i])%>%
      group_by(fishing.scalar,Code)%>%
      mutate(Biomass.tot = sum(Biomass,na.rm=T))%>%
      mutate(Biomass.prop = Biomass/Biomass.tot)%>%
      left_join(fgs)
    
    p = ggplot(data = bio.age.spp, aes(x=factor(fishing.scalar),y=agecl))+
      geom_point(pch = 21,aes(size = Biomass.prop,fill = Biomass.prop))+
      scale_size(range = c(1,15),name = '')+
      ggtitle(spp.names[i])+
      xlab('Fishing Scalar')+
      ylab('Age class')+
      theme_bw()+
      theme(panel.grid=element_blank(),
            plot.title = element_text(hjust= 0.5),
            legend.position = 'bottom')
      
    grid.arrange(p)
  }
  dev.off()
  
  pdf(file = paste0(fig.dir,'/age_distributionz_guild_',filter.type,'.pdf'),onefile = T)
  for(i in 1:length(guild.names)){
    
    bio.age.guild = bio.age.all %>% 
      ungroup()%>%
      filter(guild.name == guild.names[i])%>%
      group_by(fishing.scalar,guild.name,agecl)%>%
      summarise(Biomass = sum(Biomass,an.rm=T))%>%
      group_by(fishing.scalar,guild.name)%>%
      mutate(Biomass.tot = sum(Biomass,na.rm=T))%>%
      mutate(Biomass.prop = Biomass/Biomass.tot)
      
    
    p = ggplot(data = bio.age.guild, aes(x=factor(fishing.scalar),y=agecl))+
      geom_point(pch = 21,aes(size = Biomass.prop,fill = Biomass.prop))+
      scale_size(range = c(1,15),name = '')+
      ggtitle(guild.names[i])+
      xlab('Fishing Scalar')+
      ylab('Age class')+
      theme_bw()+
      theme(panel.grid=element_blank(),
            plot.title = element_text(hjust= 0.5),
            legend.position = 'bottom')
    
    grid.arrange(p)
  }
  dev.off()
}

# plot_fishing_sensitivity_age_distribution(data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
#                                           fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','age_distribution',''),
#                                           filter.type = 'all',
#                                           ref.years = 20,
#                                           guild.match = here::here('diagnostics','functional_groups_match.csv'),
#                                           fgs.file = here::here('currentVersion','neus_groups.csv')
#                                           )