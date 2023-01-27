#Function to plot biomass age age relative to baseline
# data.dir = here::here('data','fishing_sensitivity_extended_constant_2','')
# fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','relative_baseline','')
# filter.type = 'all'
# ref.years = 20
# guild.match = here::here('diagnostics','functional_groups_match.csv')
# fgs.file = fgs.file = here::here('currentVersion','neus_groups.csv')

plot_fishing_sensitivity_relative_baseline_age = function(data.dir,
                                                          fig.dir,
                                                          filter.type = 'all',
                                                          ref.years,
                                                          guild.match,
                                                          fgs.file
                                                          ){
  
  ssp2guild = read.csv(guild.match,as.is= T) %>%
    select(Code,Guild)
  
  fgs = read.csv(fgs.file,as.is = T)%>%select(Code,LongName)
  
  bio.age.all = readRDS(paste0(data.dir,'biomass_age_baseline_',filter.type,'.rds'))
  
  end.time = max(bio.age.all$Time)
  start.time = end.time - (365*ref.years)
  
  bio.age.all = bio.age.all %>%
    filter(Time >= start.time & Time <= end.time & fishing.scalar != 0) %>%
    left_join(ssp2guild)%>%
    filter(Guild == guild.name)%>%
    group_by(guild.name,Code,agecl,fishing.scalar)%>%
    summarise(Biomass = mean(Biomass,na.rm=T),
              Biomass.baseline = mean(Biomass.baseline,na.rm=T))%>%
    mutate(Biomass.diff =Biomass/Biomass.baseline)
  
  gc()

  guild.names= sort(unique(bio.age.all$guild.name))
  
  pdf( paste0(fig.dir,'relative_baseline_age_species_',filter.type,'.pdf'),onefile =T, width =12, height = 10)
  for(i in 1:length(guild.names)){
    
    bio.age.guild = bio.age.all %>%
      filter(guild.name == guild.names[i])%>%
      left_join(fgs)
    
    p = ggplot(data = bio.age.guild,aes(x=fishing.scalar, y=Biomass.diff,color = agecl))+
      geom_line()+
      facet_wrap(~LongName,scales = 'free_y')+
      xlab('Fishing Scalar')+
      ylab('Relative Biomass')+
      theme_bw()
    
    grid.arrange(p)
  }
  dev.off()
  
  bio.age.guild = bio.age.all %>%
      group_by(guild.name,agecl,fishing.scalar)%>%
      summarise(Biomass = sum(Biomass,na.rm=T),
               Biomass.baseline= sum(Biomass.baseline,na.rm=T))%>%
      mutate(Biomass.diff = Biomass/Biomass.baseline)
    
  ggplot(data = bio.age.guild,aes(x=fishing.scalar, y=Biomass.diff,color = agecl))+
      geom_line()+
      facet_wrap(~guild.name,scales = 'free_y')+
      xlab('Fishing Scalar')+
      ylab('Relative Biomass')+
      theme_bw()+
    ggsave(paste0(fig.dir,'relative_baseline_age_guild_',filter.type,'.pdf'),width = 12, height =10,units = 'in',dpi = 300)
  
}