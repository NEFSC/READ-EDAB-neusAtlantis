#Function to plot fishing sensitivity plots relative to baseline run
# fgs.file = here::here('currentVersion','neus_groups.csv')
# data.dir = data.dir = here::here('data','fishing_sensitivity_extended_constant_2','')
# fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','relative_baseline','')
# plot.species = T #aggreagates by species
# plot.guild = T #aggregates by guild and includes all interactions
# plot.guild.match = T #aggregates by guild but excludes indirect effects
# plot.exploitation = T #plots fishing exploitation for each species
# ref.years = 20
# filter.type = 'all'
# guild.match = here::here('diagnostics','functional_groups_match.csv')

plot_fishing_sensitivity_relative_baseline = function(fgs.file,
                                                      guild.match,
                                                      data.dir,
                                                      fig.dir,
                                                      plot.species = T,
                                                      plot.guild = T,
                                                      plot.guild.match = T,
                                                      plot.exploitation = T,
                                                      ref.years = 20,
                                                      filter.type = 'all'){
  
  library(gridExtra)
  spp2guild = read.csv(guild.match,as.is =T)
  fgs = read.csv(fgs.file,as.is =T)
  
  #Read in combined biomass data from scenarios
  bio.all = readRDS(paste0(data.dir,'biomass_baseline_',filter.type,'.rds'))
  
  #set up time window for comparisons
  end.time = max(bio.all$Time)
  start.time = end.time - (365*ref.years)
  
  #Plot species-level relative biomass to baseline over last $ref.years of runs
  if(plot.species){
    
    pdf(paste0(fig.dir,'/relative_baseline_species_',filter.type,'.pdf'),onefile = T)
    for(i in 1:nrow(fgs)){
      
      bio.spp = bio.all %>%
        filter(Code == fgs$Code[i] & Time >= start.time & Time <= end.time)%>%
        group_by(guild.name,fishing.scalar,Code)%>%
        summarise(Biomass = mean(Biomass,na.rm=T),
                  Biomass.baseline = mean(Biomass.baseline))%>%
        mutate(Biomass.diff = Biomass/Biomass.baseline)%>%
        left_join(fgs)
      
      this.spp = fgs$Code[i]
      this.guild = spp2guild$Guild[which(spp2guild$Code == this.spp)]
      plot.title = paste0(this.spp, ' \n(',this.guild,')')
      
      p = ggplot()+
        geom_line(data = bio.spp, aes(x= fishing.scalar, y = Biomass.diff, color = guild.name ))+
        geom_point(data = bio.spp, aes(x= fishing.scalar, y = Biomass.diff, color = guild.name ))+
        scale_color_manual(name = 'Guild Manipulated',values = RColorBrewer::brewer.pal(6,'Set2'))+
        geom_vline(xintercept = 1,lty = 2,size = 0.25)+
        geom_hline(yintercept = 1,lty = 2,size = 0.25)+
        ylab('Biomass Difference (mT)')+
        xlab('Fishing Scalar')+
        ggtitle(plot.title)+
        theme_bw()+
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5)
        )
      
      grid.arrange(p)
    }
    dev.off()
  }
  
  if(plot.guild){
    
    bio.all.guild = readRDS(paste0(data.dir,'biomass_baseline_guild_',filter.type,'.rds'))
    
    plot.guilds = sort(unique(bio.all.guild$Guild))
    
    pdf(paste0(fig.dir,'/relative_baseline_guild_',filter.type,'.pdf'),onefile = T)
    for(i in 1:length(plot.guilds)){
      
      bio.guild = bio.all.guild %>%
        filter(Guild == plot.guilds[i])
      
      p = ggplot()+
        geom_line(data = bio.guild, aes(x= fishing.scalar, y = Biomass.diff, color = Guild.scenario ))+
        geom_point(data = bio.guild, aes(x= fishing.scalar, y = Biomass.diff, color = Guild.scenario ))+
        scale_color_manual(name = 'Guild Manipulated',values = RColorBrewer::brewer.pal(6,'Set2'))+
        geom_vline(xintercept = 1,lty = 2,size = 0.25)+
        geom_hline(yintercept = 1,lty = 2,size = 0.25)+
        ylab('Biomass Difference (mT)')+
        xlab('Fishing Scalar')+
        ggtitle(plot.guilds[i])+
        theme_bw()+
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5)
        )
      grid.arrange(p)
    }
    dev.off()
  }
  
  if(plot.guild.match){
    
    bio.guild.match = readRDS(paste0(data.dir,'/biomass_baseline_guild_match_',filter.type,'.rds'))
   
    ggplot(bio.guild.match, aes(x=fishing.scalar,y=Biomass.diff,color = Guild))+
      geom_line()+
      geom_hline(yintercept = 0)+
      xlab('Fishing Scalar')+
      ylab('Biomass Difference (mT)')+
      theme_bw()+
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom'
      )+
      ggsave(paste0(fig.dir,'/biomass_baseline_guild_match_',filter.type,'.png'),width = 8, height = 6, units = 'in',dpi = 250)
    
  }
  
  if(plot.exploitation){
    
    f.mort.all = readRDS(paste0(data.dir,'/biomass_baseline_mortality_',filter.type,'.rds'))%>%
      left_join(dplyr::select(fgs,Code,LongName))%>%
      filter(Catch>0 & in.guild ==T)
      
    
    ggplot(f.mort.all,aes(x=fishing.scalar,y=F.mort))+
      geom_line()+
      geom_point()+
      facet_wrap(~LongName,scale = 'free_y')+       
      geom_vline(xintercept = 1,lty = 2,size = 0.25)+
      ylab('Fishing Mortality')+
      xlab('Fishing Scalar')+
      theme_bw()+
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )+
      ggsave(paste0(fig.dir,'/fishing_explotation_species_',filter.type,'.png'),width= 14, height = 12, units = 'in',dpi = 250)

  }
  
  
  
}

