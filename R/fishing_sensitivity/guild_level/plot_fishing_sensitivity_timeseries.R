#function to run multiple batcher run comparisons

plot_fishing_sensitivity_timeseries = function(run.index.file,
                                               batch.prefix,
                                               data.dir,
                                               fig.dir,
                                               guild.match,
                                               fgs.file
    ){
  
  
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  
  if(!dir.exists(fig.dir)){dir.create(fig.dir)}
  
  #Read in various reference files and functions
  run.index = readRDS(run.index.file)
  
  guild.names = sort(unique(run.index$guild.names))
  
  fgs = read.csv(fgs.file,as.is =T)
  
  spp2guild = read.csv(guild.match,as.is = T)%>%
    select(Code,Guild)%>%
    filter(Guild %in% guild.names & Code %in% fgs$Code)
  
  #Read in biomass and catch data and filter by guilds being directly manupulated
  bio.all = readRDS(paste0(data.dir,'biomass_all.rds'))%>%
    left_join(spp2guild)%>%
    filter(guild.name == Guild)%>%
    left_join(select(fgs,Code,LongName))
  
  catch.all = readRDS(paste0(data.dir,'catch_all.rds'))%>%
    left_join(spp2guild)%>%
    filter(guild.name == Guild)%>%
    left_join(select(fgs,Code,LongName))
  
  #Loop through guild names and plot timeseries comparisons
  plot.cols = viridisLite::viridis(length(unique(bio.all$fishing.scalar)))
  
  for(i in 1:length(guild.names)){
    
    bio.guild = bio.all %>% filter(guild.name == guild.names[i])
    catch.guild = catch.all %>% filter(guild.name == guild.names[i])
    
    guild.species = sort(unique(bio.guild$LongName))
  
    bio.plot.ls = catch.plot.ls = list()
    
    for(j in 1:length(guild.species)){
      
      bio.species = bio.guild %>% filter(LongName == guild.species[j])
      catch.species = catch.guild %>% filter(LongName == guild.species[j])
      
      bio.plot.ls[[j]] =ggplot(bio.species,aes(x=Time/365,y=Biomass,color = factor(fishing.scalar)))+
        geom_line()+
        xlab('year')+
        ylab('Biomass (mT)')+
        ggtitle(guild.species[j])+
        scale_color_manual(values = plot.cols,name = 'Fishing Scalar')+
        guides(color = guide_legend(title.position = 'top',nrow =1,title.hjust =  0.5))+
        theme_bw()+
        theme(panel.grid = element_blank(),
              legend.position = 'bottom',
              plot.title = element_text(hjust = 0.5)
        )
      
      
      catch.plot.ls[[j]] =ggplot(catch.species,aes(x=Time/365,y=log10(Catch),color = factor(fishing.scalar)))+
        geom_line()+
        xlab('year')+
        ylab('log(Catch) (mT)')+
        ggtitle(guild.species[j])+
        scale_color_manual(values = plot.cols,name = 'Fishing Scalar')+
        guides(color = guide_legend(title.position = 'top',nrow =1,title.hjust =  0.5))+
        theme_bw()+
        theme(panel.grid = element_blank(),
              legend.position = 'bottom',
              plot.title = element_text(hjust = 0.5)
        )
    }
    pdf(file = paste0(fig.dir,'/Raw_Biomass_Timeseries_',guild.names[i],'.pdf'),onefile = T)
    for(k in 1:length(bio.plot.ls)){grid.arrange(bio.plot.ls[[k]])}
    dev.off()
    
    pdf(file = paste0(fig.dir,'/Raw_Catch_Timeseries_',guild.names[i],'.pdf'),onefile = T)
    for(k in 1:length(catch.plot.ls)){grid.arrange(catch.plot.ls[[k]])}
    dev.off()
    
  }
  
}

# plot_fishing_sensitivity_timeseries(batch.prefix =  'fishing_sensitivity_extended_constant_2',
#                                     run.index.file = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
#                                     data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
#                                     fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','timeseries_comparisons',''),
#                                     guild.match = here::here('diagnostics','functional_groups_match.csv'),
#                                     fgs.file = here::here('currentVersion','neus_groups.csv'))
