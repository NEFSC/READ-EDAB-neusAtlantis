#Functions to make box/epu species biomass presence/absence

# Box Gradient ------------------------------------------------------------

plot_biomass_box_season = function(bio.box,
                                   bio.box.invert,
                                   plot.presence=F,
                                   species.list = NULL,
                                   save.fig = T,
                                   fig.dir,
                                   tolerance = 0.1){
  library(dplyr)
  library(ggplot2)
  
  box2epu = read.csv(here::here('Geometry','box2epu.csv'))
  month2season = read.csv(here::here('data-raw','month2season.csv'))
  season2name = data.frame(season = 1:4, season.name = unique(month2season$season.name))
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'))
  
  move.param = read.csv(here::here('data-raw','seasonal_movements.csv'))%>%
    group_by(group,box,season)%>%
    summarise(orig = mean(value,na.rm=T))%>%
    left_join(season2name)%>%
    left_join(fgs,by = c('group' = 'Code'))%>%
    rename(polygon = 'box',species = "LongName")%>%
    ungroup()%>%
    left_join(box2epu,by = c('polygon' = 'box'))%>%
    select(species,polygon,epu,season.name,orig)%>%
    mutate(orig.presence = ifelse(orig == 0,0,1))%>%
    filter(!is.na(epu))
    
  
  bio.all = rbind(bio.box,bio.box.invert)%>%
    mutate(day = time * 365)
  
  bio.all = bio.all %>%
    mutate(date = as.POSIXct(day * 86400, origin = '1964-01-01 00:00:00',tz = 'UTC'),
           month = as.numeric(format(date,format = '%m')))%>%
    left_join(month2season)%>%
    group_by(species,polygon,season.name)%>%
    summarise(atoutput = mean(atoutput,na.rm=T))
  
  if(!is.null(species.list)){
    bio.all =  bio.all %>%
      filter(species %in% species.list)
  }
  
  bio.all = bio.all %>%
    group_by(species,polygon,season.name)%>%
    summarise(atoutput = mean(atoutput,na.rm=T))
    
  bio.max = bio.all %>%
    group_by(species,season.name)%>%
    summarise(atoutput.max = sum(atoutput,na.rm=T))
    
  box.combs = expand.grid(species = unique(bio.all$species),
                          polygon = sort(unique(bio.all$polygon)),
                          season.name = unique(month2season$season.name))
    
  bio.presence = box.combs %>%
    left_join(bio.all)%>%
    left_join(box2epu,by = c('polygon' = 'box'))%>%
    mutate(presence = ifelse(atoutput == 0 | is.na(atoutput),0,1))%>%
    left_join(bio.max)%>%
    mutate(atoutput.scaled = atoutput/atoutput.max,
           atoutput.scaled = ifelse(is.na(atoutput.scaled),0,atoutput.scaled))%>%
    left_join(move.param)%>%
    tidyr::replace_na(list(atoutput = 0))%>%
    mutate(
      presence.match = ifelse(orig.presence == presence,1,0),
      prop.match = ifelse(orig >= (atoutput.scaled-tolerance) & orig <= (atoutput.scaled+tolerance), 1,0))
  
  bio.presence$polygon = factor(bio.presence$polygon,
                                levels = c(1:7,9,8,12:15,10:11,16:22))

  if(plot.presence){
      
    # plot.ls = list()
    plot.name = paste0(fig.dir,'Box_EPU_season_presence.png')
    p =   ggplot(bio.presence,aes(x=polygon,y=season.name,alpha = presence.match,fill = epu))+
      geom_tile(color = 'black')+
      facet_wrap(~species,ncol = 8)+
      xlab('Box')+
      guides(alpha = F)+
      ylab('')+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5))

  }else{
     plot.name = paste0(fig.dir,'Box_EPU_Season_scaled.png')
     p =   ggplot(bio.presence,aes(x=polygon,y=season.name,alpha = atoutput.scaled,fill = epu))+
       geom_tile(color = 'black')+
       facet_wrap(~species,ncol = 8)+
       xlab('Box')+
       guides(alpha = F)+
       ylab('')+
       theme_minimal()+
       theme(panel.grid = element_blank(),
             legend.position = 'bottom',
             plot.title = element_text(hjust = 0.5))

  }
  
  if(save.fig){
    p + theme(axis.text.x = element_text(size = 6))+ ggsave(plot.name,width = 24, height = 10, units = 'in',dpi = 350)
  }else{
    return(p)
  }

}

# Box Time Range ------------------------------------------------------------

plot_biomass_box_range = function(bio.box,
                                     bio.box.invert,
                                     day.min=NA,
                                     day.max=NA,
                                     plot.presence = T,
                                     species.list = NULL,
                                     save.fig = T,
                                     fig.dir){
  
  library(dplyr)
  library(ggplot2)
  
  box2epu = read.csv(here::here('Geometry','box2epu.csv'))
  
  bio.all = rbind(bio.box,bio.box.invert)%>%
    mutate(day = time * 365)
  
  bio.all = bio.all %>%
    filter(day >= day.min & day <= day.max)
  
  if(!is.null(species.list)){
    bio.all =  bio.all %>%
      filter(species %in% species.list)
  }
  
  bio.all = bio.all %>%
    group_by(species,polygon)%>%
    summarise(atoutput = mean(atoutput,na.rm=T))
  
  bio.max = bio.all %>%
    group_by(species)%>%
    summarise(atoutput.max = max(atoutput,na.rm=T))
  
  box.combs = expand.grid(species = unique(bio.all$species),
                          polygon = sort(unique(bio.all$polygon)))
  
  bio.presence = box.combs %>%
    left_join(bio.all)%>%
    left_join(box2epu,by = c('polygon' = 'box'))%>%
    mutate(presence = ifelse(atoutput == 0 | is.na(atoutput),0,1))%>%
    left_join(bio.max)%>%
    mutate(atoutput.scaled = atoutput/atoutput.max,
           atoutput.scaled = ifelse(is.na(atoutput.scaled),0,atoutput.scaled))
  
  bio.presence$polygon = factor(bio.presence$polygon,
                                levels = c(1:7,9,8,12:15,10:11,16:22))
  
  spp.order = bio.presence %>%
    group_by(species)%>%
    summarise(presence.freq = mean(atoutput.scaled))%>%
    arrange(desc(presence.freq))
  
  bio.presence$species = factor(bio.presence$species,
                                levels = rev(spp.order$species))
  
  if(plot.presence){
    p = ggplot(bio.presence,aes(x=polygon,y=species,alpha = presence,fill= epu))+
      geom_tile(color = 'black')+
      guides(alpha = F)+
      xlab('Box')+
      ylab('')+
      scale_fill_manual(name = 'EPU', values = RColorBrewer::brewer.pal(3,'Set2'))+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            legend.position = 'bottom')
    
    plot.name = paste0(fig.dir,'Box_EPU_summary.png')
  }else{
    
    p = ggplot(bio.presence,aes(x=polygon,y=species,alpha = atoutput.scaled,fill= epu))+
      geom_tile(color = 'black')+
      guides(alpha = F)+
      xlab('Box')+
      ylab('')+
      scale_fill_manual(name = 'EPU', values = RColorBrewer::brewer.pal(3,'Set2'))+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            legend.position = 'bottom')
    
    plot.name = paste0(fig.dir,'Box_EPU_summary.png')
  }
  
  if(save.fig){
    p + ggsave(plot.name,width = 12, height = 12, units = 'in')
  }else{
    return(p)
  }
}


# Example -----------------------------------------------------------------

# plot_biomass_box_range(bio.box = readRDS('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ZM_Spatial_Final/Post_Processed/Data/biomass_box.rds'),
#                        bio.box.invert = readRDS('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ZM_Spatial_Final/Post_Processed/Data/biomass_box_invert.rds'),
#                        fig.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ZM_Spatial_Final/Post_Processed/' ,
#                        day.min = 366,
#                        day.max = 730,
#                        # species.list = c('Carnivorous zooplankton','Mesozooplankton','Microzooplankton','Gelatinous zooplankton'),
#                        species.list = NULL,
#                        plot.presence = F,
#                        save.fig = T
#                        
# )
# 
plot_biomass_box_season(bio.box = readRDS('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ZM_Spatial_Final/Post_Processed/Data/biomass_box.rds'),
                       bio.box.invert = readRDS('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ZM_Spatial_Final/Post_Processed/Data/biomass_box_invert.rds'),
                       fig.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ZM_Spatial_Final/Post_Processed/' ,
                       # species.list = c('Carnivorous zooplankton','Mesozooplankton','Microzooplankton','Gelatinous zooplankton'),
                       species.list = NULL,
                       plot.presence = T,
                       save.fig = T

)


