run.name = 'Rescale_Mum_2'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name)
consume.thresh = 0.9
plot_prey_prevalence = function(run.dir, run.name,consume.thresh = 0.9){
  
  library(dplyr, warm.conflicts = F)
  options(dplyr.summarise.inform = F)
  library(ggplot2)
  
  diet = readRDS(paste0(run.dir,'/Post_Processed/Data/biomass_consumed.rds'))
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)
  
  groups = fgs$LongName
  
  prey.mat = pred.mat = matrix(rep(0,length(groups)^2),nrow = length(groups), ncol = length(groups))
  row.names(prey.mat) = row.names(pred.mat) = groups
  colnames(prey.mat) = colnames(pred.mat) = groups
  
  mean.pred = diet %>%
    group_by(time,pred,prey)%>%
    summarise(atoutput = sum(atoutput,na.rm=T))%>%
    group_by(pred,prey)%>%
    summarise(atoutput = mean(atoutput,na.rm=T))%>%
    ungroup()

  i=1
  for(i in 1:length(groups)){
    
    
    ##Predator perspective
    #Isolate and rank prey for each predator
    prey.sub = mean.pred %>%
      filter(pred == groups[i])
    
    if(nrow(prey.sub) == 0){
      next()
    }else{
      prey.sub = prey.sub%>%
        arrange(desc(atoutput))%>%
        mutate(rank = 1:n())
    }
    
    #Get total consumed prey
    prey.tot = sum(prey.sub$atoutput)
    
    #Determine which top prey meet the threshold proportion of total consumed
    prey.sub = prey.sub %>%
      mutate(consume.prop = atoutput/prey.tot,
             consume.cum.prop = cumsum(consume.prop),
             consume.thresh = ifelse(consume.cum.prop <= consume.thresh, 1, 0)
             )
    top.prey = prey.sub$prey[1:which(prey.sub$consume.thresh == 0)[1]]
    prey.mat[i,match(top.prey,groups)] = 1
    
    ##Prey Perspective
    #Isolate and rank each prey
    pred.sub = mean.pred %>%
      filter(prey == groups[i])
    
    if(nrow(pred.sub) == 0){
      next()
    }else{
      pred.sub = pred.sub%>%
        arrange(desc(atoutput))%>%
        mutate(rank = 1:n())
    }
    
    #Get total consumed pred
    pred.tot = sum(pred.sub$atoutput)
    
    #Determine which top prey meet the threshold proportion of total consumed
    pred.sub = pred.sub %>%
      mutate(consume.prop = atoutput/pred.tot,
             consume.cum.prop = cumsum(consume.prop),
             consume.thresh = ifelse(consume.cum.prop <= consume.thresh, 1, 0)
      )
    top.pred = pred.sub$pred[1:which(pred.sub$consume.thresh == 0)[1]]
    pred.mat[i,match(top.pred,groups)] = 1
    
  }
  
  prey.mat.long = reshape2::melt(prey.mat)
  colnames(prey.mat.long) = c('pred','prey','top.pred')
  prey.mat.long$top.pred = as.factor(prey.mat.long$top.pred)
  
  ggplot(prey.mat.long,aes(y = pred,x = prey, fill = top.pred))+
    scale_fill_manual(name = '', values = c('white','black'))+
    geom_tile(color = 'black')+
    xlab('Prey')+
    ylab('Predator')+
    guides(fill = 'none')+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1),
          axis.text = element_text(size = 16))+
    ggsave(paste0(run.dir,'/Post_Processed/Top Prey.png'),width = 30, height = 20, units = 'in', dpi = 200)
  
    
  pred.mat.long = reshape2::melt(pred.mat)
  colnames(pred.mat.long) = c('prey','pred','top.prey')
  pred.mat.long$top.prey = as.factor(pred.mat.long$top.prey)
  
  ggplot(pred.mat.long,aes(y = pred,x = prey, fill = top.prey))+
    scale_fill_manual(name = '', values = c('white','black'))+
    geom_tile(color = 'black')+
    xlab('Prey')+
    ylab('Predator')+
    guides(fill = 'none')+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1),
          axis.text = element_text(size = 16))+
    ggsave(paste0(run.dir,'/Post_Processed/Top Pred.png'),width = 30, height = 20, units = 'in', dpi = 200)
  
  
    
  # all.prey = diet %>%
  #   group_by(pred,agecl,prey)%>%
  #   summarise(consume = mean(atoutput,na.rm=T))%>%
  #   group_by(pred,prey)%>%
  #   summarise(consume = sum(consume,na.rm=T))
  # 
  # ggplot(all.prey,aes(y = pred,x = prey, fill = consume))+
  #   scale_fill_gradient(low = 'white',high = 'blue')+
  #   geom_tile(color = 'black')+
  #   xlab('Prey')+
  #   ylab('Predator')+
  #   guides(fill = 'none')+
  #   theme_bw()+
  #   theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1),
  #         axis.text = element_text(size = 16))+
  #   ggsave(paste0(run.dir,'/Post_Processed/All Prey.png'),width = 30, height = 20, units = 'in', dpi = 200)
}
