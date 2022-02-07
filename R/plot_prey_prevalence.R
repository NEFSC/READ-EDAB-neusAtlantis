# run.name = 'Pre_Merge_MumC_4_Winners'
# run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name)
plot_prey_prevalence = function(run.dir, run.name){
  
  library(dplyr, warm.conflicts = F)
  options(dplyr.summarise.inform = F)
  library(ggplot2)
  
  diet = readRDS(paste0(run.dir,'/Post_Processed/Data/data_dietcheck.rds'))
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)
  
  groups = fgs$LongName
  
  prey.mat = pred.mat = matrix(rep(0,length(groups)^2),nrow = length(groups), ncol = length(groups))
  row.names(prey.mat) = row.names(pred.mat) = groups
  colnames(prey.mat) = colnames(pred.mat) = groups
  
  i=1
  for(i in 1:length(groups)){
    
    prey.sub = diet %>%
      subset(pred == groups[i])%>%
      group_by(pred,agecl,prey)%>%
      summarise(consume = mean(atoutput,na.rm=T))%>%
      group_by(pred,prey)%>%
      summarise(consume= sum(consume,na.rm=T))%>%
      arrange(desc(consume))
    top.prey = prey.sub$prey[1:3]
    
    prey.mat[i,match(top.prey,groups)] = 1
  
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
