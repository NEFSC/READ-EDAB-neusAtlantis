
# run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_2/"

make_recruit_figs = function(run.prm,plot.out = F){
  
  library(dplyr)
  library(ggplot2)
  
  #Functions
  source(here::here('R','edit_param_BH.R'))
  source(here::here('R','get_recruit_params.R'))
  source(here::here('R','get_recruit_type.R'))
  
  #Get species name index
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%
    select(Code,LongName)%>%
    rename(species = 'LongName',group = 'Code')
  
  #Get recriutment type
  group.names = get_recruit_type(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
    filter(flagrecruit %in% c(3,11))
  
  #Get Beverton-Holt Parameters
  bh = get_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
    mutate(alpha = as.numeric(as.character(alpha)),
           beta = as.numeric(as.character(beta)))
  
  #Get Other recruitment parameters
  recruit.params = get_recruit_params(bio.prm = here::here('currentVersion','at_biology.prm'))
  KSPA = recruit.params$KSPA
  FSP = recruit.params$FSP
  FSPB = recruit.params$FSPB
  KWSR = recruit.params$KWSR
  KWRR = recruit.params$KWRR
  
  #Get SN by group
  sn.rn = readRDS(paste0(run.dir,'Post_Processed/Data/SN_age_mean.RDS'))%>%
    rename(SN = 'atoutput')%>%
    left_join(readRDS(paste0(run.dir,'Post_Processed/Data/RN_age_mean.RDS')))%>%
    rename(RN = 'atoutput',cohort = 'agecl')
  
  #Get Numbers by group
  nums = readRDS(paste0(run.dir,'Post_Processed/Data/numbers_age.RDS'))%>%
    rename(cohort = 'agecl',number = 'atoutput')
  
  #Get Biomass by group
  wgt = read.table(paste0(run.dir,'neus_outputBiomIndx.txt'),header = T)%>%
    select(Time:INV)%>%
    reshape2::melt(id.vars= 'Time')%>%
    rename(group = 'variable',biomass = 'value',time = "Time")%>%
    mutate(biomass = biomass*1E9)
  
  #Recalculate Spawning
  data.out = nums %>%
    left_join(sn.rn)%>%
    left_join(fgs)%>%
    left_join(FSPB)%>%
    left_join(KSPA)%>%
    left_join(FSP)%>%
    left_join(bh)%>%
    mutate(time = time*365,
           spawn = (((3.65*SN*FSP)-KSPA)-(3.65*SN-(SN+RN)))*number*FSPB,
           spawn = ifelse(spawn<0,0,spawn),
           biomass = (SN+RN)*number,
           SSB = biomass*FSPB)%>%
    group_by(species,group,time)%>%
    summarise(number = sum(number,na.rm=T),
              spawn = sum(spawn,na.rm=T),
              biomass = sum(biomass,na.rm=T),
              SSB = sum(SSB,na.rm=T),
              alpha = mean(alpha),
              beta = mean(beta))%>%
    filter(group %in% group.names$group)%>%
    mutate(recruit = (alpha*spawn)/(beta+biomass),
           date = as.POSIXct(time*86400, origin = '1964-01-01 00:00:00',tz = 'UTC'))
  
  return(data.out)
  
  #Diagnostic Plots
  if(plot.out){
    
    p1 = ggplot(data.out,aes(x=date,y=spawn))+
      geom_line()+
      facet_wrap(~species,scale = 'free_y',nrow = 4)+
      ggtitle('Spawn')+
      xlab('')+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
    
    p2 = ggplot(data.out,aes(x=date,y=SSB))+
      geom_line()+
      facet_wrap(~species,scale = 'free_y',nrow = 4)+
      ggtitle('SSB')+
      xlab('')+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
    
    p3 = ggplot(data.out,aes(x=date,y=spawn/SSB))+
      geom_line()+
      facet_wrap(~species,scale = 'free_y',nrow = 4)+
      ggtitle('Spawn : SSB')+
      xlab('')+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
    
    png(paste0(run.dir,'/Post_Processed/Recruit_Diagnostic.png'),width = 14, height = 14, units = 'in', res = 300)
    gridExtra::grid.arrange(p1, p2, p3)
    dev.off()
    
  }
  
}