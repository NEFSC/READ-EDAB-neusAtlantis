#Function to plot atlantis' stock-recruit relationship over output

run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_2/"


plot_neus_stock_recruit = function(run.dir){
  
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
  
  #Get SSB/Recrtuis output
  recruits.out = readRDS(paste0(run.dir,'Post_Processed/Data/ssb_recruits.RDS'))%>%
    rename(group = 'species')%>%
    left_join(KWSR)%>%
    left_join(KWRR)%>%
    mutate(ssb = ssb*1E9,
           rec.wgt = KWSR+KWRR,
           rec.mgN = (rec*1000)*rec.wgt)
  
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
           # year = format(date,format = '%Y'))%>%
    
  #Recalculate Spawning
  spawn = nums %>%
    left_join(sn.rn)%>%
    left_join(fgs)%>%
    left_join(FSPB)%>%
    left_join(KSPA)%>%
    left_join(FSP)%>%
    left_join(bh)%>%
    mutate(time = time*365,
           spawn = (((3.65*SN*FSP)-KSPA)-(3.65*SN-(SN+RN)))*number*FSPB,
           spawn = ifelse(spawn<0,0,spawn),
           rec.calc = (alpha*spawn)/(beta+((SN+RN)*number)))%>%
    group_by(species,group,time)%>%
    summarise(spawn = sum(spawn,na.rm=T),
              rec.calc = sum(rec.calc,na.rm=T))%>%
    filter(group %in% group.names$group)
  
  #Recalculate Recruitment
  recruits.check = spawn %>%
    left_join(recruits.out)%>%
    mutate(time = time*365,
           rec.ratio = rec.calc/rec.mgN)%>%
    filter(!is.na(ssb),group == 'BUT')
  
  
  ggplot(recruits.check,aes(x=time,y=rec.ratio))+
    geom_point()+
    facet_wrap(~group,nrow =5,scale = 'free')
  
  #Plot output vs calculated recruits
  rec.long = recruits.check%>%
    ungroup()%>%
    select(group,date,rec,rec.calc)%>%
    reshape2::melt(id.vars = c('date','group'))
  
  ggplot(rec.long,aes(x=date,y=value,color = variable))+
    geom_line()+
    facet_wrap(~group,nrow = 5,scale = 'free_y')+
    ylab('Recruitment (mT)')+
    xlab('')
  
  #Plot B-H
  mean.spawn = spawn %>%
    group_by(group)%>%
    summarise(spawn = mean(spawn,na.rm=T),
              wgt = mean(wgt,na.rm=T))
  
  
  
  
 
  # recruit2 = read.table(paste0(run.dir,'neus_outputYOY.txt'),header = T)
  # names = colnames(recruit2)[2:length(colnames(recruit2))]
  # names = sapply(names,function(x)strsplit(x,'\\.')[[1]][1],USE.NAMES = F)
  # colnames(recruit2)[2:ncol(recruit2)] = names
  # recruit2 = recruit2 %>%
  #   reshape2::melt(id.vars = 'Time')%>%
  #   rename(group = 'variable',rec = 'value')%>%
  #   filter(group %in% group.names$group)%>%
  #   mutate(rec = rec*1E9,
  #          date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
  #          year = format(date,format = '%Y'))%>%
  #   group_by(year,group)%>%
  #   summarise(rec = mean(rec,na.rm=T))
  
  ##test
  # ggplot(recruit2,aes(x=Time,y=rec))+geom_line()+facet_wrap(~group,scale = 'free_y')
  

  # 
  # par(mfrow= c(5,5))
  # 
  # for(i in 1:nrow(group.names)){
  #   
  #   wgt.group = wgt.year %>%
  #     mutate(group= as.character(group))%>%
  #     filter(group == group.names$group[i])
  #   
  #   rec.group = recruit2 %>%
  #     mutate(group= as.character(group))%>%
  #     filter(group == group.names$group[i])%>%
  #     left_join(wgt.group)
  #   
  #   bh.sub =  mean.spawn%>%
  #     left_join(bh)%>%
  #     mutate(group = as.character(group))%>%
  #     filter(group == group.names$group[i])
  #   
  #   # plot(rec~biomass,rec.group,type='p',pch = 16,main = group.names$group[i])
  #   curve((x*bh.sub$alpha)/(x+bh.sub$beta),0,1E15)
  # 
  #   
  #   
  # }
  # 
    

  
    
    
  
}
