#Function to plot atlantis' stock-recruit relationship over output

run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_RescaleAlphaBeta_4/"


plot_neus_stock_recruit = function(run.dir){
  
  library(rgl)
  library(dplyr)
  library(ggplot2)
  library(grid)
  
  #Functions
  source(here::here('R','make_recruit_output.R'))
  source(here::here('R','get_recruit_type.R'))
  source(here::here('R','edit_param_BH.R'))
  source(here::here('R','parse_spawn_log.R'))
  source(here::here('R','parse_recruit_log.R'))

  spawn.out = parse_spawn_log(run.dir) %>%
    group_by(Time,Code,Box,Layer)%>%
    summarise(spawn = max(TotSpawn),DEN = sum(DEN))
  
  # spawn.tot = spawn.out %>%
  #   group_by(Time,Code)%>%
  #   summarise(Spawn = sum(spawn),
  #             DEN = sum(DEN))%>%
  #   mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
  #          year = format(date,format = '%Y'))%>%
  #   ungroup()%>%
  #   select(year,Code,Spawn,DEN)
  
  spawn.tot = make_recruit_output(run.dir)%>%
      mutate(date = as.POSIXct(time*365*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
             year = format(date,format = '%Y'))%>%
    group_by(year,group)%>%
    summarise(biomass = mean(biomass,na.rm=T))%>%
      ungroup()%>%
      rename(Code = 'group')%>%
      select(year,Code,biomass)
  
  recruit.out = parse_recruit_log(run.dir) %>%
    # mutate(test = (BHalpha*Larvae)/(BHbeta+totfish))
    group_by(time,code)%>%
    summarise(recruit = mean(recruits),
              spawn = mean(spawn)
              )%>%
    mutate(date = as.POSIXct(time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
           year = format(date,format = '%Y'))%>%
    ungroup()%>%
    rename(Code = 'code')%>%
    select(year,Code,recruit,spawn)

  
  # spawn = make_recruit_calc(run.dir)

  #Get recriutment type
  group.names = get_recruit_type(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
    filter(flagrecruit %in% c(3,11))
  
  #Get Beverton-Holt Parameters
  bh = get_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
    mutate(alpha = as.numeric(as.character(alpha)),
           beta = as.numeric(as.character(beta)))
  
  
  pdf(paste0(run.dir,'Post_Processed/Stock_Recruit_BH.pdf'),width = 18, height = 12,onefile = T)
  for(i in 1:nrow(group.names)){

    spawn.group = spawn.tot %>%
      mutate(Code= as.character(Code))%>%
      filter(Code == group.names$group[i])
    
    recruit.group = recruit.out %>%
      mutate(Code= as.character(Code))%>%
      filter(Code == group.names$group[i])
    
    dat.group = left_join(spawn.group,recruit.group)
   
    bh.group = bh %>% filter(group == group.names$group[i])
    
    alpha.group = bh.group$alpha
    beta.group = bh.group$beta
    spawn.biomass = dat.group %>%
      filter(spawn != 0)%>%
      mutate(spawn.biomass = spawn/biomass)
    spawn.biomass = mean(spawn.biomass$spawn.biomass,na.rm=T)
    biomass.group = mean(dat.group$biomass)
    
    ymax = max(c(alpha.group*spawn.biomass),max(dat.group$recruit,na.rm=T))
    
    vp.Bottom <- grid::viewport(height=unit(0.33, "npc"), width=unit(1, "npc"), 
                               just=c("left","top"), 
                               y=0.33, x=0)
    
    par(mfrow= c(3,1))
    plot(recruit~year,dat.group,type='b',pch = 16, main = group.names$group[i])
    plot(biomass~year,dat.group,type='b',pch = 16)
    p = ggplot(dat.group,aes(x=biomass,y=recruit,col = as.numeric(year)))+
      geom_path(size=2)+
      scale_color_gradientn(colors= rainbow(50))+
      stat_function(fun = function(x) (x*spawn.biomass*alpha.group)/(x+beta.group) )+
      theme_bw()+
      theme(legend.position = 'bottom',
            panel.grid = element_blank())
    print(p,vp = vp.Bottom)
    
    # plot(recruit~biomass,dat.group,type='p',pch = 16,ylim = c(0,ymax),
    #      main = group.names$group[i],xlab = 'Biomass (mgN)',ylab = 'Recruits (mgN)')
    # curve((x*spawn.biomass*alpha.group)/(x+beta.group),0,1E15,add = T)
  }
  dev.off()
  
  

}
