#' Function to make recruitment diagnsotics
#' A) Consistency: % years with non-zero recruits
#' B) KSPA check: Is (W_optimal * FSP - KSPA)<=0 ?: Indicates KSPA too high
#' C) Spawn check: Is ((W_optimal * FSP - KSPA) - (W_optimal - W) ) <0? : Indicates poor condition
#' D) Stock-recruit Status: % of years where Biomass < BH_beta: Indicates that BH_beta high/low or stock status
#' 

make_recruit_diagnostics = function(run.dir){
  
  library(dplyr)
  
  source(here::here('R','make_recruit_output.R'))
  source(here::here('R','parse_recruit_log.R'))
  source(here::here('R','get_recruit_type.R'))
  source(here::here('R','edit_param_BH.R'))
  
  group.names = get_recruit_type(here::here('currentVersion','at_biology.prm')) %>%
    filter(flagrecruit ==3)
  
  bh = get_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
    mutate(alpha = as.numeric(as.character(alpha)),
           beta = as.numeric(as.character(beta)))
  
  recruit.log = parse_recruit_log(run.dir,spp = group.names$group)%>%
    filter(box == 1 & layer == 0)%>%
    rename(group = 'code')%>%
    mutate(date = as.POSIXct(time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
          year = format(date,format = '%Y'))%>%
    select(year,group,recruits,spawn)%>%
    group_by(year,group)%>%
    summarise(recruits = sum(recruits,na.rm=T),
              spawn = sum(spawn,na.rm=T))
    
  kspa.check = make_recruit_output(run.dir) %>%
    select(-spawn)%>%
    mutate(time = time * 365,
           kspa.check = ((w.opt-FSP)-KSPA),
           kspa.flag = ifelse(kspa.check<=0,1,0),
           date = as.POSIXct(time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
           year = format(date,format = '%Y'))%>%
    group_by(year,group)%>%
    summarise(biomass = sum(biomass,na.rm=T),
              kspa.flag = sum(kspa.flag,na.rm=T))%>%
    mutate(kspa.flag = ifelse(kspa.flag>0,1,0))
  
  tot.year = length(unique(recruit.log$year))
  
  recruit.check = recruit.log %>%
    left_join(bh)%>%
    left_join(kspa.check)%>%
    mutate(is.present = ifelse(biomass>0,1,0))%>%
    filter(is.present == 1)%>%
    mutate(spawn.check= ifelse(spawn<=0,1,0),
           alpha.check = recruits/alpha,
           beta.check = biomass/beta,
           spawn.biomass = spawn/biomass
           )%>%
    group_by(group)%>%
    summarise(biomass.check = sum(is.present)/tot.year,
              n.years = sum(is.present),
              # kspa.check = sum(kspa.flag)/n.years,
              spawn.check = sum(spawn.check)/n.years,
              alpha.check = mean(alpha.check,na.rm=T),
              beta.check = mean(beta.check,na.rm=T),
              spawn.biomass = mean(spawn.biomass,na.rm=T),
              mean.recruit = mean(recruits,na.rm=T))%>%
    select(-n.years)
           
  return(recruit.check)
}

# make_recruit_diagnostics(run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_RescaleAlphaBeta_4/")


