#Function to process fishing sensitivity scenario output to single data object with filters
# base.bio.prm = here::here('currentVersion','at_biology.prm')
# filter.type = 'all' #Options are 'all','fished','fished_BH','BH'
# fgs.file = here::here('currentVersion','neus_groups.csv') #functional groups file
# guild.match = here::here('diagnostics','functional_groups_match.csv') #File with NEUS group to Guild matches
# batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/'
# batch.prefix = 'fishing_sensitivity_extended_constant_2'
# data.dir = here::here('data',batch.prefix,'')
# base.biomass.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputBiomIndx.txt')
# base.biomass.age.file =here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputAgeBiomIndx.txt')
# base.catch.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputCatch.txt')
# run.index.file = paste0(data.dir,'run_index.rds')
# ref.years = 20 #Last N years of run to compare


make_fishing_sensitivity_scenario_post_processing = function(base.bio.prm,
                                                             filter.type,
                                                             fgs.file,
                                                             guild.match,
                                                             batch.dir,
                                                             batch.prefix,
                                                             data.dir,
                                                             base.biomass.file,
                                                             base.biomass.age.file,
                                                             base.catch.file,
                                                             run.index.file,
                                                             ref.years
                                                             ){
  
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  

  
  #Create output directories if not present
  
  if(!dir.exists(data.dir)){dir.create(data.dir) }
  
  #Get functional groups 
  fgs = read.csv(fgs.file,as.is = T) %>% select(Code, LongName) %>% arrange(LongName)
  spp2guild = read.csv(guild.match,as.is = T)%>%
    select(Code,Guild)
  
  #Get recruitment type for filtering
  if(filter.type %in% c('fished','fished_BH','BH')){
    source(here::here('R','get_recruit_type.R'))
    recruit.type = get_recruit_type(base.bio.prm)%>%
      rename(Code = 'group')
    }
  
  #Read in run index scenarios
  scenario.combs = readRDS(run.index.file)
  
  #Read in baseline biomass and biomass.age
  base.biomass = read.table(base.biomass.file,as.is  =T, header = T)%>%
    dplyr::select(Time:DC)%>%
    tidyr::gather(Code,Biomass.baseline,-Time)
  base.biomass.age = read.table(base.biomass.age.file,as.is  =T, header = T)%>%
    tidyr::gather(ID,Biomass.baseline,-Time)%>%
    tidyr::separate(ID,c('Code','agecl'))
  
  #Define reference period
  end.time = max(base.biomass$Time)
  start.time = end.time - (365*ref.years)
  
  #Read in baseline run catch file
  base.catch = read.table(base.catch.file,header =T)
  base.catch = base.catch %>%
    dplyr::select(tidyr::all_of(colnames(base.catch)[-grep('TsAct',colnames(base.catch))]))%>%
    tidyr::gather('Code','Catch.baseline',-Time)

  if(filter.type %in% c('fished','fished_BH')){
    groups.fished = base.catch$Code[which(base.catch$Catch.baseline > 0)]  
  }
  
  #Read in scenario output data from make_fishing_sensitivity_combined_data()
  bio.all = readRDS(paste0(data.dir,'biomass_all.rds'))
  bio.age.all = readRDS(paste0(data.dir,'biomass_age_all.rds'))
  catch.all = readRDS(paste0(data.dir,'catch_all.rds'))
    
  #Optional filters of base biomass and catch based on filter.type
  if(filter.type == 'fished'){
    base.biomass = base.biomass %>% filter(Code %in% groups.fished)
    bio.all = bio.all %>% filter(Code %in% groups.fished)
    
    base.biomass.age = base.biomass.age %>% filter(Code %in% groups.fished)
    bio.age.all = bio.age.all %>% filter(Code %in% groups.fished)
    
    base.catch = base.catch  %>% filter(Code %in% groups.fished)
    catch.all = catch.all %>% filter(Code %in% groups.fished)
    
  }else if(filter.type == 'BH'){
    base.biomass = base.biomass %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3)
    bio.all = bio.all %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3)
    
    base.biomass.age = base.biomass.age %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3)
    bio.age.all = bio.age.all %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3)
    
    base.catch = base.catch %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3)
    catch.all = catch.all %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3)
    
  }else if(filter.type == 'fished_BH'){
    base.biomass = base.biomass %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3 & Code %in% groups.fished)
    bio.all = bio.all %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3 & Code %in% groups.fished)
    
    base.biomass.age = base.biomass.age %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3 & Code %in% groups.fished)
    bio.age.all = bio.age.all %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3 & Code %in% groups.fished)
    
    base.catch = base.catch %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3 & Code %in% groups.fished)
    catch.all = catch.all %>%
      left_join(recruit.type)%>%
      filter(flagrecruit == 3 & Code %in% groups.fished)
  }
  
  #Join baseline data and cleanup
  bio.all = bio.all %>%
    left_join(base.biomass)%>%
    mutate(Biomass.diff = Biomass - Biomass.baseline)
  bio.age.all = bio.age.all %>%
    left_join(base.biomass.age)%>%
    mutate(Biomass.diff = Biomass - Biomass.baseline)
  catch.all = catch.all %>%
    left_join(base.catch)%>%
    mutate(Catch.diff = Catch - Catch.baseline)
  
  rm(base.biomass,base.biomass.age,base.catch)
  
  #Write to file
  saveRDS(bio.all,paste0(data.dir,'biomass_baseline_',filter.type,'.rds'))
  saveRDS(bio.age.all,paste0(data.dir,'biomass_age_baseline_',filter.type,'.rds'))
  saveRDS(catch.all,paste0(data.dir,'catch_baseline_',filter.type,'.rds'))
  
  rm(bio.age.all)
  gc()

  #Aggregate over guilds  
  bio.all.guild = bio.all %>%
    left_join(spp2guild)%>%
    rename(Guild.scenario = 'guild.name')%>%
    filter(Time >= start.time & Time <= end.time)%>%
    group_by(run.name, Guild.scenario,fishing.scalar, Guild,Code)%>%
    summarise(Biomass = mean(Biomass,na.rm=T),
              Biomass.baseline = mean(Biomass.baseline))%>%
    group_by(run.name,Guild.scenario,fishing.scalar,Guild)%>%
    summarise(Biomass = mean(Biomass,na.rm=T),
              Biomass.baseline = mean(Biomass.baseline))%>%
    mutate(Biomass.diff = Biomass/Biomass.baseline)%>%
    filter(!is.na(Guild))
  
  saveRDS(bio.all.guild,paste0(data.dir,'biomass_baseline_guild_',filter.type,'.rds'))
  
  #Filter bio.all.guild#Filter only data points where guild are manipulated directly
  bio.all.guild.match = bio.all.guild %>%
    filter(Guild == Guild.scenario & fishing.scalar >1)
  
  saveRDS(bio.all.guild.match,paste0(data.dir,'biomass_baseline_guild_match_',filter.type,'.rds'))
  
  #calculate effective fishing mortality 
  f.mort.all =bio.all %>%
    left_join(catch.all)%>%
    filter(!is.na(Catch)& Time >= start.time & Time <= end.time)%>%
    group_by(guild.name,fishing.scalar,Code)%>%
    summarise(Biomass = mean(Biomass,na.rm=T),
              Catch = mean(Catch,na.rm=T),
              Biomass.baseline = mean(Biomass.baseline,na.rm=T),
              Catch.baseline = mean(Catch.baseline,na.rm=T))%>%
    mutate(F.mort = ifelse(Biomass>0,Catch/Biomass,NA),
           F.mort.baseline = ifelse(Biomass.baseline>0,Catch.baseline/Biomass.baseline,NA),
           F.mort.diff = ifelse(Catch == 0, NA,F.mort/F.mort.baseline))%>%
    left_join(spp2guild)%>%
    mutate(in.guild = ifelse(Guild == guild.name,T,F))
  
  saveRDS(f.mort.all,paste0(data.dir,'biomass_baseline_mortality_',filter.type,'.rds'))
  
}

# make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
#                                                   filter.type = 'all',
#                                                   fgs.file = here::here('currentVersion','neus_groups.csv'), 
#                                                   guild.match = here::here('diagnostics','functional_groups_match.csv'), 
#                                                   batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
#                                                   batch.prefix = 'fishing_sensitivity_extended_constant_2',
#                                                   data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
#                                                   base.biomass.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputBiomIndx.txt'),
#                                                   base.biomass.age.file =here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputAgeBiomIndx.txt'),
#                                                   base.catch.file = here::here('currentVersion','CatchFiles','total_catch_extended_mean.ts'),
#                                                   run.index.file =  here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
#                                                   ref.years = 20
# )
#                                                   