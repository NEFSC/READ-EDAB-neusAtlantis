#script to check fishign scenario scalar application
library(dplyr)
start.time = 19724
end.time = start.time + (365*20)

batch.prefix = 'fishing_sensitivity_extended_constant_2'
catch.dir = here::here('currentVersion','CatchFiles',batch.prefix,'')

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)
guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')

fishing.levels = c(0,0.5,1.5,2.5,5,10,15,20,40,60,100)
fishing.levels.text = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100')

scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels' = fishing.levels) %>%
  arrange(guild.names)%>%
  left_join(data.frame(fishing.levels = fishing.levels,fishing.levels.text = fishing.levels.text))

diag.df = data.frame(run.name = paste0(batch.prefix,'_',scenario.combs$guild.names,'_',scenario.combs$fishing.levels.text),
                     guild.name = as.character(scenario.combs$guild.names),
                     scalar = scenario.combs$fishing.levels,
                     present = NA,
                     pass = NA)
group.names = fgs$Code
catch.orig = read.table(here::here('currentVersion','CatchFiles','total_catch_extended_mean.ts'))
colnames(catch.orig) = c('Time',group.names)

mean.catch = catch.orig %>%
  tidyr::gather('Code','catch',-Time)%>%
  filter(Time > start.time & Time <= end.time)%>%
  group_by(Code)%>%
  summarise(mean.catch = mean(catch,na.rm=T))

for(i in 1:nrow(diag.df)){
  
  file.name = paste0(catch.dir,diag.df$run.name[i],'.ts') 
  
  if(file.exists(file.name)){
   
    file.dat = read.table(file.name)
    colnames(file.dat) = c('Time',group.names)
    file.dat = file.dat %>%
      tidyr::gather('Code','catch',-Time)%>%
      filter(Time > start.time & Time <= end.time)%>%
      group_by(Code)%>%
      summarise(end.catch = mean(catch,na.rm=T))%>% 
      left_join(spp2guild)%>%
      left_join(mean.catch)%>%
      dplyr::filter(mean.catch > 0 &Guild == diag.df$guild.name[i])%>%
      mutate(realized.scalar = round(end.catch/mean.catch,2),
             pass = ifelse(realized.scalar == diag.df$scalar[i],T,F)
             )
    diag.df$present[i] = T
    diag.df$pass[i] = all(file.dat$pass)
     
  }else{
    diag.df$present[i] = F
    diag.df$pass[i] = NA
  }
  
}


#Test that force.prm catch file matches the expected one
file.prefix = 'at_force_LINUX_fishing_sensitivity_extended_constant_2_'

out = scenario.combs
out$catch.ts = NA
out$catch.ts.expected = NA
out$catch.ts.match = NA
i=1
for(i in 1:nrow(scenario.combs)){
  
  prm.file = here::here('currentVersion',paste0(file.prefix,scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i],'.prm'))  
  
  prm.lines = readLines(prm.file)
  match.str = grep('Catchts0.data',prm.lines,value = T)
  file.name = strsplit(match.str,paste0('Catchts0.data CatchFiles/',batch.prefix,'/',batch.prefix,'_|.ts'))[[1]][2]
  out$catch.ts[i] = file.name
  out$catch.ts.expected[i] = paste0(scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i])
  out$catch.ts.match[i] = out$catch.ts.expected[i] == out$catch.ts[i]
  
}

#Test that force.prm referenced in log.txt matches the expected one
batch.dir = paste0('/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/',batch.prefix,'/')

log.df = scenario.combs %>% mutate(expected.file = NA,actual.file = NA, file.match = NA)

for(i in 1:nrow(scenario.combs)){
  
  run.name = paste0(batch.prefix,'_',scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i])
  expected.file = paste0(file.prefix,scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i],'.prm')
  log.file = paste0(batch.dir,run.name,'/log.txt')
  log.lines = readLines(log.file)
  run.str = grep('atlantisMerged',log.lines,value = T)
  actual.file = strsplit(run.str,' ')[[1]][10]
  
  log.df$expected.file[i] = expected.file
  log.df$actual.file[i] = actual.file
  log.df$file.match[i] = expected.file == actual.file
}

log.df %>% filter(file.match == F) %>% View()
