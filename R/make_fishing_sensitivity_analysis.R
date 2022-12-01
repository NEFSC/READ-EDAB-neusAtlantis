#Script to:
# (1) Pull all fishing sensitivity biomass output
# (2) Join with dev-branch baseline output
# (3) Calculate difference to baseline
# (4) Plot difference as function of fishing manipulation per spp per guild manipulation

library(dplyr)
library(ggplot2)
library(gridExtra)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>% select(Code, LongName) %>% arrange(LongName)
spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code,Guild)
  

# (1) Pull all fishing sensitivity biomass output
batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_scenarios_1/'

batch.prefix = 'fishing_sensitivity_1'
guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore','Other')
fishing.levels.scalar = c(0.1, 0.25, 0.5, 0.75, 1.1,1.25, 1.33, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 10)
fishing.levels.text = c('0_1','0_25','0_5','0_75','1_1','1_25','1_33','1_5','2','2_5','3','4','5','6','7','10')

scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels.scalar' = fishing.levels.scalar) %>%
  arrange(guild.names)%>%
  left_join(data.frame(fishing.levels.scalar = fishing.levels.scalar,fishing.levels.text = fishing.levels.text))

bio.ls = catch.ls = list()
i=1
for(i in 1:nrow(scenario.combs)){
  
  run.name = paste0(batch.prefix,'_',scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i])  
  
  bio.ls[[i]] = read.table(paste0(batch.dir,run.name,'/neus_outputBiomIndx.txt'),as.is =T,header = T)%>%
    select(Time:DC)%>%
    tidyr::gather(Code,Biomass,-Time)%>%
    mutate(run.name = run.name,
           guild.name = scenario.combs$guild.names[i],
           fishing.scalar = scenario.combs$fishing.levels.scalar[i])%>%
    select(run.name,guild.name,fishing.scalar,Code,Time,Biomass)
  
  catch.ls[[i]] = read.table(paste0(batch.dir,run.name,'/neus_outputCatch.txt'),as.is =T,header = T)%>%
    select(Time:ZG)%>%
    tidyr::gather(Code,Catch,-Time)%>%
    mutate(run.name = run.name,
           guild.name = scenario.combs$guild.names[i],
           fishing.scalar = scenario.combs$fishing.levels.scalar[i])%>%
    select(run.name,guild.name,fishing.scalar,Code,Time,Catch)
  
  print(i)
}

bio.all = bind_rows(bio.ls)
catch.all = bind_rows(catch.ls)

# (2) Join with dev-branch baseline output
base.run = 'Dev_11032022'
base.biomass = read.table(here::here('Atlantis_Runs',base.run,'neus_outputBiomIndx.txt'),as.is  =T, header = T)%>%
  select(Time:DC)%>%
  tidyr::gather(Code,Biomass.baseline,-Time)
base.catch = read.table(here::here('Atlantis_Runs',base.run,'neus_outputCatch.txt'),as.is  =T, header = T)%>%
  select(Time:ZG)%>%
  tidyr::gather(Code,Catch.baseline,-Time)

# (3) Calculate difference to baseline
bio.all = bio.all %>% 
  left_join(base.biomass)%>%
  mutate(Biomass.diff = Biomass - Biomass.baseline)

catch.all = catch.all %>%
  left_join(base.catch)%>%
  mutate(Catch.diff = Catch - Catch.baseline)

saveRDS(bio.all,here::here('data','fishing_scenarios_1_biomass.RDS'))
saveRDS(catch.all,here::here('data','fishing_scenarios_1_catch.RDS'))

bio.all = readRDS(here::here('data','fishing_scenarios_1_biomass.RDS'))
catch.all = readRDS(here::here('data','fishing_scenarios_1_catch.RDS'))

# (4) Plot difference as function of fishing manipulation per spp per guild manipulation
end.time = max(bio.all$Time)
start.time = end.time - (365*20)

pdf(here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_1_relative_difference_by_species.pdf'),onefile = T)
for(i in 1:nrow(fgs)){
  
  bio.spp = bio.all %>%
    filter(Code == fgs$Code[i] & Time >= start.time & Time <= end.time)%>%
    group_by(guild.name,fishing.scalar,Code)%>%
    summarise(Biomass = mean(Biomass,na.rm=T),
              Biomass.baseline = mean(Biomass.baseline))%>%
    mutate(Biomass.diff = Biomass/Biomass.baseline)%>%
    left_join(fgs)
  
  this.spp = fgs$Code[i]
  this.guild = spp2guild$Guild[which(spp2guild$Code == this.spp)]
  plot.title = paste0(this.spp, ' \n(',this.guild,')')
  
  p = ggplot()+
    geom_line(data = bio.spp, aes(x= fishing.scalar, y = Biomass.diff, color = guild.name ))+
    geom_point(data = bio.spp, aes(x= fishing.scalar, y = Biomass.diff, color = guild.name ))+
    scale_color_manual(name = 'Guild Manipulated',values = RColorBrewer::brewer.pal(6,'Set2'))+
    geom_vline(xintercept = 1,lty = 2,size = 0.25)+
    geom_hline(yintercept = 1,lty = 2,size = 0.25)+
    ylab('Biomass Difference (mT)')+
    xlab('Fishing Scalar')+
    ggtitle(plot.title)+
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  grid.arrange(p)
}
dev.off()

# (5) Collapse guilds and plot same figure

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

plot.guilds = sort(unique(bio.all.guild$Guild))

pdf(here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_1_relative_difference_by_guild.pdf'),onefile = T)
for(i in 1:length(plot.guilds)){
  
  bio.guild = bio.all.guild %>%
    filter(Guild == plot.guilds[i])
  
  p = ggplot()+
    geom_line(data = bio.guild, aes(x= fishing.scalar, y = Biomass.diff, color = Guild.scenario ))+
    geom_point(data = bio.guild, aes(x= fishing.scalar, y = Biomass.diff, color = Guild.scenario ))+
    scale_color_manual(name = 'Guild Manipulated',values = RColorBrewer::brewer.pal(6,'Set2'))+
    geom_vline(xintercept = 1,lty = 2,size = 0.25)+
    geom_hline(yintercept = 1,lty = 2,size = 0.25)+
    ylab('Biomass Difference (mT)')+
    xlab('Fishing Scalar')+
    ggtitle(plot.guilds[i])+
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  grid.arrange(p)
}  
dev.off()
  
#Calculate mean fishing mortality 
f.mort.all =bio.all %>%
  left_join(catch.all)%>%
  filter(!is.na(Catch)& Time >= start.time & Time <= end.time)%>%
  group_by(guild.name,fishing.scalar,Code)%>%
  summarise(Biomass = mean(Biomass,na.rm=T),
            Catch = mean(Catch,na.rm=T),
            Biomass.baseline = mean(Biomass.baseline,na.rm=T),
            Catch.baseline = mean(Catch.baseline,na.rm=T))%>%
  mutate(F.mort = Catch/Biomass,
         F.mort.baseline = Catch.baseline/Biomass.baseline,
         F.mort.diff = ifelse(Catch == 0, 0,F.mort/F.mort.baseline)) %>%
  filter(F.mort != 0)%>%
  left_join(spp2guild)%>%
  mutate(in.guild = ifelse(Guild == guild.name,T,F))

saveRDS(f.mort.all,here::here('data','fishing_scenarios_1_mort.RDS'))

fished.spp = sort(unique(f.mort.all$Code))

i =1
pdf(here::here('Figures','fishing_sensitivity_1','fishing_sensitivity_1_fishing_mortality_species.pdf'),onefile = T)
for(i in 1:length(fished.spp)){
  
  f.mort.spp = f.mort.all %>% filter(Code == fished.spp[i])
  
  this.spp = fished.spp[i]
  this.guild = spp2guild$Guild[which(spp2guild$Code == this.spp)]
  plot.title = paste0(this.spp, ' \n(',this.guild,')')
  
  p = ggplot(data = f.mort.spp, aes(x= fishing.scalar, y = F.mort,color = guild.name))+
    geom_line()+
    geom_point()+
    scale_color_manual(name = 'Guild Manipulated',values = RColorBrewer::brewer.pal(6,'Set2'))+
    geom_vline(xintercept = 1,lty = 2,size = 0.25)+
    ylab('Fishing Mortality')+
    xlab('Fishing Scalar')+
    ggtitle(fished.spp[i])+
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
    
  grid.arrange(p)
}
dev.off()

f.mort.guild = filter(f.mort.all, in.guild == T)

fished.spp.lm = data.frame(Code = fished.spp, slope = NA)
  
for(i in 1:length(fished.spp)){
  
  f.mort.spp = filter(f.mort.guild, Code == fished.spp[i] & Catch !=0)
  
  if(any(!is.finite(f.mort.spp$F.mort))){
    fished.spp.lm$slope[i] = NA  
  } else{
    f.mort.model =  lm(F.mort~fishing.scalar,data = f.mort.spp)
    
    fished.spp.lm$slope[i] = coef(f.mort.model)[2]
    
  }
  
}
  


