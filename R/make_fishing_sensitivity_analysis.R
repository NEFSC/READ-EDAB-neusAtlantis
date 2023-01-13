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
batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/'

batch.prefix = 'fishing_sensitivity_extended_constant_2'
dir.create(here::here('Figures',batch.prefix))

guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')
fishing.levels.scalar = c(0,0.5,1.5,2.5,5,10,15,20,40,60,100)
fishing.levels.text = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100')


scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels.scalar' = fishing.levels.scalar) %>%
  arrange(guild.names)%>%
  left_join(data.frame(fishing.levels.scalar = fishing.levels.scalar,fishing.levels.text = fishing.levels.text))

bio.ls = catch.ls = bio.age.ls = list()
i=1
for(i in 1:nrow(scenario.combs)){
  
  tryCatch(
    {
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
      
      bio.age.ls[[i]] = read.table(paste0(batch.dir,run.name,'/neus_outputAgeBiomIndx.txt'),as.is = T,header =T)%>%
        tidyr::gather(ID,Biomass,-Time)%>%
        tidyr::separate(ID,c('Code','agecl'))%>%
        mutate(run.name = run.name,
               guild.name = scenario.combs$guild.names[i],
               fishing.scalar = scenario.combs$fishing.levels.scalar[i])
    }, error = function(e){}
  )

  print(i)
}

bio.all = bind_rows(bio.ls)
catch.all = bind_rows(catch.ls)
bio.age.all = bind_rows(bio.age.ls)
  
# (2) Join with dev-branch baseline output
base.run = 'Extended_Constant_Catch'
base.biomass = read.table(here::here('Atlantis_Runs',base.run,'neus_outputBiomIndx.txt'),as.is  =T, header = T)%>%
  select(Time:DC)%>%
  tidyr::gather(Code,Biomass.baseline,-Time)
base.biomass.age = read.table(here::here('Atlantis_Runs',base.run,'neus_outputAgeBiomIndx.txt'),as.is  =T, header = T)%>%
  tidyr::gather(ID,Biomass.baseline,-Time)%>%
  tidyr::separate(ID,c('Code','agecl'))
base.catch = read.table(here::here('Atlantis_Runs',base.run,'neus_outputCatch.txt'),as.is  =T, header = T)%>%
  select(Time:ZG)%>%
  tidyr::gather(Code,Catch.baseline,-Time)

# (3) Calculate difference to baseline
bio.all = bio.all %>% 
  left_join(base.biomass)%>%
  mutate(Biomass.diff = Biomass - Biomass.baseline)

bio.age.all = bio.age.all %>%
  left_join(base.biomass.age)%>%
  mutate(Biomass.diff = Biomass - Biomass.baseline)

catch.all = catch.all %>%
  left_join(base.catch)%>%
  mutate(Catch.diff = Catch - Catch.baseline)

#Test on Catch data
catch.scalar.test = catch.all %>%
  left_join(spp2guild)%>%
  filter(guild.name == Guild)%>%
  mutate(realized.scalar = round(Catch/Catch.baseline,2),
         pass = realized.scalar == fishing.scalar)

spp.per.guild = base.catch %>%
  filter(Catch.baseline > 0 & Time == max(Time))%>%
  left_join(spp2guild)%>%
  group_by(Guild)%>%
  summarise(N = n())
  
catch.scalar.summ = catch.scalar.test %>%
  filter(Catch > 0)%>%
  group_by(run.name,guild.name,Code)%>%
  summarise(pass = sum(pass,na.rm=T))%>%
  filter(pass == 0)%>%
  group_by(run.name,guild.name)%>%
  summarise(N.spp.missing = n())%>%
  left_join(spp.per.guild,by = c('guild.name' = 'Guild'))
  
  
sort(unique(catch.scalar.test$run.name))

#Filter a setby removing unfished groups
catch.base = read.table(here::here('currentVersion','CatchFiles','total_catch_extended_mean.ts'))
colnames(catch.base) = c('Time',read.csv(here::here('currentVersion','neus_groups.csv'),stringsAsFactors = F)$Code)
catch.base = catch.base %>%
  tidyr::gather('Code','Catch',-Time)%>%
  group_by(Code)%>%
  summarise(Catch = sum(Catch,na.rm=T))
groups.fished = catch.base$Code[which(catch.base$Catch > 0)]

bio.all.fished = bio.all %>%
  filter(Code %in% groups.fished)
bio.age.all.fished = bio.age.all %>%
  filter(Code %in% groups.fished)
catch.all.fished = catch.all %>%
  filter(Code %in% groups.fished)

saveRDS(bio.all,here::here('data',paste0(batch.prefix,'_biomass.RDS')))
saveRDS(bio.age.all,here::here('data',paste0(batch.prefix,'_biomass_age.RDS')))
saveRDS(catch.all,here::here('data',paste0(batch.prefix,'_catch.RDS')))

saveRDS(bio.all.fished,here::here('data',paste0(batch.prefix,'_biomass_fished.RDS')))
saveRDS(bio.age.all.fished,here::here('data',paste0(batch.prefix,'_biomass_age_fished.RDS')))
saveRDS(catch.all.fished,here::here('data',paste0(batch.prefix,'_catch_fished.RDS')))

bio.all = readRDS(here::here('data',paste0(batch.prefix,'_biomass.RDS')))
catch.all = readRDS(here::here('data',paste0(batch.prefix,'_catch.RDS')))

# (4) Plot difference as function of fishing manipulation per spp per guild manipulation
end.time = max(bio.all$Time)
start.time = end.time - (365*20)

pdf(here::here('Figures',batch.prefix,paste0(batch.prefix,'_relative_difference_by_species.pdf')),onefile = T)
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


bio.all.guild.fished = bio.all.fished %>%
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

pdf(here::here('Figures',batch.prefix,paste0(batch.prefix,'_relative_difference_by_guild.pdf')),onefile = T)
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

pdf(here::here('Figures',batch.prefix,paste0(batch.prefix,'_relative_difference_by_guild_fished.pdf')),onefile = T)
for(i in 1:length(plot.guilds)){
  
  bio.guild = bio.all.guild.fished %>%
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

saveRDS(f.mort.all,here::here('data',paste0(batch.prefix,'_mort.RDS')))

fished.spp = sort(unique(f.mort.all$Code))

i =1
pdf(here::here('Figures',batch.prefix,paste0(batch.prefix,'_fishing_mortality_species.pdf')),onefile = T)
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
  


