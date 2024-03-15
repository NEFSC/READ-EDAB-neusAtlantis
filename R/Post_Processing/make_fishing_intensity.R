#Script to make diagnostic of hindcast fishing intensity
library(dplyr)

#Read in totalcatch.ts and parse

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)

catch.ts = read.table(here::here('currentVersion','CatchFiles','total_catch.ts'),header = T)

colnames(catch.ts) = c('Time',fgs$Code)

n.years = 20
max.time = max(catch.ts$Time)

catch.end = catch.ts %>%
  filter(Time > (max.time - n.years*365))%>%
  tidyr::pivot_longer(cols = MAK:DC,
                      names_to = 'Code',
                      values_to = 'Catch')%>%
  mutate(Catch.mt = Catch/0.278)

catch.mean = catch.end %>%
  group_by(Code)%>%
  summarise(Catch.mt = mean(Catch.mt,na.rm=T))

ref.run = 'Batcher_10272022a'
init.bio = read.table(here::here('Atlantis_Runs',ref.run,'neus_outputBiomIndx.txt'),header = T)%>%
  filter(Time == 0)%>%
  select(MAK:DC)%>%
  tidyr::pivot_longer(cols = MAK:DC,
                      names_to = 'Code',
                      values_to = 'Biomass')

rank.catch = function(x){
  if(is.na(x)|x==0){
    return('N')
  }else if(x<0.05){
    return('L')
  }else if(x>=0.05 & x<0.25){
    return('M')
  }else {
    return('H')
  }
}

F.mean = catch.mean %>%
  left_join(init.bio)%>%
  mutate(F.mort = Catch.mt/Biomass)

F.mean$fishing.intensity = sapply(F.mean$F.mort,rank.catch,USE.NAMES = F)

write.csv(F.mean,here::here('diagnostics','fishing_intensity.csv'),row.names = F)
