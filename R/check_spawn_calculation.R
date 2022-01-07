#Read in spawning data from log.txt
library(dplyr)
source(here::here('R','parse_spawn_log.R'))

spawn1 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/log.txt',spp = 'HAD')
spawn2 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/log.txt',spp = 'HAD')
spawn3 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day_2/log.txt',spp = 'HAD')

#Get Age biomass data


age.dat = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat$total = rowSums(age.dat[,3:11])

age.dat2 = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat2$total = rowSums(age.dat2[,3:11])

age.dat3 = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day_2/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat2$total = rowSums(age.dat2[,3:11])

##Comparison plot between Age 0 and Total Biomass
plot_spawn_pop = function(file,spp){
  age.dat = read.table(file, header = T) %>%
    select(Time, starts_with(spp))
  age.dat$total = rowSums(age.dat[,3:11])
  
  plot(total~Time,age.dat,'l', ylim = c(0,4E6))
  lines(HAD.0~Time,age.dat,col = 2)  
}

par(mfrow= c(3,1))

plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/neus_outputAgeBiomIndx.txt','HAD')
plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/neus_outputAgeBiomIndx.txt','HAD')
plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day_2/neus_outputAgeBiomIndx.txt','HAD')


#Check Total Spawn calculation
spawn.summ = spawn2 %>%
  filter(Time == 30)%>%
  mutate(TotSpawn.mt = TotSpawn/ 1E9,
         w.opt = (1+2.65)*SN,
         w = SN+RN,
         sp = (((w.opt*FSP)-KSPA)-(w.opt-w))*DEN*FSPB
         )%>%
  mutate(sp = ifelse(sp>0,sp,0),
         spawn.ratio = TotSpawn/sp,
         recalc.TotSpawn = IndSpawn*DEN,
         flag.scale.up = TotSpawn - recalc.TotSpawn)%>%
  select(Box,Layer,Cohort,TotSpawn,recalc.TotSpawn,IndSpawn,DEN,flag.scale.up)%>%
  filter(TotSpawn>0)
  # select(Box,Layer,Cohort,TotSpawn,sp,spawn.ratio)
  
x =spawn.summ  %>%
  group_by(Box,Layer)%>%
  mutate(cs = cumsum(recalc.TotSpawn))%>%
  select(Box:TotSpawn,cs,recalc.TotSpawn)


mean(spawn2$spawn.ratio)
