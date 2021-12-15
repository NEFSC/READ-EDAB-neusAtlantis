#Read in spawning data from log.txt
library(dplyr)
source(here::here('R','parse_spawn_log.R'))

spawn1 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/log.txt',spp = 'HAD')
spawn2 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/log.txt',spp = 'HAD')

#Get Age biomass data

age.dat = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat$total = rowSums(age.dat[,3:11])

age.dat2 = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat2$total = rowSums(age.dat2[,3:11])

##Comparison plot between Age 0 and Total Biomass
plot(total~Time,age.dat,'l', ylim = c(0,4E6))
lines(HAD.0~Time,age.dat,col = 2)

plot(total~Time,age.dat2,'l', ylim = c(0,4E6))
lines(HAD.0~Time,age.dat2,col = 2)

#