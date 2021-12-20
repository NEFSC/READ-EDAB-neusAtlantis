#Read in spawning data from log.txt
library(dplyr)
library(ggplot2)
source(here::here('R','parse_spawn_log.R'))

spawn1 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/log.txt',spp = 'HAD')
spawn2 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/log.txt',spp = 'HAD')
spawn3 = parse_spawn_log(file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X_UpMumC_UpFSP/log.txt',spp = 'HAD')

#Get Age biomass data


age.dat = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat$total = rowSums(age.dat[,3:11])

age.dat2 = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
age.dat2$total = rowSums(age.dat2[,3:11])

age.dat3 = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X/neus_outputAgeBiomIndx.txt', header = T) %>%
  select(Time, starts_with('HAD'))
# age.dat2$total = rowSums(age.dat2[,3:11])

plot(HAD.0~Time,age.dat3,'l')
##Comparison plot between Age 0 and Total Biomass
plot_spawn_pop = function(file,spp){
  age.dat = read.table(file, header = T) %>%
    select(Time, starts_with(spp))
  age.dat$total = rowSums(age.dat[,3:11])
  
  plot(total~Time,age.dat,'l', ylim = c(0,max(c(age.dat$total,age.dat$HAD.0))),xlim = c(0,400))
  lines(HAD.0~Time,age.dat,col = 2)  
}

age.dat3.long = reshape2::melt(age.dat3,id.var = 'Time')

ggplot(age.dat3.long,aes(x=Time,y=value,fill = variable))+
  geom_bar(position = 'fill',stat = 'identity')+
  coord_flip()

par(mfrow= c(4,1))

plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_Test_1day/neus_outputAgeBiomIndx.txt','HAD')
plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_1day/neus_outputAgeBiomIndx.txt','HAD')
# plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_UpFSP/neus_outputAgeBiomIndx.txt','HAD')
plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_NoMQ/neus_outputAgeBiomIndx.txt','HAD')
plot_spawn_pop('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X_UpMumC_UpFSP/neus_outputAgeBiomIndx.txt','HAD')


plot_spawn_comp = function(files,spp){
  
  dat.ls = list()
  for(i in 1:length(files)){
    age.dat = read.table(files[i], header = T) %>%
      select(Time, starts_with(spp))
    age.dat$total = rowSums(age.dat[,3:11])
    age.dat$ratio = age.dat$HAD.0/age.dat$total
    
    dat.ls[[i]] = select(age.dat,Time,ratio)%>%mutate(file = i)
  }

  df = bind_rows(dat.ls)
  
  ggplot(data = df,aes(x=Time,y=ratio,color = as.factor(file)))+geom_line()+xlim(0,400)
}

plot_spawn_comp(files = c('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_NoMQ/neus_outputAgeBiomIndx.txt',
                          # 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH/neus_outputAgeBiomIndx.txt',
                          'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X/neus_outputAgeBiomIndx.txt',
                          'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X_UpMumC/neus_outputAgeBiomIndx.txt',
                          'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X_UpMumC_UpFSP/neus_outputAgeBiomIndx.txt'
                          ),
                spp = 'HAD')

plot_spawn_comp2 = function(files,spp){
  
  dat.ls = list()
  for(i in 1:length(files)){
    age.dat = read.table(files[i], header = T) %>%
      select(Time, starts_with(spp))%>%
      reshape2::melt(id.vars = 'Time')%>%
      mutate(file = i)
    dat.ls[[i]] =age.dat
  }
  
  df = bind_rows(dat.ls)%>%
    filter(variable %in% c('HAD.0','HAD.1'))
  
  ggplot(data = df,aes(x=Time,y=value,color = as.factor(file)))+
    geom_line()+
    # xlim(0,100)+
    facet_wrap(~variable,nrow = 2,scales = 'free_y')
}

plot_spawn_comp2(files = c('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_NoMQ/neus_outputAgeBiomIndx.txt',
                          # 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH/neus_outputAgeBiomIndx.txt',
                          'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X/neus_outputAgeBiomIndx.txt',
                          'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X_UpMumC/neus_outputAgeBiomIndx.txt',
                          'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HAD_Test_BH_Up10X_UpMumC_UpFSP/neus_outputAgeBiomIndx.txt'
),
spp = 'HAD')

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
  
# x = spawn2 %>%
#   group_by(Box,Layer)%>%
#   summarise(max.spawn = max(TotSpawn))%>%
#   mutate(recruits = max.spawn/114)
# 
# sum(x$recruits)
# 
# x =spawn.summ  %>%
#   group_by(Box,Layer)%>%
#   mutate(cs = cumsum(recalc.TotSpawn))%>%
#   select(Box:TotSpawn,cs,recalc.TotSpawn)
# 
# 
# mean(spawn2$spawn.ratio)

xx = spawn3 %>% 
  mutate(w = SN+RN,
         w0 = 3.65*SN,
         test = (w/w0)*((1/(FSPB*DEN))-1)+(KSPA/w0)+1)%>%
  filter(IndSpawn>0)
mean(xx$test)
