atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/PersistCheck_2/'
atl.dir2 = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/PersistCheck_3/'
age.dat = read.table(paste0(atl.dir,'neus_outputAgeBiomIndx.txt'),header= T, stringsAsFactors = F)
age.dat2 = read.table(paste0(atl.dir2,'neus_outputAgeBiomIndx.txt'),header= T, stringsAsFactors = F)

library(ggplot2)

spp.name = 'BLF'

new.dat = age.dat[,c(1,grep(spp.name,colnames(age.dat)))]
colnames(new.dat)[-1] = sapply(colnames(new.dat)[-1],function(x) strsplit(x,paste0(spp.name,'.'))[[1]][2])
new.dat = reshape2::melt(new.dat,id.var = 'Time',variable.name = 'Cohort',value.name = 'biomass')

new.dat2 = age.dat2[,c(1,grep(spp.name,colnames(age.dat2)))]
colnames(new.dat2)[-1] = sapply(colnames(new.dat2)[-1],function(x) strsplit(x,paste0(spp.name,'.'))[[1]][2])
new.dat2 = reshape2::melt(new.dat2,id.var = 'Time',variable.name = 'Cohort',value.name = 'biomass')


ggplot(new.dat2,aes(x= Time, y = biomass, col = Cohort))+
  geom_line(size = 1.2)+xlim(0,1500)

recruits = dplyr::filter(new.dat, Cohort %in% as.character(0))
recruits2 = dplyr::filter(new.dat2, Cohort %in% as.character(0))


ggplot(recruits2,aes(x= Time, y = biomass, col = Cohort))+
  geom_line(size = 1.2)

plot(biomass~Time,recruits2,type='l')
lines(biomass~Time,recruits,col = 'red')


new.dat2 = new.dat2 %>% filter(Cohort != 0)
ggplot(new.dat2,aes(x= Time, y = biomass, col = Cohort))+
  geom_line(size = 1.2)
  xlim(0,1500)

xx = new.dat$`1`-new.dat$`0`
plot(xx,type = 'l')
abline(v = 5)
