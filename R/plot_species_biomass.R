atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/FinalPersist_2/'
age.dat = read.table(paste0(atl.dir,'neus_outputAgeBiomIndx.txt'),header= T, stringsAsFactors = F)

library(ggplot2)

spp.name = 'DRM'

new.dat = age.dat[,c(1,grep(spp.name,colnames(age.dat)))]
colnames(new.dat)[-1] = sapply(colnames(new.dat)[-1],function(x) strsplit(x,paste0(spp.name,'.'))[[1]][2])
new.dat2 = reshape2::melt(new.dat,id.var = 'Time',variable.name = 'Cohort',value.name = 'biomass')

# new.dat2 = dplyr::filter(new.dat2, Cohort %in% c('1','2','3'))
ggplot(new.dat2,aes(x= Time, y = biomass, col = Cohort))+
  geom_line(size = 1.2)

xx = new.dat$`1`-new.dat$`0`
plot(xx,type = 'l')
abline(v = 5)
