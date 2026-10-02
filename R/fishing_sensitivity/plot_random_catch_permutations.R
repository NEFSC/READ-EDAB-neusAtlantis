#Plot overlay of all random catch permuations
library(ggplot2)
library(dplyr)

experiment.id = 'random_catch1'

data.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/',experiment.id,'/')
figure.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/figures/',experiment.id,'/')

base.catch.file = here::here('currentVersion','CatchFiles','total_catch_random_catch1.ts')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)

base.catch1 = read.table(base.catch.file)
  

colnames(base.catch1) = c('Time',fgs$Code)

base.catch = base.catch1 %>%
  tidyr::gather('Code','Catch',-Time)
  # mutate(year = floor((Time+1)/365)+1)
  

catch.perm = readRDS(paste0(data.dir,paste0(experiment.id,'_permutations.rds')))

spp.names = fgs$Code

i=which(spp.names == 'BUT')
# for(i in 1:length(spp.names)){

  base.catch.spp = base.catch %>%
    filter(Code == spp.names[i])%>%
    arrange(Time)%>%
    mutate(Time = Time +1)%>%
    filter(Time <= max(base.catch$Time))
    
  
  png(paste0(figure.dir,experiment.id,'_MAK_distribution_NoBaseline.png'))
  plot(0,0,type='n',xlim = c(0, max(base.catch.spp$Time)),ylim = c(0,max(base.catch.spp$Catch)),xlab= 'Year',ylab = 'Catch (mT)',main = 'Butterfish Catch')
  for(j in 1:20){
    new.time = base.catch.spp$Time
    new.catch = base.catch.spp$Catch[catch.perm[[j]]]
    lines(new.time,new.catch)
  }
  lines(base.catch.spp$Time, base.catch.spp$Catch,col =2 , lwd =3)
  dev.off()
  
  png(paste0(figure.dir,experiment.id,'_MAK_distribution_Baseline.png'))
  plot(0,0,type='n',xlim = c(0, max(base.catch.spp$Time)),ylim = c(0,max(base.catch.spp$Catch)),xlab= 'Year',ylab = 'Catch (mT)',main = 'Butterfish Catch')
  # for(j in 1:20){
  #   new.time = base.catch.spp$Time
  #   new.catch = base.catch.spp$Catch[catch.perm[[j]]]
  #   lines(new.time,new.catch)
  # }
  lines(base.catch.spp$Time, base.catch.spp$Catch,col =2 , lwd =3)
  dev.off()
  
# }
