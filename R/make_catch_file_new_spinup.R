#Script to generate Catch forcing with new spinup
#Remove any group with minimal annual catch
#Remove catch for groups with minimal catch during spinup (if genuine)
#Set catch to fixed catch rate during spinup

library(dplyr)
library(ggplot2)

header <- c("MAK","HER","WHK","BLF","WPF","SUF","WIF","WTF","FOU","HAL","PLA","FLA","BFT","TUN","BIL","MPF","BUT","BPF","ANC","GOO","MEN","FDE","COD","SHK","OHK","POL","RHK","BSB","SCU","TYL","RED","OPT","SAL","DRM","STB","TAU","WOL","SDF","FDF","HAD","YTF","DOG","SMO","SSH","DSH","BLS","POR","PSH","WSK","LSK","SK","SB","PIN","REP","RWH","BWH","SWH","TWH","INV","LSQ","ISQ","SCA","QHG","CLA","BFF","BG","LOB","RCB","BMS","NSH","OSH","ZL","BD","MA","MB","SG","BC","ZG","PL","DF","PS","ZM","ZS","PB","BB","BO","DL","DR","DC")

#Read in original catch
catch = read.table(here::here('currentVersion','CatchFiles','total_catch_raw.txt'),header = F)
colnames(catch) = c('time',header)

date = as.Date(as.POSIXct(catch$time*86400, origin = '1964-01-01 00:00:00', tz = 'UTC'))
year = as.numeric(format(date, format = '%Y'))

#Write Catch for spinup period
spin.yr = 20

date.spinup = which( year < 1964+spin.yr)
date.rest = which( year >= 1964+spin.yr)

#Zero out any group who's mean catch is less than 1 ton per day
mean.catch = colMeans(catch)*86400*1E-9*5.7*20
mean.zero = which(mean.catch <= 0.0274)
catch[,mean.zero] = 0


nonzero.groups = header[-(mean.zero-1)]

#Set spinup catch for rest of groups scaled by fixed percentile of catch (catch.p)
catch.p = 0.1
for(i in 1:length(nonzero.groups)){
  ind = which(header == nonzero.groups[i])
  orig.catch = catch[,ind+1]
  orig.catch = orig.catch[which(orig.catch > 0)]
  #Carve-out for HER (needs to be much lower)
  if(nonzero.groups[i] == 'HER'){
    spinup.catch = min(orig.catch) * 0
  }else{
    spinup.catch = quantile(orig.catch,catch.p,names = F)  
  }
  catch[1:length(date.spinup),ind+1] = spinup.catch
}

#Set spinup catch for rest of groups to be scaled by fixed.f
#get initial biomass from any run
# init.biomass = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_12172021/neus_outputBiomIndx.txt',header =T)[1,2:89]
#  select(all_of(nonzero.groups[-which(nonzero.groups == 'RCB')]))
# 
# for(i in 1:length(nonzero.groups)){
#   ind = which(header == nonzero.groups[i])
#   catch[1:length(date.spinup),ind+1] = init.biomass[1,ind] * fixed.f *1E9 / (365*86400*5.7*20)
# }

#Zero out any group in with spinup catch less than 10 mT/yr
spinup.mean = colMeans(catch[date.spinup,])[-1]*86400*1E-9*5.7*20
zero.spinup = which(spinup.mean <= 0.0274)
catch[date.spinup,zero.spinup+1] = 0

for(i in 2:ncol(catch)){
  catch[,i] = round(catch[,i],2)
}

write.table(catch,here::here('currentVersion','CatchFiles','total_catch_new_spinup.txt'),col.names = F, row.names = F, sep = " " )

#plot catch forcing
pdf(here::here('currentVersion','CatchFiles','Catch_Forcing_New_Spinup.pdf'),width = 12 , height = 12,onefile = T)
catch.long = catch %>% reshape2::melt(id.vars = 'time')
p1= ggplot(catch.long,aes(x=time,y=value))+
  geom_line()+
  ylab('mgN/second')+
  facet_wrap(~variable,scale = 'free_y')+
  theme_bw()
gridExtra::grid.arrange(p1)
p2= ggplot(catch.long,aes(x=time,y=value*86400*5.7*20*1E-9))+
  geom_line()+
  ylab('mT/day')+
  facet_wrap(~variable,scale = 'free_y')+
  theme_bw()
gridExtra::grid.arrange(p2)
dev.off() 
