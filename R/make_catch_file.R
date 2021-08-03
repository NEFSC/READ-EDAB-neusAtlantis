library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
# NOTE: comland_meatwt_deflated.RData NOT TO BE POSTED ON GITHUB DUE TO POTENTIAL CONFIDENTIALITY CONCERNS
# ask Sean for it

##Old Comland Pulls
# load("C:/Users/robert.gamble/Desktop/Atlantis_Catch/comland_meatwt_deflated.RData")
#load('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/comland_meatwt_deflated_stat_areas.RData')
# from https://github.com/NOAA-EDAB/Atlantis-Catch-Files/blob/master/Atlantis_1_5_groups_svspp_nespp3.csv

##New Comland Pull
comland = readRDS('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Catch_Data/comland_livewt_deflated_stat_areas.Rds')

#Read stat areas
load('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/strata_stat_box.Rdata')
stat.neus = stat.neus %>%
  sf::st_drop_geometry()

# spcodes <- readr::read_csv("C:/Users/robert.gamble/Desktop/Atlantis_Catch/Atlantis_1_5_groups_svspp_nespp3.csv")
spcodes = readr::read_csv(here::here('data-raw','Atlantis_1_5_groups_svspp_nespp3.csv'))
spcodes <- filter(spcodes,!is.na(NESPP3))

# Read StockSmart to Comland Conversion Factor
ss.comland.rat = readRDS(here::here('data-raw','stocksmart_comland_ratio.Rds'))

# Conversion factor to appropriate Atlantis units
# CONVFACTOR = 40 / 71.902080 ##Technically incorrect, but accounts for incorrect 0.5 activity scalar
CONVFACTOR = 0.278

# stat.names = unique(stat.neus$Id)
hindcast_catch = comland %>% 
  left_join(stat.neus, by = c('AREA' = 'Id')) %>%
  mutate(SPPLIVMT.scaled = SPPLIVMT * overlap_pct) %>%
  group_by(YEAR,NESPP3) %>%
  summarize(WGT = (sum(SPPLIVMT.scaled,na.rm=T)*CONVFACTOR)) %>%
  inner_join(spcodes,by = 'NESPP3') %>%
  left_join(ss.comland.rat, by = c('Code' = 'Group')) %>%
  mutate(ss.comland.conv = ifelse(is.na(ss.comland.conv),1,ss.comland.conv),
         WGT = WGT * ss.comland.conv) %>%
  group_by(YEAR,Code) %>%
  summarize(grpWGT = sum(WGT,na.rm=T)) %>%
  filter(YEAR >= 1964)

# Species names/catch_ts header
header <- c("MAK","HER","WHK","BLF","WPF","SUF","WIF","WTF","FOU","HAL","PLA","FLA","BFT","TUN","BIL","MPF","BUT","BPF","ANC","GOO","MEN","FDE","COD","SHK","OHK","POL","RHK","BSB","SCU","TYL","RED","OPT","SAL","DRM","STB","TAU","WOL","SDF","FDF","HAD","YTF","DOG","SMO","SSH","DSH","BLS","POR","PSH","WSK","LSK","SK","SB","PIN","REP","RWH","BWH","SWH","TWH","INV","LSQ","ISQ","SCA","QHG","CLA","BFF","BG","LOB","RCB","BMS","NSH","OSH","ZL","BD","MA","MB","SG","BC","ZG","PL","DF","PS","ZM","ZS","PB","BB","BO","DL","DR","DC")

#Fill gaps in timeseries
hindcast_catch_ls = list()
for(i in 1:length(header)){
  dat = filter(hindcast_catch, Code == header[i])
  diff.yr = diff(dat$YEAR)
  if(all(diff.yr==1)){
    hindcast_catch_ls[[i]] = dat
  }else{
    # print(header[i])
    year.seq = dat$YEAR[1]:last(dat$YEAR)
    year.interp = approx(dat$YEAR,dat$grpWGT,xout = year.seq,method = 'linear')

    # plot(grpWGT~YEAR,dat,type='l')
    # lines(year.interp$x,year.interp$y,col=2,type='l')
    
    hindcast_catch_ls[[i]] = data.frame(YEAR = year.interp$x,Code = header[i],grpWGT = year.interp$y)
  }
}
hindcast_catch2 = dplyr::bind_rows(hindcast_catch_ls) %>%
  filter(YEAR >= 1964 & YEAR <=2018)
hindcast_catch2$grpWGT[which(!is.finite(hindcast_catch2$grpWGT))] = 0

x2 = filter(hindcast_catch2, Code == 'PS')
plot(grpWGT~YEAR,x2,type='l')
timesteps <- 55 * 365

# Set up catch tibble for time series file

catch <- tibble(time = 0:timesteps)
catch[,header]=0

year <- 0
for (sp in 2:91) {
  # timestep <- 1
  spp_ts <- filter(hindcast_catch2, Code == header[sp-1])
  if(nrow(spp_ts)==0){
    next()
  }
  start.year = min(spp_ts$YEAR)
  if(start.year == 1964){
    catch[1:(365*nrow(spp_ts)),sp] = rep(spp_ts$grpWGT,each = 365)
  }else{
    start.day = (365*(start.year-1964)+1)
    catch[start.day:(start.day + 365*nrow(spp_ts) - 1),sp] = rep(spp_ts$grpWGT,each = 365)
  }
  # if (nrow(spp_ts) > 0) {
  #   for (i in 1:54) {
  #     year <- 1963 + i
  #     if (length(spp_ts$grpWGT[which(spp_ts$YEAR==year)] > 0)) {
  #       for (day in 1:365) {
  #         catch[timestep,sp] <- spp_ts$grpWGT[which(spp_ts$YEAR == year)]
  #         timestep = timestep + 1
  #       }
  #     } else {
  #       for (day in 1:365) {
  #         timestep = timestep + 1
  #       }
  #     }
  #   }
  # }    
}

#Remove Salmon Catch
catch[,which(colnames(catch)=='SAL')] = 0

#test mean catch by group
sort(colMeans(catch,na.rm=T))

#plot catch forcing
pdf(here::here('currentVersion','CatchFiles','Catch_Forcing.pdf'),width = 12 , height = 12,onefile = T)
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
  

# write.table(catch,"/home/rgamble/Desktop/Atlantis-Catch/catch_ts_all.txt",col.names = F, row.names = F, sep = " ")
write.table(catch,here::here('currentVersion','CatchFiles','total_catch_new.txt'),col.names = F, row.names = F, sep = " ")
    
#Write Catch for spinup period
spin.yr = 15

catch.spinup = catch %>%
  mutate(date = as.Date(as.POSIXct(time*86400, origin = '1964-01-01 00:00:00', tz = 'UTC')),
         year = as.numeric(format(date, format = '%Y')) )

date = as.Date(as.POSIXct(catch$time*86400, origin = '1964-01-01 00:00:00', tz = 'UTC'))
spinup.year = as.numeric(format(date, format = '%Y'))
               
date.spinup = which( spinup.year < 1964+spin.yr)
date.rest = which( spinup.year >= 1964+spin.yr)

spinup.mean = colMeans(catch[date.rest,])
catch2 = as.data.frame(catch)
for(i in 1:length(date.spinup)){
  catch2[i,2:ncol(catch2)] = spinup.mean[2:length(spinup.mean)]
}

write.table(catch2,here::here('currentVersion','CatchFiles','total_catch_new_spinup.txt'),col.names = F, row.names = F, sep = " " )

#Spinup with fixed Herring catch 

#catch in mt yr-1
her.catch = 25000

#Converted to mgN s-1
her.catch2 = her.catch * CONVFACTOR

catch3 = catch
catch3[date.spinup,which(colnames(catch)=='HER')] = her.catch2

write.table(catch3,here::here('currentVersion','CatchFiles','total_catch_new_HERspinup_2.txt'),col.names = F, row.names = F, sep = " " )

