
#####
# Note Jan 2018, nutrients must be forced as tracers NOT as point source/sinks.
# Need to make nc files rather than ts files as doing below.


#####


## COLUMN1.name Time
## COLUMN1.long_name Time
## COLUMN1.units days since 1964-01-01 0:00:00 +10
##
## COLUMN2.name NH3
## COLUMN2.long_name NH3
## COLUMN2.units mg s-1
## COLUMN2.missing_value -999
#
## COLUMN3.name NO3
## COLUMN3.long_name NO3
## COLUMN3.units mg s-1
## COLUMN3.missing_value -999
##
## COLUMN4.name Lab_Det_N
## COLUMN4.long_name Lab_Sed_N
## COLUMN4.units mg s-1
## COLUMN4.missing_value -999
##
## COLUMN5.name Si
## COLUMN5.long_name Silica
## COLUMN5.units mg s-1
## COLUMN5.missing_value -999
##
## COLUMN6.name Det_Si
## COLUMN6.long_name Detrital Silica
## COLUMN6.units mg s-1

# time, NH3, NO3, Lab_Det, Si, Det_Si
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles'
d1="/home/ryan/Git/atneus_RM/tsfiles"
setwd(d1)

bks=read.table('/home/ryan/Git/atneus_RM/tsfiles/banksupwelling_div_1000.ts') #1st mod
bks=read.table('/home/ryan/Git/atneus_RM/tsfiles/banksupwelling.ts') # original file
bks=read.table('/home/ryan/Git/atneus_RM/tsfiles/banksupwelling_div_1000_v2.ts') #final file

idhe=read.table('/home/ryan/Downloads/box34nut.ts')
idt=idhe
idt[,2:6]=idhe[,2:6]/86400
max(idt[,2])

expt=read.table('/home/ryan/AtlRuns/20180316a/export.ts')

summary(bks)
bks$V3=bks$V3*10
bks$V4=bks$V4/10
summary(bks)

#subset to 1 year and add smoothed line
test=bks[1:365,]
test[,2:6]=test[,2:6]*100
summary(test)
# change nutrient concentrations from binary lo/high to fitted profile
for (ii in 2:6){
lo=loess(test[,ii]~test[,1])
lo$fitted[lo$fitted<0]=0.1
# plot(test[,ii]~test[,1], type='p', main=paste('col',ii))
# lines(predict(lo), col='red')
test[,ii]=round(predict(lo),digits=2)
}
test$V1=test$V1*86400 # change time to match Atlantis time
write.table(test, file='banksupwelling_div_1000_v4.ts', col.names= F, row.names= F, sep=' ')

test$V1=test$V1/86400
write.table(test, file='banksupwelling_div_1000_v5.ts', col.names= F, row.names= F, sep=' ')
v5=read.table('/home/ryan/Git/atneus_RM/tsfiles/banksupwelling_div_1000_v5.ts')

bas=read.table('/home/ryan/Git/atneus_RM/tsfiles/basindeep_div_1000.ts')
summary(bas)
bas$V4=bas$V4/100
bas$V2=bas$V2/2
bas$V5=bas$V5/5
bas$V6=bas$V6/10
summary(bas)

write.table(bas, file='basindeep_div_1000_v2.ts', col.names= F, row.names= F, sep=' ')

bost=read.table('/home/ryan/Git/atneus_RM/tsfiles/bostonharbor_div_1000.ts')
summary(bost)
bost$V2=bost$V2/10
bost$V5=bost$V5/50
bost$V6=bost$V6/10
summary(bost)

write.table(bost, file='bostonharbor_div_1000_v2.ts', col.names= F, row.names= F, sep=' ')

### irradiance
irr=read.table('/home/ryan/Git/atneus_RM/tsfiles/solar.ts')
plot(irr$V2~irr$V1, type='l')
plot(irr$V2[1:365]~irr$V1[1:365], type='l')
library(readxl)
new.irr=read_excel('/media/ryan/Samsung USB/shinyRatlantis/request_hc3v5lat39.397_lon-48.158_2005-01-01_2006-01-01_309652420.xlsx')
plot(new.irr$X__3[22:387], type='l')
nirr=data.frame(seq(1:365))
nirr$irr=as.numeric(new.irr$X__3[22:386])
write.table(nirr, file='RMirradiance.ts', col.names= F, row.names= F, sep=' ')
library(zoo)
nz=zoo(nirr)
index(nz)=nz[,1]
nz_approx=na.approx(nz)
nirr$full=nz_approx[,2]
nirr[,2]=NULL
write.table(nirr, file='RMirradiance.ts', col.names= F, row.names= F, sep=' ')

### read in data from Chesapeake Bay Program
# direct data link: http://data.chesapeakebay.net/api.CSV/WaterQuality/WaterQuality/1-1-2013/12-31-2014/6/7/Station/1174,1176,1190/30,35,60,63,65,67,78
# http://data.chesapeakebay.net/api.CSV/WaterQuality/WaterQuality/1-1-1985/12-31-2017/6/7/Station/1337,1338,1190,1189,1188,1174/30,60,63,65,67,78
#  data from this site, select station and data, save as csv: http://data.chesapeakebay.net/WaterQuality
library(lubridate)
wq=read_excel('/home/ryan/Downloads/WaterQualityWaterQualityStation.xlsx') #3 years
# wq=read_excel('/home/ryan/WaterQualityWaterQualityStation(1).xlsx') # long term 1985-2017
wq=read_excel('/home/ryan/WaterQualityWaterQualityStation (1).xlsx') # long term 1985-2017
### choose station
unique(wq$MonitoringStation)
wqSta=wq[which(wq$MonitoringStation=="CB7.4"),]; sta=wqSta$MonitoringStation[1]
wqSta$date=mdy(wqSta$SampleDate)

### choose depth
wqStaZ=wqSta[which(wqSta$Layer=="S"),]; dep='Surface' #surface
wqStaZ=wqSta[which(wqSta$Layer=="B"),]; dep='Bottom' # bottom depth
# unique(wqStaZ$Parameter)
# [1] "DIN"   "DON"   "NH4F"  "NO23F" "NO2F"  "NO3F"  "PO4F" 

### choose nutrient ###
t=wqStaZ[which(wqStaZ$Parameter=="N023F"),]; nut="NO23"
t=wqStaZ[which(wqStaZ$Parameter=="NH4F"),]; nut="NH4" # mg/L
t=wqStaZ[which(wqStaZ$Parameter=="NO3F"),]; nut="NO3" #mg/L

### plot
plot(t$MeasureValue~t$date, type='l', main=paste(sta,dep,nut), xlab='date',ylab='mg/L')

### get monthly means for time seriesi creation, convert from mg/l to mg/m3
t2=aggregate(t$MeasureValue*1000, by=list(month(t$date)), FUN=mean, na.rm=T) # convert from mg/l to mg/m3

# t2$um=t2$x/1000/14*1e6 # convert to micromolar
# plot(t2$x~t2$Group.1, type='b', main=paste(sta,dep,nut), ylab='N mg/l', xlab='month')
plot(t2$x~t2$Group.1, type='b', main=paste(sta,dep,nut), ylab='N mg/m3', xlab='month')
plot(t2$x/1e6/14*1e6~t2$Group.1, type='b', main=paste(sta,dep,nut), ylab='uM N', xlab='month')

# NEUSbox_X area x depth
b1m3=1.2286957937188164e10 * 17.6
b4m3=3.6860108530107193E9 * 20.5
b7m3=1.0361542520342838E10 * 33.8
# plot((t2$x * b1m3) ~t2$Group.1, type='b', main=paste(sta,dep,nut), ylab='N load ', xlab='month')

nuts=t2
colnames(nuts)[2]=paste(dep, nut, 'mg/m3')
# nuts$bx1load=t2$x*b1m3 #/86400
# nuts$bx4load=t2$x*b4m3 #/86400
# nuts$bx7load=t2$x*b7m3 #/86400

# nh4=nuts[,1]
# nh4$'Surface NH4'=t2$x
# nh4$Sbx1load=nuts$bx1load
# nh4$Sbx4load=nuts$bx4load
# nh4$Sbx7load=nuts$bx7load
# nh4$'Bottom NH4'=t2$x
# nh4$Bbx1load=nuts$bx1load
# nh4$Bbx4load=nuts$bx4load
# nh4$Bbx7load=nuts$bx7load

## convert monthly means into smoothed daily loads
Yy=do.call("rbind", replicate(31, nuts[1,2], simplify = FALSE))
dl=c(31,28,31,30,31,30,31,31,30,31,30,31)
for(i in 2:12){
Yx=do.call("rbind", replicate(dl[i], nuts[i,2], simplify = FALSE))
Yy=rbind(Yy, Yx)
}
Yy=data.frame(Yy)
Yy$date=seq(1:365)
lo=loess(Yy[,1]~seq(1:365))
Yy$fitted=lo$fitted
Yy$Sbx1load=lo$fitted *86400 #b1m3
Yy$Sbx4load=lo$fitted *86400 #b4m3
# Yy$Sbx7load=lo$fitted *b7m3

### verify
sta
dep
nut
### assign
CB.NH4surface=Yy 
CB.NH4bottom=Yy # better annual pattern
CB.NO3surface=Yy # better annual pattern
CB.NO3bottom=Yy

CBAY=v5 # copy to modify
CBAY$V2=CB.NH4bottom$Sbx1load
CBAY$V3=CB.NO3surface$Sbx1load
CBAY$V7=10000
write.table(CBAY, file='CBAY2.ts', col.names= F, row.names= F, sep=' ')
CBlo=CBAY
CBlo$V2=CBAY$V2/10 # reduce by 1 order of magnitude from Bay concentrations
CBlo$V3=CBAY$V3/10 # reduce by 1 order of magnitude from Bay concentrations
write.table(CBlo, file='CBAYlow.ts', col.names= F, row.names= F, sep=' ')


HUD=v5 # Hudson River NY based on CBAY nutrients but scaled to box 4
HUD$V2=CB.NH4bottom$Sbx4load
HUD$V3=CB.NO3surface$Sbx4load
write.table(HUD, file='HUDSON.ts', col.names= F, row.names= F, sep=' ')
# lower nuts for NH4 and NO3
HUDlo=HUD
HUDlo$V2=HUD$V2/2000
HUDlo$V3=HUD$V3/2000
write.table(HUDlo, file='HUDSONlow.ts', col.names= F, row.names= F, sep=' ')




plot(NH4surface$Sbx1load~seq(1:365), ylab='', xlab='', type='l', col='red')
par(new=T)
plot(NH4bottom$Sbx1load~seq(1:365), ylab='', xlab='', type='l', col='blue', axes=F)
axis(4)


#### Narragansett Bay Nutirent Data #### (for NEUS box 7)
# load data from URI GSO MERL lab (Candace Oviatt)
# http://www.gso.uri.edu/merl/data.htm

gso=read_excel('/home/ryan/Downloads/T-98 Bay to share.xlsx', skip=2, col_names = T) #
colnames(gso)[1:17]=c('DATE','YR/DAY','T_FLR','T_DCMU','SAL','CHLa','PHAEO','NO3NO2','PO4','SiO2','NH4','NO2','TEMP','t-zoop','zoop biomass',	'pH',	'Comment')
# units for gso 3:17= T_FLR	T_DCMU	psu	mg/L	mg/L	uM	uM	uM	uM	uM	ºC	#/m3	dry wt mg/m3

plot(gso$NO3NO2~gso$DATE, type='l')

### choose nutrient
nuts=aggregate((gso$NH4*14), by=list(month(gso$DATE)), FUN=mean, na.rm=T); nut='NH4' # convert from uM to mg/m3
nuts=aggregate(gso$NO3NO2*14, by=list(month(gso$DATE)), FUN=mean, na.rm=T); nut='NOx' # convert from uM to mg/m3
sta='NBay'
dep='Surface'

## convert monthly means into smoothed daily loads
Yy=do.call("rbind", replicate(31, nuts[1,2], simplify = FALSE))
dl=c(31,28,31,30,31,30,31,31,30,31,30,31)
for(i in 2:12){
  Yx=do.call("rbind", replicate(dl[i], nuts[i,2], simplify = FALSE))
  Yy=rbind(Yy, Yx)
}
Yy=data.frame(Yy)
Yy$date=seq(1:365)
lo=loess(Yy[,1]~seq(1:365))
Yy$fitted=lo$fitted
# Yy$Sbx1load=lo$fitted *b1m3
# Yy$Sbx4load=lo$fitted *b4m3
Yy$Sbx7load=lo$fitted *86400 #b7m3

### assign
NBayNH4=Yy
NBayNOx=Yy

plot(NBayNH4$Sbx7load~NBayNH4$date, type='l')
plot(NBayNOx$Sbx7load~NBayNOx$date, type='l')

NBAY=v5 # copy to modify
NBAY$V2=NBayNH4$Sbx7load
NBAY$V3=NBayNOx$Sbx7load
write.table(NBAY, file='NBAY.ts', col.names= F, row.names= F, sep=' ')
NBlo=NBAY
NBlo$V2=NBAY$V2/10 #reduce concentrations by 1 order of magnitude from Bay
NBlo$V3=NBAY$V3/10 #reduce concentrations by 1 order of magnitude from Bay
write.table(NBlo, file='NBAYlow.ts', col.names= F, row.names= F, sep=' ')

### Delaware Bay data from EPA data warehouse
# https://ofmpub.epa.gov/storpubl/legacy/proc_count_simple
DB=read_excel('/home/ryan/Downloads/delawarebay.xlsx', col_names = T) # not much here...
names=unique(DB$`Parameter Long Name`)
test=DB[which(DB$`Parameter Long Name`==names[4])]
plot(test$`Result Value`*1000~test$`Start Date`, type='p')


### read in finished data files
v5=read.table(paste(d1,'/banksupwelling_div_1000_v5.ts', sep=''))
CBlo=read.table(paste(d1,'/CBAYlow.ts', sep=''))
NBlo=read.table(paste(d1,'/NBAYlow.ts', sep=''))

## repeat 1 year, 3 years total to match hydro time
library(dplyr)
v5.3=bind_rows(replicate(3, v5, simplify = FALSE))
CBlo.3=bind_rows(replicate(3, CBlo, simplify = FALSE))
NBlo.3=bind_rows(replicate(3, NBlo, simplify = FALSE))

summary(v5)
summary(CBlo)
summary(NBlo)

x=apply(CBlo, 2, max)/86400
x=apply(CBlo, 2, mean)/86400
options(scipen=999)
plot(CBlo$V6/86400, type='l')

### fit harmonic to seasonal pattern to replicate 1 year over N years with smooth transitions
## RM 20180409
fitharmonic=function(xx, N){
ssp=spectrum(xx)
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
t=1:365
reslm <- lm(xx ~ sin(2*pi/per*t)+cos(2*pi/per*t))
summary(reslm)
plot(xx~t)
lines(reslm$fitted,col=2)
test=rep(reslm$fitted, 3)
plot(test)
### pass through once more
ssp=spectrum(test)
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
t=1:(365*N)
reslm <- lm(test ~ sin(2*pi/per*t)+cos(2*pi/per*t))
plot(reslm$fitted.values)
lines(test, col='red')
reslm$fitted.values[reslm$fitted.values<0.05]=0.05 #limit to real values
plot(reslm$fitted.values)
lines(test, col='red')
return(reslm$fitted.values)
}

### fit N year harmonic nutrient cycles
CBlo.3$V2=fitharmonic(CBlo$V2,3)
CBlo.3$V3=fitharmonic(CBlo$V3,3)
NBlo.3$V2=fitharmonic(NBlo$V2,3)
NBlo.3$V3=fitharmonic(NBlo$V3,3)

v5.3$V2=fitharmonic(v5$V2, 3)
v5.3$V3=fitharmonic(v5$V3, 3)
v5.3$V4=fitharmonic(v5$V4, 3)
v5.3$V5=fitharmonic(v5$V5, 3)
v5.3$V6=fitharmonic(v5$V6, 3)

CBlo.3[,c('V4', 'V5', 'V6')]=v5.3[,c('V4', 'V5', 'V6')] # copy Lab_det_N, Si, Det_Si to NBAY and CBAY

NBlo.3[,1]=1:1095
CBlo.3[,1]=1:1095
v5.3[,1]=1:1095


write.table(v5.3, file='v5_3yr.ts', col.names= F, row.names= F, sep=' ')
write.table(NBlo.3, file='NBAYlo3yr.ts', col.names= F, row.names= F, sep=' ')
write.table(CBlo.3, file='CBAYlo3yr.ts', col.names= F, row.names= F, sep=' ')

### Now fix irradiance file
irr=read.table(paste(d1,'/RMirradiance.ts', sep=''))
test=bind_rows(replicate(3, irr, simplify = FALSE))
plot(test$V2, type='l')
test2=test
test2$V2=fitharmonic(irr$V2,3)
test2$V1=1:1095
write.table(test2, file='RMirradiance_smooth_3yr.ts', col.names= F, row.names= F, sep=' ')
write.table(test, file='RMirradiance_raw_3yr.ts', col.names= F, row.names= F, sep=' ')



