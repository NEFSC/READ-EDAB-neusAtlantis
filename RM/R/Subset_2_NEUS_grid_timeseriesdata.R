library(maptools)
library(sp)
library(rgeos)
library(mgcv)
library(raster)
library(ncdf4)
library(rbgm)

# load("I:/1 RM/0 R workspaces/Atlantis - Sean Lucey fish biomass 20170606.RData")
# load("/media/ryan/TOSHIBA EXT/1 RM/0 R workspaces/Atlantis - Sean Lucey fish biomass 20170606.RData") # linux
load("I:/1 RM/0 R workspaces/20170629_HERMESchlorophyll_updated_1998_2016_bottomNO3_ZooBiomass.RData") # toshiba


# box geomery file
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'
setwd(wd2)
# bgm.file <- ("neus30_v15.bgm")
# bgm.file <- ("test_winding_passes.bgm")
bgm.file <- ("neus_tmerc_RM.bgm")

neus <- bgmfile(bgm.file)
box <- boxSpatial(neus)
projection(box) <- "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"
NESarea=sum(box$area[c(1,26:30)])/1e6+264723.9 #km^2
NEUSarea=sum(box$area[2:25])/1e6

# use to integrate depth for Chl and zoo
bgm.z=data.frame(box$botz)
bgm.z$chlZ=ifelse(bgm.z$box.botz >-50, bgm.z$box.botz*-1,50)
bgm.z$area=box$area
numlayers=c(2, 1, 1, 3, 1, 2, 2, 1, 2, 2, 2, 3, 2, 1, 3, 2, 3, 2, 2, 3, 3, 3, 2, 0, 0, 2, 3, 4, 4, 4)
bgm.z$nz=numlayers
bgm.z$vol=bgm.z$area*bgm.z$box.botz*-1 # area * depth (m3)

setwd("/media/ryan/TOSHIBA EXT/1 RM/KINGSTON/transfer/shapefiles/epu_shapes")
setwd("G:/1 RM/KINGSTON/transfer/shapefiles/epu_shapes")
setwd('C:/Users/ryan.morse/Desktop/Iomega Drive Backup 20171012/1 RM/KINGSTON/transfer/shapefiles/epu_shapes')
### Note: dataframe 't' - from Sean Lucey is trawl survey biomass, q-corrected, in tonnes
# atl.biomass atl.discards and atl.landings in KG; t was calculated from atl.biomass
t=atl.biomass/1000
t5=colMeans(t[1:5,], na.rm=T) #5-year initial timepoint mean
tall=colMeans(t, na.rm=T) # all years mean

### plot biomass time series to decide what to include for intial 'virgin' biomass in NEUS v1.5
# wd2="H:/1 RM/10 ATLANTIS transfer"
# fname='NEUS v1.5 Trawl Survey biomass Qcorrected.pdf'
# mypath=file.path(wd2, fname)
# pdf(file=mypath)
# for (i in 2:ncol(t)){
#   plot(t[,i]~t$Year, type='b', main=paste(colnames(t[i]), 'NES biomass (tonnes)'),ylab='', xlab='Year', las=1)
#   abline(h=t5[i], lty=3)
#   abline(h=tall[i], lty=1)
#   legend('topright', lty=c(3,1), legend=c(paste('1964-1969 mean:',round(t5[i], digits=1)), paste('TS mean:',round(tall[i], digits=1))), bty='n')
# }
# dev.off()

### plot biomass time series of non-Q corrected to decide what to include for intial 'virgin' biomass in NEUS v1.5
t.nq=(tot.biomass.kg.not.Qcorrected/1000) # not q corrected, more species present
t.nq$Year=tot.biomass.kg.not.Qcorrected$Year # replace year
t.nq5=colMeans(t.nq[1:5,], na.rm=T)
t.nqall=colMeans(t.nq, na.rm=T)
# wd2="H:/1 RM/10 ATLANTIS transfer"
# fname='NEUS v1.5 Trawl Survey biomass NOT_Q_corrected.pdf'
# mypath=file.path(wd2, fname)
# pdf(file=mypath)
# for (i in 2:ncol(t.nq)){
#   plot(t.nq[,i]~t.nq$Year, type='b', main=paste(colnames(t.nq[i]), 'NES biomass (tonnes)'),ylab='', xlab='Year', las=1)
#   abline(h=t.nq5[i], lty=3)
#   abline(h=t.nqall[i], lty=1)
#   legend('topright', lty=c(3,1), legend=c(paste('1964-1969 mean:',round(t.nq5[i], digits=1)), paste('TS mean:',round(t.nqall[i], digits=1))), bty='n')
# }
# dev.off()
write.table(t.nq, file='ATL_biomass_tonnes_NotQCorr.csv', sep=', ',col.names = T, row.names=F)
write.table(t, file='ATL_biomass_tonnes_Qcorr.csv', sep=', ',col.names = T, row.names=F)
atl.discards.t=atl.discards
atl.discards.t[,2:356]=atl.discards.t[,2:356]*0.001
atl.landings.t=atl.landings
atl.landings.t[,2:480]=atl.landings.t[,2:480]*0.001
write.table(atl.discards.t, file='ATL_discardes_tonnes.csv', sep=', ',col.names = T, row.names=F)
write.table(atl.landings.t, file='ATL_landings_tonnes.csv', sep=', ',col.names = T, row.names=F)

### plot total biomass time series of all Q-corrected species in tonnes
MyCol <- topo.colors(38)
barplot(as.matrix(t(t[,2:39])), names.arg=t$Year, col=MyCol)
legend("top",legend.text,fill=MyCol,ncol=7, bty='n')



wd=getwd()
gbk=readShapeSpatial("EPU_GBKPoly.shp")
gom=readShapeSpatial("EPU_GOMPoly.shp")
mab=readShapeSpatial("EPU_MABPoly.shp")
scs=readShapeSpatial("EPU_SCSPoly.shp")
#combine shapefiles GOM and GBK
gom.gbk.shp=gUnion(gom, gbk, byid=F, id=NULL)
gom.gbk.shp=gUnion(gom, gbk, byid=F, id=NULL)
gom.scs.shp=gUnion(gom, scs, byid=F, id=NULL)
mab.gbk.shp=gUnion(mab, gbk, byid=F, id=NULL)
NES.shp=gUnion(mab.gbk.shp, gom.scs.shp, byid=F, id=NULL)

#Load zooplankton biomass data from Todd O'Brian Copepod site
wd=('H:/1 RM/copepod-2012__biomass-fields/data')
wd=('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions/copepod-2012__biomass-fields/data')
setwd(wd)
files=list.files(wd, pattern=('_cmass-'))
zoovol=lapply(files, function(x) read.csv(x))
nms=gsub("\\.csv", "", files)
names(zoovol)=nms

wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'

# NEUS.ll=readShapeSpatial(file.path(wd2,'NEUS_LL.shp')) # this is in bgm format
# neus.shp=readShapeSpatial(file.path(wd2,'NEUS_Long_Lat.shp')) # this one is in (long, lat format)
neus.shp=readShapeSpatial(file.path(wd2,'Neus_ll_0p01.shp')) # newest, from Bec Gorton DEC 2017
# plot(NEUS.ll, add=T)
plot(neus.shp)
neus.shp@polygons[[2]]@Polygons[[1]]@coords
## read in statistical areas
# statarea=readShapeSpatial('C:/Users/ryan.morse/Downloads/StatAreas-20190314T135243Z-001/StatAreas/Statistical_Areas_2010.shp')




test=gConvexHull(neus.shp) # creates convex hull of basic NEUS shape for exclusion of data
test.mat=as.matrix(test@polygons[[1]]@Polygons[[1]]@coords)
# NES.mat=as.matrix(NES.shp@polygons[[1]]@Polygons[[1]]@coords)

# Find mean zooplankton biomass (mg C m^-3) in each box of the NEUS Atlantis model for each month based
# on displacement volume and conversion found in Moriarty and O'Brien 2013 (data from NOAA Copepod site)
box.zoo.biomass=list()
for (i in 2:13){
  c0=data.frame(zoovol[[i]])
  m4=as.matrix(c0[,c('Longitude','Latitude')]) #lon,lat from ZPD
  c0$epu=NA
  c0$epu[which(in.out(test.mat, m4))]='NEUS' #using large convex hull
  # c0$epu[which(in.out(NES.mat, m4))]='NEUS' #EDAB EPU area
  c0.neus=subset(c0, epu=='NEUS') # limit to only those in NEUS area
  coordinates(c0.neus)=~Longitude+Latitude #transform to Spatialpointsdataframe
  pointsin=over(c0.neus, neus.shp) #find which boxes samples belong to
  c0.neus2 <- data.frame(c0.neus, pointsin)
  boxbio=aggregate(formula=Total.Carbon.Mass..mg.C.m3.~BOX_ID, data=c0.neus2, FUN=mean)
  f=(boxbio$BOX_ID)
  ord=as.numeric(levels(f))[f] # deal with factor numeric conversion
  boxbio$BOX_ID=ord
  boxbio=boxbio[order(boxbio$BOX_ID),]
  box.zoo.biomass[i-1]=list(boxbio)
}

library(data.table)
zoo.bio=rbindlist(box.zoo.biomass, fill=T)

zoo.1=matrix(unlist(box.zoo.biomass[[1]]),ncol=2)
zoo.2=matrix(unlist(box.zoo.biomass[[2]]),ncol=2)
zoo.3=matrix(unlist(box.zoo.biomass[[3]]),ncol=2)
zoo.4=matrix(unlist(box.zoo.biomass[[4]]),ncol=2)
zoo.5=matrix(unlist(box.zoo.biomass[[5]]),ncol=2)
zoo.6=matrix(unlist(box.zoo.biomass[[6]]),ncol=2)
zoo.7=matrix(unlist(box.zoo.biomass[[7]]),ncol=2)
zoo.8=matrix(unlist(box.zoo.biomass[[8]]),ncol=2)
zoo.9=matrix(unlist(box.zoo.biomass[[9]]),ncol=2)
zoo.10=matrix(unlist(box.zoo.biomass[[10]]),ncol=2)
zoo.11=matrix(unlist(box.zoo.biomass[[11]]),ncol=2)
zoo.12=matrix(unlist(box.zoo.biomass[[12]]),ncol=2)

base=data.frame(c(seq(from=0,to=29,by=1)))
colnames(base)='V1'
base=merge(base, zoo.1, by=c('V1'), all=T)
base=merge(base, zoo.2, by=c('V1'), all=T)
base=merge(base, zoo.3, by=c('V1'), all=T)
base=merge(base, zoo.4, by=c('V1'), all=T)
base=merge(base, zoo.5, by=c('V1'), all=T)
base=merge(base, zoo.6, by=c('V1'), all=T)
base=merge(base, zoo.7, by=c('V1'), all=T)
base=merge(base, zoo.8, by=c('V1'), all=T)
base=merge(base, zoo.9, by=c('V1'), all=T)
base=merge(base, zoo.10, by=c('V1'), all=T)
base=merge(base, zoo.11, by=c('V1'), all=T)
base=merge(base, zoo.12, by=c('V1'), all=T)


#rename (units still mg C/m3 zooplankton biomass)
NEUS.zoo.bio=data.frame((base[,2:13])) #transpose)
rownames(NEUS.zoo.bio)=c(seq(from=0, to=29, by=1))
colnames(NEUS.zoo.bio)=c(seq(from=1, to=12, by=1))
NEUS.zoo.bio$`col.mn`=rowMeans(NEUS.zoo.bio, na.rm=T) # this may not work... 
# NEUS.zoo.bio$`col.sum.N`=rowSums(NEUS.zoo.bio, na.rm=T)/5.7 # this may not work...
NEUS.zoo.bio$mean.tonnes=NEUS.zoo.bio$col.mn*bgm.z$vol

### this is the final data set in tonnes to use for calibration (need to partition into ZG, ZL, ZM, ZS) RM 20190312
zoo.bio.tonnes=(NEUS.zoo.bio[,1:12]*bgm.z$vol*1e-9)
tot.zoo.tonnes=colSums(zoo.bio.tonnes[2:23,], na.rm=T)

# barplot(NEUS.zoo.bio$'col.sum.N', main='NEUS Monthly Zooplankton Biomass Sums (Mg N/m3)', xlab='month')
barplot(NEUS.zoo.bio$`col.mn`, na.rm=T, main='NEUS Mean Annual Zooplankton Biomass (Mg C/m3)', xlab='box')
barplot(NEUS.zoo.bio$`col.mn`/5.7, na.rm=T, main='NEUS Mean Annual Zooplankton Biomass (Mg N/m3)', xlab='box')
barplot(NEUS.zoo.bio$`col.mn`*bgm.z$vol*1e-9, na.rm=T, main='NEUS Mean Annual Zooplankton Biomass (tonnes)', xlab='box')



# Break up total zoo N biomass into NEUS groups based on NEUS 1.0 partition of biomass
# ---> use these for each depth layer in initial conditions file (RM 6/2017)
# Because these are mg C/m3 and from oblique tows (top to bottom) -> divide by Redfield, 
barplot((NEUS.zoo.bio$'col.mn'/5.7), main='Mean Annual Zooplankton Biomass (Mg N/m3)', xlab='box')

# use to fill initial condition values -> same values througout water column
FILL.init=function(tt, bgm.z){
tt2=matrix(nrow=30, ncol=5, NA)
for (i in 1:nrow(tt)){
  tt2[i,bgm.z$nz[i]]=tt[i]
  tt2[i,5]=tt[i]
  if (bgm.z$nz[i]-1 > 0){
    tt2[i,bgm.z$nz[i]-1]=tt[i]
  }
  if (bgm.z$nz[i]-2 > 0){
    tt2[i,bgm.z$nz[i]-2]=tt[i]
  }
  if (bgm.z$nz[i]-3 > 0){
    tt2[i,bgm.z$nz[i]-3]=(tt[i])
  }
}
tt2[is.na(tt2)]='_'
return(tt2)
}


tt=round(matrix(NEUS.zoo.bio$'col.mn'/5.7*0.214),digits=3) # ZG mg N m
ZG=FILL.init(tt,bgm.z)
tt=round(matrix(NEUS.zoo.bio$'col.mn'/5.7*0.125),digits=3) # ZL
ZL=FILL.init(tt,bgm.z)
tt=round(matrix(NEUS.zoo.bio$'col.mn'/5.7*0.134),digits=3) # ZM
ZM=FILL.init(tt,bgm.z)
tt=round(matrix(NEUS.zoo.bio$'col.mn'/5.7*0.528),digits=3) # ZS
ZS=FILL.init(tt,bgm.z)

setwd('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions')
write.table(ZG, file='ZG3.csv', sep=', ',col.names = F, row.names=F)
write.table(ZL, file='ZL3.csv', sep=', ',col.names = F, row.names=F)
write.table(ZM, file='ZM3.csv', sep=', ',col.names = F, row.names=F)
write.table(ZS, file='ZS3.csv', sep=', ',col.names = F, row.names=F)

  
### Aggregate mean biomass to box area and depth and sum for total NEUS biomass in tonnes for scaling initial biomass
NEUS.zoo.box.t=((NEUS.zoo.bio[,1:12]))
# NEUS.zoo.box.t[is.na(NEUS.zoo.box.t)]=0 # drop NA's to allow multiplications
# bgm.z$box.botz=bgm.z$box.botz*-1
xxt=bgm.z$box.botz*bgm.z$area*1e-9 ## conversion of mg N to tonnes
NEUS.zoo.box.t[1:30,]=NEUS.zoo.box.t[1:30,]*xxt ## Total Zoo tonnes per box
# NEUS.zoo.box.t[which(NEUS.zoo.box.t==0)]=NA # return NAs

### NOW Convert to kg N
NEUS.zoo.box.N=NEUS.zoo.box.t/5.7
ZG=xx*0.214
ZL=xx*.125
ZM=xx*.135
ZS=xx*.528
plankton=data.frame(ZL, ZM, ZS, ZG)
test=do.call("rbind", replicate(20, plankton, simplify = FALSE)) # repeat yearly pattern N times to match to atlantis
test$Time=seq(from=0, to=30*length((test[,1]))-1, by=30) # set time point for output 30d, 365d, etc...
write.table(test, file='Zooplankton_total_biomass_tonnes_N_20yrs.csv', col.names = T, row.names=F, sep=',')


# NEUS biomass by month - use as virgin biomass for scaling
xx=colSums(NEUS.zoo.box.N, na.rm=T)
barplot(xx, main='NEUS Zooplankton monthly biomass (t)')

# NEUSyearly mean biomass by box - use as in init file
xx=rowSums(NEUS.zoo.box.t, na.rm=T)
barplot(xx, main='NEUS Zooplankton box biomass (t)')


#_______________________________________________________________________________________________________
#load Chl from HERMES merged product - older set used for plankton analysis (see below for update)
# load("G:/1 RM/2 Plankton Spatial Plots/data/1997-2015 - chl/Spring_Chl_1998_2015_shp.dat.rdata")
# plot(shp.dat[[1]])
# lines(test@polygons[[1]]@Polygons[[1]]@coords, col='blue')
# 
# v2=list()
# for(i in 1:dim(shp.dat)[3]){
# v=extract(shp.dat[[i]], neus.shp)
# v1=lapply(v, function(x) mean(x, na.rm=T))
# v2[i]=list(v1)
# # names(v1[i])=seq(0:29)
# }
# m=matrix(unlist(v2), ncol=18, nrow=30) # box 0-29 =rows, years =cols
# 
# v1=extract(shp.dat[[1]], neus.shp)
# v1m=lapply(v1, function(x) mean(x, na.rm=T))
# 
# ### Spring time series trends by box (can use for calibration later)
# year=seq(1998, 2015, 1)
# for (i in 1:nrow(m)){
#   plot(m[i,]~year, type='l', ylim=c(0, 8), col='gray80', ylab='', xlab='')
#   par(new=T)
# }
# plot(colMeans(m, na.rm=T)~year, type='l', col='blue', ylim=c(0,8), lwd=2)
# par(new=F)
#_______________________________________________________________________________________________________
### load monthly mean 25 KM Chl 1997(9-12 only) through 2015(1-4 only) = 212 months (17 years + 8 months) -> (OLD)
#20170610 - added new data from HERMES GlobColour, now all monthly from 1998-2016 included
# load("G:/1 RM/3 gridded data/HERMES merged CHL 25km/monthly_chl_hermes_merged_gsm_25km.rdata")
setwd('G:/1 RM/3 gridded data/HERMES merged CHL 25km/1998_2016')
setwd('C:/Users/ryan.morse/Desktop/Iomega Drive Backup 20171012/1 RM/3 gridded data/HERMES merged CHL 25km/1998_2016')
wd=getwd()
files.AV=list.files(wd, pattern=('_AV-'))
files.GSM=list.files(wd, pattern=('_GSM-'))

files.01GSM=list.files(wd, pattern='0101');files.01GSM=grep(files.01GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.01GSM)
files.01GSM=files.01GSM[c(1:13,17:22)] #21
ii=data.frame(files.01GSM)
files.02GSM=list.files(wd, pattern='0201');files.02GSM=grep(files.02GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.02GSM)
files.02GSM=files.02GSM[c(1:4,6:20)] #drop 20020131-20020228
files.03GSM=list.files(wd, pattern='0301');files.03GSM=grep(files.03GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.03GSM)
files.03GSM=files.03GSM[c(1:5,7:20)] #drop 20030131-20030228
files.04GSM=list.files(wd, pattern='0401');files.04GSM=grep(files.04GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.04GSM)
files.04GSM=files.04GSM[c(1:6,8:20)] #drop 20040131-20040228
files.05GSM=list.files(wd, pattern='0501');files.05GSM=grep(files.05GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.05GSM)
files.05GSM=files.05GSM[c(1:7,9:20)] 
files.06GSM=list.files(wd, pattern='0601');files.06GSM=grep(files.06GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.06GSM)
files.06GSM=files.06GSM[c(1:8,10:20)] 
files.07GSM=list.files(wd, pattern='0701');files.07GSM=grep(files.07GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.07GSM)
files.07GSM=files.07GSM[c(1:9,11:20)] 
files.08GSM=list.files(wd, pattern='0801');files.08GSM=grep(files.08GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.08GSM)
files.08GSM=files.08GSM[c(1:10,12:20)] 
files.09GSM=list.files(wd, pattern='0901');files.09GSM=grep(files.09GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.09GSM)
files.09GSM=files.09GSM[c(1:11,13:20)] 
files.10GSM=list.files(wd, pattern='1001');files.10GSM=grep(files.10GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.10GSM)
files.10GSM=files.10GSM[c(1:12,14:20)] 
files.11GSM=list.files(wd, pattern='1101');files.11GSM=grep(files.11GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.11GSM)
files.11GSM=files.11GSM[c(1:13,15:20)] 
files.12GSM=list.files(wd, pattern='1201');files.12GSM=grep(files.12GSM, pattern='_GSM-', inv=F, value=T)
ii=data.frame(files.12GSM)
files.12GSM=files.12GSM[c(1:14,16:20)] 

### function to rasterize all files in a list, stack raster and return raster stack
### RM 20170609
nc2raster=function(x){
  s=stack()
  for (i in 1:length(x)){
    r <- raster(x[i],  varname = "CHL1_mean")
    s=stack(s, r)
  }
  return(s)
}

# s=stack()
# for (i in 1:length(files.01GSM)){
#   r <- raster(files.01GSM[i],  varname = "CHL1_mean")
#   s=stack(s, r)
# }

### raster stacks of monthly data
r1=nc2raster(files.01GSM)
r2=nc2raster(files.02GSM)
r3=nc2raster(files.03GSM)
r4=nc2raster(files.04GSM)
r5=nc2raster(files.05GSM)
r6=nc2raster(files.06GSM)
r7=nc2raster(files.07GSM)
r8=nc2raster(files.08GSM)
r9=nc2raster(files.09GSM)
r10=nc2raster(files.10GSM)
r11=nc2raster(files.11GSM)
r12=nc2raster(files.12GSM)

### Function to extract data using a shapefile
extractMonths=function(x, shp){
  v2=list()
  for(i in 1:dim(x)[3]){
    v=extract(x[[i]], shp)
    v1=lapply(v, function(xx) mean(xx, na.rm=T))
    v2[i]=list(v1)
  }
  m=matrix(unlist(v2), ncol=dim(x)[3], nrow=30) # box 0-29 =rows, years =cols
  colnames(m)=seq(1998, 2016, by=1)
  rownamse(m)=
  return(m)
}

### returns Mean Chl per box (0-29, rows), by year from 1998-2016 (columns) for month indicated (mg chl a /m3)
Jan.chl=extractMonths(r1, neus.shp)
Feb.chl=extractMonths(r2, neus.shp)
Mar.chl=extractMonths(r3, neus.shp)
Apr.chl=extractMonths(r4, neus.shp)
May.chl=extractMonths(r5, neus.shp)
Jun.chl=extractMonths(r6, neus.shp)
Jul.chl=extractMonths(r7, neus.shp)
Aug.chl=extractMonths(r8, neus.shp)
Sep.chl=extractMonths(r9, neus.shp)
Oct.chl=extractMonths(r10, neus.shp)
Nov.chl=extractMonths(r11, neus.shp)
Dec.chl=extractMonths(r12, neus.shp)

All.chl=nc2raster(files.GSM)
Chl.time=seq(ISOdate(1998,1,15), ISOdate(2016,12,15), "month") # monthly mean values 1998-2016
# dimnames(All.chl[,,3])=Chl.time

NEUSplotChlRaster=function(data, i, maxV){
  rasterX=data[[i]]
  col5=colorRampPalette(c('blue','white','red'))
  max_abolute_value=maxV #what is the maximum absolute value of raster?
  color_levels=20
  br <- seq(0, max_abolute_value, length.out=color_levels+1) 
  rng2=cellStats(rasterX, range)
  rng=c(0, maxV, rng2[2])
  arg=list(at=rng, labels=round(rng,2))
  plot(rasterX, col=col5(length(br) - 1), breaks=br,axis.args=arg, xlim=c(-77,-64),ylim=c(35,45),
       las=1, legend=F, main=Chl.time[[i]])
  map("worldHires", xlim=c(-77,-64),ylim=c(35,45), fill=T,border=0,col="gray", add=T)
  plot(rasterX, legend.only=T, col=col5(length(br) - 1),breaks=br,axis.args=arg, legend.shrink=0.5,
       smallplot=c(0.19,0.21, 0.6,0.80) )
}
NEUSplotChlRaster(All.chl, 10, 4) # data, choose date (1-228), max value

### Mg Chl a /m3 -> Mg Chl a per box (take mg chla/m3 * box area * chl depth (max 50m))
ts1=Jan.chl*bgm.z$chlZ*bgm.z$area
ts2=Feb.chl*bgm.z$chlZ*bgm.z$area
ts3=Mar.chl*bgm.z$chlZ*bgm.z$area
ts4=Apr.chl*bgm.z$chlZ*bgm.z$area
ts5=May.chl*bgm.z$chlZ*bgm.z$area
ts6=Jun.chl*bgm.z$chlZ*bgm.z$area
ts7=Jul.chl*bgm.z$chlZ*bgm.z$area
ts8=Aug.chl*bgm.z$chlZ*bgm.z$area
ts9=Sep.chl*bgm.z$chlZ*bgm.z$area
ts10=Oct.chl*bgm.z$chlZ*bgm.z$area
ts11=Nov.chl*bgm.z$chlZ*bgm.z$area
ts12=Dec.chl*bgm.z$chlZ*bgm.z$area


### Create 3D array of mean chl per box by year, (array is [month, box, year] ie: [1:12,1:30,1:19])
library(abind)
BoxChl.array=abind(Jan.chl, Feb.chl, Mar.chl, Apr.chl, May.chl, Jun.chl, Jul.chl, Aug.chl,
                Sep.chl, Oct.chl, Nov.chl, Dec.chl, along=0)

box.mon=apply(BoxChl.array, c(1,2), mean, na.rm=T) # monthly means over all years for each box
box.yr=apply(BoxChl.array, c(2,3), mean, na.rm=T) # yearly means over all months for each box
box.NES=apply(BoxChl.array, c(1,3), mean, na.rm=T) # NEUS (all boxes) monthly means in each year

library(gplots) # heatmap of time series of  CHL 1998-2016 by month
my_palette <- colorRampPalette(c("turquoise", "yellow", "red"))(n = 299)
heatmap.2(box.NES, density.info="none",  trace="none", col=my_palette, dendrogram='none', Rowv=FALSE, Colv=FALSE)   




#limit to boxes 1-22 (no boundary boxes or islands)
BoxChl.array.NES=abind(Jan.chl[2:23,], Feb.chl[2:23,], Mar.chl[2:23,], Apr.chl[2:23,], May.chl[2:23,], 
                       Jun.chl[2:23,], Jul.chl[2:23,], Aug.chl[2:23,], Sep.chl[2:23,], Oct.chl[2:23,], 
                       Nov.chl[2:23,], Dec.chl[2:23,], along=0)

### SUM all boxes for NEUS total biomass time series: 
# NEUSchlTS=apply(BoxChl.array.NES, c(1,3), sum, na.rm=T) # NEUS (all boxes) monthly means in each year
# test=as.data.frame(NEUSchlTS)
# colnames(test)=seq(from=1998, to=2016, by=1)
# NEUSchlTS.vec=unlist(as.data.frame(NEUSchlTS))
# final.chl.ts=NEUSchlTS.vec*7*20*5.7*1e-9 #(=mg chl * x_CHLN * wetdry * X_CN * convert to tonnes)

### NOW as above but with area and chlorophyll depth applied to each box:
BoxChl.array.NES=abind(ts1[2:23,], ts2[2:23,], ts3[2:23,], ts4[2:23,],ts5[2:23,], 
                       ts6[2:23,], ts7[2:23,], ts8[2:23,], ts9[2:23,], ts10[2:23,], 
                       ts11[2:23,], ts12[2:23,], along=0)
NEUSchlTS=apply(BoxChl.array.NES, c(1,3), sum, na.rm=T) # NEUS (all boxes) monthly means in each year
test=as.data.frame(NEUSchlTS)
colnames(test)=seq(from=1998, to=2016, by=1)
NEUSchlTS.vec=unlist(as.data.frame(NEUSchlTS))
### update 20180718 revised N values of plankton
final.chl.ts=NEUSchlTS.vec /7 *20 *5.7 *1e-9 #(=mg chl / x_CHLN * wetdry * X_CN * convert to tonnes)
## average weight of plankton (in mg N m^-3) is Chl a (mg m^-3) * 1/7 * proportion of total chla 

### partition Chl time series into 3 groups based on initial conditions ratio:
PL.ts=final.chl.ts*0.6
PS.ts=final.chl.ts*0.18
DF.ts=final.chl.ts*0.22
phyto=data.frame(PL.ts, PS.ts, DF.ts)
phyto$time=rep(seq(from=1, to=length(phyto$PL), by=1))
phyto$month=rep(seq(from=1, to=12, by=1))
phyto$days=30*phyto$time
phyto$time=NULL
write.table(phyto, file='phytoplankton_timeseries.csv', col.names=T, row.names = F, sep=',')
plot(final.chl.ts, type='l')
plot(PL.ts~phyto$days, type='l')


### Transpose 3D array into timeseries by box (1:30) for all years
BoxChl.ts=aperm(BoxChl.array, c(2,1,3))
### Transform to 2D time series for calibration to NEUS model:
# set columns to monthly mean Chl by date starting with Jan 1998 thru Dec 2016, rows to boxes 1:30
dim(BoxChl.ts)=c(30,228)
# BoxChl.time=ts(BoxChl.ts[1:30,], start=c(1998,1), end=c(2016,12), frequency = 12)




### Mean of all years for each box
Chl=rowMeans(Jan.chl, na.rm=T)
Chl=rbind(Chl, rowMeans(Feb.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Mar.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Apr.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(May.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Jun.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Jul.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Aug.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Sep.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Oct.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Nov.chl, na.rm=T))
Chl=rbind(Chl, rowMeans(Dec.chl, na.rm=T))
Chl=t(Chl)

# Annual Mean of Chl by box -> partition into phytoplankton based on NEUS 1.0 virgin biomass proportion:
# *** USE THESE FOR INITIAL CONDTIONS FILE for phytoplankton ***
Chl.mn=rowMeans(Chl, na.rm=T)
tt=round(matrix((Chl.mn/7)*0.6),digits=3) # PL
PL=FILL.init(tt,bgm.z)
tt=round(matrix((Chl.mn/7)*0.18),digits=3) # PS
PS=FILL.init(tt,bgm.z)
tt=round(matrix((Chl.mn/7)*0.22),digits=3) # DF
DF=FILL.init(tt,bgm.z)

setwd('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions')
write.table(PL, file='PL.csv', sep=', ',col.names = F, row.names=F)
write.table(PS, file='PS.csv', sep=', ',col.names = F, row.names=F)
write.table(DF, file='DF.csv', sep=', ',col.names = F, row.names=F)


# Convert Chl a (mg Chl/m3) to mg N per Box -> mg chl/m3 * depth (m) * box area (m^2) for box up to 50m * 7 mg N/1 mg chl a
# then convert mg N  biomass (tonnes) (*wetdry*redfield*1g/1000mg) *(1kg/1000g *1 tonne/1000 kg)
Jan.biomassChl=Jan.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Feb.biomassChl=Feb.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Mar.biomassChl=Mar.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Apr.biomassChl=Apr.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
May.biomassChl=May.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Jun.biomassChl=Jun.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Jul.biomassChl=Jul.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Aug.biomassChl=Aug.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Sep.biomassChl=Sep.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Oct.biomassChl=Oct.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Nov.biomassChl=Nov.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6
Dec.biomassChl=Dec.chl*bgm.z$chlZ *bgm.z$area*7*20*5.7/1000*1e-6

# System-wide Biomass of Primary Producers (PL + PS) (satellite-derived Chl) in tonnes of C by month from 1998-2016
Chl.bio.t=colSums(Jan.biomassChl, na.rm=T)
Chl.bio.t=rbind(Chl.bio.t, colSums(Feb.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Mar.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Apr.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(May.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Jun.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Jul.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Aug.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Sep.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Oct.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Nov.biomassChl, na.rm=T))
Chl.bio.t=rbind(Chl.bio.t, colSums(Dec.biomassChl, na.rm=T))

colSums(Chl.bio.t)
mean(colSums(Chl.bio.t)) # This should be the sum of Primary producer biomass


rowSums(Chl.bio.t)
mean(rowSums(Chl.bio.t)) 
barplot(rowSums(Chl.bio.t), main='NEUS Chl biomass (t)')
abline(h=mean(rowSums(Chl.bio.t)), lty=3)

### use to scale diatom + picoplankton N
Chl.N=Chl/7 #Chl converted to N using x_ChlN 7 (Atlantis prm)
rowSums(Chl.N, na.rm=T)
barplot(rowSums(Chl.N, na.rm=T))

### Spring time series trends by box (can use for calibration later)
year=seq(1998, 2016, 1)
for (i in 1:nrow(m)){
  plot(m[i,]~year, type='l', ylim=c(0, 8), col='gray80', ylab='', xlab='')
  par(new=T)
}
plot(colMeans(m, na.rm=T)~year, type='l', col='blue', ylim=c(0,8), lwd=2)
par(new=F)

col5=colorRampPalette(c('blue','white','red'))
mn=0
mx=10
high=10
br <- seq(0, 30, by = 1) 
cl <- col5(length(br) - 1) 

barplot(Jan.chl, ylim=c(0,70), col=cl)
barplot(Feb.chl, ylim=c(0,70), col=cl)
barplot(Mar.chl, ylim=c(0,70), col=cl)
barplot(Apr.chl, ylim=c(0,70), col=cl)
barplot(May.chl, ylim=c(0,70), col=cl)
barplot(Jun.chl, ylim=c(0,70), col=cl)
barplot(Jul.chl, ylim=c(0,70), col=cl)
barplot(Aug.chl, ylim=c(0,70), col=cl)
barplot(Sep.chl, ylim=c(0,70), col=cl)
barplot(Oct.chl, ylim=c(0,70), col=cl)
barplot(Nov.chl, ylim=c(0,70), col=cl)
barplot(Dec.chl, ylim=c(0,70), col=cl)

# boxes on X axis, time on Y
barplot(t(Jan.chl), ylim=c(0,70), col=cl)
barplot(t(Feb.chl), ylim=c(0,70), col=cl)
barplot(t(Mar.chl), ylim=c(0,70), col=cl)
barplot(t(Apr.chl), ylim=c(0,70), col=cl)
barplot(t(May.chl), ylim=c(0,70), col=cl)
barplot(t(Jun.chl), ylim=c(0,70), col=cl)
barplot(t(Jul.chl), ylim=c(0,70), col=cl)
barplot(t(Aug.chl), ylim=c(0,70), col=cl)
barplot(t(Sep.chl), ylim=c(0,70), col=cl)
barplot(t(Oct.chl), ylim=c(0,70), col=cl)
barplot(t(Nov.chl), ylim=c(0,70), col=cl)
barplot(t(Dec.chl), ylim=c(0,70), col=cl)
### Box climatology - use to train Atlantis model
year=seq(1998, 2015, 1)
for (i in 1:nrow(m)){
  plot(m[i,]~year, type='l', ylim=c(0, 8), col='gray80', ylab='', xlab='')
  par(new=T)
}
plot(colMeans(m, na.rm=T)~year, type='l', col='blue', ylim=c(0,8), lwd=2)
par(new=F)

#__________________________________________________________________________________________________
#### bottom NO3 climatology from Nathan Rebuck 20170613 units are uM (uM * 14 = mg/m3)
library(R.matlab)
setwd('C:/Users/ryan.morse/Downloads')
no3=readMat('bottomnitrate.mat')
rotate_counter_clockwise <- function(x) { apply(     t(x),2, rev)}
NO3=stack()
for (i in 1:12){
  nit.1=as.matrix(no3$botnite[,,i])
  rownames(nit.1)=no3$yi
  colnames(nit.1)=no3$xi
  m=nit.1
  m2=t(m)
  m2=rotate_counter_clockwise(m2)
  test=raster(m2)*14
  bb <- extent(min(no3$xi), max(no3$xi), min(no3$yi), max(no3$yi))
  extent(test)=bb
  # plot(test)
  NO3=stack(NO3, test)
}

wd3='C:/Users/ryan.morse/Desktop/NEUS Atl files'
filename=paste('monthly_NO3_mg_m3',".pdf", sep="")
mypath=file.path(wd3, filename)
pdf(file=mypath) #width=4.5, height=4.5
for(i in 1:12){
  plot(NO3[[i]], main=paste('Month:',i, 'Bottom NO3 (mg/m3)'))
}
dev.off()

NEUSplotN=function(data, i, maxV){
  rasterX=data[[i]]
  col5=colorRampPalette(c('blue','white','red'))
  max_abolute_value=maxV #what is the maximum absolute value of raster?
  color_levels=20
  br <- seq(0, max_abolute_value, length.out=color_levels+1) 
  rng2=cellStats(rasterX, range)
  rng=c(0, maxV, rng2[2])
  arg=list(at=rng, labels=round(rng,2))
  plot(rasterX, col=col5(length(br) - 1), breaks=br,axis.args=arg, xlim=c(-77,-64),ylim=c(35,45),
       las=1, legend=F, main=paste('Month:',i, 'Bottom NO3 (mg/m3)'))
  map("worldHires", xlim=c(-77,-64),ylim=c(35,45), fill=T,border=0,col="gray", add=T)
  plot(rasterX, legend.only=T, col=col5(length(br) - 1),breaks=br,axis.args=arg, legend.shrink=0.5,
       smallplot=c(0.19,0.21, 0.6,0.80) )
}
for(i in 1:12){
NEUSplotN(NO3, i, 500) # data, choose date (1-228), max value
}


Jan.no3=extractMonths(NO3[[1]], neus.shp)
Feb.no3=extractMonths(NO3[[2]], neus.shp)
Mar.no3=extractMonths(NO3[[3]], neus.shp)
Apr.no3=extractMonths(NO3[[4]], neus.shp)
May.no3=extractMonths(NO3[[5]], neus.shp)
Jun.no3=extractMonths(NO3[[6]], neus.shp)
Jul.no3=extractMonths(NO3[[7]], neus.shp)
Aug.no3=extractMonths(NO3[[8]], neus.shp)
Sep.no3=extractMonths(NO3[[9]], neus.shp)
Oct.no3=extractMonths(NO3[[10]], neus.shp)
Nov.no3=extractMonths(NO3[[11]], neus.shp)
Dec.no3=extractMonths(NO3[[12]], neus.shp)

NO3.box=data.frame(c(seq(from=0,to=29,by=1)))
colnames(NO3.box)='box'
NO3.box$'1'=Jan.no3 *14
NO3.box$'2'=Feb.no3*14
NO3.box$'3'=Mar.no3*14
NO3.box$'4'=Apr.no3*14
NO3.box$'5'=May.no3*14
NO3.box$'6'=Jun.no3*14
NO3.box$'7'=Jul.no3*14
NO3.box$'8'=Aug.no3*14
NO3.box$'9'=Sep.no3*14
NO3.box$'10'=Oct.no3*14
NO3.box$'11'=Nov.no3*14
NO3.box$'12'=Dec.no3*14

NO3.box$mean=rowMeans(NO3.box[,2:13], na.rm=T)
NO3.box=as.matrix(NO3.box)

### aggregate to total 'biomass' time series for NEUS boxes
NO3.box.biom=NO3.box[2:23,2:13]

### function to estimate nutrient concentrations through the water column based on values
### at depth given value and number of layers (e.g. in Atlantis)
reduceN=function(x, n){
  if (n=1){
    v2=x
  } else if (n==2){
    v2=(x + x*0.1)/2
  } else if (n==3){
    v2=(x + x*0.1 + x*0.01)/3
  } else if (n==4){
    v2=(x + x*0.1 + x*0.01 +x*0.001)/4
  }
  return(v2)
}

NO3.box.biom=NO3.box.biom*bgm.z$area



i=11 # set box number to plot
barplot(NO3.box[i+1,c(2:13)], main=paste('box',i, 'monthly bottom NO3 mg/m3'))



# use to fill initial condition values from bottom up (value highest at bottom, same at sediment,
# left to right decrease (up in wc), divide by 10)
FILL.init.nuts=function(tt, bgm.z){
  tt2=matrix(nrow=30, ncol=5, NA)
  for (i in 1:nrow(tt)){
    tt2[i,1]=tt[i] #bottom layer
    tt2[i,5]=tt[i] #sediment
    if (bgm.z$nz[i]==2){
      tt2[i,2]=tt[i]/10
    }
    else if (bgm.z$nz[i]==3){
      tt2[i,2]=tt[i]/10
      tt2[i,3]=tt[i]/10
    }
    else if (bgm.z$nz[i]==4){
      tt2[i,2]=tt[i]/10
      tt2[i,3]=tt[i]/10
      tt2[i,4]=tt[i]/10
    }
  }
  tt2[is.na(tt2)]='_'
  return(tt2)
}

bot.NO3=rowMeans(NO3.box[,2:13])
tt=round(matrix((bot.NO3)),digits=3) # NO3
bNO3=FILL.init.nuts(tt,bgm.z)
setwd('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions')
write.table(bNO3, file='NO3_mgM3.csv', sep=', ',col.names = F, row.names=F)

write.table(NO3.box, file='NO3_BoxMean_monthly_mgM3.csv', sep=', ',col.names = T, row.names=F)
NH3.box=NO3.box
NH3.box[,2:12]=NO3.box[,2:12]*.35
write.table(NH3.box, file='NH3_BoxMean_monthly_mgM3.csv', sep=', ',col.names = T, row.names=F) ## note this is derived from NO3*0.35

# Estimate NH3 as half of nitrate values
tt=round(matrix((bot.NO3/2)),digits=3) # NH3
bNH3=FILL.init.nuts(tt,bgm.z)
setwd('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions')
write.table(bNH3, file='NH3_mgM3.csv', sep=', ',col.names = F, row.names=F)




# col5=colorRampPalette(c('blue','white','red'))
col5=colorRampPalette(brewer.pal(9,'Reds'))
# col5=colorRampPalette(brewer.pal(9,'YlOrRd'))
cl <- col5(29)
arg=seq(from=0,to=30,by=1)
plot(s[[i]], col=cl)

for(i in 1:12){
  plot(s[[i]], col=cl,breaks=seq(0, 30, 1), main=paste('month=',i))
}


#________________________________________________________________
#testing CZCS...
setwd('C:/Users/ryan.morse/Downloads')
m=nc_open('C19782741978304.L3m_MO_CHL_chlor_a_9km.nc')
plot(m)
m1=nc2raster(m)

#__________________________________________________________________
# MARMAP chlorophyll
# DATA UNITS:
#   
#   CRUISE IS CHARACTER 6
# STA IS CONSECUTIVE STATION
# YEAR
# DEPTH IS METERS BELOW SEA SURFACE
# CHLA IS FLUOROMETRIC CHLOROPHYLL A (UG/L) == (mg m^-3)
# PHAE IS FLUOROMETRIC PHAEOPHYTIN A (UG/L)
setwd('G:/1 RM')
setwd('C:/Users/ryan.morse/Desktop/Iomega Drive Backup 20171012/1 RM')
MM.chl=read.csv('MARMAPchlorophyll.csv', skip=74, stringsAsFactors = F)
colnames(MM.chl)=c('CRUISE', 'STA', 'YEAR', 'MON',	'DAY', 'HR',	'MIN',	'LATD',	'LOND',	'DEPTH',	'CHLA',	'PHAE')

MM.surf=MM.chl[which(MM.chl$DEPTH<15),] # limit depth to: surface - 15 m

# MM.chl2=MM.chl[complete.cases(MM.chl),]
MM.chl2=MM.surf[complete.cases(MM.surf),]

coordinates(MM.chl2)=~LOND+LATD #transform to Spatialpointsdataframe
pointsin=over(MM.chl2, neus.shp) #find which boxes samples belong to
BOX_ID=as.numeric(levels(pointsin$BOX_ID))[pointsin$BOX_ID]
DEPTH=as.numeric(levels(pointsin$DEPTH))[pointsin$DEPTH]
MM.boxbio=data.frame(MM.chl2, BOX_ID, DEPTH) #pointsin

### compute yearly mean Chl per box
MM.boxes=aggregate(MM.boxbio$CHLA,list('box'=as.numeric(MM.boxbio$BOX_ID), 'Y'=MM.boxbio$YEAR), mean)
MM.boxes=reshape(MM.boxes, idvar='box', timevar = 'Y', direction = 'wide')
MM.boxes=MM.boxes[order(MM.boxes['box']),]
mm.t=seq(from=1977, to=1987, by=1)

### compute monthly means per box
MM.boxes.mon=aggregate(MM.boxbio$CHLA,list('box'=MM.boxbio$BOX_ID, 'M'= MM.boxbio$MON), mean)
MM.boxes.mon=reshape(MM.boxes.mon, idvar='box', timevar = 'M', direction = 'wide')
MM.boxes.mon=MM.boxes.mon[order(MM.boxes.mon['box']),]

### Difference between MARMAP chl and SeaWiFS/MODIS/Viirs
test=data.frame(rowMeans(MM.boxes.mon[,2:13], na.rm=T),  rowMeans(Chl[c(1:23, 26:30),])) #drops islands (boxes 23 24)
test$diff=test[,1]-test[,2] #Marmap-contemporary
test$box=MM.boxes$box
barplot(test$diff, names.arg=test$box)
chldiff=test[2:23,]
colnames(chldiff)=c('MARMAP Chl', 'Contemporary Chl', 'diff', 'box')
barplot(chldiff$diff, names.arg=chldiff$box)

### heatmaps
library(gplots)
some_col_func <- colorspace::diverge_hcl
my_palette <- colorRampPalette(c("white", "green"))(n = 7)
my_palette2 <- colorRampPalette(c("blue", "yellow", "green"))(n = 20)
st.anom=apply(dataT, 2, function(x) (x-mean(x))/std(x)) # calc standardized anomaly of data

## monthly Chl value for MARMAP
my_palette <- colorRampPalette(c("white", "red"))(n = 7)
mm.mon.anom=heatmap.2(as.matrix(MM.boxes.mon[,2:13]), breaks=c(0,1,2,3,4,5,6,7),Rowv=F, Colv=F, dendrogram = 'none', na.color='gray',
                      labCol = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'), labRow = MM.boxes.mon[,1], col=my_palette,
                      tracecol = 'black', main='Mean Monthly Chl 1977-1987')

chl.mon.anom=heatmap.2(as.matrix(Chl[c(1:23, 26:30),]), breaks=c(0,1,2,3,4,5,6,7), Rowv=F, Colv=F, dendrogram = 'none', na.color='gray',
                      labCol = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'), labRow = c(0:22, 25:29),col=my_palette,
                      tracecol = 'black',main='Mean Monthly Chl 1998-2016')

# chl.mon.anom=heatmap.2(as.matrix(t(box.mon)), breaks=c(0,1,2,3,4,5,6,7), Rowv=F, Colv=F, dendrogram = 'none', na.color='gray',
#                        labCol = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'), labRow = c(0:29),col=my_palette,
#                        tracecol = 'black')
### difference between MARMAP and contemporary by box and month
Chl.time.dif=(MM.boxes.mon[1:23,2:13] - Chl[1:23,1:12])
my_palette <- colorRampPalette(c("blue", "white","red"))(n = 10)
heatmap.2(as.matrix(Chl.time.dif), breaks=c(-4,-3,-2,-1,-0.5,0,0.5,1,2,3,4), Rowv=F, Colv=F, dendrogram = 'none', na.color = 'gray',
          labCol = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'),labRow = c(0:22),col=my_palette,
          tracecol = 'black',main='MARMAP - Contemporary')


## Mean Yearly Chl
mm.mon.anom=heatmap.2(as.matrix(MM.boxes[,2:13]), breaks=c(0,1,2,3,4,5,6,7),Rowv=F, Colv=F, dendrogram = 'none', na.color='gray',
                      labCol = mm.t, labRow = MM.boxes.mon[,1], col=my_palette,
                      tracecol = 'black', main='MARMAP Mean Chl 1977-1987')
chl.mon.anom=heatmap.2(as.matrix(box.yr[c(1:23, 26:30),]), breaks=c(0,1,2,3,4,5,6,7), Rowv=F, Colv=F, dendrogram = 'none', na.color='gray',
                       labCol = colnames(box.yr), labRow = c(0:22, 25:29),col=my_palette,
                       tracecol = 'black', main="Contemporary Mean Yearly Chl")

mm.anom=apply(MM.boxes[,2:13], 2, function(x) (x-mean(x, na.rm=T))/sd(x, na.rm=T)) # calc standardized anomaly of data

my_palette <- colorRampPalette(c("white", "red"))(n = 6)
mm.mon.anom=heatmap.2(as.matrix(mm.anom), breaks=c(-3,-2,-1,0,1,2,3),Rowv=F, Colv=F, dendrogram = 'none', na.color='gray',
                      labCol = mm.t, labRow = MM.boxes.mon[,1], col=my_palette,
                      tracecol = 'black', main='MARMAP Mean Chl 1977-1987')


#### MARMAP size fractionated Chlorophyll from Kim Hyde
MM.chl.sz=read.csv('C:/Users/ryan.morse/Downloads/MARMAP-SIZE_FRACTION-CHL-SURFACE_FOR_RMORSE.csv', stringsAsFactors = F)
MM.chl.sz.z=read.csv('C:/Users/ryan.morse/Downloads/MARMAP-SIZE_FRACTION-CHL-PROFILES-FOR_RMORSE.csv', stringsAsFactors = F)

MM.chl.sz.z=read.csv('/home/ryan/Downloads/MARMAP-SIZE_FRACTION-CHL-PROFILES-FOR_RMORSE.csv')
MM.chl.sz=read.csv('/home/ryan/Downloads/MARMAP-SIZE_FRACTION-CHL-SURFACE_FOR_RMORSE.csv')

MM.surf=MM.chl.sz[which(MM.chl.sz.z$DEPTH<15),] # limit depth to: surface - 15 m
MM.surf$YEAR=as.numeric(substr(MM.surf$DATE,1,5))*1000 ## coax year out of DATE string
MM.surf$MON=as.numeric(substr(MM.surf$DATE,6,7)) ## coax Month from DATE


# MM.chl2=MM.chl[complete.cases(MM.chl),]
MM.chl2=MM.surf[complete.cases(MM.surf$LON),]

coordinates(MM.chl2)=~LON+LAT #transform to Spatialpointsdataframe
pointsin=over(MM.chl2, neus.shp) #find which boxes samples belong to
MM.boxbio=data.frame(MM.chl2, pointsin)
test=as.character(MM.boxbio$BOX_ID)
test2=as.numeric(test)
MM.boxbio$BOX_ID=test2

### compute yearly mean Chl per box
# MM.boxes.nano=aggregate(MM.boxbio$NANO_CHL,list('box'=as.integer(MM.boxbio$BOX_ID), 'Y'=MM.boxbio$YEAR, 'M'=MM.boxbio$MON), mean)
# MM.boxes.net=aggregate(MM.boxbio$NET_CHL,list('box'=as.integer(MM.boxbio$BOX_ID), 'Y'=MM.boxbio$YEAR, 'M'=MM.boxbio$MON), mean)
MM.boxes.nano=aggregate(MM.boxbio$NANO_CHL,list('box'=as.integer(MM.boxbio$BOX_ID), 'Y'=MM.boxbio$YEAR), mean)
MM.boxes.net=aggregate(MM.boxbio$NET_CHL,list('box'=as.integer(MM.boxbio$BOX_ID), 'Y'=MM.boxbio$YEAR), mean)


MM.boxes.nano=reshape(MM.boxes.nano, idvar='box', timevar = 'Y', direction = 'wide')
MM.boxes.nano=MM.boxes.nano[order(MM.boxes.nano['box']),]
# mm.t=seq(from=1977, to=1987, by=1)
MM.boxes.nano$box=as.numeric(MM.boxes.nano$box)
MM.boxes.nano$means=rowMeans(MM.boxes.nano[,2:13], na.rm=T) # mean by box for time series (all months)

MM.boxes.net=reshape(MM.boxes.net, idvar='box', timevar = 'Y', direction = 'wide')
MM.boxes.net=MM.boxes.net[order(MM.boxes.net['box']),]
# mm.t=seq(from=1977, to=1987, by=1)
MM.boxes.net$box=as.numeric(MM.boxes.net$box)
MM.boxes.net$means=rowMeans(MM.boxes.net[,2:13], na.rm=T) # mean by box for time series (all months)

# Annual Mean of Chl by box -> partition into phytoplankton based on NEUS 1.0 virgin biomass proportion:
# *** USE THESE FOR INITIAL CONDTIONS FILE for phytoplankton ***
## UPDATE - now using MARMAP nanoplankton and netplankton (>20 um) 20180525

tt=round(matrix((MM.boxes.nano$means/7)),digits=3) # PS
PS=FILL.init(tt,bgm.z)
tt=round(matrix((MM.boxes.net$means/7)*0.75),digits=3) # PL
PL=FILL.init(tt,bgm.z)
tt=round(matrix((MM.boxes.net$means/7)*0.25),digits=3) # DF
DF=FILL.init(tt,bgm.z)

setwd('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions')
write.table(PL, file='PL_2.csv', sep=', ',col.names = F, row.names=F)
write.table(PS, file='PS_2.csv', sep=', ',col.names = F, row.names=F)
write.table(DF, file='DF_2.csv', sep=', ',col.names = F, row.names=F)



### compute monthly means per box
MM.boxes.mon=aggregate(MM.boxbio$NANO_CHL,list('box'=MM.boxbio$BOX_ID, 'M'= MM.boxbio$MON), mean)
MM.boxes.mon=reshape(MM.boxes.mon, idvar='box', timevar = 'M', direction = 'wide')
MM.boxes.mon=MM.boxes.mon[order(MM.boxes.mon['box']),]


