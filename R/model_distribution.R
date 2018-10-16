library(raster)


library(RColorBrewer)
library(raster)
library(marmap)
library(maps)
library(graphics)
library(wql)

svspp_abbr = read.csv("C:/rf_v2/input_data/SVSPP_abbr 3.csv")


# #-------------------------------------------------------------------------------
# setwd("C:/rf_v2/output_data/spring")
# 
# setwd("C:/rf_v2/output_data/fall")
# #-------------------------------------------------------------------------------
# 
# #spsblkdir ="C:/raw_dist/spsmasks/rasters/"
# 
# files = list.files(pattern="RAST")
# sixnames = substr(files,6,11)
# sixnames = unique(sixnames)
# 
# 
# k = 1
# for ( k in 1:length(sixnames)){
# 
# print(k)
# 
# files = list.files(pattern=paste0("RAST_",sixnames[k]))
# files = data.frame(files)
# colnames(files)="fname"
# 
# 
# pafiles = files[!grepl("BM", files$fname),]
# s=stack()
# for(i in 1:length(pafiles)){
# #  print(i)
#   load(as.character(pafiles[i]))
#   s=stack(s, masked.raster)
# }
# 
# masked.raster = calc(s, mean)
# 
# save(masked.raster,file=paste0("C:/for ryan/fall/fall_pa_",sixnames[k],".rdata"))
# 
# 
# bmfiles = files[!grepl("PA", files$fname),]
# s=stack()
# for(i in 1:length(bmfiles)){
# #  print(i)
#   load(as.character(bmfiles[i]))
#   s=stack(s, masked.raster)
# }
# 
# masked.raster = calc(s, mean)
# 
# save(masked.raster,file=paste0("C:/for ryan/fall/fall_bm_",sixnames[k],".rdata"))
# 
# 
# 
# } # end k
library(maps)
library(maptools)
library(sp)
library(rgeos)
library(mgcv)
library(raster)
library(ncdf4)
library(rbgm)


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

# box geomery file
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'
setwd(wd2)
bgm.file <- ("neus30_v15.bgm")
bgm.file <- ("test_winding_passes.bgm")
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

neus.shp=readShapeSpatial(file.path(wd2,'Neus_ll_0p01.shp')) # this one is in (long, lat format)
test=gConvexHull(neus.shp) # creates conves hull of basic NEUS shape for exclusion of data
test.mat=as.matrix(test@polygons[[1]]@Polygons[[1]]@coords)
NES.mat=as.matrix(NES.shp@polygons[[1]]@Polygons[[1]]@coords)

setwd('C:/Users/ryan.morse/Desktop/for ryan/spring')
setwd('C:/Users/ryan.morse/Desktop/for ryan/fall')

files = list.files(pattern="spring")
bmfiles=list.files(pattern="_bm") # biomass
pafiles=list.files(pattern="_pa") # presence absence

names=strsplit(bmfiles, split="_")

nm=rep(NA, length(names))
for (i in 1:length(names)){
nm[i]=sapply(strsplit(names[[i]][3], "[.]"), function(a) a[1])
}


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
       las=1, legend=F, main=nm[j])
  map("worldHires", xlim=c(-77,-64),ylim=c(35,45), fill=T,border=0,col="gray", add=T)
  plot(rasterX, legend.only=T, col=col5(length(br) - 1),breaks=br,axis.args=arg, legend.shrink=0.5,
       smallplot=c(0.19,0.21, 0.6,0.80) )
}
j=1 # set name, choose raster from nm[j]
load(bmfiles[j])
load(pafiles[j])
# load"C:/Users/ryan.morse/Desktop/for ryan/spring/spring_bm_acared.rdata")
# plot(masked.raster)

NEUSplotChlRaster(masked.raster, 1, 1) # data, choose date (1-228), max value
lines(neus.shp)

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

Jan.chl=extractMonths(r1, neus.shp)


c0=data.frame(zoovol[[i]])
m4=as.matrix(c0[,c('Longitude','Latitude')]) #lon,lat from ZPD
c0$epu=NA
c0$epu[which(in.out(test.mat, m4))]='NEUS' #using large convex hull
# c0$epu[which(in.out(NES.mat, m4))]='NEUS' #EDAB EPU area
c0.neus=subset(c0, epu=='NEUS') # limit to only those in NEUS area
coordinates(c0.neus)=~Longitude+Latitude #transform to Spatialpointsdataframe
pointsin=over(c0.neus, neus.shp) #find which boxes samples belong to
c0.neus2 <- data.frame(c0.neus, pointsin)
boxbio=aggregate(formula=Total.Carbon.Mass..mg.C.m3.~box_id, data=c0.neus2, FUN=mean)
box.zoo.biomass[i-1]=list(boxbio)

setwd('C:/Users/ryan.morse/Desktop/NEUS Atl files/RM_initial_conditions')
write.table(ZG, file='ZG2.csv', sep=', ',col.names = F, row.names=F)




library(ks)
# source("G:/1 RM/8 R Functions/KDE_funcs.R")
source("C:/Users/ryan.morse/Desktop/Iomega Drive Backup 20171012/1 RM/8 R Functions/KDE_funcs.R")
biomass.wt.kde.input=function(sdat2, minyr, maxyr){
  clons1 = sdat2$LON[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==1)]
  clons2 = sdat2$LON[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==2)]
  clons3 = sdat2$LON[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==3)]
  clons4 = sdat2$LON[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==4)]
  clons5 = sdat2$LON[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==5)]
  clons1 <- na.omit(clons1) # get rid of missings, KS does not like
  clons2 <- na.omit(clons2)
  clons3 <- na.omit(clons3)
  clons4 <- na.omit(clons4)
  clons5 <- na.omit(clons5)
  clons=c(clons1,clons2,clons2,clons3,clons3,clons3,clons4,clons4,clons4,clons4, clons5, clons5, clons5, clons5, clons5)
  clats1 = sdat2$LAT[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==1)]
  clats2 = sdat2$LAT[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==2)]
  clats3 = sdat2$LAT[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==3)]
  clats4 = sdat2$LAT[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==4)]
  clats5 = sdat2$LAT[(sdat2$YEAR>=minyr & sdat2$YEAR<=maxyr & sdat2$LOGBIO ==5)]
  clats1 <- na.omit(clats1)
  clats2 <- na.omit(clats2)
  clats3 <- na.omit(clats3)
  clats4 <- na.omit(clats4)
  clats5 <- na.omit(clats5)
  clats=c(clats1,clats2,clats2,clats3,clats3,clats3,clats4,clats4,clats4,clats4, clats5, clats5, clats5, clats5, clats5)
  x=cbind(clons,clats) # combine lons and lats
  return(x)
}

# subset sdat to 10 years
sdat2=sdat[which(sdat$YEAR<1979),]

for (i in c(9,10,11,13,20)){
  sdat2=sdat2[which(sdat$SVSPP==i),]
  kde.list=list()
  lat=sdat2$LAT
  lon=sdat2$LON
  year=sdat2$YEAR #yr
  # epu=sdat$epu #EPU
  # sdat=data.frame(lat, lon, year)
  # zoodns=as.numeric(unlist(sdat[i]))
  spp=as.character(sps$SPNAME[which(i==sps$SVSPP)])
  # LOGBIO=floor(log10(zoodns+1))
  # sdat$LOGBIO=LOGBIO
  # sdat$zoodns=zoodns
  # yrlist=data.frame(yrlist1, yrlist1) #only for zooplankton only full time series
  
  ### USE THIS to match zoop time series to different length data sets; yearly, (e.g. 1987-2011)
  ## MUST LOAD TIME SERIES DATA SET (below) FIRST TO GET year.val !!!
  yrlist1=unique(sdat2$YEAR)
  # yy1=unique(year.val)
  # yy2=yrlist1[yrlist1 %in% yy1]
  # yrlist=data.frame(yy2, yy2)
    # minyr=yrlist[1,1]; maxyr=yrlist[1,2]
  minyr=min(yrlist1, na.rm=T)
  maxyr=max(yrlist1, na.rm=T)
  
  x=biomass.wt.kde.input(sdat2, minyr, maxyr)
  # fhat.pi1 <- kde(x, compute.cont=T, binned=F, xmin=c(-80, 32), xmax=c(-60, 48)) # specify grid to match raster stack of fronts... etc.
  fhat.pi1 <- kde(x, compute.cont=T, binned=T, xmin=c(-76, 35), xmax=c(-64, 45)) # specify grid to match raster stack of OISST... etc.
  kde.list[[1]]=fhat.pi1
  lon2=unlist(fhat.pi1[[2]][1])
  lat2=unlist(fhat.pi1[[2]][2])
  test2=as.matrix(fhat.pi1[[3]])
  rownames(test2)=lon2
  colnames(test2)=lat2
  bb <- extent(min(lon2), max(lon2), min(lat2), max(lat2))
  m2=t(test2)[ncol(test2):1,]
  kd.stack=raster(m2)
  extent(kd.stack)=bb
  # Continue raster stack for time series
  # for (j in 2:length(yrlist[,1])){
  #   minyr=yrlist[j,1]; maxyr=yrlist[j,2]
  #   x=biomass.wt.kde.input(sdat, minyr, maxyr)
  #   #   fhat.pi1 <- kde(x, compute.cont=T, binned=F,xmin=c(-80, 32), xmax=c(-60, 48)) # for fronts
  #   fhat.pi1 <- kde(x, compute.cont=T, binned=T, xmin=c(-76, 35), xmax=c(-64, 45)) # specify grid to match raster stack of OISST... etc.
  #   kde.list[[j]]=fhat.pi1
  #   lon2=unlist(fhat.pi1[[2]][1])
  #   lat2=unlist(fhat.pi1[[2]][2])
  #   test2=as.matrix(fhat.pi1[[3]])
  #   rownames(test2)=lon2
  #   colnames(test2)=lat2
  #   bb <- extent(min(lon2), max(lon2), min(lat2), max(lat2))
  #   #   bb <- extent(-80, -60, 32, 48)
  #   m2=t(test2)[ncol(test2):1,]
  #   test2=raster(m2)
  #   extent(test2)=bb  
  #   kd.stack=stack(kd.stack, test2)
  # }
  # if(datalab=='Wind'){
  #   kd.stack=dropLayer(kd.stack, 2) #drop 1988
  #   kd.stack=dropLayer(kd.stack, 1) #drop 1987
  #   kde.list[[2]]=NULL
  #   kde.list[[1]]=NULL
  # }
  # 
  # kd.stack2=mask(kd.stack, NES.shp)
  # small=extent(-76,-64,35,45)
  # kd.stack3=crop(kd.stack2, small)
  # if (dim(kd.stack3)[3]==19){
  #   # kd.stack3=dropLayer(kd.stack3, 1) # drop 1997 for spring chlorophyll data
  #   kd.stack3=dropLayer(kd.stack3, 19) # drop 2015 for fall chlorophyll data
  # }
  # # wd3= "G:/1 RM/2 Plankton Spatial Plots/data"
  # wd3='G:/1 RM/2 Plankton Spatial Plots/data/1982_2015/Zoo/1998_2015'
  # filename=paste(SEASON, spp, yrlist[1,1],yrlist[length(yrlist[,1]),1],"kd.stack.rdata", sep="_")
  # mypath=file.path(wd3, filename)
  # save(kd.stack3, file=mypath)
  # filename=paste(SEASON, spp, yrlist[1,1],yrlist[length(yrlist[,1]),1],"kde.list.rdata", sep="_")
  # mypath=file.path(wd3, filename)
  # save(kde.list, file=mypath)
}

plot(kd.stack)
map("worldHires", xlim=c(-76,-66),ylim=c(36,44.5), fill=T,border=0,col="gray60", add=T)

wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'
neus.shp=readShapeSpatial(file.path(wd2,'Neus_ll_0p01.shp')) # newest, from Bec Gorton DEC 2017
# plot(NEUS.ll, add=T)
plot(neus.shp)

neus.shp@polygons[[2]]@Polygons[[1]]@coords

test=gConvexHull(neus.shp) # creates conves hull of basic NEUS shape for exclusion of data
test.mat=as.matrix(test@polygons[[1]]@Polygons[[1]]@coords)
NES.mat=as.matrix(NES.shp@polygons[[1]]@Polygons[[1]]@coords)