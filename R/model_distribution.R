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

