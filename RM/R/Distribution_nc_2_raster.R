# Download and process distribution maps for species that have no representation in trawl survey
# Used to create/update range for NEUS Atlantis v1.5
# RM 20190214
# https://www.aquamaps.org/search.php


library(raster)
library(mgcv)
library(sp)
library(maptools)
library(marmap)
library(rgeos)
library(ncdf4)
library(abind)
library(maps)

#load data (download individually as approrpriate)
# setwd('G:/1 RM/3 gridded data/HERMES merged CHL 25km')
setwd('/home/ryan/Downloads')
setwd('/media/ryan/Iomega_HDD/for ryan/distribution maps')
wd=getwd()
files.nc=list.files(wd, pattern=('.nc'))

# nc1=nc_open('/home/ryan/Downloads/Morone_saxatilis.nc')
# lon=ncvar_get(nc1, 'longitude')
# lat=ncvar_get(nc1, 'latitude')
# prob=ncvar_get(nc1, 'probability')

t=raster('/home/ryan/Downloads/Morone_saxatilis.nc')
i=4
t=raster(files.nc[i])
plot(t)
library(maps)
world.outlines <- map("world", plot=FALSE)
world.outlines.sp <- map2SpatialLines(world.outlines, proj4string = CRS("+proj=longlat"))
lines(world.outlines.sp)




