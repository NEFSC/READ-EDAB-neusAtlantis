## Project .CSV file from UTM (Atlantis coordinates) to Lat / Lon
#By: Erik Olsen
# 13.12.2013

library(rgdal)

setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS")

NEUSutm<-read.table("neusgeometry.csv", header=TRUE, sep=";", dec=",")

NEUSgeom<-project(cbind(NEUSutm$X, NEUSutm$Y), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE) # projects the X and Y coordinates to a Trasverse mercator projection - this can be changed according to the projection in each case

# Documentation on Trasverse mercator projection coding from http://www.remotesensing.org/geotiff/proj_list/transverse_mercator.html
# +proj=tmerc +lat_0=Latitude of natural origin 
# +lon_0=Longitude of natural origin
# +k=Scale factor at natural origin 
# +x_0=False Easting
# +y_0=False Northing

NEUSgeom2<-as.data.frame(cbind(NEUSutm$PID, NEUSutm$POS, NEUSgeom[,1], NEUSgeom[,2]))
colnames(NEUSgeom2)<-c("PID", "POS", "LON", "LAT")

write.table(NEUSgeom2, "NEUSlatlon.csv", sep=";", dec=",", col.names=TRUE, row.names=FALSE)

#_______________________________________________________________________________


library(rgdal)
library(mapview)
library(sf)
library(rbgm)
library(bgmfiles)
library(viridis)
library(maptools)
library(sp)
library(atlantistools)
library(dplyr)



wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'
setwd(wd2)

bgm.file <- ("test_winding_passes.bgm") #neus30_v15.bgm")
neus <- bgmfile(bgm.file)
box <- boxSpatial(neus)
# projection(box) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=30000000 +scale=1"
projection(box) <- "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"
depthbin=c(0,25,50,70,100,150,200,500)
zbin=.bincode(box$botz*-1, depthbin)
box$colour=zbin
box$colour <- substr(viridis::viridis(nrow(box)), 1, 7)
box$colour[box$boundary] <- NA
mapview(box, color = box$colour)

breakproj <- function(x) {
  paste(strsplit(x, " ")[[1]], collapse = "\n")
}
plot(box, col = ifelse(box$boundary, "#88888880", sample(rainbow(nrow(box), alpha = 0.5))))
op <- par(xpd = NA)
llgridlines(box)
par(op)
title('NEUS', cex = 0.8)
mtext(breakproj(proj4string(box)), cex = 0.75, side = 2, las = 1, adj = 0, line = 2, at = par("usr")[3], xpd = NA)

par(new=T)
points(-70, 40, pch=19)

wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'
# NEUS.ll=readShapeSpatial(file.path(wd2,'NEUS_LL.shp')) # this is in bgm format
neus.shp=readShapeSpatial(file.path(wd2,'NEUS_Long_Lat.shp')) # this one is in (long, lat format)
# plot(NEUS.ll, add=T)
plot(neus.shp)#, add=T)

library(maptools)
library(plot3D)
ShapeFile <- neus.shp #readShapeSpatial('Test.shp')
Polygons <- slot(ShapeFile,"polygons")
temp <- do.call(rbind, lapply(Polygons, function(x) slot(slot(x, "Polygons")[[1]], "coords")))
polygon3D(temp[, 1], temp[, 2], rep(0, nrow(temp)), col="transparent")
for (i in seq_along(Polygons)) {
  temp <- slot(slot(Polygons[[i]], "Polygons")[[1]], "coords")
  depth = neus.shp@data$botz
  # polygon3D(temp[, 1], temp[, 2], rep(0, nrow(temp)), add = TRUE)
  polygon3D(temp[, 1], temp[, 2], depth[i], add = TRUE)
  
}
spdf <- SpatialPointsDataFrame(coords=matrix(rnorm(60), ncol = 2), 
                               data=data.frame(z = rnorm(30)))
test=cbind(coordinates(spdf), ShapeFile@data)


gdepth=raster("H:/1 RM/KINGSTON/transfer/zoo shelf pos/nes_bath_data.nc", band=1) # # read in depth grid

# ### Function to extract data using a shapefile
# extractMonths=function(x, shp){
#   v2=list()
#   for(i in 1:dim(x)[3]){
#     v=extract(x[[i]], shp)
#     v1=lapply(v, function(xx) mean(xx, na.rm=T))
#     v2[i]=list(v1)
#   }
#   m=matrix(unlist(v2), ncol=dim(x)[3], nrow=30) # box 0-29 =rows, years =cols
#   colnames(m)=seq(1998, 2016, by=1)
#   rownamse(m)=
#     return(m)
# }

maxdepth=extract(gdepth, neus.shp)
max.z=lapply(maxdepth, FUN=min)
min.z=lapply(maxdepth, FUN=max)
mean.z=lapply(maxdepth, FUN=mean)
neus.depth=data.frame(unlist(max.z))
neus.depth$min.z=unlist(min.z)
neus.depth$mean.z=round(unlist(mean.z),digits=0)

### returns Mean Chl per box (0-29, rows), by year from 1998-2016 (columns) for month indicated
Jan.chl=extractMonths(r1, neus.shp)

          
# # Point source/sink lists
# npointss 6
# 
# ## nutrient forcing             
# pss0.name GeorgesBank
# pss0.location 1234274.466 3098307.516 -40
# pss0.data tsfiles/banksupwelling.ts
# pss0.rewind 1
# 
# pss1.name GulfStreamTrap
# pss1.location 1239774.298 3024140.334 -130
# pss1.data tsfiles/banksupwelling.ts
# pss1.rewind 1
# 
# pss2.name Basin16
# pss2.location 1102189.803 3299498.894 -150
# pss2.data tsfiles/basindeep.ts
# pss2.rewind 1
# 
# pss3.name Basin20
# pss3.location 1223937.669 3213347.009 -220
# pss3.data tsfiles/basindeep.ts
# pss3.rewind 1
# 
# pss4.name Basin11
# pss4.location 1100958.262 3181632.652 -170
# pss4.data tsfiles/basindeep.ts
# pss4.rewind 1
# 
# pss5.name Boston
# pss5.location 1016047.729 3243653.893 -15
# pss5.data tsfiles/bostonharbornuts.ts
# pss5.rewind 1

## old ways: 

## transform to longlat for test plot
library(USAboundaries)
states <- USAboundaries::us_states()
box_ll <- spTransform(box, proj4string(states))
plot(box_ll)
plot(states, add = TRUE, col = "grey")
plot(spTransform(box, proj4string(states)), col = box_ll$colour, add = TRUE)
op <- par(xpd = NA)
rgdal::llgridlines(box_ll)
par(op)

# from point source in forcing file
banks=c(1234274.466, 3098307.516)
GStrap=c(1239774.298, 3024140.334)
basin16=c(1102189.803, 3299498.894)
basin20=c(1223937.669, 3213347.009)
basin11=c(1100958.262, 3181632.652)
boston=c(1016047.729, 3243653.893)

pointsource=data.frame(c(1234274.466, 1239774.298, 1102189.803, 1223937.669,1100958.262, 1016047.729),
                   c(3098307.516, 3024140.334, 3299498.894, 3213347.009, 3181632.652,3243653.893))
points(project(cbind(banks[1], banks[2]), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE), pch=19, col='red')
points(project(cbind(pointsource[,1], pointsource[,2]), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE), pch=19, col='red')

# add new point source locations
CBay=c(531250, 2625000)
DBay=c(610000, 2810000)
Hudson=c(710000, 3000000)
NBay=c(937500, 3100000)
points(project(cbind(CBay[1], CBay[2]), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE), pch=19, col='red')
points(project(cbind(DBay[1], DBay[2]), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE), pch=19, col='red')
points(project(cbind(Hudson[1], Hudson[2]), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE), pch=19, col='red')
points(project(cbind(NBay[1], NBay[2]), "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +k=1 +x_0=1000000 +y_0=3000000  ellps=WGS84", inv=TRUE), pch=19, col='red')



bgm.file <- ("neus_tmerc_RM.bgm") #neus30_v15.bgm")
neus <- bgmfile(bgm.file)
box <- boxSpatial(neus)
projection(box) <- "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"
plot(box, axes=T)
points(1234274.466,3098307.516, pch=19, col='red')
points(1239774.298,3024140.334, pch=19, col='red')
points(1102189.803,3299498.894, pch=19, col='red')
points(1223937.669,3213347.009, pch=19, col='red')
points(1100958.262,3181632.652, pch=19, col='red')
points(1016047.729,3243653.893, pch=19, col='red')
points(CBay[1], CBay[2], pch=19, col='blue')
points(DBay[1], DBay[2], pch=19, col='blue')
points(NBay[1], NBay[2], pch=19, col='blue')
points(Hudson[1], Hudson[2], pch=19, col='blue')

points(1220475.7351718375, 3429637.9873865936, pch=19, col='red') #box 18
points(1098345.3045816745, 3367268.9230297254, pch=19, col='red') #box 17
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}
library(marmap)
plot(box_ll)
plot(states, add = TRUE, col = "grey")
plot(spTransform(box, proj4string(states)), col = box_ll$colour, add = TRUE)
nesbath=getNOAA.bathy(lon1=-77,lon2=-65,lat1=35,lat2=45, resolution=10, keep=F)
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col=addTrans('red',150),lty=1)
plot(nesbath,deep=-100, shallow=-100, step=1,add=T,lwd=1,col=addTrans('red',250),lty=1)
plot(nesbath,deep=-500, shallow=-500, step=1,add=T,lwd=1,col=addTrans('red',50),lty=1)
box()
axis(1)
axis(2, las=1)

library(maps)
library(mapdata)
library(marmap)
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray70")
map.axes(las=1)
data(stateMapEnv)
map('state', fill = F, add=T) # add state lines
lines(neus.shp)
# map.grid(col=addTrans('black',35), labels=F)
# plot(nesbath,deep=-50, shallow=-50, step=1,add=T,lwd=1,col="gray60",lty=1)
# plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray80",lty=1)
# plot(nesbath,deep=-100, shallow=-100, step=1,add=T,lwd=1,col="gray70",lty=1)
plot(nesbath,deep=-50, shallow=-50, step=1,add=T,lwd=1,col=addTrans('blue',125),lty=1)
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col=addTrans('blue',50),lty=1)
plot(nesbath,deep=-100, shallow=-100, step=1,add=T,lwd=1,col=addTrans('blue',75),lty=1)

## try in the projection it came with
## (we have to trim out regions in radically different longitudes
pstates <- spTransform(subset(states, coordinates(states)[,1] > -120 & coordinates(states)[,1] < -60), proj4string(box))

plot(box)
plot(pstates, add = TRUE, col = "grey")
plot(box, col = box$colour, add = TRUE)
op <- par(xpd = NA)
rgdal::llgridlines(box)
par(op)


#_________________________________________________________________

#' BGM to map script
#' original code from Alexander Keth
#' 
str_split_twice <- function(char, min_only){
  patterns <- c(" ", "\t")
  if (all(!stringr::str_detect(string = char, pattern = patterns))) {
    stop("Neither space nor tab present in variable char (string)!")
  }
  for (i in seq_along(patterns)) {
    char <- unlist(stringr::str_split(string = char, pattern = patterns[i]))
  }
  char <- suppressWarnings(as.numeric(char))
  if (all(is.na(char))) stop("No numeric value in variable char (string)!")
  if (min_only) char <- char[min(which(!is.na(char)))]
  return(char)
}
bgm_to_map <- function(bgm_file, bgm_string){
  bgm <- readLines(con = bgm_file)
  proj_in <- stringr::str_split(string = bgm[grep(pattern = "projection", x = bgm)], pattern = " ", n = 2)[[1]][2]
  n_boxes <- str_split_twice(char = bgm[grep(pattern = "nbox", x = bgm)], min_only = T)
  box_strings <- paste0("box", 0:(n_boxes - 1), bgm_string)
  
  for (i in seq_along(box_strings)) {
    if (i == 1) bgm_data <- list()
    bgm_data[[i]] <- bgm[grep(pattern = box_strings[i], x = bgm)]
    # remove entries with "vertmix" (also found with box_strings)
    if (bgm_string == ".vert") bgm_data[[i]] <- bgm_data[[i]][-grep(pattern = "vertmix", x = bgm_data[[i]])]
    bgm_data[[i]] <- lapply(bgm_data[[i]], str_split_twice, min_only = F)
    bgm_data[[i]] <- data.frame(x = as.numeric(sapply(bgm_data[[i]], function(x) x[2])),
                                y = as.numeric(sapply(bgm_data[[i]], function(x) x[3])),
                                polygon = i - 1)
  }
  
  bgm_data <- do.call(rbind, bgm_data)
  
  lat_long <- proj4::project(bgm_data[, 1:2], proj = proj_in, inverse = T)
  
  bgm_data$long <- lat_long$x
  bgm_data$lat <- lat_long$y
  
  return(bgm_data)
}

tt=bgm_to_map(bgm.file)

#___________________________________________________________
library(rgdal)

#obtain projection of spatial data, returns CRS object
proj.test.obj = proj4string(test.obj)

#define projection of spatial object
proj4string(test.obj) = CRS("proj=aea lat_1=43 lat_2=62 lat_0=30 lon_0=10 x_0=0 y_0=0 ellps=intl units=m no_defs")

#Transform projection spatial object

poly.data = spTransform(poly.data, proj.test.obj)

poly.data = spTransform(poly.data, CRS("proj=aea lat_1=43 lat_2=62 lat_0=30 lon_0=10 x_0=0 y_0=0 ellps=intl units=m no_defs"))





#________________ write shapefile for NEUS

library(rbgm)
library(bgmfiles)
library(rgdal)
library(USAboundaries)
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
bgm.file='/home/ryan/Git/atneus_RM/test_winding_passes.bgm'
setwd(wd2)
# bgm=bgmfile("neus30_v15.bgm")
# boxes <- boxSpatial(bgm)
# boxLL <- spTransform(boxes, "+proj=longlat +ellps=WGS84")
# boxLL <- spTransform(boxes, "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1")
# plot(boxLL)
# writeOGR(boxLL, ".", "NEUS_LL", "ESRI Shapefile")
bgm.file <- ("neus30_v15.bgm")
neus <- bgmfile(bgm.file)
box <- boxSpatial(neus)
# projection(box) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=30000000 +scale=1"
projection(box) <- "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"
states <- USAboundaries::us_states()
box_ll <- spTransform(box, proj4string(states))
plot(box_ll)
plot(states, add = TRUE, col = "grey")
# plot(spTransform(box, proj4string(states)), col = box_ll$colour, add = TRUE)
op <- par(xpd = NA)
rgdal::llgridlines(box_ll)
par(op)
library(USAboundaries)
states <- USAboundaries::us_states()
# box_ll <- spTransform(boxes, proj4string(states))
plot(box_ll)
plot(states, add = TRUE, col = "grey")
plot(spTransform(box, proj4string(states)), col = box_ll$colour, add = TRUE)
writeOGR(box_ll, ".", "NEUS_LL", "ESRI Shapefile")


neus.shp=readShapeSpatial(file.path(wd2,'NEUS_Long_Lat.shp')) # this one is in (long, lat format)
plot(neus.shp, add=T)

nodes=nodeSpatial(neus)
faces=faceSpatial(neus)
points=pointSpatial(neus)
boundary=boundarySpatial(neus)

XYnodes=nodes@coords
colnames(XYnodes)=c('x', 'y')
nodes_ll=spTransform(XYnodes,proj4string(states))
XYnodes=SpatialPoints(XYnodes, proj4string = CRS("+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"))
nodes_ll=spTransform(XYnodes,"+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1")


library(atlantistools)
at.neus=convert_bgm(bgm.file)

NOBA.file <- ('C:/Users/ryan.morse/Documents/GitHub/box-geometry-models/data/bgm/Nordic02.bgm')
NOBA <- bgmfile(NOBA.file)
NOBA.sp <- boxSpatial(NOBA)
NOBA.nodes=nodeSpatial(NOBA)
NOBA.face=faceSpatial(NOBA)
NOBA.pt=pointSpatial(NOBA)
NOBA.bound=boundarySpatial(NOBA)
NOBA.sp$color=substr(viridis::viridis(nrow(NOBA.sp)), 1, 7)
mapview(NOBA.sp, color=NOBA.sp$color)

#Atlantistools 
at.NOBA=convert_bgm(NOBA.file)
plot_boxes(data=at.NOBA)
NOBA.XY=NOBA.nodes@coords
proj_in='+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs'
lat_long <- proj4::project(NOBA.XY[, 1:2], proj = proj_in, inverse = T)

## NEUS
bgm.file='/home/ryan/Git/atneus_RM/test_winding_passes.bgm'
at.neus=convert_bgm(bgm.file)
plot_boxes(data=at.neus)


#devtools::install_github("mdsumner/gris", ref = "cran-sprint")
library(gris)
  bgm <- bgmfile(files[i])
  boxes <- boxSpatial(bgm)
  bll <- if (isLonLat(boxes)) boxes else spTransform(boxes, "+proj=longlat +ellps=WGS84")
  g <- gris(bll)
  gt <- triangulate(g)
  plot3d(gt, add = i > 1)

plot3d(triangulate(gris(wrld_simpl)), add = TRUE, col = "black")
rgl::light3d(specular = "aliceblue", viewpoint.rel = FALSE)
rgl::bg3d("black")

#____________________________________________________________________________________
# Create file to use to slice/dice ROMS data based on Cecilie Hansen's NOBA file
# https://github.com/cecilieha/NoBA/blob/master/corners_neighbours_nordic.txt

cecilie=read.table('file:///C:/Users/ryan.morse/Documents/GitHub/NoBA/corners_neighbours_nordic.txt', sep='\t', header = T)
m2=is.nan(cecilie$neighbour)
sum(m2)
at.NOBA2=transform(at.NOBA, ll=paste0(round(lat, 5), ', ', round(long, 5)))
m=unique(at.NOBA2$ll)
m=duplicated(at.NOBA2$ll)


noba_file  <- bgmfiles::bgmfiles("Nordic")
library(rbgm)

## BGM is a doubly-connected-edge-list
## here in 'related tables' form
noba_edge_list <- bgmfile(noba_file)
nb.box <- boxSpatial(noba_edge_list)
# projection(box) <- "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"
nb.box$colour <- substr(viridis::viridis(nrow(nb.box)), 1, 7)
nb.box$colour[nb.box$boundary] <- NA
mapview(nb.box, color = nb.box$colour)

plot(boxSpatial(noba_edge_list), col = grey(seq(0, 1, length = nrow(noba_edge_list$boxes)), alpha = 0.5))
plot(nb.box, main = "boxes only")
text(coordinates(nb.box), lab = (nb.box$box_id))

# ### original from Mike Sumner
library(dplyr)
noba_edge_list$boxes %>% dplyr::select(.bx0, botz, area) %>%
  inner_join(noba_edge_list$facesXboxes, ".bx0") %>%
  inner_join(noba_edge_list$faces, c("iface" = ".fx0"))
  # etc.

# messing around...
result=noba_edge_list$boxes %>% dplyr::select(.bx0, botz, area) %>% 
  inner_join(noba_edge_list$facesXboxes, ".bx0") %>%
  inner_join(noba_edge_list$boxesXverts, ".bx0") #%>% 


result=noba_edge_list$boxes %>% dplyr::select(.bx0, botz, area) %>% 
  inner_join(noba_edge_list$facesXboxes, ".bx0") %>%
  # left_join(noba_edge_list$facesXverts, c("iface"=".vx0") %>%
  left_join(noba_edge_list$vertices, c("iface"=".vx0"))
            
  left_join(noba_edge_list$vertices, c(".bx0"=".vx0"))
  left_join(noba_edge_list$facesXverts, )

  inner_join(noba_edge_list$faces, c("iface" = ".fx0"))
  inner_join(noba_edge_list$faces, c("iface" = ".fx0"))
  left_join(noba_edge_list$boundaryvertices, c("iface" = ".fx0"))
  
  # strategy...
  boxes (.bx0, botz, area)
  facesXboex (.bx0, iface, ibox)
  vertices (x,y, .vxo) #254 (1 extra????)
  
  facesXverts ".fxo" #match to box 0:252
##etc.

cecilie2=transform(cecilie, ll=paste0(Lat1, ', ', Lon1))
mergResult=semi_join( at.NOBA2, cecilie2, by='ll')

### testing to see matching lat lon combos, round first
cecilie.rd=cecilie
cecilie.rd$Lon1=round(cecilie$Lon1, digits=4)
cecilie.rd$Lon2=round(cecilie$Lon2, digits=4)
cecilie.rd$Lat2=round(cecilie$Lat2, digits=4)
cecilie.rd$Lat1=round(cecilie$Lat1, digits=4)
cecilie.rd$ll=paste(cecilie.rd$Lon1,cecilie.rd$Lat1, sep=',')

at.NOBA.rd=at.NOBA
at.NOBA.rd$lat=round(at.NOBA$lat, digits=4)
at.NOBA.rd$long=round(at.NOBA$long, digits=4)
at.NOBA.rd$ll=paste(at.NOBA.rd$long, at.NOBA.rd$lat, sep=',')
sum(cecilie.rd$ll %in% at.NOBA.rd$ll)

#testing
ii=18# choose box
test1=cecilie.rd[which(cecilie$Area==ii),]
test2=at.NOBA.rd[which(at.NOBA$polygon==ii),]
test1$ll %in% test2$ll
sum(test2$ll %in% test1$ll)
plot(test2$lat~test2$long, type='b', main=paste('at.NOBA box',ii))
plot(test1$Lat1~test1$Lon1, type='b',main=paste('CH1 box',ii))
plot(test1$Lat2~test1$Lon2, type='b',main=paste('CH2 box',ii))

## testing atlantistools against cecilie's table to see if there are missing points
t=matrix(ncol=4,nrow=60, NA)
for (i in 0:59){
t[i+1,1]=sum(at.NOBA$polygon==i)
t[i+1,2]=sum(cecilie$Area==i)
t[i+1,3]=(t[i+1,1]-t[i+1,2])
t[i+1,4]=i
}
colnames(t)=c('at.NOBA', 'Cecilie', 'diff', 'box')



## Do Same for NEUS
test=neus$faces
plot(box_ll@polygons[[1]]@Polygons[[1]]@coords)
lines(box_ll@polygons[[1]]@Polygons[[1]]@coords)
box_ll@polygons[[1]]@Polygons[[1]]@coords


### ATLANTISTOOLS SOLUTION TO CREATE NEIGHBORS FILE LIKE CECILIE HANSEN's RM 8/10/2017
# _____________________________________________________________
library(atlantistools)
noba_file  <- bgmfiles::bgmfiles("Nordic")
box=load_box(noba_file)

## create function (or not)
convert_bgm_Cecilie <- function(bgm) {
  # box <- load_box(bgm = bgm)
  
  # Get info of projection used! Some models don't use '+' to split their
  # arguments in the projection. So we add them here ;)
  proj_in <- box$projection
  if (!any(grepl(pattern = "[+]", proj_in))) {
    proj_in <- strsplit(proj_in, "[[:space:]]")
    for (i in seq_along(proj_in)) {
      proj_in[[i]] <- paste0("+", proj_in[[i]])
    }
    proj_in <- paste(unlist(proj_in), collapse = " ")
  }
  
  n_boxes <- box$nbox
  n_faces=box$nface
  
  # Extract appearance if boxes.
  result <- list()
  for (i in 1:n_boxes) {
    result[[i]] <- data.frame(box$boxes[[i]]$vert)
    names(result[[i]]) <- c("lat", "long")
    result[[i]]$inside_lat <- box$boxes[[i]]$inside[1]
    result[[i]]$inside_long <- box$boxes[[i]]$inside[2]
    result[[i]]$polygon <- i - 1
    result[[i]]$Area = box$boxes[[i]]$area
    result[[i]]$meanDepth = box$boxes[[i]]$botz
    # result[[i]]$iface=box$boxes[[i]]$iface # face number in list of vertices
    # result[[i]]$ibox=box$boxes[[i]]$ibox # neighbor for same face number
    
  }
  result <- do.call(rbind, result)
  
  # Convert coordinates to map-coordinates!
  lat_long <- proj4::project(result[, 1:2], proj = proj_in, inverse = T)
  result$long <- lat_long$x
  result$lat <- lat_long$y
  lat_long <- proj4::project(result[, 3:4], proj = proj_in, inverse = T)
  result$inside_long <- lat_long$x
  result$inside_lat <- lat_long$y
  
  r.boxXfaces=list()
  for (i in 1:n_boxes) {
    r.boxXfaces[[i]] <- data.frame(rep(i-1,length(box$boxes[[i]]$ibox)))
    r.boxXfaces[[i]]$iface=box$boxes[[i]]$iface # face number in list of vertices
    r.boxXfaces[[i]]$ibox=box$boxes[[i]]$ibox # neighbor for same face number
  }
  r.boxXfaces <- do.call(rbind, r.boxXfaces)
  colnames(r.boxXfaces)[1]='polygon'
    
r.faces <- list()
  for (i in 1:n_faces) {
    r.faces[[i]]=data.frame(box$faces[[i]][1,'p1'], fix.empty.names = F, row.names = i)
    # names(r.faces[[i]]) <- "Lat1"
    r.faces[[i]]$Lat1 =data.frame(box$faces[[i]][1,'p1'],fix.empty.names = F,row.names = i)
    r.faces[[i]]$Lon1 =data.frame(box$faces[[i]][2,'p1'],fix.empty.names = F,row.names = i)
    r.faces[[i]]$Lat2 =data.frame(box$faces[[i]][1,'p2'],fix.empty.names = F,row.names = i)
    r.faces[[i]]$Lon2 =data.frame(box$faces[[i]][2,'p2'],fix.empty.names = F,row.names =i)
    r.faces[[i]]$length.km =data.frame(box$faces[[i]][1,'length']/1000,fix.empty.names = F,row.names = i)
    r.faces[[i]]$l=data.frame(box$faces[[i]][1,'lr'],fix.empty.names = F, row.names=i)
    r.faces[[i]]$r=data.frame(box$faces[[i]][2,'lr'],fix.empty.names = F, row.names=i)
    
    r.faces[[i]][1]=NULL
    names(r.faces[[i]])=c("Lat1", "Lat2", "Lon1", "Lon2", "length.km", "L", "R")
  }
  result.faces2 <- do.call(rbind, r.faces)
  result.faces=data.frame(matrix(unlist(r.faces), nrow=n_faces, ncol=7, byrow = T))
  result.faces$iface=seq(from=0, to=n_faces-1, by=1)
  
  # Convert coordinates to map-coordinates!
  lat_long <- proj4::project(result.faces[, 1:2], proj = proj_in, inverse = T)
  result.faces$Lat1 <- lat_long$y
  result.faces$Lon1 <- lat_long$x
  lat_long <- proj4::project(result.faces[, 3:4], proj = proj_in, inverse = T)
  result.faces$Lat2 <- lat_long$y
  result.faces$Lon2 <- lat_long$x
  result.faces=result.faces[,5:11]
  colnames(result.faces)[1]='length.km'
  colnames(result.faces)[2]='L'
  colnames(result.faces)[3]='R'
  
  return(result, result.faces)
}

face_vertex_ind <- match(box_verts$.vx0, dc_edge_list$facesXverts$.vx0)
box_verts$face_id <- dc_edge_list$facesXverts$.fx0[face_vertex_ind]

faces.rm=merge(r.boxXfaces, result.faces, by='iface', all.x=T)
newdf4=merge(newdf2, faces.rm, by=c("Lat1", "Lon1", "Lat2", "Lon2"), all.x=T)

newdf4=left_join(newdf2, faces.rm, by=c("Lat1", "Lon1", "Lat2", "Lon2"))
# newdf4=inner_join(newdf2, faces.rm, by=c("Lat1", "Lon1", "Lat2", "Lon2"), all.x)
df1=paste(newdf4$Lat1, newdf4$Lon1, newdf4$Lat2, newdf4$Lon2, sep=',')
newdf5=newdf4[!duplicated(df1),] # gets length correct by neighbors are still wrong, even using 'ibox'


# 
# df1=paste(result$lat, result$long, sep=',')
# df2=paste(result.faces$Lat1, result.faces$Lon1, sep=',')
# test=match(df1, df2, nomatch=NA, incomparables = NA)
# result$Neighbour <- ifelse(result.faces$L[test] == result$polygon, result.faces$R, result.faces$L)

# df1=paste(newdf2$Lat1, newdf2$Lon1, sep=',')
# df2=paste(result.faces$Lat1, result.faces$Lon1, sep=',')
# test=match(df1, df2, nomatch=NA, incomparables = NA)
# newdf2$Neighbour <- ifelse(result.faces$L[test] == newdf2$polygon, result.faces$R, result.faces$L)

newdf3=newdf2 # duplicate
newdf3$Lat2=NULL
newdf3$Lon2=NULL
newdf4=merge(newdf3, result.faces, by=c("Lat1", "Lon1"))
newdf4=newdf4[complete.cases(newdf4$length.km),]

#close but not correct
newdf4=merge(newdf2, result.faces, by=c("Lat1", "Lon1", "Lat2", "Lon2"), all=T)

df1=paste(newdf2$Lat1, newdf2$Lon1, sep=',')
df2=paste(newdf2$Lat2, newdf2$Lon2, sep=',')
test=data.frame(df1, df2)
View(test)
test$dupDF1=duplicated(df1)
test$dupDF2=duplicated(df2)
test$both=(duplicated(df1)&duplicated(df2))
test2=test[,c(1,2,6)]

test1=data.frame(newdf2$Lat1, newdf2$Lon1, newdf2$polygon)
test6=aggregate(test1, by=list(test1[,1], test1[,2]), FUN=unique)
length(test6$numel[which(test6$numel==3)])
test7=aggregate(test2$poly, by=list(test2$df1, test2$df2), FUN=unique)

# match newdf2 (Lat1 Lon1), (Lat2 Lon2) to test6 (Group.1 and Group.2) to get polygons for each point, 
# find like connection between points (not equal to newdf2$poly) and select as neighbor
newdf2.Lat1Lon1.in.Group1=match(paste(newdf2$Lat1, newdf2$Lon1, sep=','), paste(test6$Group.1, test6$Group.2, sep=','))
newdf2.Lat2Lon2.in.Group1=match(paste(newdf2$Lat2, newdf2$Lon2, sep=','), paste(test6$Group.1, test6$Group.2, sep=','))
toMatch=data.frame(newdf2.Lat1Lon1.in.Group1, newdf2.Lat2Lon2.in.Group1)
toMatch=data.frame(newdf2.Lat1Lon1.in.Group1, newdf2.Lat2Lon2.in.Group1)
toMatch$poly1=test6$newdf2.polygon[toMatch$newdf2.Lat1Lon1.in.Group1]
toMatch$poly2=test6$newdf2.polygon[toMatch$newdf2.Lat2Lon2.in.Group1]
toMatch$box=newdf2$polygon # original polygon
for (i in 1:dim(toMatch)[1]){
  a=toMatch$poly1[[i]] # boxes for Lat1 Lon1
  b=toMatch$poly2[[i]] # boxes for Lat2 Lon2
  c=Reduce(intersect, list(a,b)) # find common boxes between points
  d=toMatch$box[i] # polygon box number for face segment
  if (length(c) < 2){ # select neighbor box that is not equal to polygon box
    if (c==d){
      e=NA # 
    } else {
      e=c
    }
  }else if (length(c)==2) {
    e=c[which(c!=d)]
  }
  toMatch$Neighbor[i]=e #select neighbor box, not boxface
}
newdf2$Neighbour=toMatch$Neighbor

### atlantistools solution from canned atlantistools 'convert_bgm' routine
newdf=at.NOBA[1,]
newdf[1,]=NA
newdf$Lat2=NA
newdf$Lon2=NA
for (i in 1:length(unique(at.NOBA$polygon))){
  ii=i-1
  xx=subset(at.NOBA[at.NOBA$polygon==ii,])
  for (j in 2:dim(xx)[1]-1){
    xx$Lat2[j]=xx$lat[j+1]
    xx$Lon2[j]=xx$long[j+1]
  }
  xx=xx[-c(dim(xx)[1]),]
  newdf=rbind(newdf, xx)
}
newdf2=newdf[-1,]


### atlantistools solution from 'result' created above, from modified 'convert_begm_Cecilie'
newdf=result[1,]
newdf[1,]=NA
newdf$Lat2=NA
newdf$Lon2=NA
for (i in 1:length(unique(result$polygon))){
  ii=i-1
  xx=subset(result[result$polygon==ii,])
  for (j in 2:dim(xx)[1]-1){
    xx$Lat2[j]=xx$lat[j+1]
    xx$Lon2[j]=xx$long[j+1]
  }
  xx=xx[-c(dim(xx)[1]),]
  newdf=rbind(newdf, xx)
}
newdf2=newdf[-1,]


colnames(newdf2)
colnames(newdf2)[1]='Lat1'
colnames(newdf2)[2]='Lon1'
faces2=result.faces[order(result.faces$L),]
# newdf2=newdf

df.1=newdf2 %>% inner_join(faces2, by=c('Lat1', 'Lon1'))
df.2=df.1[complete.cases(df.1),]


df.1=merge(newdf, faces2, by=c('Lat1', 'Lon1'), all=F)

#verify with plot
plot(newdf2$Lat1~newdf2$Lon1, type='p')
# plot(result.faces$Lat1~result.faces$Lon1, type='p')
segments(newdf2$Lon1,newdf2$Lat1, newdf2$Lon2, newdf2$Lat2, col='blue') # all segments
segments(result.faces$Lon1,result.faces$Lat1, result.faces$Lon2, result.faces$Lat2, col='red') # just dynamic boxes

## plot faces by order (black, red, green, blue, cyan, magenta, yellow, gray, (rep),)
tt=newdf2[newdf2$polygon==1,]
plot(c(tt$Lon1, tt$Lon2), c(tt$Lat1, tt$Lat2), type = "n")
for(i in 1:dim(tt)[1]){
segments(tt$Lon1[i], tt$Lat1[i], tt$Lon2[i], tt$Lat2[i], col=i)
}
# plot segments L and R
tt=result.faces[which(faces.rm$polygon==1),]
plot(c(tt$Lon1, tt$Lon2), c(tt$Lat1, tt$Lat2), type = "n")
for(i in 1:dim(tt)[1]){
  segments(tt$Lon1[i], tt$Lat1[i], tt$Lon2[i], tt$Lat2[i], col=i)
}
#_______________________________________________________________________________