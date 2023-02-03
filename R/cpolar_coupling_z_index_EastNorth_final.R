## ROMS Atlantis coupling MDSumner, Jessica Melbourne-Thomas April 2017

## NOTES

#https://confluence.csiro.au/display/Atlantis/Hydro+FAQ

#https://github.com/cecilieha/NoBA/blob/master/create_flux_ncdf_atlantis.R

##corners <- read.delim("https://raw.githubusercontent.com/cecilieha/NoBA/master/corners_neighbours_nordic.txt")

## curvlinear vector rotation: https://www.myroms.org/forum/viewtopic.php?f=3&t=295

## hydro stuff
# https://confluence.csiro.au/display/Atlantis/Forcing+Files
# https://confluence.csiro.au/display/Atlantis/Current+Forcing+File+Structure
# https://confluence.csiro.au/display/Atlantis/HydroConstruct
# https://confluence.csiro.au/display/Atlantis/Matlab+-+Hydro

## dummy hydro
# sohydro <- "~/projects/AtlantisEastAntartica/SOhydrodummy.nc"
# library(ncdump)
# nc <- NetCDF(sohydro)

#' Convenience function to transform map projection . 
#'
#' Transform `x` to whatever the projection of `to` is. 
#' @param x  object to transform
#' @param to object with a map projection
#'
#' @return `x`, transformed
#' @export
#' @importFrom raster projection
#' @importFrom sp spTransform
project_to <- function(x, to) {
  spTransform(x, CRS(projection(to)))
}
## devtools::install_github(c("mdsumner/angstroms"))
library(angstroms)
library(rbgm) ## read BGM
library(bgmfiles) ## archive of BGM files
library(raadtools) #devtools::install_github("AustralianAntarcticDivision/raadtools")
library(ncdump) 
library(dplyr)
library(ggplot2)
library(geosphere)
complete=tidyr::complete # tidyr masks extract from raster, so just import fn
select=dplyr::select #interference from another package makes this required

### use this command to download selected data for use on windows, change indices to relevant selection, output name
### cd C:\nco 
# ncks -d ocean_time,0,1  -v ocean_time,u,v,salt,temp,lat_rho,lon_rho,lat_u,lon_u,lat_v,lon_v,angle,Cs_r,h,zeta,
# s_rho,hc http://tds.marine.rutgers.edu/thredds/dodsC/roms/doppio/hidden/2007-2016/avg roms.nc

### Add information on depths of boxes from initial conditions file
dz_box=read.csv('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/dz.csv', header=T)
dz_box=read.csv('/home/ryan/AtlRuns/dz.csv', header=T) # depths for each layer by box
# dz_box[is.na(dz_box)]=0 # fill NA with zeros

roms_file='C:/Users/ryan.morse/Desktop/roms/roms2.nc'
roms_file='/home/ryan/roms2008.nc'
roms_file='http://tds.marine.rutgers.edu/thredds/dodsC/roms/doppio/hidden/2007-2016/avg'
roms_file='/media/ryan/TOSHIBA EXT/1 RM/10 ATLANTIS transfer/Hydro work 20170711/2018/daily/roms2008all.nc'
roms_file='/media/ryan/TOSHIBA EXT/1 RM/10 ATLANTIS transfer/Hydro work 20170711/2018/daily/2009/roms2009all.nc' #fixed order
roms_file='/media/ryan/TOSHIBA EXT/1 RM/10 ATLANTIS transfer/Hydro work 20170711/2018/daily/roms2010all.nc'

roms_file='C:/Users/ryan.morse/Desktop/NEUS Atl files/HYDRO/roms2008all.nc'
roms_file='C:/Users/ryan.morse/Desktop/NEUS Atl files/HYDRO/roms2009all.nc'
roms_file='C:/Users/ryan.morse/Desktop/NEUS Atl files/HYDRO/roms2010all.nc'


file_db <- bind_rows(lapply(roms_file, function(x) {
  nc <- NetCDF(x)
  tlen <- filter(nc$dimension, name == "ocean_time")$len
  tibble(fullname = rep(x, tlen), band_level = seq_len(tlen))
}))


## get a BGM and read it
# bfile <- bgmfiles::bgmfiles("antarctica_28")
# bgm <- bgmfile(bfile)
# bfile="/home/ryan/Git/atneus_RM/test_winding_passes.bgm"
bfile="/home/ryan/Git/atneus_RM/neus_tmerc_RM.bgm" ### new file from Bec, modified faces
bfile='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/neus_tmerc_RM.bgm'
bgm <- bgmfile(bfile)


### testing differences in L/R box for faces between bgm file versions - should these be so large do they matter???
# bfile="/home/ryan/Git/atneus_RM/test_winding_passes.bgm"
# bgm <- bgmfile(bfile)
# test=data.frame(table(factor(bgm$faces$left, lev=0:nboxes-1)))
# test$R=table(factor(bgm$faces$right, lev=0:nboxes-1))
# bfile="/home/ryan/Git/atneus_RM/neus_tmerc_RM.bgm" ### new file from Bec, modified faces
# bgm <- bgmfile(bfile)
# test$tmerc.L=table(factor(bgm$faces$left, lev=0:nboxes-1))
# test$tmerc.R=table(factor(bgm$faces$right, lev=0:nboxes-1))
# test$flows.L=table(factor(face_props2$left, lev=0:(nboxes-1)))
# test$flows.R=table(factor(face_props2$right, lev=0:(nboxes-1)))

## we need the unsullied boxes to identify points inside them
boxes <- boxSpatial(bgm)

library(rgdal)
library(maptools)
library(sp)
wd2='/home/ryan/Git/atneus_RM'
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
neus.shp=readShapeSpatial(file.path(wd2,'NEUS_Long_Lat.shp')) # this one is in (long, lat format)
# neus.shp=readShapeSpatial('/home/ryan/Git/atneus_RM/Neus_ll_0p01.shp') # same as BGM file not working with current code
spatialbbox=list()
for (i in 1:length(neus.shp@data$area)){
spatialbbox[[i]]=bbox(neus.shp@polygons[[i]])
}
# df <- do.call("rbind", spatialbbox)
spbox=rbind(spatialbbox)
spbox[[1]][2,] #y

### now get width of boxes E-W and height N-S for hyperdiffusion calculations using a bounding box for each polygon
bgm_bbox=data.frame(matrix(ncol=7, nrow=length(bgm$boxes$label), data=NA))
colnames(bgm_bbox)=c('box','min_x','max_x', 'min_y','max_y', 'dist_NS' , 'dist_EW')
for (i in 1:30){
  bgm_bbox[i,1]=i-1
  bgm_bbox[i,2]=spatialbbox[[i]][1,1]
  bgm_bbox[i,3]=spatialbbox[[i]][1,2]
  bgm_bbox[i,4]=spatialbbox[[i]][2,1]
  bgm_bbox[i,5]=spatialbbox[[i]][2,1]
  test=bbox(neus.shp@polygons[[i]])
  bgm_bbox$dist_NS[i]=distMeeus(test[,1], cbind(test[1,1],test[2,2]), a=6378137, f=1/298.577223563) #distance in meters
  bgm_bbox$dist_EW[i]=distMeeus(test[,1], cbind(test[1,2],test[2,1]), a=6378137, f=1/298.577223563) #distance in meters
}


# get the longitude/latitude arrays
# roms_ll <- romscoords(roms_file, transpose = TRUE)
### added to normalize grid between u and v and rho due to roms cell structure --- update, use u_east & v_north instead they use lat/lon_rho
# roms_ll_u <- romscoords(roms_file, ncdf=F, transpose = T, spatial = c("lon_u", "lat_u"))
# roms_ll_v <- romscoords(roms_file, transpose = F, spatial = c("lon_v", "lat_v"))
roms_ll_rho<- romscoords(roms_file, transpose = TRUE, spatial = c("lon_rho", "lat_rho"))

# roms_face_u@lines$`2`@Lines$`2`@coords
# roms_ll_u[64,40]#y.x is  wrong
# roms_ll_u[48,33] 
# roms_ll_u[40,64]#x,y is more wrong...
# roms_ll_u[33,48]

### Nov.16 - issues with roms_ll_u (and probably others)
# map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray70")
# map.axes(las=1)
# roms_face_u@lines$`1`@Lines$`1`@coords #point 1 for face 0
# points(roms_ll_u[64,40], col='red')
# points(roms_ll_u[48,33], col='green')
# points(roms_ll_u[48,33], col='red')
# points(roms_ll_u[45,36], col='green')


### this is working now!! just being an idiot earlier... see below for iterative approach
# map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray70")
# map.axes(las=1)
# roms_face_u@lines$`7`@Lines$`7`@coords #point 1 for face 0
# points(roms_ll_u[cellFromXY(roms_ll_u, c(40,64))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(33,48))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(36,45))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(33,41))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(37,38))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(38,37))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(41,59))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(36,45))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(54,68))])
# points(roms_ll_u[cellFromXY(roms_ll_u, c(41,59))])


# roms_face <- romsmap(project_to(faceSpatial(bgm), "+init=epsg:4326"), roms_ll)
# roms_face_u <- romsmap(project_to(faceSpatial(bgm), "+init=epsg:4326"), roms_ll_u)
# roms_face_v <- romsmap(project_to(faceSpatial(bgm), "+init=epsg:4326"), roms_ll_v)
roms_face_rho <- romsmap(project_to(faceSpatial(bgm), "+init=epsg:4326"), roms_ll_rho)

### Get coordinates of BGM file in lat/long from roms_ll_ 
# roms_face_u@lines$`1`@Lines$`1`@coords[1,] # need to iterate through this
# noquote(paste0("roms_face_u@lines$`",i,"`@Lines$`",i,"`@coords[1,]"))
bgm_lstXY=coordinates(roms_face_rho) # list of 152 faces for NEUS model
bgmXY=matrix(ncol=length(bgm_lstXY), nrow=4, unlist(bgm_lstXY))
rownames(bgmXY)=c('x1','x2','y1','y2') # POINT 1 = x1,y1, POINT= 2 x2,y2 IMPORTANT ORDER FOR GEOMETRY
bgmXY=data.frame(t(bgmXY)) # now a long dataframe with roms grid X and Y coordinates, use with roms_ll_...
bgmXY$lat1=NA # add empty values for to fill
bgmXY$lat2=NA
bgmXY$lon1=NA
bgmXY$lon2=NA
### Find lat and lon for the BGM shape with extractions by roms XY coordinates
for (i in 1:length(bgm_lstXY)){
bgmXY$lat1[i]=roms_ll_rho[cellFromXY(roms_ll_rho, c(bgmXY[i,1], bgmXY[i,3]))][1,2]
bgmXY$lat2[i]=roms_ll_rho[cellFromXY(roms_ll_rho, c(bgmXY[i,2], bgmXY[i,4]))][1,2]
bgmXY$lon1[i]=roms_ll_rho[cellFromXY(roms_ll_rho, c(bgmXY[i,1], bgmXY[i,3]))][1,1]
bgmXY$lon2[i]=roms_ll_rho[cellFromXY(roms_ll_rho, c(bgmXY[i,2], bgmXY[i,4]))][1,1]
if (i %% 10 == 0) print(i)
}
bgmXY$face=seq(nrow(bgmXY))-1 ### NOTE FACE SHOULD START AT 0 NOT 1


# https://stackoverflow.com/questions/1185408/converting-from-longitude-latitude-to-cartesian-coordinates
angles=bgmXY # copy to not lose terrible extraction data
RE=6371 # radius of earth in KM
# angles$lat1rad=angles$lat1*(pi/180) # convert to radians
# angles$lat2rad=angles$lat2*(pi/180) # convert to radians
# angles$lon1rad=angles$lon1*(pi/180) # convert to radians
# angles$lon2rad=angles$lon2*(pi/180) # convert to radians
### convert radian locations to cartesian coords to get angles of faces
# angles$P1cartX=RE*cos(angles$lat1rad)*cos(angles$lon1rad) #cartesian coords 
# angles$P1cartY=RE*cos(angles$lat1rad)*sin(angles$lon1rad)
# angles$P1cartZ=RE*sin(angles$lat1rad)
# angles$P2cartX=RE*cos(angles$lat2rad)*cos(angles$lon2rad) #cartesian coords 
# angles$P2cartY=RE*cos(angles$lat2rad)*sin(angles$lon2rad)
# angles$P2cartZ=RE*sin(angles$lat2rad)

### RM get angles of faces in un-rotated native BGM projection
# angles$dX=angles$P2cartX-angles$P1cartX
# angles$dY=angles$P2cartY-angles$P1cartY
# angles$dZ=angles$P2cartZ-angles$P1cartZ
# angles$slope=tan(angles$dY/angles$dX)
### THESE WORK BETTER FOR NEUS MODEL
bearmat=matrix(ncol=4, nrow=length(angles$lat1), data=c(angles$lon1, angles$lat1, angles$lon2, angles$lat2)) # matrix of bearings
### (THIS WILL NOT WORK AT HIGH LAT)
angles$bearingRhumb=bearingRhumb(bearmat[,1:2], bearmat[,3:4]) # get bearing of a rhumb line from point 1 to point 2 (THIS WILL NOT WORK AT HIGH LAT)
angles$XYbearingRhumb=((360-angles$bearingRhumb)+90)%%360 ### This converts bearing into polar XY coords where 0 is East (x) 90 is North (y)
# angles$XYbearingRhumb2=(angles$XYbearingRhumb %% 360 +360) %% 360 # limit so that (360 >= x >= 0)
### (THIS MAY NOT WORK AT HIGH LAT -> initial bearing only as it will change over the line)
# angles$bearing=bearing(bearmat[,1:2], bearmat[,3:4]) # get initial bearing of a line from point 1 to point 2on geodesic
# angles$XYbearing=(360-angles$bearing)+90 ### This converts bearing into XY coords where 0 is East (x) 90 is North (y)
# angles$XYbearing=(angles$XYbearing %% 360 +360) %% 360 # limit so that (360 >= x >= 0)

### note - 1 point results in an NA bearing (face 31) because lat and long points are the same in BGM file. ###

### With new BGM, face numbering changed, use to verify points, angles, segments, numbering...
# plot(angles$lon1[1], angles$lat1[1], type='n', xlim=c(-75, -65), ylim=c(35,45)) #empty plot
# segments(angles$lon1, angles$lat1, angles$lon2, angles$lat2) # draw relevant faces
# points(angles$lon1[1], angles$lat1[1], col='red')
# points(angles$lon2[1], angles$lat2[1], col='red')
# points(angles$lon1[2], angles$lat1[2], col='blue')
# points(angles$lon2[2], angles$lat2[2], col='blue')
# points(angles$lon1[3], angles$lat1[3], col='green')
# points(angles$lon2[3], angles$lat2[3], col='green') # etc...


## which box does each point fall in
index_box <- function(box_sp, roms_ll) {
  ind <- sp::over(project_to(coords_points(roms_ll), box_sp) , as(box_sp, "SpatialPolygons"))
#  tibble(box = ind,
  tibble(box = box_sp$label[ind], 
         cell = seq_len(ncell(roms_ll))) %>% 
    filter(!is.na(box))
}


## build the index for each box to the ROMS cells it contains
## and each face for the ROMS cells it traverses
# box_roms_index <- index_box(boxes, roms_ll)
# box_roms_uindex <- index_box(boxes, roms_ll_u)
# box_roms_vindex <- index_box(boxes, roms_ll_v)
box_roms_rhoindex <- index_box(boxes, roms_ll_rho)

### RM try merging at this level....
# box_roms_uv_index=merge(box_roms_uindex, box_roms_vindex, by='box')

# ind_face <- cellFromLine(romsdata(roms_file, "u"), roms_face)
# face_roms_index <- tibble(face = roms_face$label[rep(seq_len(nrow(roms_face)), lengths(ind_face))],
#                           cell = unlist(ind_face))
ind_face_u <- cellFromLine(romsdata(roms_file, "u_eastward"), roms_face_rho)
ind_face_v <- cellFromLine(romsdata(roms_file, "v_northward"), roms_face_rho)

# ind_face_uv=list(ind_face_u, ind_face_v)

face_roms_vindex <- tibble(face = roms_face_rho$label[rep(seq_len(nrow(roms_face_rho)), lengths(ind_face_v))], 
                          cell = unlist(ind_face_v))
face_roms_uindex <- tibble(face = roms_face_rho$label[rep(seq_len(nrow(roms_face_rho)), lengths(ind_face_u))], 
                           cell = unlist(ind_face_u))

# face_roms_uv_index <- tibble(face = roms_face_u$label[rep(seq_len(nrow(roms_face_u)), lengths(ind_face_u))], 
                           # cell = unlist(ind_face_u))
# face_roms_uvindex=merge(face_roms_uindex, face_roms_vindex, by=unique('face'))



#' return the ramp of positive depths from surface down 
#' (so that the order is native to the NetCDF order)
roms_level <- function(Cs_r, h, cell) {
  extract(h, cell) *  Cs_r
}

## important to readAll here, else extract is very slow in the loop
h <- readAll(raster(roms_file, varname = "h", ncdf=T)) # depth raster
## Cs_r is the S-coord stretching
Cs_r <- rawdata(roms_file, "Cs_r")

### this is the angle between XI-axis and East
# can use this to reconstruct difference between bgm and rotated grid for direction of flux
ang=readAll(raster(roms_file, varname="angle", ncdf=T))



## build the level index between Atlantis and ROMS
# list_nc_z_index <- vector('list', nrow(box_roms_index))
list_nc_z_rhoindex <- vector('list', nrow(box_roms_rhoindex))
list_nc_z_uindex <- vector('list', nrow(face_roms_uindex))
list_nc_z_vindex <- vector('list', nrow(face_roms_vindex))


# max_depth <- max(extract(h, unique(box_roms_index$cell)))
# atlantis_depths <- -rev(cumsum(c(0, rev(rbgm::build_dz(-max_depth)))))
max_depth <- 500 # max(extract(h, unique(box_roms_index$cell)))
atlantis_depths <- -rev(cumsum(c(0, rev(rbgm::build_dz(-max_depth, zlayers = c(-500, -300, -120, -50, 0)))))) # Specific to the NEUS model

# RM 20180319 checking depths... something not right, 4 levels for boxes  0  3  6  9 14 15 20 21 26 27 28 29
# find atlantis_level of the depth of the bottom layer cell in box
test=box_roms_rhoindex #copy
for (i in seq_len(nrow(box_roms_rhoindex))) {
  test$z[i] <- roms_level(Cs_r, h, box_roms_rhoindex$cell[i])
  test$ntrvl[i]=findInterval(test$z[i], atlantis_depths)
  test$nczrhoindex[i]=length(atlantis_depths) - findInterval(test$z[i], atlantis_depths, all.inside = T)
}
test2=test[which(test$box=='Box20'),]
table(test2$nczrhoindex)

for (i in seq_len(nrow(box_roms_rhoindex))) {
  rl <- roms_level(Cs_r, h, box_roms_rhoindex$cell[i])
  ## implicit 0 at the surface, and implicit bottom based on ROMS
  list_nc_z_rhoindex[[i]] <- length(atlantis_depths) - findInterval(rl, atlantis_depths, all.inside = T) # + 1
  # list_nc_z_rhoindex[[i]][which(list_nc_z_rhoindex[[i]]==length(atlantis_depths)]=NA ### remove depths greater than deepest
if (i %% 1000 == 0) print(i)
}
# 
# ll <- extract(roms_ll, box_roms_index$cell)
# box_roms_index$lon <- ll[, 1]
# box_roms_index$lat <- ll[, 2]
# library(ggplot2)
# ggplot(box_roms_index, aes(lon, lat, colour = box)) + geom_point(pch = ".")
# 

## join the box-xy-index to the level index using rho coordinates
box_z_index <- bind_rows(lapply(list_nc_z_rhoindex, 
                 function(x) tibble(atlantis_level = pmax(1, x), roms_level = seq_along(x))), 
          .id = "cell_index") %>% 
  inner_join(mutate(box_roms_rhoindex, cell_index = as.character(row_number()))) %>% 
  select(-cell_index)


# ll <- extract(roms_ll, box_z_index$cell)
# box_z_index$lon <- ll[, 1]
# box_z_index$lat <- ll[, 2]
# library(ggplot2)
# ggplot(box_z_index, aes(lon, lat, colour = roms_level)) + geom_point(pch = ".") + 
#   facet_wrap(~atlantis_level)

### for u indices
for (i in seq_len(nrow(face_roms_uindex))) {
  rl <- roms_level(Cs_r, h, face_roms_uindex$cell[i])
  ## implicit 0 at the surface, and implicit bottom based on ROMS
  list_nc_z_uindex[[i]] <- length(atlantis_depths) -findInterval(rl, atlantis_depths) #+ 1
  list_nc_z_uindex[[i]][which(list_nc_z_uindex[[i]]==5)]=NA ### remove depths greater than 500
  if (i %% 1000 == 0) print(i)
}
## join the face-xy-index to the level index
face_z_uindex <- bind_rows(lapply(list_nc_z_uindex, 
                                function(x) tibble(atlantis_level = x, roms_level = seq_along(x))), 
                         .id = "cell_index")  %>% 
  inner_join(mutate(face_roms_uindex, cell_index = as.character(row_number()))) %>% 
  select(-cell_index)


### for v indices
for (i in seq_len(nrow(face_roms_vindex))) {
  rl <- roms_level(Cs_r, h, face_roms_vindex$cell[i])
  ## implicit 0 at the surface, and implicit bottom based on ROMS
  list_nc_z_vindex[[i]] <- length(atlantis_depths) -findInterval(rl, atlantis_depths) #+ 1
  list_nc_z_vindex[[i]][which(list_nc_z_vindex[[i]]==5)]=NA ### remove depths greater than 500
  if (i %% 1000 == 0) print(i)
}
## join the face-xy-index to the level index
face_z_vindex <- bind_rows(lapply(list_nc_z_vindex, 
                                  function(x) tibble(atlantis_level = x, roms_level = seq_along(x))), 
                           .id = "cell_index") %>% 
  inner_join(mutate(face_roms_vindex, cell_index = as.character(row_number()))) %>% 
  select(-cell_index)

### merge face_z_uindex with face_zvindex at overlapping points

# face_z_uvindex=semi_join(face_z_uindex, face_z_vindex, by=c("face", "atlantis_level", "roms_level"))
# face_z_uvindex=inner_join(face_z_uindex, face_z_vindex, by=c("atlantis_level", "roms_level","face"))
# face_z_uvindex=semi_join(face_z_uindex, face_z_vindex, by=c("atlantis_level", "roms_level","face"))

# test=merge(face_z_uindex, face_z_vindex, by=c("atlantis_level", "roms_level",as.character("face")))

# face_z_uvcomplete=face_z_uvindex[complete.cases(face_z_uvindex),]

#z_index$atlantis_depth <- c(0, rev(rbgm::build_dz(box_roms_index$botz[i])))[z_index$atlantis_level]
#z_index$roms_depth <- rl[z_index$roms_level]
## driver function to loop over x raster by levels
## matching $cell for the right group
extract_at_level <- function(x, cell_level) {
  ulevel <- unique(cell_level$level)
  values <- numeric(nrow(cell_level))
  for (ul in seq_along(ulevel)) {
    asub <- cell_level$level == ulevel[ul]
    values[asub] <- extract(x[[ulevel[ul]]], 
            cell_level$cell[asub])
  }
  values
}

set_indextent <- function(x) {
  setExtent(x, extent(0, ncol(x), 0, nrow(x)))
}

# add data for max numlayers for Atlantis model
countZLayer=apply(dz_box[,2:5], c(1,2), function(x) any(is.finite(x)))
NEUSz=data.frame(dz_box[,1]); colnames(NEUSz)='.bx0'
NEUSz$NEUSlevels=rowSums(countZLayer)

#file_db <- file_db[1, ]
box_props <- face_props <- face_props_sum <- vector("list", nrow(file_db))
i_timeslice <- 1
for (i_timeslice in seq(nrow(file_db))) {
  print(i_timeslice)
  roms_file <- file_db$fullname[i_timeslice]
  level <- file_db$band_level[i_timeslice]
  # ru <- set_indextent(brick(roms_file, varname = "u", lvar = 4, level = level, ncdf=T))
  ru_e <- set_indextent(brick(roms_file, varname = "u_eastward", lvar = 4, level = level, ncdf=T))
  # face_z_uindex$u <- extract_at_level(ru, rename(face_z_uindex, level = roms_level, cell = cell))
  face_z_uindex$ue <- extract_at_level(ru_e, rename(face_z_uindex, level = roms_level, cell = cell))
  # rv <- set_indextent(brick(roms_file, varname = "v", lvar = 4, level = level, ncdf=T))
  rv_n <- set_indextent(brick(roms_file, varname = "v_northward", lvar = 4, level = level, ncdf=T))
  # rdir=atan2(rv, ru) # direction in radians for u_eastward and v_northward
  # rdirdeg=rdir*(180/pi) # direction in degrees
  # face_z_vindex$v <- extract_at_level(rv, rename(face_z_vindex, level = roms_level, cell = cell))
  face_z_vindex$vn <- extract_at_level(rv_n, rename(face_z_vindex, level = roms_level, cell = cell))
  rw <- set_indextent(brick(roms_file, varname = "w", lvar = 4, level = level, ncdf=T))
  box_z_index$w <- extract_at_level(rw, rename(box_z_index, level = roms_level, cell = cell))
  # face_z_index$w <- extract_at_level(rw, rename(face_z_index, level = roms_level, cell = cell))
  rdata <- set_indextent(brick(roms_file, varname = "temp", lvar = 4, level = level, ncdf=T))
  box_z_index$temp <- extract_at_level(rdata, rename(box_z_index, level = roms_level, cell = cell))
  rdata <- set_indextent(brick(roms_file, varname = "salt", lvar = 4, level = level, ncdf=T))
  box_z_index$salt <- extract_at_level(rdata, rename(box_z_index, level = roms_level, cell = cell))
  # box_props[[i_timeslice]] <- box_z_index %>% group_by(atlantis_level, box) %>% 
  #   summarize(temp = mean(temp, na.rm = TRUE), salt = mean(salt ,na.rm = TRUE), vertflux=mean(w, na.rm=T)) %>% 
  #   mutate(band_level = level)
  # # face_props[[i_timeslice]] <-  face_z_index %>% group_by(atlantis_level, face) %>% 
  # #   summarize(flux = mean(sqrt(u * u + v * v), na.rm = TRUE) * 24 * 3600) %>% 
  # #   mutate(band_level = level)
  # ### drop cell from face_z_uindex and _vindex to merge
  # # face_z_uindex$cell=NULL
  # # face_z_vindex$cell=NULL
  # face_z_uv_index=left_join(face_z_uindex, face_z_vindex, by = c("atlantis_level", "roms_level", "face")) # join u and v indices together
  # face_props[[i_timeslice]] <-  face_z_uv_index %>% group_by(atlantis_level, face) %>% 
  #   summarize(velocity = mean(sqrt(ue^2 + vn^2), na.rm = TRUE), dir.uv=atan2(mean(vn, na.rm=T),mean(ue, na.rm=T)), na.rm=T) %>% 
  #   mutate(band_level = level)
  # face_props_sum[[i_timeslice]] <-  face_z_uv_index %>% group_by(atlantis_level, face) %>% 
  #   summarize(velocity = sum(sqrt(ue^2 + vn^2), na.rm = TRUE), dir.uv=atan2(sum(vn, na.rm=T),sum(ue, na.rm=T)), na.rm=T) %>% 
  #   mutate(band_level = level)
  ### added to get missing data back in as NA dimensions should be 30x4=120 for each date
  # note - ungroup and complete (both vars) needed to get to desired dimension, works now
  box_z_index2=left_join(box_z_index, bgm$boxes[c("label", ".bx0")], by=c("box"="label")) ## add proper box number to sort on
  ### RM 20180320 drop data (set NA) in boxes deeper than atlantis_depth by box numberusing NEUSz (above)
  box_z_index2=left_join(box_z_index2, NEUSz, by='.bx0') ## add number of total NEUS Atlantis levels per box
  idx=test$atlantis_level>test$NEUSlevels # index where number of levels in roms is greater than atlantis box depth
  box_z_index2[idx,'w']=NA
  box_z_index2[idx,'salt']=NA
  box_z_index2[idx,'temp']=NA
  box_props[[i_timeslice]] <- box_z_index2 %>% group_by(atlantis_level, .bx0) %>% 
    summarize(temp = mean(temp, na.rm = TRUE), salt = mean(salt ,na.rm = TRUE), vertflux=mean(w, na.rm=T)) %>% 
    ungroup(box_z_index2)%>%
    complete(atlantis_level, .bx0)%>%
    mutate(band_level = level)
  face_z_uv_index=left_join(face_z_uindex, face_z_vindex, by = c("atlantis_level", "roms_level", "face")) # join u and v indices together
  face_z_uv_index2=left_join(face_z_uv_index, bgm$faces[c("label", ".fx0")], by=c("face"="label")) ## add proper box number to sort on
  ### old method -> 604 entries per time (151*4)
  # face_props[[i_timeslice]] <-  face_z_uv_index2 %>% group_by(atlantis_level, .fx0) %>% 
  #   summarize(velocity = mean(sqrt((ue*ue) + (vn*vn)), na.rm = TRUE), dir.uv=atan2(sum(vn, na.rm=T),sum(ue, na.rm=T)), na.rm=T) %>% 
  #   ungroup(face_z_uvindex2) %>%
  #   complete(atlantis_level, .fx0) %>%
  #   mutate(band_level = level)
  ### RM mod 20180320 *** MEAN *** -> good, direction is same as previous, drop complete cases to reduce NAs -> 379(5) per time
  # face_z_uv_index2=left_join(face_z_uv_index2, NEUSz, by='.bx0') ## add number of total NEUS Atlantis levels per box
  # idx=face_z_uv_index2$atlantis_level>face_z_uv_index2$NEUSlevels # index where number of levels in roms is greater than atlantis box depth
  # box_z_index2[idx,'w']=NA
  # box_z_index2[idx,'salt']=NA
  # box_z_index2[idx,'temp']=NA
  face_props[[i_timeslice]] <-  face_z_uv_index2 %>% group_by(atlantis_level, .fx0) %>% 
    summarize(velocity = sqrt((mean(ue, na.rm=T)^2) + (mean(vn, na.rm = TRUE)^2)), 
              dir.uv=atan2(mean(vn, na.rm=T),mean(ue, na.rm=T)), na.rm=T) %>% 
    ungroup(face_z_uvindex2) %>%
    complete(atlantis_level, .fx0) %>%
    mutate(band_level = level)
  
}
# NOTE RM 20180112 - these need to be means, not sums. Then these will be scaled 0-1 depending on their angle
# relative to the face they cut across.
# Final flows need to be aggregated in time (per day, hydrocontruct may take care of that)
### note, this is not handling NA properly... als0 may need to limit to atlantis depths -> both fixed with 20180320 update

### Note, direction atan2 function gives radians, not degrees, which is needed for later calculation of relative angle
## HOWEVER, range is from -pi to pi, must standardize to 0:2pi!!! (20180117)
# DROP degree conversion previously was: degrees=atan2(mean(vn, na.rm=T),mean(ue, na.rm=T))*(180/pi)
# rdir=atan2(rv, ru) # direction in radians for u_eastward and v_northward
# rdirdeg=rdir*(180/pi) # direction in degrees

box_props <- bind_rows(box_props)
face_props <- bind_rows(face_props)
# face_props=face_props[complete.cases(face_props$velocity),] # drop NAs for island faces, added 20180320
# face_props_sum <- bind_rows(face_props_sum) # not used anymore, incorrect
### remove data from islands
box_props[which(box_props$.bx0==23 | box_props$.bx0==24), c('temp', 'salt', 'vertflux')]=-999 #islands
box_props[is.na(box_props$vertflux),c('temp', 'salt', 'vertflux')]=-999 # change NA to fill value

### found issue with depth layers -> fixed 20180320 RM
# table(box_z_index2$atlantis_level)
# test=box_props[which(box_props$atlantis_level==4),]
# test2=test[1:30,]
# test2$.bx0[which(test2$temp>-990)]


### may not be needed anymore... was converting to degrees but this should be in radians as is
### limit so that (360 >= x >= 0)# XY face angles N is 90, E is 0
# face_props$dir.uv=(face_props$dir.uv %% 360 +360) %% 360 

## relative angle between face and current velocity - theta to use in cosine function to scale flux
### example below:
# a=130 *pi/180 ### current angle in radians
# b=30 *pi/180 ### face angle in radians
# c=a-b
# d=(c+180) %% 360 - 180 
# theta=d+(pi/2) # THIS IS THETA, use in cosine to adjust flux across face relative to angled flows, 90 degrees added to normalize to N/S
# THETAscale=cos(theta) # is the scalar for fluxes, 
### NOTE positive cos(theta) indicates flows from left to right; negative cos(theta) is flow from right to left -> destination box
### may be hemispere local... this is for northern hemisphere

angles2=data.frame(angles$XYbearingRhumb)*(pi/180) # convert to radians
angles2$face=angles$face
angles2$dist=distMeeus(cbind(angles$lon1, angles$lat1), cbind(angles$lon2, angles$lat2)) # in meters, WGS84 default
## testing - check XYbearing against these points to make sure it is reasonable
# plot(angles$y1[1]~angles$x1[1], type='n', xlim=c(-76, -64), ylim=c(36, 46))
# for(i in 1:151){
# points(angles$lon1[i], angles$lat1[i], pch=19, col=ifelse(i%%2, 'red', 'blue'))
# points(angles$lon2[i], angles$lat2[i], pch=19, col=ifelse(i%%2, 'red', 'blue'))
# }

### Fix direction of currents from face_props2 to positive radians (atan2 func gives -pi:pi; change -> 0:2pi)
range(face_props$dir.uv, na.rm=T)*180/pi
try=(face_props$dir.uv)*180/pi
try1=(face_props$dir.uv)
try2=try1
try2[which(!is.na(try1) & try1<0)]=try1[which(!is.na(try1) & try1<0)]+2*pi # normalize 0 to 2pi
range(try1, na.rm=T)
range(try2, na.rm=T)
test=data.frame(try1[1:500], try2[1:500])
test$add=NA
test$add[which(test$try1.1.500.<0)]=test$try1.1.500.[which(test$try1.1.500.<0)]+2*pi
# test=face_props2$dir.uv
# sum(is.na(test))
# test[which(!is.na(test) & test<0)]=test[which(!is.na(test) & test<0)]+2*pi # normalize 0 to 2pi
# ## test[which(!is.na(test)>2*pi)]=test[which(!is.na(test)>2*pi)]-2*pi
# range(test,na.rm=T)
face_props$dir.uv[which(!is.na(face_props$dir.uv) & face_props$dir.uv<0)]=face_props$dir.uv[which(!is.na(face_props$dir.uv) & face_props$dir.uv<0)]+2*pi # normalize 0 to 2pi
# face_props_sum$dir.uv[which(!is.na(face_props_sum$dir.uv) & face_props_sum$dir.uv<0)]=face_props_sum$dir.uv[which(!is.na(face_props_sum$dir.uv) & face_props_sum$dir.uv<0)]+2*pi # normalize 0 to 2pi

### get quadrant of face to use to determine which direction flow is going (left:right or right:left)
angles2$quadrant=NA
for (i in 1:length(angles2$quadrant)){
  if (is.na(angles2$angles.XYbearingRhumb[i])){
    next
  } else if (angles2$angles.XYbearingRhumb[i] <= (90*pi/180)){
    angles2$quadrant[i]=1
  } else if (angles2$angles.XYbearingRhumb[i] <=(180*pi/180)){
    angles2$quadrant[i]=2
  } else if (angles2$angles.XYbearingRhumb[i] <=(270*pi/180)){
    angles2$quadrant[i]=3
  } else if (angles2$angles.XYbearingRhumb[i] <=(360*pi/180)){
    angles2$quadrant[i]=4
  }
}

### add some BGM box info, used later on merge and for hyperdiffusion correction
box.area=data.frame(bgm$boxes$area)
box.area$box=bgm$boxes$.bx0
box.area$nconn=bgm$boxes$nconn  # used to determine max number of connections in .NC hydro forcing file
box.area$z=bgm$boxes$botz
box.area$volume=box.area$bgm.boxes.area*box.area$z*-1

#add BGM info on faces gets left right boxes looking from p1 to p2 to use with 'angles'
# face_props2=left_join(face_props_sum, bgm$faces, by=".fx0") # uses sum - transport values look more reasonable after hyperdiff correction
face_props2=left_join(face_props, bgm$faces, by=".fx0") # 
# face_props2=left_join(face_props, bgm$faces, by=c("face"="label")) # uses mean - yields small transport with hyperdiffusion

# https://stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
# face_props2$sloperad=(face_props2$sine/face_props2$cosine)
### Note that u_east and v_north were used for direction calculation, so no rotation is necessary for current direction
face_props2=left_join(face_props2, angles2, by=c(".fx0"="face"))
# face_props2$theta=((((face_props2$dir.uv*pi/180)-(face_props2$angles.XYbearingRhumb*pi/180)+180) %% 360 -180)+pi/2)
# face_props2$theta=((((face_props2$dir.uv - face_props2$angles.XYbearingRhumb)+180) %% 360 - 180)+pi/2)
# face_props2$cos_theta=cos(face_props2$theta)

### THIS DETERMINES DIRECTION OF FLOW: pos values flow R:L, neg flows L:R from p1 looking to p2 of a face
### This is correct 20180322, the relative angle method below works and is correct. Positive rel_angle indicates flow 
### from R:L across a face, and L:R has a negative rel_angle. THE CONVERSION TO 0:2pi is NOT NEEDED AND MAY BE WRONG
face_props2$rel_angle=atan2(sin(face_props2$dir.uv -face_props2$angles.XYbearingRhumb), cos(face_props2$dir.uv -face_props2$angles.XYbearingRhumb))

# 20180322 Updata: copy prior to crazyness below... THIS MAY NOT BE CORRECT
# face_props2$rel_angle_std=face_props2$rel_angle
# face_props2$rel_angle_std[which(!is.na(face_props2$rel_angle) & face_props2$rel_angle<0)]=face_props2$rel_angle_std[which(!is.na(face_props2$rel_angle) & face_props2$rel_angle<0)]+2*pi # normalize 0 to 2pi
### CHECK ON ADDITION OF 90 Degrees... 20180322
face_props2$cos_theta=abs(cos(face_props2$rel_angle + pi/2)) ### this is the scalar for flux velocity across face, plus 90 degrees rotation
# face_props2$cos_theta_std=abs(cos(face_props2$rel_angle_std + pi/2)) ### this is the scalar for flux velocity across face, plus 90 degrees rotation


### added 2018/1/23 from stack overflow link above
## this works out exactly the same as the first method as a scalar for the fluxes
face_props2$a=(face_props2$dir.uv*180/pi)-(face_props2$angles.XYbearingRhumb*180/pi) #targetA - sourceA
# face_props2$a.rad=face_props2$a*180/pi
face_props2$aA=(face_props2$a+180)%%360-180 # smallest angle between 2 lines
face_props2$aA.rad=face_props2$aA*pi/180
face_props2$cos_theta2=abs(cos(face_props2$aA.rad + pi/2)) ### this is the scalar for flux velocity across face, plus 90 degrees rotation
face_props2$cos_theta1=abs(cos(face_props2$rel_angle + pi/2)) ### this is the scalar for flux velocity across face, plus 90 degrees rotation


### direction of flow depends on quadrant the face lies in: (this may be hemisphere dependent???)
### where x=current direction in XY coords, y=bearing of face from p1 to p2 (rhumbline) in XY coords (all in radians)
## I if ((x-y) > 0 & (x-(y+pi/2) < 0) then flow is right to left (positive flux)
##   if ((x-y) < 0 | (x+(y+pi/2)) > 0) then flow is L:R (negative flux)
## II (same as I)
## III if ((x-(y-pi/2)) < 0 | (x-y) > 0) flow is R:L
##   if ((x-y) <0 & (x-(y-pi/2)) > 0) flow is L:R
## IV if ((x-(y-pi/2)) > 0 & (x-y) < 0) flow is L:R
##   if ((x-(y-pi/2))<0 | (x-y) > 0 ) flow is R:L (positive)
face_props2$destbox=NA
face_props2$destbox.lr=NA
face_props2$fluxsign=NA
for (i in 1:length(face_props2$left)){
  if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$atlantis_level)*100, '%'))
  x=face_props2$dir.uv[i]
  y=face_props2$angles.XYbearingRhumb[i]
  if (is.na(face_props2$cos_theta[i]) | is.na(face_props2$quadrant[i])){
    next
  } else if (face_props2$quadrant[i]==1 | face_props2$quadrant[i]==2){
      if ((x-y) > 0 & (x-(y+pi/2) < 0)){
        face_props2$destbox[i]=face_props2$left[i]
        face_props2$fluxsign[i]=1 #positive flux R:L
        face_props2$destbox.lr[i]='l'
      } else {
        face_props2$destbox[i]=face_props2$right[i]
        face_props2$fluxsign[i]=-1 #negative flux L:R
        face_props2$destbox.lr[i]='r'
      } 
  } else if (face_props2$quadrant[i]==3 | face_props2$quadrant[i]==4){
    if ((x-y) < 0 & (x-(y-pi/2) > 0)){
      face_props2$destbox[i]=face_props2$right[i]
      face_props2$fluxsign[i]=-1 # negative flux L:R
      face_props2$destbox.lr[i]='r'
    } else {
      face_props2$destbox[i]=face_props2$left[i]
      face_props2$fluxsign[i]=1 #positve flux R:L
      face_props2$destbox.lr[i]='l'
    }
  }
}

# add area -> length of face x depth from dz file... need to merge on dz_box "polygon"="left" (same for "right" later) && "l1"="atlantis_level==1"
# depth$boxleftZ[which(depth$boxleftZ==1)] <- dz_box$`l1`[match(face_props2$atlantis_level$left, dz_box$polygon)]
face_props2=left_join(face_props2, dz_box, by=c("left"="polygon")) ### l1.x-l4.x is boxleft
face_props2=left_join(face_props2, dz_box, by=c("right"="polygon")) ### l1.y-l4.y is boxright
face_props2$Sediment_1.x=NULL
face_props2$Sediment_1.y=NULL


##### testing difference of angles... to see how orientation affects flow from R:L or L:R
##  x is target angle
##  y is starting angle
## theta=atan2(sin(x-y), cos(x-y)) ### this determines flow direction: if this is (+) flow is R:L; if (-) L:R
# from: stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
#____________________________________________________________
# test=data.frame(atan2(sin(face_props2$dir.uv -face_props2$angles.XYbearingRhumb), cos(face_props2$dir.uv -face_props2$angles.XYbearingRhumb)))
# colnames(test)[1]='atan2'
# test$bear=face_props2$angles.XYbearingRhumb
# test$curr=face_props2$dir.uv
# test$deg=test[,1]*180/pi # degrees of angle difference between vector and face
# # test$cos_atan2=cos(test[,1]) ### this is shifted 90 degrees (pi/2)
# test$theta=test[,1]+pi/2 ### add 90 degrees in order to take cosine
# test$cos_2=cos(test$theta) ### this is the correct scalar for velocity

### 20180323 NOTE: RM the above (long method) does not seem correct, the 'rel_angle (non standardized) gives the direction of the flow, the above does not agree
# face_props2$destbox=NA
# face_props2$destbox.lr=NA
# face_props2$fluxsign=NA #... may just fill these vals with data from face_props
test=face_props2[,c("rel_angle", "fluxsign")]
test$q=face_props2$quadrant
test$ra=ifelse(test$rel_angle>0, 1, -1)
table(test$q[which(test$ra != test$fluxsign)])
test2=test[complete.cases(test),]
table(test2$q[which(test2$ra != test2$fluxsign)]) # so not an NA issue...

### try this instead: 20180323
face_props2$destbox.new=ifelse(face_props2$rel_angle>0, face_props2$left, face_props2$right) #R to L is pos flow, LtoR neg
face_props2$origbox.new=ifelse(face_props2$rel_angle<0, face_props2$left, face_props2$right) #R to L is pos flow, LtoR neg
face_props2$destbox.lr.new=ifelse(face_props2$rel_angle>0, 'l', 'r') # probably not needed
face_props2$fluxsign.new=ifelse(face_props2$rel_angle>0, 1, -1)
### 20% of flux signs are different... depending on method (fluxsign.new is correct 20180325)
test=face_props2$fluxsign!=face_props2$fluxsign.new
sum(test, na.rm=T)/(length(face_props2$fluxsign)-sum(is.na(face_props2$fluxsign))) 

### add number of levels for box left (x) and boxright(y)
face_props2=left_join(face_props2, NEUSz, by=c("destbox.new"=".bx0")) ### NEUSlevels.x is for destination box
face_props2=left_join(face_props2, NEUSz, by=c("origbox.new"=".bx0"),suffix = c("_dest", "_orig")) # rename dest and orig
# face_props2$levels_diff=ifelse(face_props2$fluxsign>1, face_props2$NEUSlevels.x- face_props2$NEUSlevels.y, 
#                                face_props2$NEUSlevels.y - face_props2$NEUSlevels.x) # find out if dest box has fewer layers than origin box

### manually try this... 20180323, this works, slow, could make bottom more like top, also add face area...
# face_props2$orig_z=NA
# face_props2$dest_z=NA
face_props2$orig_z=ifelse((!is.na(face_props2$fluxsign) & face_props2$atlantis_level==1), 1, NA)
face_props2$dest_z=ifelse((!is.na(face_props2$fluxsign) & face_props2$atlantis_level==1), 1, NA) # misses areas with NA (islands...) ->fixed
face_props2$facearea=NA 

### origin level - added 20180325
face_props2$orig_z=ifelse(face_props2$atlantis_level <= face_props2$NEUSlevels_orig, face_props2$atlantis_level, face_props2$NEUSlevels_orig)
### destination level - added 20180325
face_props2$dest_z=ifelse(face_props2$atlantis_level <= face_props2$NEUSlevels_dest, face_props2$atlantis_level, face_props2$NEUSlevels_dest)
dz_box2=dz_box[,c(5,4,3,2,1)] # get depths of boxes, reverse order 1=shallow, 4=deep NEUS ONLY
### depth of origin box * lenght of face added 20180325
face_props2$facearea=NA
for (i in 1:length(face_props2$atlantis_level)){
  if (is.na(face_props2$fluxsign[i])){
    next
  } else {
    face_props2$facearea[i]=dz_box2[(face_props2$origbox.new[i]+1) ,face_props2$orig_z[i]]*face_props2$length[i]
  }
}

# face_props2$facearea=apply(face_props2, function(x) dz_box2[face_props2$origbox.new ,face_props2$orig_z]*face_props2$length)

### commented out 20180325
# for(i in 1:length(face_props$atlantis_level)){
#   if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$atlantis_level)*100, '%'))
#   if (is.na(face_props2$fluxsign[i])){
#     next
#   } else if (face_props2$atlantis_level[i]==2){
#     face_props2$orig_z[i]=2
#     face_props2$dest_z[i]=ifelse(face_props2$NEUSlevels_dest[i] >= 2, 2, face_props2$NEUSlevels_dest[i])
#   } else if (face_props2$atlantis_level[i]==3){
#     face_props2$orig_z[i]=2
#     face_props2$dest_z[i]=ifelse(face_props2$NEUSlevels_dest[i] >= 3 , 3, face_props2$NEUSlevels_dest[i])
#   } else if (face_props2$atlantis_level[i]==4){
#     face_props2$orig_z[i]=4
#     face_props2$dest_z[i]=ifelse(face_props2$NEUSlevels_dest[i] >= 4, 4, face_props2$NEUSlevels_dest[i])
#   }
# }


### Commented out 20180325, replaced with above
# ### Get area by box
# ### match atlantis_depth to layers l1-l4: x is left box, y is right box looking from p1 to p2 in 'faces'
# face_props2$orig_z=NA
# face_props2$dest_z=NA
# face_props2$dest_z_layer=NA
# face_props2$facearea=NA ### this is the area of the face
# # face_props2=face_props2[complete.cases(face_props2$atlantis_level),] ### drop where depth is > max depth
# for (i in 1:length(face_props2$atlantis_level)){
#   if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$atlantis_level)*100, '%'))
#   ### for the right box as destination box, face_props$rel_angle < 0
#   if (is.na(face_props2$destbox.lr[i])){
#     next
#   } else if (face_props2$destbox.lr[i]=='l'){
#     face_props2$orig_z[i]=face_props2$l1.y[i]
#     face_props2$dest_z[i]=face_props2$l1.x[i]
#     if (face_props2$atlantis_level[i]==1){
#       face_props2$facearea[i]=face_props2$l1.x[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=1
#     } else if (face_props2$atlantis_level[i]==2){
#       face_props2$facearea[i]=face_props2$l2.x[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=2
#     } else if (face_props2$atlantis_level[i]==3){
#       face_props2$facearea[i]=face_props2$l3.x[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=3
#     } else if (face_props2$atlantis_level[i]==4){
#       face_props2$facearea[i]=face_props2$l4.x[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=4
#     } else if (face_props2$atlantis_level[i]==5){
#       face_props2$facearea[i]=face_props2$l4.x[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=4
#     }
#   } else if (face_props2$destbox.lr[i]=='r') { 
#     face_props2$orig_z[i]=face_props2$l1.x[i]
#     face_props2$dest_z[i]=face_props2$l1.y[i]
#     if (face_props2$atlantis_level[i]==1){
#       face_props2$facearea[i]=face_props2$l1.y[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=1
#     } else if (face_props2$atlantis_level[i]==2){
#       face_props2$facearea[i]=face_props2$l2.y[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=2
#     } else if (face_props2$atlantis_level[i]==3){
#       face_props2$facearea[i]=face_props2$l3.y[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=3
#     } else if (face_props2$atlantis_level[i]==4){
#       face_props2$facearea[i]=face_props2$l4.y[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=4
#     } else if (face_props2$atlantis_level[i]==5){
#       face_props2$facearea[i]=face_props2$l4.y[i]*face_props2$length[i]
#       face_props2$dest_z_layer[i]=4
#     }
#   } 
# }


#### flux= velocity*cos(theta)*area in meters

# face_props2$dest_box=NA
# face_props2$origin_box=NA
face_props2$flux=NA
for (i in 1:length(face_props2$rel_angle)){
  if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$rel_angle)*100, '%'))
  if (is.na(face_props2$destbox.lr[i])){
  next
  } #else if (face_props2$rel_angle >= 0){
  #   face_props2$dest_box[i]=face_props2$left[i]
  #   face_props2$origin_box[i]=face_props2$right[i]
  # } else if (face_props2$rel_angle < 0){
  #   face_props2$dest_box[i]=face_props2$right[i]
  #   face_props2$origin_box[i]=face_props2$left[i]
  # }
  face_props2$flux[i]=face_props2$cos_theta[i] *face_props2$fluxsign.new[i] * face_props2$velocity[i] *face_props2$facearea[i]
}
face_props2$fluxtime=face_props2$flux *3600 ### add time back in to daily mean flows (not sure which to use yet 20180325)

### Hyperdiffusion correction for each flux across a face prior to aggregation into flux into/from polygons
### - E/W flows divided by bbox width of destination polygon in E/W direction
### - N/S flow across a face divided by bbox height of destination polygon in N/S direction

### now correct for hyperdiffusion divide flow into box by box area -> done in hydroconstruct
# intermed=data.frame(face_props2[,c('destbox','flux', '.fx0', 'length', 'facearea')])
# intermed2=left_join(intermed, box.area, by=c("destbox"="box"))
# intermed3=left_join(intermed2, bgm_bbox, by=c("destbox"="box")) ## add N/S E/W distance by box, for hyperdiffusion fix
# intermed3$flux_time=intermed3$flux*86400
# face_props2$flux_hyper=intermed3$flux_time/intermed3$bgm.boxes.area


### simplify, and aggregate fluxes from multiple faces per box back to box level
# Flux=data.frame(face_props2[,c('atlantis_level', 'origin_box', 'dest_box', 'flux', 'dest_z','orig_z', 'band_level')])
# Flux=Flux[complete.cases(Flux$flux),]
# Atlantis_flux=aggregate(Flux$flux, by=list(level=Flux$atlantis_level, dt=Flux$band_level, ob=Flux$origin_box, 
                                           # dz=Flux$dest_z, db=Flux$dest_box, oz=Flux$orig_z), FUN=sum, na.rm=T)

### corrected for input required by hydroconstruct:
# Flux=data.frame(face_props2[,c('origin_box', '.fx0', 'band_level', 'atlantis_level', 'flux', 'flux_hyper')])
# Flux=data.frame(face_props2[,c('origin_box', '.fx0', 'band_level', 'atlantis_level', 'flux', 'flux_hyper')])

# Flux=Flux[complete.cases(Flux$flux),]
# Atlantis_flux=aggregate(Flux, by=list(polygon=Flux$origin_box, face_number=Flux$`.fx0`, 
#                                            Time_step=Flux$band_level, Depth_level=Flux$atlantis_level), FUN=sum, na.rm=T)
# 
# Atlantis_flux_sum=aggregate(Flux$flux_hyper, by=list(polygon=Flux$origin_box, Time_step=Flux$band_level, 
                                                     # Depth_level=Flux$atlantis_level), FUN=sum, na.rm=T)

## still need to deal with the changes in depth between faces depth2 to depth1, etc.
# if oz < dz levels same
# if oz > dz check levels to make sure overlap accounted for, otherwise dest_z is shifted

### need to drop areas with islands - so if `dz` or `oz` == 0, then drop row from Atlantis_flux
# Atlantis_flux2=Atlantis_flux[Atlantis_flux$oz !=0 & Atlantis_flux$dz !=0, ]

# Box_df=left_join(box_props, bgm$boxes, by=c(".bx0"=".bx0"))
# Atlantis_box=data.frame(Box_df[,c('band_level', '.bx0', 'atlantis_level', 'vertflux', 'temp', 'salt')])
# colnames(Atlantis_box)=c('Time step', 'Polygon', 'Depth Layer', 'Vertical Velocity', 'Average Temperature', 'Average Salinity' )
# Atlantis_box[which(Atlantis_box$Polygon==23), 4:6]=NA
# Atlantis_box[which(Atlantis_box$Polygon==24), 4:6]=NA
# 
# 
# write.table(Atlantis_box, file='AtlantisBox2.dat', col.names=T, na="NA", row.names=F)
# write.table(Atlantis_flux, file='AtlantisFlux2.dat', col.names=T, na="NA", row.names=F)

### now correct for hyperdiffusion divide flow into box by box area -> done in hydroconstruct
# intermed=data.frame(Atlantis_flux2[,c('db','x')])
# intermed2=left_join(intermed, box.area, by=c("db"="box"))
# intermed3=left_join(intermed2, bgm_bbox, by=c("db"="box")) ## add N/S E/W distance by box, for hyperdiffusion fix

## from the user manual...
# in hydroconstruct code: option 1 or 2 to correct -
#   divide flow by boxarea of arrival box for simple correction, otherwise:
# divides the east-west water flows by the width of the
# box (in metres) in the east-west orientation and north-
#   south flows by the width of the box in the north-south
# orientation.




# Atlantis_flux2$hyperflux=Atlantis_flux2$x *3600 *24 / intermed2$bgm.boxes.area



### adding to create lookup table similar to Isaac's for Puget sound to use in hydroconstruct
# test=data.frame(bgm$boxes$.bx0)
# colnames(test)='polygon'
# test2=left_join(test, bgm$facesXboxes, by=c('polygon'='.bx0'))
# z2=read.csv('NEUS depth.xlsx')
# test3=left_join(test2, d)
# 
# z3=read.csv('NEUSdepth.csv', header=T)
# try=list()
# lev=list()
# for(i in 1:length(z3$Box)){
#   if (z3$layers[i] > 0){
#     try[[i]]=rep(z3$Box[i], z3$layers[i])
#     lev[[i]]=seq(from=1, to=z3$layers[i], by=1)
#   }else
#     next
# }
# 
# try2=data.frame(unlist(try))
# try2$depth=unlist(lev)
# lev2=unlist(lev)
# colnames(try2)=c('box', 'depth')
# test3=test2[,1:2]
# final=left_join(test3, try2, by=c('polygon'='box'))
# final$last=1
# final$iface=final$iface+1
# write.table(final, file='NEUSlookup.csv', col.names = F, sep=',', row.names = F)

### RM - note: depths are not right, there should be 4 not 5, atlantis layer 1 
# is surface to 50m, layer 2 is 50m... need to average the bottom 4 and 5 values?
#..... Dealt with above, levels are good now FIXED


#### RM added to get indices for code similar to Cecilie's
# for (i in 1:length(ind_face)){
# ind_face[[i]] 
# roms_face@lines[[i]]@Lines[[i]]@coords
# roms_face$left[i]
# }

### Define dimensions for the two NetCDF files that go into hydroconstruct:
# max_neigh=max(box.area$nconn) #25 from BGM file
nboxes=length(unique(box_props$.bx0)) #30
ntimes=length(unique(face_props2$band_level))
nlevel=length(unique(face_props2$atlantis_level))
atl.level=unique(face_props2$atlantis_level)
nfaces=length(unique(face_props2$.fx0))

### fix time dimension - needs to be from 1964 for NEUS
ynum=0 # use for 2008 data (first year of data)
ynum=365 # use for 2009 data (1 year already processed)
ynum=730 # use for 2010 data (2 years already processed)

ocean_time=ncdf4::ncvar_get(ncdf4::nc_open(roms_file), varid='ocean_time') # need to change these:
# t_start=min(unique(Atlantis_flux$Time_step))
t_start=min(unique(face_props2$band_level))+ynum ### MAKE SURE TO SELECT ABOVE ynum CORRECTLY
dt=86400 # seconds in one day
# t_stop=max(unique(Atlantis_flux$Time_step)) #ocean_time[1]+(dim(exch_nc)[4]-1)*86400
t_stop=max(unique(face_props2$band_level))+ynum ### MAKE SURE TO SELECT ABOVE ynum CORRECTLY
t_tot=seq(t_start*dt,t_stop*dt,dt)

## variables in transport file:
faces=angles$face
pt1_x=angles$lon1
pt2_x=angles$lon2
pt1_y=angles$lat1
pt2_y=angles$lat2
dest_boxid=bgm$faces$left
source_boxid=bgm$faces$right # positive flow is right to left across face from p1 to p2
### create 3d array of transport vals
transport=array(NA, dim=c(nlevel, nfaces, ntimes))
for (i in 1:length(face_props2$flux)){
  if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$flux)*100, '%'))
  j=face_props2$band_level[i] #time
  k=face_props2$.fx0[i]+1 ### Face NOTE added 1 because index cannot be 0, must remove later (maybe not?)
  l=face_props2$atlantis_level[i]# depth
  transport[l,k,j]=face_props2$fluxtime[i] # time now added back in (flux per day in seconds)
}

### create vars for box structure data
box.boxes=bgm$boxes$.bx0 ### NOTE added 1 because index cannot be 0, must remove 1 later w/ ncks
salinity=array(NA, dim=c(nlevel,nboxes, ntimes))
temperature=array(NA, dim=c(nlevel,nboxes,ntimes))
vertical_flux=array(NA, dim=c(nlevel,nboxes,ntimes))
for (i in 1:length(box_props$temp)){
  if (i %% 5000 ==0) print(paste('progress:',i/length(box_props$temp)*100, '%'))
  j=box_props$band_level[i] #time
  k=(box_props$.bx0[i])+1 ### box NOTE added 1 because index cannot be 0, must remove later
  l=box_props$atlantis_level[i]# depth
  vertical_flux[l,k,j]=box_props$vertflux[i]
  temperature[l,k,j]=box_props$temp[i]
  salinity[l,k,j]=box_props$salt[i]
}


library(ncdf4)

### FOR TRANSPORT NC FILE
filename="RM_NEUS_transport_2010_20180403_fluxtime_fix.nc"

#define dimensions
timedim=ncdim_def("time", "", 1:length(t_tot), unlim=T, create_dimvar = F) #as.double(t_tot)
leveldim=ncdim_def("level", "", 1:nlevel, create_dimvar = F)
facesdim=ncdim_def("faces", "", 1:nfaces, create_dimvar = F)

## from Cecilie:
# dimd=ncdim_def("dest","",1:max_neigh,create_dimvar=FALSE)
# dimb=ncdim_def("b","",1:tot_bnr,create_dimvar=FALSE)
# dimz=ncdim_def("z","",1:nlay,create_dimvar=FALSE)
# dimt=ncdim_def("t1","",1:length(t_tot),unlim=TRUE)#,create_dimvar=FALSE)

#create variables
#NB!!!!!! Unlimited rec needs to be on the right - otherwise R complains!
#origMissVal_ex=0.0
var.time=ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
var.face=ncvar_def("faces", "", facesdim, longname="Face IDs", prec='integer')
var.lev=ncvar_def("level","",leveldim,longname="layer index; 1=near surface",prec="integer")
var.trans=ncvar_def("transport","m3/s",list(leveldim,facesdim,timedim),0,prec="float")
var.destb=ncvar_def("dest_boxid","id", facesdim,longname="ID of destination box", prec="integer")
var.sourceb=ncvar_def("source_boxid","id", facesdim,longname="ID of source box",prec="integer")
var.pt1x=ncvar_def("pt1_x", "degree_east", facesdim, longname = "x-coord of pt 1 of face", prec='float')
var.pt2x=ncvar_def("pt2_x", "degree_east", facesdim, longname = "x-coord of pt 2 of face", prec='float')
var.pt1y=ncvar_def("pt1_y", "degree_north", facesdim, longname = "y-coord of pt 1 of face", prec='float')
var.pt2y=ncvar_def("pt2_y", "degree_north", facesdim, longname = "y-coord of pt 1 of face", prec='float')

nc_transp=nc_create(filename,list(var.time,var.face, var.lev, var.destb,var.sourceb, var.pt1x, var.pt2x, var.pt1y, var.pt2y,var.trans))

#assign global attributes to file
ncatt_put(nc_transp,0,"title","Transport file, NEUS")
ncatt_put(nc_transp,0,"geometry","neus_tmerc_RM.bgm")
ncatt_put(nc_transp,0,"parameters","")

#assign attributes to variables
ncatt_put(nc_transp,var.time,"dt",86400,prec="double")

#assign variables to file
ncvar_put(nc_transp,var.trans,transport, count=c(nlevel,nfaces, ntimes))
ncvar_put(nc_transp,var.time,t_tot)
ncvar_put(nc_transp,var.destb,dest_boxid)
ncvar_put(nc_transp,var.lev,atl.level)
ncvar_put(nc_transp,var.sourceb,source_boxid)
ncvar_put(nc_transp,var.pt1x,pt1_x)
ncvar_put(nc_transp,var.pt1y,pt1_y)
ncvar_put(nc_transp,var.pt2x,pt2_x)
ncvar_put(nc_transp,var.pt2y,pt2_y)
ncvar_put(nc_transp,var.face,faces)

nc_close(nc_transp)


### For T, S, Vertical Flux NC file
filename="RM_NEUS_variables_2010_20180325_fix.nc"

#define dimensions
timedim=ncdim_def("time", "", 1:length(t_tot), unlim=T, create_dimvar = F) #as.double(t_tot)
leveldim=ncdim_def("level", "", 1:nlevel, create_dimvar = F)
boxesdim=ncdim_def("boxes", "", 1:nboxes, create_dimvar = F)

#create variables
#NB!!!!!! Unlimited rec needs to be on the right - otherwise R complains!
#origMissVal_ex=0.0
var.time=ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
var.box=ncvar_def("boxes", "", boxesdim, longname="Box IDs", prec='integer')
var.lev=ncvar_def("level","",leveldim,longname="layer index; 1=near surface; positice=down" ,prec="integer")
var.vertflux=ncvar_def("verticalflux","m3/s",list(leveldim, boxesdim, timedim),-999,longname="vertical flux averaged over floor of box",prec="float")
var.temp=ncvar_def("temperature","degree_C",list(leveldim, boxesdim, timedim),-999,longname="temperature volume averaged",prec="float")
var.salt=ncvar_def("salinity","psu",list(leveldim,boxesdim,timedim),-999,longname="salinity volume averaged",prec="float")

nc_varfile=nc_create(filename,list(var.time,var.box, var.lev, var.salt, var.temp, var.vertflux))

#assign global attributes to file
ncatt_put(nc_varfile,0,"title","Box averaged properties file, NEUS")
ncatt_put(nc_varfile,0,"geometry","neus_tmerc_RM.bgm")
ncatt_put(nc_varfile,0,"parameters","")

#assign attributes to variables
ncatt_put(nc_varfile,var.time,"dt",86400,prec="double")

#assign variables to file
ncvar_put(nc_varfile,var.vertflux,vertical_flux, count=c(nlevel,nboxes, ntimes))
ncvar_put(nc_varfile,var.time,t_tot)
ncvar_put(nc_varfile,var.lev,atl.level)
ncvar_put(nc_varfile,var.salt,salinity, count=c(nlevel,nboxes, ntimes))
ncvar_put(nc_varfile,var.temp,temperature, count=c(nlevel,nboxes, ntimes))
ncvar_put(nc_varfile,var.box,box.boxes)

nc_close(nc_varfile)

