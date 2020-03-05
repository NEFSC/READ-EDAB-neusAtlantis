#### Diagnostic tests for post-hydroconstruct output (input files for Atlantis) ####
# 1) Boxes exchange to/from all neighboring boxes
# 2) Boxes exchange ONLY to neighboring boxes (no jumps)
# 3) Net flux within box close to 0 for a given time


# Packages ----------------------------------------------------------------

library(ncdf4)
library(ggplot2)
library(raster)
library(rbgm)
library(tictoc)
library(dplyr)
library(gridExtra)
library(shinyrAtlantis)
# Read in box-aggregated data ---------------------------------------------
setwd('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/NEUS_Example/Done/')

flux.nc = nc_open('hydro_out.nc')
# flux.old = nc_open('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/flowOutAll_fix_20180402.nc')
# flux.old = nc_open('C:/Users/joseph.caracappa/Documents/Atlantis/Atlantis_Code/hydroconstruct/trunk/Example/Done/SEAP132_hydro.nc')


#3 vars, from wiki:
# dest_b matrix of destination box of flows from each layer of each box at each timestep
# desk_k matrix of destination layer of flows from each layer of each box at each timestep
# exchange matrix of actual exchanges
# t vector of timestamps associated with each timestep
# 4 dims in dest_b,dest_k, exchange: t number of tsteps; b (30) number of boxes; z (4) max num of layers; dest (32) max num of boxes any box can exchange with
# dest is 32 (2 boxes exchange with Box 0) something weird here

exchange = ncvar_get(flux.nc,'exchange')
# dest_b.old = ncvar_get(flux.old,'dest_b');dest_b.old[,1,,1]
dest_b = ncvar_get(flux.nc,'dest_b');dest_b[,1,,1]
dest_k = ncvar_get(flux.nc,'dest_k')

bgm = bgmfile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm')
load('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_boxes_info.R')

dz_box = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',header=T)
dz_box$nlevel = apply(dz_box[,2:5],1,function(x) sum(!is.na(x)))
level_dz = dz_box[,5:2]

areas = bgm$boxes$area
volumes = level_dz * areas
  
#match table for signs
flux.sign = data.frame(bx.left = c(1,1,0,0),bx.right = c(0,0,1,1),sign = c(0,1,0,1),dir = c(-1,1,1,-1))

boxes = 0:29
#Determine which boxes have box 'b' as a destination

#list of all boxes each box connects to
box.connections = lapply(seq_along(flux.nc$dim$t$vals), function(x){
  data.frame(box = boxes,time = NA, all.connected = NA)
})

#Look at forcing wiht shinyrAtlantis

###sh.prm - Explore biology parameter files
bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm'
salinity.file = 'salt_out.nc'
temperature.file = 'temp_out.nc'
exchange.file = 'hydro_out.nc'
cum.depth = c(0,50,120,300,500,1000)
obj.force = make.sh.forcings.object(bgm.file,exchange.file,cum.depth,temperature.file,salinity.file)

# bgm.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Atlantis_Code/hydroconstruct/trunk/Example/SEAP_14052012_aea.bgm'
# exchange.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/flowOutAll_fix_20180402.nc'
# exchange.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Atlantis_Code/hydroconstruct/trunk/Example/Done/SEAP132_hydro.nc'
# cum.depth = c(0,20,30,50,150,450,3600)
# exchange =nc_open(exchange.file)
# obj.force = make.sh.forcings.object(bgm.file,exchange.file,cum.depth)

#sh.forcings(obj.force)

box.layer.flow = function(b,t){return(cbind(dest_b[,,b+1,t]+(exchange[,,b+1,t]*0),dest_k[,,b+1,t]+(exchange[,,b+1,t])*0,exchange[,,b+1,t]))}
box.faces = function(b) { return(bgm$faces %>% filter(left == b | right == b))}
box.layer.flow(9,1)
box.faces(10)

b = 10
t = 1

  ls = list()
  x = dest_b[,1,,1]
  x.id=rbind(which(x == b,arr.ind=T),cbind(which(!is.na(x[,b+1])),b+1))
  x.id = x.id[!duplicated(x.id),]
  df = data.frame(from = x.id[,2]-1,to = x[x.id] , exch = exchange[,1,,t][x.id])

time.ls = list()
t = 1;bx = 2; lev = 1

for( t in seq_along(flux.nc$dim$t$vals)){
  
  # box.connections[[t]]$time = t
  box.ls = list()
  
  for(bx in seq_along(flux.nc$dim$b$vals)){
    
    #Identifies connected boxes to given box
    dest.box = dest_b[,1,,t]
    box.coords = rbind(which(dest.box==boxes[bx],arr.ind = T),cbind(which(!is.na(dest_b[,1,bx,1])),bx))
    box.coords = box.coords[!duplicated(box.coords),]

    lev.ls = list()
    
    for(lev in seq_along(flux.nc$dim$z$vals)){
      
      #Create a mask of all points with non-NA flux
      b.mask = dest_b[,lev,,t]*NA
      b.mask[box.coords] = 1
      b.mask = b.mask * exchange[,lev,,1]*0+1
      mask.coords =which(!is.na(dest.box*b.mask),arr.ind=T)
      
      if(nrow(mask.coords) == 0){
        next
      }
      #apply mask to dest_b, dest_k, exchange
      box.exch = data.frame(b.orig = mask.coords[,2]-1,
                            b.dest = dest_b[,lev,,t][mask.coords],
                            z.orig = lev,
                            z.dest = dest_k[,lev,,t][mask.coords],
                            exch = exchange[,lev,,t][mask.coords] ,
                            exch.sign = sign(exchange[,lev,,t][mask.coords]),
                            time = t,
                            volume = volumes[bx,lev]
                            )
      box.exch$exch = apply(box.exch, 1, function(x){
        if(is.na(x[5])){
          return(NA)
        }else if(x[1] == boxes[bx] & x[2] == boxes[bx]){
         if(x[3] == (lev-1) & x[6] >= 0){
            return( abs(x[5]))
          } else if(x[4] == (lev-1) & x[6]<=0){
            return( abs(x[5]))
          } else {
            return( -1*abs(x[5]))
          }
        } else if(x[1] == boxes[bx] & x[6] >= 0){
          return( abs(x[5]))
        } else if(x[2] == boxes[bx] & x[6] <=0){
          return(abs(x[5]))
        } else {
          return(-1*abs(x[5]))
        }
      })
      lev.ls[[lev]] = box.exch
    }
    
    box.ls[[bx]] = bind_rows(lev.ls)
    
  }
  time.ls[[t]] = bind_rows(box.ls)
  
}

exchange.all = bind_rows(time.ls)
box.connections = bind_rows(box.connections)

exchange.summ = exchange.all %>% group_by(time,b.orig,z.orig) %>%
  summarize(net.exch = sum(exch,na.rm=T),volume = mean(volume,na.rm=T)) %>%
  mutate(net.exch.pct = signif(100*net.exch/volume,2))

## Identify if for each time step, each box is connected to all the appropriate boxes/layers with a non-zero exchange
for(t in seq_along(flux.nc$dim$t$vals)){
  
  for(bx in seq_along(flux.nc$dim$b$vals)){
    
    dat = exchange.all %>% filter(time == t & b.orig == boxes[bx] )
    flux.faces = bgm$faces %>% filter( left == boxes[bx] | right == boxes[bx])
    
    
  }
}





# box.conn.final = unique(c(box.coords[,2]-1,dest_b[,1,,t][box.coords]))
# box.conn.final = box.conn.final[-which(box.conn.final == boxes[bx])]
# 
# # flux.faces = bgm$faces %>% filter( left == boxes[bx] | right == boxes[bx])
# box.conn.orig = unique(c(flux.faces$left,flux.faces$right,0))
# box.conn.orig = box.conn.orig[-which(box.conn.orig == boxes[bx])] 
# 
# if(bx == 1){
#   box.connections[[t]]$all.connected[bx] = length(box.conn.final) == 29
# } else {
#   box.connections[[t]]$all.connected[bx] = all(sapply(box.conn.final, function(x) return(x %in% box.conn.orig)))  
# }
# 
# min.z = numeric()
# for(k in 1:nrow(box.coords)){
#  b.match =  c(box.coords[k,2]-1,dest_b[,1,,t][box.coords[k,1],box.coords[k,2]])
#  min.z[k] =min(dz_box$nlevel[b.match+1])\
# 
#    
# }
# 
# box.ref[[bx]] = sort(unique(c(boxes[which(dest.box==boxes[bx],arr.ind = T)[,2]],dest_b[,1,bx,1])))
#Which box coords go to same as level selected
# lev.coords = box.coords[which(dest_k[,lev,,t][box.coords]==(lev-1)),]
# #identify exchanges for those coords
# lev.ls2[[lev]] = data.frame(time = t,box = boxes[bx], level = lev-1, non.zero.flux = all(!is.na(exchange[,lev,,t][lev.coords])))
# 
