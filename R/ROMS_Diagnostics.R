#### Diagnostic tests for box-aggregated ROMS-COBALT output ####
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
# Read in box-aggregated data ---------------------------------------------
setwd('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/NEUS_Example/')

box.nc = nc_open('neus_variables_test.nc')
face.nc = nc_open('neus_transport_test.nc')
face.nc.old = nc_open('C:/Users/joseph.caracappa/Documents/Atlantis/Atlantis_Code/hydroconstruct/trunk/Example/trans_SEAP132_1year.nc')

hflux = ncvar_get(face.nc,'transport')
hflux.old = ncvar_get(face.nc.old,'transport')
source.box = ncvar_get(face.nc,'source_boxid')
dest.box = ncvar_get(face.nc,'dest_boxid')
vflux = ncvar_get(box.nc,'verticalflux')

bgm = bgmfile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm')
load('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_boxes_info.R')

dz_box = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',header=T)
dz_box$nlevel = apply(dz_box[,2:5],1,function(x) sum(!is.na(x)))
level_dz = dz_box[,5:2]

#match table for signs
flux.sign = data.frame(bx.left = c(1,1,0,0),bx.right = c(0,0,1,1),sign = c(0,1,0,1),dir = c(-1,1,1,-1))

# Box Related Tests (1,2,3) -------------------------

boxes = 0:29
faces = 0:150
dt = 1:dim(hflux)[3]

# Output DF
boxes.exchange = data.frame(.bx0 = boxes, exchange.all = NA, exchange.pct = NA, flux.net = NA, neighbors.only = NA,time = NA)
boxes.exchange.ls = lapply(dt,function(x) boxes.exchange)

#DF for box-level net flux
box.net.flux = data.frame(box = boxes,l1 = NA, l2 = NA, l3 = NA, l4 = NA, l1.pct = NA, l2.pct = NA, l3.pct = NA, l4.pct = NA,
                          l1.vol = NA, l2.vol = NA, l3.vol = NA, l4.vol = NA,time = NA)
box.net.flux.ls = lapply(dt,function(x) box.net.flux)

bx = 4
t = 1
lev = 1
for(bx in seq_along(bgm$boxes$.bx0)){
  
  if(bx %in% c(24,25)){
    next
  }
  #Identify faces connected to each box (excludes Islands B23 + B24)
  flux.faces = bgm$faces %>% filter( (left == boxes[bx] | right == boxes[bx]) & !(left %in% c(23,24)| right %in% c(23,24)))
  
  #Determine Face Ids for each box
  face.id = which(faces %in% flux.faces$.fx0)
  
  #Number of levels within box
  nz = dz_box$nlevel[dz_box$polygon== boxes[bx]]
  
  #Identify the origin and destination of flux for faces
  box.flux = hflux[,face.id,]
  orig = source.box[face.id]
  dest = dest.box[face.id]
  
  #Determine depth of each box and select the one with the shallowest depth (no flow deep to shallow layers)
  orig.z = sapply(orig,function(x)dz_box$nlevel[dz_box$polygon == x])
  dest.z = sapply(dest,function(x)dz_box$nlevel[dz_box$polygon == x])
  z.mat = as.data.frame(cbind(orig,dest,orig.z,dest.z))
  z.mat$minz = apply(z.mat[,3:4],1,min)
  
  #Box area
  box.area = subset(bgm$boxes,.bx0 == boxes[bx])$area
  

  #Loop along timesteps and calculate diagnostics
  for(t in seq_along(dt)){
    
    #Is there flow to all faces for all connected layers?
    full.exchange = sapply(seq_along(flux.faces$.fx0), function(x) {
      all(!is.na(box.flux[1:z.mat$minz[x],x,t]))
    })
    # As a whole
    boxes.exchange.ls[[t]]$exchange.all[bx] = all(full.exchange)
    # Percent of face/layers that are connected
    boxes.exchange.ls[[t]]$exchange.pct[bx] = round(100*sum(full.exchange)/length(full.exchange))
    
    #Is there flow to at least one layer of all neighboring boxes?
    boxes.exchange.ls[[t]]$neighbors.only[bx] = all(sapply(seq_along(flux.faces$.fx0),function(x) any(!is.na(box.flux[,x,t]))))
    
    boxes.exchange.ls[[t]]$time = dt[t]
    
    
    for(lev in 1:nz){
      
      
      #calculate box-level volume
      lev.vol = box.area * level_dz[bx,lev]
        
      # Horiz. flux at each level
      hflux.level = box.flux[lev,,t]
      hflux.level[is.na(hflux.level)] = 0
      
      # Determine whether face flows are in our out of box
      hflux.level = abs(hflux.level) *apply(cbind(flux.faces$left == boxes[bx],flux.faces$right == boxes[bx],hflux.level > 0),1,function(x){
         return(flux.sign$dir[which(apply(flux.sign[,1:3],1,function(y) all(y == x)))])
      })
      
      # Vert. flux at each level (above, within, below)
      if(lev == 1){
        vflux.level = c(NA,vflux[lev,bx,t],vflux[lev+1,bx,t])
      } else if(lev == nz){
        vflux.level = c(vflux[lev-1,bx,t],vflux[lev,bx,t],NA)
      } else {
        vflux.level = c(vflux[lev-1,bx,t],vflux[lev,bx,t],vflux[lev+1,bx,t])
      }
      #Multiply vflux by area of box to get transport
      vflux.level = vflux.level * box.area * 86400
      
      # sum of horizontal and vertical flows
      
      #Change sign for proper sum
      vflux.level[1] = ifelse(vflux.level[1]>0 | is.na(vflux.level[1]),0,vflux.level[1])
      vflux.level[2] = ifelse(is.na(vflux.level[2]),0,-abs(vflux.level[2]))
      vflux.level[3] = ifelse(vflux.level[3]<0 | is.na(vflux.level[3]),0,vflux.level[3])
      
      vflux.net = sum(vflux.level)
      
      flux.net = sum(hflux.level,vflux.net,na.rm=T)
      flux.pct = round(100*flux.net/lev.vol,1)
      
      box.net.flux.ls[[t]][bx,lev+1]=format(flux.net,scientific = T)
      box.net.flux.ls[[t]][bx,lev+5]=flux.pct
      box.net.flux.ls[[t]][bx,lev+9] = lev.vol
      
    }
    box.net.flux.ls[[t]]$time = dt[t]
  }
}

boxes.net.flux = bind_rows(box.net.flux.ls)
boxes.exchange = bind_rows(boxes.exchange.ls)


#Plot net flux % over time for each box
net.flux.pct = boxes.net.flux %>% select(box,l1.pct,l2.pct,l3.pct,l4.pct,time)
net.flux.pct = reshape2::melt(net.flux.pct,id.vars = c('box','time'))
flux.pct.plots = list()
for(i in seq_along(boxes)){
  sub = net.flux.pct %>% filter(box == boxes[i])
  flux.pct.plots[[i]] = ggplot(data = sub, aes(x= time,y=value))+
    geom_bar(stat='identity')+
    ylim(-100,100)+
    facet_wrap(~variable)+
    ggtitle(paste0('Box ',boxes[i]))
}
pdf('Net Flux Percent.pdf',width = 12,height = 8)
for(i in seq_along(boxes)){grid.arrange(flux.pct.plots[[i]])}
dev.off()
