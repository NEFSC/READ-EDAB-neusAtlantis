#' Runs diagnostic routine for box/face-aggregated ROMS data
#' 
#' Diagnostic tests for aggregated ROMS data before feeding into
#' hydroconstruct. This includes 3 major tests: 1) Boxes exchange 
#' to/from all neighboring boxes (i.e. full connectivity), 2) Boxes 
#' exchange ONLY to neighboring boxes (i.e. no jumps), and 3) The 
#' net flux within a box/layer is close to 0 at all timesteps
#' (i.e. mass balance). ROMS data is assumed to be aggregated such 
#' that fluxes are in ncdf4 files that countain horizontal fluxes
#' with the dimensions (levels,faces,time), and box variables have
#' dimensions (levels, boxes, time).
#' 
#' @roms.dir string. Path to folder containing roms data as .nc files
#' @roms.prefix string. Prefix for name of roms files
#' @bgm.file string. Name of atlantis bgm file in atlantis coordinates
#' @dz.file string. Name of file containing box-level information. Should be column names (box, l1,l2,l3,l4)
#' @output.name string. Desired plot and table naming prefix
#' @output.dir string. Path to save output
#' 
#' @return ???
#' 
#' Author: J. Caracappa

transport.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/transport/'
statevars.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/phys_statevars/'
output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Pre Forcing Diagnostics/'
bgm.file = 'neus_tmerc_RM2.bgm'
dz.file = 'dz.csv'

ROMS_Diagnostics = function(roms.dir,roms.prefix,bgm.file,dz.file,output.name,output.dir){

`%>%` = dplyr::`%>%`

# Read in box-aggregated data ---------------------------------------------

transport.files = list.files(transport.dir,glob2rx('roms_cobalt+*.nc'),full.names = T)
statevar.files = list.files(statevars.dir,glob2rx('roms_cobalt+*.nc'),full.names = T)

#Pull variables from annual netCDF
hflux.ls =vflux.ls = list()
for(i in 1:length(transport.files)){
  transport.nc = ncdf4::nc_open(transport.files[i])
  hflux.ls[[i]] = ncdf4::ncvar_get(transport.nc,'transport')
  if(i == 1){
    source.box = ncdf4::ncvar_get(transport.nc,'source_boxid')
    dest.box = ncdf4::ncvar_get(transport.nc,'dest_boxid')
  }
  ncdf4::nc_close(transport.nc)
  
  statevar.nc = ncdf4::nc_open(statevar.files[i])
  vflux.ls[[i]] = ncdf4::ncvar_get(statevar.nc,'verticalflux')
  ncdf4::nc_close(statevar.nc)
}

#Combine years into single array/matrix for each variable. Last dimension is time (34 yrs ~ 12395 d)
hflux = abind::abind(hflux.ls,along =3)
vflux = abind::abind(vflux.ls, along =3)

bgm = rbgm::bgmfile(here::here('Geometry',bgm.file))
dz_box = read.csv(here::here('Geometry','dz.csv'),header = T)
dz_box$nlevel = apply(dz_box[,2:5],1,function(x) sum(!is.na(x)))
level_dz = dz_box[,5:2]

#match table for signs
flux.sign = data.frame(bx.left = c(1,1,0,0),bx.right = c(0,0,1,1),sign = c(0,1,0,1),dir = c(-1,1,1,-1))

# Box Related Tests (1,2,3) -------------------------

boxes = 0:(dim(vflux)[2]-1)
faces = 0:(dim(hflux)[2]-1)
dt = 1:dim(hflux)[3]

# Output DF
boxes.exchange = data.frame(.bx0 = boxes, exchange.all = NA, exchange.pct = NA, flux.net = NA, neighbors.only = NA,time = NA)
boxes.exchange.ls = lapply(dt,function(x) boxes.exchange)

#DF for box-level net flux
box.net.flux = data.frame(box = boxes,l1 = NA, l2 = NA, l3 = NA, l4 = NA, l1.pct = NA, l2.pct = NA, l3.pct = NA, l4.pct = NA,
                          l1.vol = NA, l2.vol = NA, l3.vol = NA, l4.vol = NA,time = NA)
box.net.flux.ls = lapply(dt,function(x) box.net.flux)

# bx = 4
# t = 1
# lev = 1
for(bx in seq_along(bgm$boxes$.bx0)){
  
  if(bx %in% c(24,25)){
    next
  }
  #Identify faces connected to each box (excludes Islands B23 + B24)
  flux.faces = bgm$faces %>% dplyr::filter( (left == boxes[bx] | right == boxes[bx]) & !(left %in% c(23,24)| right %in% c(23,24)))
  
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

boxes.net.flux = dplyr::bind_rows(box.net.flux.ls)
boxes.exchange = dplyr::bind_rows(boxes.exchange.ls)



#Plot net flux % over time for each box
net.flux.pct = boxes.net.flux %>% dplyr::select(box,l1.pct,l2.pct,l3.pct,l4.pct,time)
net.flux.pct = reshape2::melt(net.flux.pct,id.vars = c('box','time'))
flux.pct.plots = list()
for(i in seq_along(boxes)){
  sub = net.flux.pct %>% dplyr::filter(box == boxes[i])
  flux.pct.plots[[i]] = ggplot2::ggplot(data = sub, ggplot2::aes(x= time,y=value))+
    ggplot2::geom_bar(stat='identity')+
    ggplot2::ylim(-100,100)+
    ggplot2::facet_wrap(~variable)+
    ggplot2::ggtitle(paste0('Box ',boxes[i]))
}
pdf(paste0(output.dir,output.name,'Net_Flux_Pct.pdf'),width = 12,height = 8)
for(i in seq_along(boxes)){
  gridExtra::grid.arrange(flux.pct.plots[[i]])
  print(i)
  }
dev.off()

save(boxes.net.flux,boxes.exchange, net.flux.pct,file = paste0(output.dir,output.name,'_box_diagnostics.R'))

}


ROMS_Diagnostics(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/transport',
                 roms.prefix = 'roms_cobalt_v10_transport_1981*',
                 bgm.file = 'neus_tmerc_RM2.bgm',
                 dz.file = 'dz.csv',
                 output.name = 'roms_diag_allyears',
                 output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Pre Forcing Diagnostics/'
)
