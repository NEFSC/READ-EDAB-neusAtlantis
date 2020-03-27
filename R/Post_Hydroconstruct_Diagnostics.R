#' Diagnostis for atlantis forcing files (post-hydroconstruct)
#' 
#' Diagnostic tests for post-hydroconstruct output (input files for Atlantis) 
#' 1) Boxes exchange to/from all neighboring boxes
#' 2) Boxes exchange ONLY to neighboring boxes (no jumps)
#' 3) Net flux within box close to 0 for a given time
#' 
#' @force.dir string. Directory of forcing files
#' @hydro.name string. name of hydro file
#'
#'Author: J.caracappa

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Done/'
hydro.name = 'roms_cobalt_flows_03242020_1980_1989.nc'
bgm.file = 'neus_tmerc_RM2.bgm'
dz.file = 'dz.csv'
level.key.file = 'box layer key rev.csv'
level.key.rev.file = 'box layer key.csv'

forcing.diagnostics = function(force.dir,hydro.name){

  `%>%` = dplyr::`%>%`
  
  flux.nc = ncdf4::nc_open(paste0(force.dir,hydro.name))
  
  exchange = ncdf4::ncvar_get(flux.nc,'exchange')
  dest_b = ncdf4::ncvar_get(flux.nc,'dest_b')
  dest_k = ncdf4::ncvar_get(flux.nc,'dest_k')
  
  #3 vars, from wiki: 
  # dest_b matrix of destination box of flows from each layer of each box at each timestep
  # desk_k matrix of destination layer of flows from each layer of each box at each timestep
  # exchange matrix of actual exchanges
  # t vector of timestamps associated with each timestep
  # 4 dims in dest_b,dest_k, exchange: t number of tsteps; b (30) number of boxes; z (4) max num of layers; dest (32) max num of boxes any box can exchange with
  # dest is 32 (2 boxes exchange with Box 0) something weird here

  bgm = rbgm::bgmfile(here::here('Geometry',bgm.file))
  dz_box = read.csv(here::here('Geometry',dz.file),header=T)
  dz_box$nlevel = apply(dz_box[,2:5],1,function(x) sum(!is.na(x)))
  level_dz = dz_box[,5:2]
  level_key = read.csv(here::here('Geometry',level.key.file))
  level_key_rev = read.csv(here::here('Geometry',level.key.rev.file))
  
  boxes = 0:29
  t.tot = length(flux.nc$dim$t$vals)
  areas = bgm$boxes$area
  volumes = level_dz * areas
  
  # #match table for signs
  # flux.sign = data.frame(bx.left = c(1,1,0,0),bx.right = c(0,0,1,1),sign = c(0,1,0,1),dir = c(-1,1,1,-1))

  
  # Assess forcing files using rShinyAtlantis -------------------------------
  
  ###sh.prm - Explore biology parameter files
  # bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm'
  # salinity.file = 'salt_out.nc'
  # temperature.file = 'temp_out.nc'
  # exchange.file = 'hydro_out.nc'
  # cum.depth = c(0,50,120,300,500,1000)
  # obj.force = make.sh.forcings.object(bgm.file,exchange.file,cum.depth,temperature.file,salinity.file)
  # 
  # bgm.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Atlantis_Code/hydroconstruct/trunk/Example/SEAP_14052012_aea.bgm'
  # exchange.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/flowOutAll_fix_20180402.nc'
  # exchange.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Atlantis_Code/hydroconstruct/trunk/Example/Done/SEAP132_hydro.nc'
  # cum.depth = c(0,20,30,50,150,450,3600)
  # exchange =nc_open(exchange.file)
  # obj.force = make.sh.forcings.object(bgm.file,exchange.file,cum.depth)
  
  #sh.forcings(obj.force)
  
  # Function to pull long-format net exchanges for given focal box ----------
  
  box.layer.flow = function(b,t){
    
    if(b %in% c(23,24)){
      return(NULL)
    }else{
      exch.ls = list()
      for( lev in 1:4){
      # lev = 3
        # lev.id = level_key[,2:5][which(boxes == b),lev]
        db = dest_b[,lev,,t]
        dl = dest_k[,lev,,t]
        exch = exchange[,lev,,t]
        dat =  data.frame( source.b = rep(1:ncol(db)-1, each = nrow(db)), array.l = lev,
                                                     dest.b = c(db), dest.l = c(dl), exch = c(exch))
        dat$source.l = level_key[dat$source.b+1,lev+1]
        dat = dat %>% dplyr::select(source.b,source.l,array.l,dest.b,dest.l,exch)
        exch.ls[[lev]] =dat
      }
      exch.all = dplyr::bind_rows(exch.ls)
      
      # exch.lev = exch.all %>% filter( (source.b == b | dest.b == b) & !is.na(exch)) %>% arrange(array.l,dest.b,dest.l)
      dumm1 = exch.all %>% dplyr::filter(source.b == b & !is.na(exch))
      dumm2 = exch.all %>% dplyr::filter(dest.b == b & !is.na(exch))
      exch.lev = rbind(dumm1,dumm2)
      
      exch.lev2 = exch.lev
      
      source.b = exch.lev$source.b
      source.l = exch.lev$source.l
      dest.b = exch.lev$dest.b
      dest.l = exch.lev$dest.l
      
      match.dest = which(exch.lev$source.b != b)
      exch.lev$dest.b[match.dest] = exch.lev$source.b[match.dest]
      exch.lev$source.b = b
      
      exch.lev$source.l[match.dest] = dest.l[match.dest]
      exch.lev$dest.l[match.dest] = source.l[match.dest]
      
      exch.lev$exch[match.dest] = exch.lev$exch[match.dest] * -1
      exch.lev = exch.lev %>% dplyr::select(-array.l) %>% dplyr::filter(!duplicated(exch.lev))
      
      exch.lev$duplicate = duplicated(exch.lev[,1:4])
      
      exch.lev = exch.lev %>% dplyr::group_by(source.b,source.l,dest.b,dest.l) %>% dplyr::summarize(exch = sum(exch,na.rm=T))
      exch.lev$time = t
      
      return(exch.lev)
    }
  }
  
  
  t.ls = list()
  for(t in 1:t.tot){
    b.ls = list()
    for(b in seq_along(boxes)){
      b.ls[[b]] = box.layer.flow(b-1,t)
    }
    t.ls[[t]] = dplyr::bind_rows(b.ls)
    print(t)
  }
  full.exchanges = dplyr::bind_rows(t.ls)
  full.exchanges$source.b = factor(full.exchanges$source.b)
  full.exchanges$dest.b = factor(full.exchanges$dest.b)
  
  # save('full.exchanges', file = paste0(force.dir,'Long Format Net Exchanges.R'))
  
  #Example plot
  # ggplot(data = subset(full.exchanges,source.b == 1), aes(x = time, y = exch, col = dest.b))+geom_path(stat = 'sum',size = 1)
  
  
  
  # Connectivity and Mass Balance Tests -------------------------------------
  
  
  #list of all boxes each box connects to
  box.connections = lapply(seq_along(flux.nc$dim$t$vals), function(x){
    data.frame(box = boxes,time = NA, all.connected = NA)
  })
  
  #Determine possible connections from faces
  box.match = list()
  for(b in seq_along(boxes)){
    face.match = bgm$faces %>% dplyr::filter( left == (b-1) | right == (b-1))
    face.match = unique(c(face.match$left,face.match$right))
    face.match = face.match[-which(face.match == boxes[b])]
    face.match = face.match[-which(face.match %in% c(23,24))]
    box.match[[b]] = face.match
  }
  
  
  # Does each box connect to each neighboring box? --------------------------
  for(t in 1:t.tot){
    box.connections[[t]]$time = t
    for(b in seq_along(boxes)){
      sub = full.exchanges %>% dplyr::filter(source.b == (b-1) & time == t) 
      if( b == 1){
        conn.flag = length(unique(sub$dest.b))==30
      } else if(b %in% c(24,25)){
        conn.flag = NA
      } else {
        conn.flag = all(box.match[[b]] %in% unique(sub$dest.b))
      }
      box.connections[[t]]$all.connected[b] = conn.flag
    }
  }
  box.connections2 = dplyr::bind_rows(box.connections)
  as.data.frame(box.connections2 %>% dplyr::group_by(box,all.connected) %>% dplyr::summarize(total = sum(all.connected)))
  
  #  Is the net flux across a box/layer close to zero? Mass balance? --------
  
  
  #Table for depth, area, volume for each box-layer
  box.level = data.frame(box = unlist(lapply(seq_along(boxes),function(x) return((level_dz*0)[x,]+boxes[x]))),
                         depth = unlist(lapply(seq_along(boxes),function(x) return(cumsum(t(level_dz[x,]))))),
                         dz = unlist(lapply(seq_along(boxes),function(x) return(t(level_dz[x,])))),
                         at.level = unlist(lapply(seq_along(boxes),function(x) return(level_key_rev[x,2:5]))))
  box.level = box.level[which(!is.na(box.level$box)),]
  #Add area and volume
  box.level$area = sapply(box.level$box,function(x) bgm$boxes$area[which(bgm$boxes$.bx0 == x)])
  box.level$volume = box.level$area * box.level$dz
  box.level$time = NA
  box.level$net.flux = NA
  box.level$pct.flux = NA
  #place in temporal list
  box.level.ts = lapply(1:t.tot,function(x) {
    DF = box.level
    DF$time = x
    return(DF)
  })
  box.level.all = dplyr::bind_rows(box.level.ts)
  
  #Loop over box/levels for each time step and calculate net flux and percent of layer volume
  for(i in 1:nrow(box.level.all)){
    
    #subset full exchange output
    DF=full.exchanges %>% 
      dplyr::filter( source.b == box.level.all$box[i] & source.l == box.level.all$at.level[i] & time == box.level.all$time[i]) 
    DF= DF[apply(DF,1,function(x) return( !all( c(x[1],x[2]) == c(x[3],x[4])))),]
    
    box.level.all$net.flux[i ] = sum(DF$exch,na.rm=T)
    box.level.all$pct.flux[i] = 100*abs(box.level.all$net.flux[i]) / box.level.all$volume[i]
  }
  
  box.level.all = box.level.all %>% dplyr::filter( !(box %in% c(23,24)))
  summary(box.level.all$pct.flux)
