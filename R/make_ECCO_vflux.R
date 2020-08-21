# Function to pull monthly vflux from ECCO and get into Atlantis' netCDF format


# ecco.dir = 'D:/Ecco/Data/monthly/WVELMASS/1993/'
# ecco.dir = 'C:/Users/joseph.caracappa/Documents/ECCO/vflux_monthly/1993/'
# ecco.prefix = 'WVELMASS_*'
# ecco.files = list.files(ecco.dir,ecco.prefix)
# out.dir = 'C:/Users/joseph.caracappa/Documents/ECCO/vflux_daily/'
# dz.file = here::here('Geometry','dz.csv')
# # bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
# bgm.file = here::here('Geometry','neus_ll_WGS84.bgm')
# shp.file = here::here('Geometry','Neus_ll_0p01.shp')
# name.out = 'ECCO_vflux_daily_'

make_ECCO_vflux = function(ecco.dir,
                             ecco.prefix,
                             ecco.files,
                             out.dir,
                             dz.file,
                             bgm.file,
                             shp.file,
                             name.out){
  # Packages ----------------------------------------------------------------
  library(angstroms)
  library(rbgm)
  library(bgmfiles)
  # library(raadtools)
  library(ncdump)
  library(ggplot2)
  library(dplyr)
  library(geosphere)
  library(rgdal)
  library(maptools)
  library(sp)
  library(sf)
  library(ncdf4)
  library(ncdf4)
  library(here)
  library(spatstat)
  library(tictoc) #just for timing
  
  #Make sure package conflicts are sorted out
  complete = tidyr::complete
  select = dplyr::select
  # `%>%` = dplyr::`%>%`
  
  # Convenience Functions ---------------------------------------------------
  
  # Transforms map projection
  project_to <- function(x, to) {
    sp::spTransform(x, sp::CRS(raster::projection(to)))
  }
  
  # Determine which box each point falls within
  index_box = function(box_sp, roms_ll){
    ind <- sp::over(project_to(angstroms::coords_points(roms_ll), box_sp) , as(box_sp, "SpatialPolygons"))
    tidyr::tibble(box = box_sp$label[ind], cell = seq_len(raster::ncell(roms_ll))) %>%
      dplyr::filter(!is.na(box))
  }
  
  # Returns a ramp of positive depths from the surface down (makes the order native to NetCDF order)
  roms_level <- function(Cs_r, h, cell) {
    raster::extract(h, cell) *  Cs_r
  }
  
  # matching cell for the right group
  extract_at_level <- function(x, cell_level) {
    #identify all levels and remove NA
    ulevel <- unique(cell_level$level)
    na.lev = which(is.na(ulevel))
    ulevel = ulevel[-na.lev]
    
    #Create empty vector for out values
    values <- numeric(nrow(cell_level))
    #Which cell_level rows are NAs
    val.na = which(is.na(cell_level$level))
    #places NA values
    values[val.na] = NA
    
    #loop through levels and extract cell-level vals
    for (ul in seq_along(ulevel)) {
      asub <- cell_level$level == ulevel[ul]
      # asub = asub[-val.na]
      rs.vals =  raster::extract(x[[ulevel[ul]]],cell_level$cell[asub])
      rs.na = which(is.na(rs.vals))
      if(length(rs.na)==length(rs.vals)){
        values[which(asub)] = NA
      }else{
        values[which(asub)] <- rs.vals[-rs.na]  
      }
      
    }
    values
  } 
  
  # sets the extent of indexed data
  set_indextent <- function(x) {
    raster::setExtent(x, extent(0, ncol(x), 0, nrow(x)))
  }
  
  #Determine number of days within month
  month.days = function(date){
    m = format(date,format = '%m')
    while(format(date, format = '%m') == m){
      date = date + 1
    }
    return(as.integer(format(date-1, format = '%d')))
  }
  
  #st_join with tolerance
  #From thread: https://stackoverflow.com/questions/51381900/r-overlay-points-and-polygons-with-a-certain-degree-of-tolerance
  st_nearest_feature2 = function(x, y, tolerance = 111000) {
    isec = st_intersects(x, y)
    no_isec = which(lengths(isec) == 0)
    
    sink('file')
    for (i in no_isec) {
      nrst = st_nearest_points(st_geometry(x)[i], st_geometry(y))
      nrst_len = st_length(nrst)
      nrst_mn = which.min(nrst_len)
      isec[i] = ifelse(as.vector(nrst_len[nrst_mn]) > tolerance, integer(0), nrst_mn)
    }
    sink()
    unlist(isec)
    
  }
  
  # Read in External Data Files ---------------------------------------------
  # Read box_depth data (shows depth of each layer in each box)
  
  dz_box = read.csv(dz.file,header=T)
  # dz_box = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',header = T)
  
  # Read BGM file
  bgm = rbgm::bgmfile(bgm.file)
  # bgm = bgmfile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm')
  
  # Read boxes shape file
  neus.shp = rgdal::readOGR(shp.file)
  # neus.shp = rgdal::readOGR('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp')
  
  
  # Read in ecco Output  -----------------------------------------------------

  #Creates table of ecco output files: For daily files (don't need RM old function, all have model_time length of 1)
  #Might still need if dailies are concatenated
  
  #Time is in hours from 1950-01-01 00:00:00
  file_db_ls = list()
  for(f in 1:length(ecco.files)){
    file.nc = ncdf4::nc_open(paste0(ecco.dir,ecco.files[f]))
    file.split = strsplit(ecco.files[f],paste0(ecco.prefix, '|.nc'))[[1]][2]
    file_db_ls[[f]] = data.frame(fullname = ecco.files[f],
                                 year = strsplit(file.split,'_')[[1]][1],
                                 month = strsplit(file.split,'_')[[1]][2])
    file_db_ls[[f]]$band_level = 1:nrow(file_db_ls[[f]])
    ncdf4::nc_close(file.nc)
    print(f)
  }
  file_db = dplyr::bind_rows(file_db_ls)
  file_db$time_id = 1:nrow(file_db)

  # Format and Summarize Box and Shape Files --------------------------------------------
  
  #Atlantis depths
  max_depth <- 500 # max(extract(h, unique(box_roms_index$cell)))
  # Specific to the NEUS model
  atlantis_depths <- cumsum(c(0, rev(rbgm::build_dz(-max_depth, zlayers = c(-500, -300, -120, -50, 0)))))
  
  # add data for max numlayers for Atlantis model
  countZLayer=apply(dz_box[,2:5], c(1,2), function(x) any(is.finite(x)))
  NEUSz=data.frame(dz_box[,1]); colnames(NEUSz)='.bx0'
  NEUSz$NEUSlevels=rowSums(countZLayer)
  NEUSz$zmax = apply(dz_box[,2:5],1,function(x) sum(x,na.rm=T))
  
  # rbgm::boxSpatial returns spatial polygons as a spatial object
  boxes = rbgm::boxSpatial(bgm)
  boxes = sf::st_as_sf(boxes)
  
  # Determine boundary box for each neus box
  spatialbbox=list()
  for (i in 1:length(neus.shp@data$BOX_ID)){
    spatialbbox[[i]]=raster::bbox(neus.shp@polygons[[i]])
  }
  spbox=rbind(spatialbbox)

  # Convert NEUS boxes to ecco coordinates ----------------------------------
  
  #Using rho points for all export variables. Rho is in center and u- and v-points on verticles of cells.
  #Not as percise, but on scale on Atlantis boxes, point deviations shouldn't matter (can revisit if necessary)
  
  # Obtain lat/lon for rho-points
  #Read in grid file (grid_10 when starting at 00, 11 when at 01)
  # grid.file = 'D:/ECCO/Data/ECCO-GRID_10.nc'
  grid.file = 'C:/Users/joseph.caracappa/Documents/ECCO/ECCO-GRID_10.nc'
  ecco_grid = nc_open(grid.file) 
  # for(i in 1:length(names(ecco_grid$var))){print(ncatt_get(ecco_grid,names(ecco_grid$var)[i])$long_name)}
  
  #Get Lat/lon cood brick
  # ecco_coords = angstroms::romscoords(grid.file,spatial = c('XC','YC'),ncdf = T)
  XC = raster(grid.file,ncdf=T,varname = 'XC')
  YC = raster(grid.file,ncdf=T,varname = 'YC')
  coord.ls = lapply(list(XC,YC), function(x) setExtent(x,extent(0, nrow(x), 0, ncol(x))))
  ecco_coords = stack(coord.ls)

  
  #Build Index for the ecco Cell of NEUS Boxes and Faces -------------------
  
  #For each face, which cells does it traverse
  # box_index = index_box(boxes, ecco_coords)
  coords = extract(readAll(ecco_coords),1:ncell(raster(XC)))
  cell.index = data.frame(cell = 1:ncell(XC),lon = coords[,1],lat = coords[,2])

  #match box shape to cells
  pnts.shp = sf::st_as_sf(cell.index, coords = c('lon','lat'),crs = sf::st_crs(boxes))
  box_index_ls = list()
  for(i in 1:30){
    match = st_join(boxes[i,],pnts.shp,join = nngeo::st_nn,maxdist = 25000,k = nrow(pnts.shp),progress = F)
    if(all(is.na(match$cell))){
      match = st_join(boxes[i,],pnts.shp, join = nngeo::st_nn,maxdist = Inf, k= 1, progress = F)
    }
    
    match = match %>% as.data.frame %>% dplyr::select(label,cell)
    box_index_ls[[i]] = match
    # plot(extract(ecco_coords,match$cell),type = 'n',xlim = c(-78,-65),ylim = c(34,47))
    # plot(boxes,add = T)
    # points(extract(ecco_coords,match$cell),col = 'red',pch = 16)
  }
  
  #Combinde box and face into one df for processing (split out later)
  box_index = bind_rows(box_index_ls)
  colnames(box_index) = c('ID','cell')

  #Retreive cell dimensions from coordinates file
  #Add cell area to box_index
  
  grid_area = raster(grid.file,ncdf = T, varname = 'rA')
  grid_bathy = raster(grid.file,ncdf = T, varname = 'Depth')

  box_index$area = extract(grid_area,box_index$cell)
  box_index$bathy = extract(grid_bathy,box_index$cell)
  box_index$XC = extract(XC,box_index$cell)
  box_index$YC = extract(YC,box_index$cell)
  ggplot(box_index, aes(x= XC, y = YC, col = ID))+geom_text(data=box_index,aes(label = ID))
  # Create Index between ecco and NEUS Levels for all box and face cells -------------------------------
  
  #Grid depth centers and intervals
  grid_z = ncvar_get(ecco_grid,'Z')
  grid_dz = ncvar_get(ecco_grid,'drC')
  
  #Vertical weights from depth bin intervals (changes with cell though)
  # z_wgt = diff(c(0,z/z[length(z)]))
  

  
  ## build the level index between Atlantis and ecco
  list_nc_z_index = vector('list', nrow(box_index))
  
  # Loop through each cell, identify depths of ecco layers, identify corresponding NEUS layers
  for (i in seq_len(nrow(box_index))) {
    
    #Retreives depths at each cell, and calculates cell volume (for volume-based weights)
    cell.bath = box_index$bathy[i]
    nlev = length(which(grid_z >= -cell.bath))
    na.flag = ifelse(is.na(nlev)|nlev == 0,T,F)
    
    #identify box and/or face ID as well as whether island box
    box.id = bgm$boxes$.bx0[bgm$boxes$label == box_index$ID[i]]
    island.flag = ifelse(box.id %in% c(23,24),T,F)
    
    #Fill placeholder for cells in island or with no output values
    if(na.flag | island.flag){
      list_nc_z_index[[i]] = data.frame(box = paste0('Box',box.id),
                                        atlantis_levels = NA,
                                        ecco_levels = NA,
                                        cell.z = NA,
                                        cell.dz = NA,
                                        z.wgt = NA,
                                        cell = box_index$cell[i],
                                        test.ind = i)
    }else{
      
      #Calculate total z intervals
      cell.dz = grid_dz[1:nlev]
      
      #If bathymetry is less than bottom of the deepest depth level, take halfway between 2nd bottom and bathy
      cell.z = -grid_z[1:nlev]
      cell.w = cumsum(cell.dz)
      
      if(cell.w[nlev]>cell.bath){
        cell.z[nlev] = ((cell.bath-cell.w[nlev-1])/2)+cell.w[nlev-1]
      }
      
      cell.dz = cell.dz[1:nlev]
      
      
      z.box = NEUSz$zmax[NEUSz$.bx0 == box.id]
      if(cell.z[nlev]>z.box){
        z.match = which(findInterval(cell.z,z.box)==1)[1]
        cell.dz = cell.dz[1:z.match]
        cell.z = cell.z[1:z.match]
      }else{
        cell.dz = cell.dz[cell.z <= z.box]
        cell.z = cell.z[cell.z <= z.box]
      }
      
      # length(atlantis_depths) - 
      z_index <- findInterval(cell.z, atlantis_depths, all.inside = F) # + 1
      neus.max = NEUSz$NEUSlevels[which(NEUSz$.bx0 == box.id)]
      z_index[which(z_index > neus.max)] = NA
      z_index[which(z_index==5)]=NA ### remove depths greater than 500
      z_index = z_index[!is.na(z_index)]
      z.ls = list()
      for(L in 1:length(atlantis_depths)){
        dumm1 = cell.dz[which(z_index==L)]
        z.ls[[L]] = dumm1/sum(dumm1)
      }
      z.wgt.test = unlist(lapply(z.ls,sum))[1:length(unique(z_index))]
      if(any(!(z.wgt.test %in% c(0,1)))){
        break
      }
      
      #remove NA values from output (non-existent atlantis levels)
      cell.z = cell.z[1:length(z_index)]
      cell.dz = cell.dz[1:length(z_index)]
      list_nc_z_index[[i]] = data.frame(box = paste0('Box',box.id),
                                        atlantis_levels = z_index,
                                        ecco_levels = 1:length(z_index),
                                        cell.z = cell.z,
                                        cell.dz = cell.dz,
                                        z.wgt = unlist(z.ls),
                                        cell = box_index$cell[i],
                                        test.ind = i)
    }
    
    
    if (i %% 1000 == 0) print(i)
    
  }
  gc()
  
  # join the box-xy-index to the level index using rho coordinates
  box_z_index = bind_rows(list_nc_z_index)
  box_z_index = inner_join(box_z_index,box_index, by = c('box' = 'ID','cell'))  
  
  # View(box_z_index %>% group_by(box) %>% summarize(nlev = max(atlantis_levels,na.rm=T)))
  #test z.wgts sum to 1 per cell/level
  # not1 = box_z_index %>% 
  #   group_by(cell,box,atlantis_levels) %>%
  #   summarize(tot.wgt = sum(z.wgt,na.rm=T)) %>% 
  #   filter(tot.wgt != 1) %>%
  #   mutate(lat = extract(ecco_coords$latitude,cell),
  #          lon = extract(ecco_coords$longitude,cell))
  
  #Plot locations of offending points
  
  # ggplot()+
  #   coord_cartesian()+
  #   annotation_map(map_data('worldHires'),fill = 'grey70')+
  #   geom_point(data = not1, aes(x=lon,y=lat),col = 'red',size = 6)+
  #   xlim(-78,-62)+
  #   ylim(34,47)

  # Loop over ecco files and calculate state variables and fluxes -----------
  
  # creating placeholders
  box_props <- vector("list", nrow(file_db))
  i_timeslice <- 1
  
  box_cell = dplyr::rename(box_z_index, level = ecco_levels)  
  
  model_time = numeric(nrow(file_db))
  
  # for(i in 1:nrow(file_db)){model_time[i] = ncvar_get(nc_open(file_db$fullname[i]),'model_time')}
  
  
  for (i_timeslice in seq(nrow(file_db))) {
    # for(i_timeslice in 300){
    
    print(i_timeslice)
    
    #ecco file name and band name
    ecco_file <-file_db$fullname[i_timeslice]
    
    model_time[i_timeslice] = file_db$time[i_timeslice]
    
    level <- file_db$band_level[i_timeslice]
    
    #Sets extent of u,v,w, temp, and salt
    file.nc = nc_open(paste0(ecco.dir,ecco_file))
    
    #Test transposition from ncdf4::ncvar_get to brick
    # x = ncvar_get(file.nc,'XC')[,,11]
    # y = ncvar_get(file.nc,'YC')[,,11]
    # y = y[,90:1]
    # x = x[,90:1]
    # x = t(raster(x))
    # y = t(raster(y))
    # x2 = extract(x,box_index$cell)
    # y2 = extract(y,box_index$cell)
    # plot(y2~x2)
    # points(YC~XC, box_index,col = 'red')
    w = ncvar_get(file.nc,'WVELMASS')[,90:1,11,]
    r_w = set_indextent(t(brick(w)))
    nc_close(file.nc)
    # r_w = set_indextent(brick(paste0(ecco.dir,ecco_file), varname = 'WVELMASS',lvar = 4, level = level, ncdf = T))
    
    box_z_index$w = extract_at_level(r_w,box_cell)
    rm(r_w)
    ### added to get missing data back in as NA dimensions should be 30x4=120 for each date

    box_z_index1=left_join(box_z_index, bgm$boxes[c("label", ".bx0")], by=c("box"="label")) 
    
    box_z_index1=left_join(box_z_index1, NEUSz, by='.bx0') 
    
    # box_z_index1 = filter(box_z_index1, !is.na(ecco_levels))
    
        #Aggregate vertically weighted by z weights
    box_z_index2 = box_z_index1 %>%
      group_by(.bx0,atlantis_levels,cell,area) %>%
      summarize(vflux = weighted.mean(w,z.wgt,na.rm=T))
    #get total area of all cells within Atlantis box
    box_cell_wgt = box_z_index2 %>% 
      group_by(.bx0,atlantis_levels) %>%
      summarize(tot_area = sum(area,na.rm=T))
    #add area totals and calculate area weigths
    box_z_index2 = box_z_index2 %>% 
      inner_join(box_cell_wgt) %>%
      mutate(cell_wgt = area/tot_area)
    
    #aggregate over boxes and weight by cell area
    box_props[[i_timeslice]] <- box_z_index2 %>% group_by(atlantis_levels, .bx0) %>% 
      summarize(vflux = weighted.mean(vflux, cell_wgt, na.rm = TRUE)) %>% 
      ungroup(box_z_index2)%>%
      complete(atlantis_levels, .bx0) %>%
      filter(!is.na(atlantis_levels))
    box_props[[i_timeslice]]$time_id = file_db$time_id[i_timeslice]
  
    
  }
  
  # save(box_props,file = paste0(ecco.dir,'Test Dump.R'))
  # load(paste0(ecco.dir,'Test Dump.R'))
  
  # Combine box and face properties
  box_props <- bind_rows(box_props)  
  
  #Remove data from islands (Boxes 23 and 24)
  box_props[which(box_props$.bx0==23 | box_props$.bx0==24), 'vflux']=NA #islands
  # box_props[is.na(box_props$vflux),'vflux']=-999 # change NA to fill value

  # test =box_props %>% filter(time_id == 1) %>% group_by(.bx0) %>% summarize(nlev = sum(!is.na(vflux)))
  # View(left_join(test,NEUSz))
# Pad out monthly to daily ------------------------------------------------
  box_props_date = left_join(box_props,file_db)
  box_props_daily_ls = list()
  

  for(i in 1:nrow(box_props_date)){
   start.date = as.Date(paste(box_props_date$year[i],box_props_date$month[i],'01',sep='-')) 
   ndays = month.days(start.date)
   dat = box_props_date[rep(i,ndays),]
   dat$day = sprintf('%02d',1:ndays)
   dat$date =sapply(1:ndays,function(x) return(paste(dat$year[x],dat$month[x],dat$day[x],sep = '-')))
   dat$jday = sapply(1:ndays,function(x) return(difftime(as.Date(dat$date[x],format = '%Y-%m-%d'),as.Date(paste0(box_props_date$year[1],'-01-01')),units = 'days')+1))
   box_props_daily_ls[[i]] =dat
  }
  box_props_daily = bind_rows(box_props_daily_ls)

  #jagged timeseries -> smooth out
  #Perform smoothing by box-level
  box.lev.index = apply(box_props_daily[,c('.bx0','atlantis_levels')],1, function(x) return(paste(x[1],x[2],sep='-')))
  box.lev.index = unique(box.lev.index)
  box.lev.index = do.call('rbind',lapply(box.lev.index, function(x){
    dum = strsplit(x,'-')
    return(as.numeric(c(.bx0=dum[[1]][1], atlantis_levels=dum[[1]][2])))
  }))
  
  for(i in 1:nrow(box.lev.index)){
    ind.match = which(box_props_daily$.bx0 == box.lev.index[i,1] & box_props_daily$atlantis_levels == box.lev.index[i,2])
    dat = box_props_daily[ind.match,]
    
    #loess pretty good
    # dat.smooth = predict(loess(vflux~jday,dat,span = 0.2))
    
    #kernal smoothing much better span = 15 (~half month)
    dat.smooth = with(dat,ksmooth(jday,vflux,kernel = 'normal',bandwidth = 15))
    
    # plot(dat.vflux,col ='red',type='l')
    # points(dat.smooth,type='l')
    box_props_daily$vflux[ind.match] = dat.smooth$y
    
  }
  # test = filter(box_props_daily,.bx0 == 0 & atlantis_levels == 1)
  # plot(vflux~jday,test,type = 'l')
  
  nit = length(box_props_daily$vflux)
  bands = box_props_daily$jday
  box.id = box_props_daily$.bx0
  level.id = box_props_daily$atlantis_levels
 
  ### Define dimensions for the two NetCDF files that go into hydroconstruct:
  nboxes = length(unique(box.id))
  ntimes = length(unique(bands))
  nlevel = length(unique(level.id))
  atl.level = unique(level.id)

  #Convert to NEUS is 1964 reference point
  t_tot = as.numeric(difftime(unique(box_props_daily$date),as.POSIXct('1964-01-01 00:00:00',tz = 'UTC'),units = 'secs'))
  dt=86400 # seconds in one day

  year =as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",file_db$fullname[1])))
  
  ### create vars for box structure data
  ### NOTE added 1 because index cannot be 0, must remove 1 later w/ ncks
  box.boxes=bgm$boxes$.bx0 
  
  vertical_flux=array(NA, dim=c(nlevel,nboxes,ntimes))
  
  for (i in 1:nit){
    if (i %% 5000 ==0) print(paste('progress:',i/nrow(box_props_daily)*100, '%'))
    #time
    j=bands[i]
    ### box NOTE added 1 because index cannot be 0, must remove later
    k=box.id[i]+1 
    # depth
    l=level.id[i]
    
    vertical_flux[l,k,j]=box_props_daily$vflux[i]
    
  }
  save('vertical_flux',file = paste0(out.dir,name.out,'statevars_',year,'.R'))  
  
  ### For T, S, Vertical Flux NC file
  filename=paste0(out.dir,name.out,year,'.nc')
  
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

  nc_varfile=nc_create(filename,list(var.time,var.box, var.lev,var.vertflux))
  
  #assign global attributes to file
  ncatt_put(nc_varfile,0,"title","Box averaged properties file, NEUS")
  ncatt_put(nc_varfile,0,"geometry","neus_tmerc_RM.bgm")
  ncatt_put(nc_varfile,0,"parameters","")
  
  #assign attributes to variables
  ncatt_put(nc_varfile,var.time,"dt",86400,prec="double")
  
  #assign variables to file
  ncvar_put(nc_varfile,var.vertflux,vertical_flux, count=c(nlevel,nboxes, ntimes))
  ncvar_put(nc_varfile,var.time,t_tot,verbose = F)
  ncvar_put(nc_varfile,var.lev,atl.level)
  ncvar_put(nc_varfile,var.box,box.boxes)
  
  nc_close(nc_varfile)

}

# for(i in 1:30){
#   if(all(is.na(vertical_flux[1,i,]))){next}
#   plot(vertical_flux[1,i,],type='l',main = paste0('Box',i-1))
# }


