#' Converts raw glorys-formatted output into the form used by hydroconstruct :: EXTRACTS CELL ONLY
#' 
#' Modified from original script used to process glorys-Doppio output into Atlantis.
#' Reads in physical and biological float variables on glorys grid structure and transforms
#' into Atlantis boxes/faces. Then uses glorys vertical stretching coordinates to match to
#' Atlantis layers. State variables (temperature, salinity, biological groups, etc.) are
#' aggregated across all glorys cells within an Atlantis box. All horizontal fluxes (flows)
#' are aggregated across glorys cells intersecting Atlantis faces. Output is organized in arrays
#' and exported as .nc files to be further processed by Hydroconstruct. 
#' 
#' This script is based on ROMS_COBALT physical and lower trophic level variables. Must ensure correct names are used
#' for different output.Function is also hard-coded to NEUS Atlantis model 
#' 
#' @param glorys.dir String. Defines path to glorys output. All .nc files within path must be glorys output
#' @param glorys.prefix String. glorys output prefix to pattern match
#' @param out.dir String. Path for output files
#' @param name.out String. Prefix for output (arrays and .nc) files
#' 
#' @return Output is a 4D array [NEUS level,Box/Face, Time, Var] for each processed variable as well as well as
#' .nc files for horizontal exchanges, phyical state variables, and biological state variables
#' 
#' #' Created by R. Morse and modified by J. Caracappa
# 
# glorys.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Data/1993/'
# glorys.prefix = 'GLORYS_REANALYSIS_*'
# glorys.files = list.files(glorys.dir,glorys.prefix)
# out.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Atlantis_Format/'
# dz.file = here::here('Geometry','dz.csv')
# bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
# bgm.ll.file = here::here('Geometry','neus_ll_WGS84.bgm')
# shp.file = here::here('Geometry','Neus_ll_0p01.shp')
# name.out = 'GLORYS_Atlantis_'
# make.hflux = F
# make.physvars = T
# out.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/Summary/NEUS_level_alt/'
# 


make_GLORYS_files = function(glorys.dir,
                           glorys.prefix,
                           glorys.files,
                           out.dir,
                           dz.file,
                           bgm.file,
                           bgm.ll.file,
                           shp.file,
                           name.out,
                           make.hflux,
                           make.physvars
                           ){
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
  
  #Get date from file name
  date_from_file = function(file){
    x = strsplit(file, paste0(glorys.prefix,'|.nc'))[[1]][2]
    return(x)
    # return(as.Date(x,format = '%Y-%m-%d'))
  }
  
  # weighted.median2 = function(values,weights){
  #   v = values[order(values)]
  #   w = weights[order(values)]
  #   prob = cumsum(w)/sum(w)
  #   ps = which(abs(prob - 0.5) == min(abs(prob - 0.5)))
  #   return(v[ps])
  # }
  
  # weighted.median2= function(values,weights){
  #   return(values[order(values)][which.min(abs(cumsum(weights[order(values)])-0.5))])
  # }
  # Read in External Data Files ---------------------------------------------
  # Read box_depth data (shows depth of each layer in each box)
  
  dz_box = read.csv(dz.file,header=T)
  # dz_box = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',header = T)
  
  # Read BGM file
  bgm = rbgm::bgmfile(bgm.file)
  bgm.ll = rbgm::bgmfile(bgm.ll.file)
  # bgm = bgmfile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm')
  
  # Read boxes shape file
  neus.shp = rgdal::readOGR(shp.file)
  # neus.shp = rgdal::readOGR('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp')
  

  # Read in glorys Output  -----------------------------------------------------
  # setwd(glorys.dir)
  # glorys.files = dir(glorys.dir,pattern = glorys.prefix)
  
  #Creates table of glorys output files: For daily files (don't need RM old function, all have model_time length of 1)
  #Might still need if dailies are concatenated
  
  #Time is in hours from 1950-01-01 00:00:00
  file_db_ls = list()
  for(f in 1:length(glorys.files)){
    file.nc = ncdf4::nc_open(paste0(glorys.dir,glorys.files[f]))
    file_db_ls[[f]] = data.frame(fullname = glorys.files[f],
                                    date = as.Date(date_from_file(glorys.files[f])),
                                    model_time = file.nc$dim$time$vals)
    file_db_ls[[f]]$band_level = 1:nrow(file_db_ls[[f]])
    ncdf4::nc_close(file.nc)
    print(f)
  }
  file_db = dplyr::bind_rows(file_db_ls)
  file_db = dplyr::arrange(file_db,model_time)
  file_db$time_id = 1:nrow(file_db)
  # file_db$band_level = 1:nrow(file_db)
  
  # file_db = dplyr::tibble(fullname = roms_files,
  #                         date = as.Date(sapply(roms_files,date_from_file)),
  #                         band_level = 1:length(roms_files))
  # 
  
  # Format and Summarize Box and Shape Files --------------------------------------------
  
  # rbgm::boxSpatial returns spatial polygons as a spatial object
  boxes = rbgm::boxSpatial(bgm)
  
  # Determine boundary box for each neus box
  spatialbbox=list()
  for (i in 1:length(neus.shp@data$BOX_ID)){
    spatialbbox[[i]]=raster::bbox(neus.shp@polygons[[i]])
  }
  spbox=rbind(spatialbbox)
  
  # Get width (E/W) and height (N/S) of boxes for hyperdiffusion calculation using a bounding box for each polygon
  bgm_bbox = data.frame(matrix(ncol = 7, nrow = length(bgm$boxes$label),data = NA))
  for (i in 1:30){
    bgm_bbox[i,1]=i-1
    bgm_bbox[i,2]=spatialbbox[[i]][1,1]
    bgm_bbox[i,3]=spatialbbox[[i]][1,2]
    bgm_bbox[i,4]=spatialbbox[[i]][2,1]
    bgm_bbox[i,5]=spatialbbox[[i]][2,1]
    test=raster::bbox(neus.shp@polygons[[i]])
    # distMeeus calculates shortest distance between two points on defined ellipsoid (Meeus method)
    #NS distance in meters
    bgm_bbox$dist_NS[i]=geosphere::distMeeus(test[,1], cbind(test[1,1],test[2,2]), a=6378137, f=1/298.577223563) 
    #EW distance in meters
    bgm_bbox$dist_EW[i]=geosphere::distMeeus(test[,1], cbind(test[1,2],test[2,1]), a=6378137, f=1/298.577223563)
  }
  
  # save(bgm,file='C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_boxes_info.R')
  
  # Convert NEUS boxes to glorys coordinates ----------------------------------
  
  #Using rho points for all export variables. Rho is in center and u- and v-points on verticles of cells.
  #Not as percise, but on scale on Atlantis boxes, point deviations shouldn't matter (can revisit if necessary)
  
  
  # Obtain lat/lon for rho-points
  #Read in grid file
  grid.file = 'C:/USers/joseph.caracappa/Documents/GLORYS/GLORYS_coords_neus.nc'
  mask.file = 'C:/USers/joseph.caracappa/Documents/GLORYS/GLORYS_mask_bathy_neus.nc'
  mdt.file = 'C:/USers/joseph.caracappa/Documents/GLORYS/GLORYS_mdt_neus.nc'
  glorys_grid = nc_open(grid.file) 
  glorys_mask = nc_open(mask.file)
  glorys_mdt = nc_open(mdt.file)
  
  
  #Get Lat/lon cood brick
  glorys_lat = glorys_grid$dim$latitude$vals
  glorys_lon = glorys_grid$dim$longitude$vals
  #Format to cell array for raster
  # glorys_lat_mat = matrix(rep(rev(glorys_lat),length(glorys_lon)),ncol = length(glorys_lon), nrow = length(glorys_lat),byrow = F)
  glorys_lat_mat = matrix(rep(rev(glorys_lat),length(glorys_lon)),ncol = length(glorys_lon), nrow = length(glorys_lat),byrow = F)
  glorys_lon_mat = matrix(rep(glorys_lon,length(glorys_lat)),ncol = length(glorys_lon), nrow = length(glorys_lat),byrow = T)
  
  glorys_lat_rs = raster(glorys_lat_mat)
  glorys_lon_rs = raster(glorys_lon_mat)
  
  coord.ls = list(longitude = glorys_lon_rs,latitude = glorys_lat_rs)
  coord.ls = lapply(coord.ls, function(x) setExtent(x, extent(0, nrow(x), 0, ncol(x))))
  glorys_coords = stack(coord.ls)
  
  # obtain find nearest-neighbor of bgm faces on glorys grid - NN doesn't work when mapping to same 2 grid points. Use actual vertices
  face_coords <- angstroms::romsmap(project_to(rbgm::faceSpatial(bgm.ll), "+init=epsg:4326"), glorys_coords)
  
  #get coordinates of faces in terms of glorys XY
  # bgm_lstXY = sp::coordinates(face_coords)
  bgm_lstXY = sp::coordinates(rbgm::faceSpatial(bgm.ll))
  #creates matrix of face coordinates
  bgmXY = matrix(ncol = length(bgm_lstXY), nrow = 4, unlist(bgm_lstXY))
  
  #IMPORTANT ORDER FOR GEOMETRY! Point 1 = x1,y1; Point 2 = x2,y2
  # rownames(bgmXY) = c('x1','x2','y1','y2')
  rownames(bgmXY)= c('lon1','lon2','lat1','lat2')
  #Turn into long dataframe with glorys grid X and Y coordinates, to use with romes_ll_...
  bgmXY = data.frame(t(bgmXY))
  #Add empty values for fill for lat/lon of face coords
  # bgmXY$lat1=glorys_lat[bgmXY$y1]
  # bgmXY$lat2=glorys_lat[bgmXY$y2]
  # bgmXY$lon1=glorys_lon[bgmXY$x1]
  # bgmXY$lon2=glorys_lon[bgmXY$x2]
  
  #testplot bgmXY
  # ggplot(data = bgmXY,aes(x = x1, xend = x2, y = y1, yend = y2))+geom_segment()
  # ggplot(data = bgmXY,aes(x = lon1, xend = lon2, y = lat1, yend = lat2))+geom_segment()
  # 
  ### Find lat and lon for the BGM shape with extractions by glorys XY coordinates
  ##7/21/2020 Not working with GLORYS data. Don't know why. Not sure if new way is any different
  
  
  
  # p1 = raster::cellFromRowCol(glorys_coords,bgmXY[,1],bgmXY[,3])
  # p2 =  raster::cellFromRowCol(glorys_coords,bgmXY[,2],bgmXY[,4])
  # 
  # # p1 = raster::cellFromXY(glorys_coords,cbind(bgmXY[,1],bgmXY[,3]))
  # # p2 = raster::cellFromXY(glorys_coords,cbind(bgmXY[,2],bgmXY[,4]))
  # bgmXY$lon1 = glorys_coords[p1][,1]
  # bgmXY$lat1 = glorys_coords[p1][,2]
  # bgmXY$lon2 = glorys_coords[p2][,1]
  # bgmXY$lat2 = glorys_coords[p2][,2]

  
  
  #Create face ID. Starts at 0 not 1
  bgmXY$face = seq(nrow(bgmXY))-1
  
  
  if(make.hflux){
    # Get Rhumb Line for Faces -------------------------------------------------
    # Converting from lat/lon to cartesian coordinates
    # https://stackoverflow.com/questions/1185408/converting-from-longitude-latitude-to-cartesian-coordinates
    
    #Copy of bgmXY (long time to run)
    angles = bgmXY
    # radius of earth in km
    RE = 6371
    # Matrix of bearings, this method doesn't work for high latitudes
    bearmat = matrix(ncol = 4, nrow = length(angles$lat1), data = c(angles$lon1, angles$lat1, angles$lon2,angles$lat2))
    
    #Get bearing of a rhumb line from point 1 to point 2
    ## Rumb line is a straight heading on a curved surface
    angles$bearingRhumb = geosphere::bearingRhumb(p1 = bearmat[,1:2],p2 = bearmat[,3:4])
    # convert bearing into polar XY coordinates where 0 is east (x) and 90 is North(y)
    angles$XYbearingRhumb = ((360-angles$bearingRhumb)+90)%%360
    
    ### note - 1 point results in an NA bearing (face 31) because lat and long points are the same in BGM file. ###
    
    #Plot boxes and Rhumb line to check
    # ggplot()+geom_segment(data=angles,aes(x= lon1,xend = lon2,y = lat1,yend = lat2))
    # angles$distRhumb = distRhumb(p1 = bearmat[,1:2],p2 = bearmat[,3:4])
    # data(merc)
    # data(wrld)
    # plot(wrld, type='l', xlim=c(-78,-65), ylim=c(34,45), main='Equirectangular',lty=NA)
    # for(i in 1:nrow(angles)){
    #   pr=gcIntermediate(angles[i,c(5,7)],angles[i,c(6,8)])
    #   lines(pr[,2:1])
    # }
    # points(bearmat[32,1],bearmat[32,2],cex = 2,col=2,pch=16)
  }

  #Build Index for the glorys Cell of NEUS Boxes and Faces -------------------
  
  #For each face, which cells does it traverse
  box_index = index_box(boxes, glorys_coords)
  
  #raster::cellFromLine determines which cells each face line intersects (u and v)
  ind_face = raster::cellFromLine(angstroms::romsdata(paste0(glorys.dir,glorys.files[1]),'uo'),face_coords)
    
  # creates table of face cell index for u and v points
  face_index <- dplyr::tibble(face = face_coords$label[rep(seq_len(nrow(face_coords)), lengths(ind_face))], 
                               cell = unlist(ind_face))
  
  
  #Combinde box and face into one df for processing (split out later)
  colnames(box_index) = c('ID','cell')
  colnames(face_index) = c('ID','cell')
  box_index$type = 'box'
  face_index$type = 'face'
  full_index = bind_rows(box_index,face_index)
  
  #Retreive cell dimensions from coordinates file
  #Add cell area to box_index
  grid_dx = raster(grid.file,ncdf = T, varname = 'e1t')
  grid_dy = raster(grid.file,ncdf = T, varname = 'e2t')
  grid_dz = brick(grid.file,ncdf4 = T, varname = 'e3t')
  grid_nlev = raster(mask.file,ncdf = T, varname = 'deptho_lev')
  grid_bathy = raster(mask.file,ncdf = T, varname = 'deptho')
  
  full_index$dx = extract(grid_dx,full_index$cell)
  full_index$dy = extract(grid_dy,full_index$cell)
  full_index$cell_area = full_index$dx * full_index$dy
  full_index$nlev = extract(grid_nlev,full_index$cell)
  
  full_index_dz=extract(grid_dz,full_index$cell)

  # Create Index between GLORYS and NEUS Levels for all box and face cells -------------------------------
  
  #Read cell depths from glorys: These are cell mid points
  t.z = glorys_grid$dim$depth$vals

  #Vertical weights from depth bin intervals (changes with cell though)
  # z_wgt = diff(c(0,z/z[length(z)]))
  
  #Atlantis depths
  max_depth <- 500 # max(extract(h, unique(box_roms_index$cell)))
  # Specific to the NEUS model
  atlantis_depths <- cumsum(c(0, rev(rbgm::build_dz(-max_depth, zlayers = c(-500, -300, -120, -50, 0)))))
  
  # add data for max numlayers for Atlantis model
  countZLayer=apply(dz_box[,2:5], c(1,2), function(x) any(is.finite(x)))
  NEUSz=data.frame(dz_box[,1]); colnames(NEUSz)='.bx0'
  NEUSz$NEUSlevels=rowSums(countZLayer)
  NEUSz$zmax = apply(dz_box[,2:5],1,function(x) sum(x,na.rm=T))
  
  ## build the level index between Atlantis and glorys
  list_nc_z_index = vector('list', nrow(full_index))

  # Loop through each cell, identify depths of glorys layers, identify corresponding NEUS layers
  for (i in seq_len(nrow(full_index))) {
  
    #Retreives depths at each cell, and calculates cell volume (for volume-based weights)
    nlev = full_index$nlev[i]
    na.flag = ifelse(is.na(nlev),T,F)
    
    #identify box and/or face ID as well as whether island box
    if(full_index$type[i] == 'box'){
      box.id = bgm$boxes$.bx0[bgm$boxes$label == full_index$ID[i]]
      island.flag = ifelse(box.id %in% c(23,24),T,F)
    }else{
      face.id = full_index$ID[i]  
      
      box.left = bgm$faces$left[bgm$faces$label == face.id]
      box.right = bgm$faces$right[bgm$faces$label == face.id]
      
      island.flag = ifelse(box.left %in% c(23,24) | box.right %in% c(23,24),T,F)
    }
    
    #Fill placeholder for cells in island or with no output values
    if(na.flag | island.flag){
      list_nc_z_index[[i]] = data.frame(ID = full_index$ID[i],
                                        atlantis_levels = NA,
                                        glorys_levels = NA,
                                        cell.z = NA,
                                        cell.dz = NA,
                                        z.wgt = NA,
                                        cell.vol = NA,
                                        cell = full_index$cell[i])
    }else{

      cell.bath = extract(grid_bathy,full_index$cell[i])
      #Calculate total z intervals
      cell.dz = as.numeric(extract(grid_dz,full_index$cell[i])) 
      cell.w = t.z + 0.5*cell.dz
      
      # plot(t.z[1:nlev],ylim = c(0,cell.bath),pch = 16)
      # abline(h = cell.w[1:nlev],lty = 2)
      # points(cumsum(cell.dz),col = 2)
      # abline(h=cell.bath,col = 3, lwd = 2)
      
      #Calculate cell volume
      cell.vol = cell.dz * full_index$cell_area[i]
      cell.vol = cell.vol[1:nlev]
      
      #If bathymetry is less than bottom of the deepest depth level, take halfway between 2nd bottom and bathy
      cell.z = t.z[1:nlev]
      if(cell.w[nlev]>cell.bath){
        cell.z[nlev] = ((cell.bath-cell.w[nlev-1])/2)+cell.w[nlev-1]
      }
      
      cell.dz = cell.dz[1:nlev]
      
      if(full_index$type[i] == 'box'){
        z.box = NEUSz$zmax[NEUSz$.bx0 == box.id]
        
        if(cell.z[nlev] > z.box){
          z.ind = which(findInterval(cell.z,z.box)==1)[1]
          cell.vol = cell.vol[1:z.ind]
          cell.dz = cell.dz[1:z.ind]
          cell.z = cell.z[1:z.ind]
        }else{
          cell.vol = cell.vol[cell.z <= z.box]
          cell.dz = cell.dz[cell.z <= z.box]
          cell.z = cell.z[cell.z <= z.box]
        }
        
        neus.max = NEUSz$NEUSlevels[which(NEUSz$.bx0 == box.id)]
      }else{
        
        z.left = NEUSz$zmax[NEUSz$.bx0 == box.left]
        z.right = NEUSz$zmax[NEUSz$.bx0 == box.right]
        
        box.min = ifelse(z.left<=z.right,box.left,box.right)
        box.max = ifelse(z.left>z.right,box.left,box.right)
        
        
        z.min = min(c(z.left,z.right))
        
        if(cell.z[nlev] > z.min){
          z.ind = which(findInterval(cell.z,z.min)==1)[1]
          
          cell.vol = cell.vol[1:z.ind]
          cell.dz = cell.dz[1:z.ind]
          cell.z = cell.z[1:z.ind]
        }else {
          cell.vol = cell.vol[cell.z <= z.min]
          cell.dz = cell.dz[cell.z <= z.min]
          cell.z = cell.z[cell.z <= z.min]
        }
        
        neus.max = NEUSz$NEUSlevels[which(NEUSz$.bx0 == box.min)]
      }
      # length(atlantis_depths) - 
      z_index <- findInterval(cell.z, atlantis_depths, all.inside = F) # + 1
      z.ls = list()
      for(L in 1:length(atlantis_depths)){
        dumm1 = cell.dz[which(z_index==L)]
        z.ls[[L]] = dumm1/sum(dumm1)
      }
      z_index[which(z_index==5)]=NA ### remove depths greater than 500
      z_index[which(z_index > neus.max)] = NA
      
      list_nc_z_index[[i]] = data.frame(ID = full_index$ID[i],
                                        atlantis_levels = z_index,
                                        glorys_levels = 1:length(z_index),
                                        cell.z = cell.z,
                                        cell.dz = cell.dz,
                                        z.wgt = rev(unlist(z.ls)),
                                        cell.vol = cell.vol,
                                        cell = full_index$cell[i],
                                        lon = extract(glorys_coords$longitude,full_index$cell[i]),
                                        lat = extract(glorys_coords$latitude,full_index$cell[i]))
    }

    
    if (i %% 1000 == 0) print(i)
  
  }
  gc()

  cell.index = bind_rows(list_nc_z_index)%>%
    filter(glorys_levels == 1 & grepl('Box*',ID))%>%
    select(ID,cell,lon,lat)
    # mutate(box = strsplit(as.character(ID),'Box')[[1]][2])%>%
  
  cell.index$box = sapply(as.character(cell.index$ID),function(x) return(strsplit(x,'Box')[[1]][2]))
  write.csv(cell.index,file = 'C:/Users/joseph.caracappa/Documents/GLORYS/GLORYS_NEUS_Index_2.csv',row.names = F)
  
  #test cell index
  # ggplot(cell.index,aes(x= lon,y=lat,col = box))+geom_point()
  
  # join the box-xy-index to the level index using rho coordinates
  full_z_index =  bind_rows(list_nc_z_index) %>% inner_join(full_index, by = c('ID','cell')) 

  #test z.wgts sum to 1 per cell/level
  # full_z_index %>% group_by(cell,atlantis_levels) %>% summarize(tot.wgt = sum(z.wgt,na.rm=T)) %>% filter(tot.wgt != 1)

  #Split full index into box and face index
  box_z_index = full_z_index %>% filter(type == 'box') %>% select (-type) %>% rename(box = ID)
  
  face_z_index = full_z_index %>% filter(type == 'face') %>% select (-type) %>% rename(face = ID)

  # Loop over glorys files and calculate state variables and fluxes -----------
  
  # creating placeholders
  box_props <- face_props <- face_props_sum <- vector("list", nrow(file_db))
  i_timeslice <- 1
  
  box_cell = dplyr::rename(box_z_index, level = glorys_levels)  
  face_cell = dplyr::rename(face_z_index, level = glorys_levels)

  model_time = numeric(nrow(file_db))
  
  # for(i in 1:nrow(file_db)){model_time[i] = ncvar_get(nc_open(file_db$fullname[i]),'model_time')}
  
  
  for (i_timeslice in seq(nrow(file_db))) {
  # for(i_timeslice in 300){
  
    print(i_timeslice)
    
    #glorys file name and band name
    roms_file <-file_db$fullname[i_timeslice]

    model_time[i_timeslice] = file_db$time[i_timeslice]
    
    level <- file_db$band_level[i_timeslice]
    
    #Sets extent of u,v,w, temp, and salt
    if(make.hflux){
      r_u <- set_indextent(brick(paste0(glorys.dir,roms_file), varname = "uo", lvar = 4, level = level, ncdf=T))
      r_v <- set_indextent(brick(paste0(glorys.dir,roms_file), varname = "vo", lvar = 4, level = level, ncdf=T))
    }
    if(make.physvars){
      # r_w <- set_indextent(brick(paste0(glorys.dir,roms_file), varname = "w", lvar = 4, level = level, ncdf=T))
      r_temp <- set_indextent(brick(paste0(glorys.dir,roms_file), varname = "thetao", lvar = 4, level = level, ncdf=T))
      r_salt <- set_indextent(brick(paste0(glorys.dir,roms_file), varname = "so", lvar = 4, level = level, ncdf=T))
    }
    
    # tic()
    if(make.hflux){
      face_z_index$u <- extract_at_level(readAll(r_u), face_cell)
        rm(r_u)
      face_z_index$v <- extract_at_level(readAll(r_v), face_cell)
        rm(r_v)
    }
    
    if(make.physvars){
      box_z_index$temp <- extract_at_level(readAll(r_temp), box_cell)
        rm(r_temp)
      box_z_index$salt <- extract_at_level(readAll(r_salt), box_cell)
        rm(r_salt)
    }

    ### added to get missing data back in as NA dimensions should be 30x4=120 for each date
    
    ##WHICH ONE?
    # note - ungroup and complete (both vars) needed to get to desired dimension, works now
    ## add proper box number to sort on
    box_z_index1=left_join(box_z_index, bgm$boxes[c("label", ".bx0")], by=c("box"="label")) 
    
    ### RM 20180320 drop data (set NA) in boxes deeper than atlantis_depth by box numberusing NEUSz (above)
    ##Add number of total NEUS Atlantis levels per box
    box_z_index1=left_join(box_z_index1, NEUSz, by='.bx0') 

    if(make.physvars){
      
      #Aggregate vertically weighted by z weights
      box_z_index2 =x=box_z_index1 %>%
        group_by(.bx0,atlantis_levels,cell,cell_area) %>%
        summarize(temp = weighted.mean(temp,z.wgt,na.rm=T),
                  salt = weighted.mean(salt,z.wgt,na.rm=T))%>%
        mutate(date = file_db$date[i_timeslice])
      
      out.name = paste0(out.dir,'GLORYS_Statevars_NEUS_Level_',file_db$date[i_timeslice],'.csv')
      write.csv(box_z_index2,file = out.name,row.names = F)
    }

  }
  

  
}

# make_ROMS_files(glorys.dir,glorys.prefix,glorys.files,out.dir,dz.file,bgm.file,shp.file, name.out, make.hflux =T, make.physvars = T)
x = left_join(x,cell.index)
ggplot(x,aes(x=lon,y=lat,fill = temp))+geom_tile()
