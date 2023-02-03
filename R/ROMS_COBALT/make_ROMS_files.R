#' Converts raw ROMS-formatted output into the form used by hydroconstruct
#' 
#' Modified from original script used to process ROMS-Doppio output into Atlantis.
#' Reads in physical and biological float variables on ROMS grid structure and transforms
#' into Atlantis boxes/faces. Then uses ROMS vertical stretching coordinates to match to
#' Atlantis layers. State variables (temperature, salinity, biological groups, etc.) are
#' aggregated across all ROMS cells within an Atlantis box. All horizontal fluxes (flows)
#' are aggregated across ROMS cells intersecting Atlantis faces. Output is organized in arrays
#' and exported as .nc files to be further processed by Hydroconstruct. 
#' 
#' This script is based on ROMS_COBALT physical and lower trophic level variables. Must ensure correct names are used
#' for different output.Function is also hard-coded to NEUS Atlantis model 
#' 
#' @param roms.dir String. Defines path to ROMS output. All .nc files within path must be ROMS output
#' @param roms.prefix String. ROMS output prefix to pattern match
#' @param out.dir String. Path for output files
#' @param name.out String. Prefix for output (arrays and .nc) files
#' 
#' @return Output is a 4D array [NEUS level,Box/Face, Time, Var] for each processed variable as well as well as
#' .nc files for horizontal exchanges, phyical state variables, and biological state variables
#' 
#' #' Created by R. Morse and modified by J. Caracappa

# # roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Test_Output/1980/'
# roms.dir = 'D:/NWA/1980/'
# roms.prefix = 'RM_NWA-SZ.HCob05T_avg_'
# out.dir = 'D:/OUtput/1980/'
# name.out = 'roms_cobalt_'

# roms.dir =local.dir
# roms.prefix = 'RM_NWA-SZ.HCob05T_avg_'
# out.dir = paste0(local.output.dir,dir.names[yr],'/')
# name.out = 'roms_cobalt_'

make_ROMS_files = function(roms.dir,roms.prefix,out.dir,name.out){
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
  library(tictoc) #just for timing
  
  #Make sure package conflicts are sorted out
  complete = tidyr::complete
  select = dplyr::select
  
  # Convenience Functions ---------------------------------------------------
  
  # Transforms map projection
  project_to <- function(x, to) {
    spTransform(x, CRS(projection(to)))
  }
  
  # Determine which box each point falls within
  index_box = function(box_sp, roms_ll){
    ind <- sp::over(project_to(coords_points(roms_ll), box_sp) , as(box_sp, "SpatialPolygons"))
    tibble(box = box_sp$label[ind], cell = seq_len(ncell(roms_ll))) %>% 
      filter(!is.na(box))
  }
  
  # Returns a ramp of positive depths from the surface down (makes the order native to NetCDF order)
  roms_level <- function(Cs_r, h, cell) {
    raster::extract(h, cell) *  Cs_r
  }
  
  # matching cell for the right group
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
  
  # sets the extent of indexed data
  set_indextent <- function(x) {
    setExtent(x, extent(0, ncol(x), 0, nrow(x)))
  }
  
  
  # Read in External Data Files ---------------------------------------------
  # Read box_depth data (shows depth of each layer in each box)
  
  dz_box = read.csv(here('Geometry','dz.csv'),header=T)
  # dz_box = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/dz.csv',header = T)
  
  # Read BGM file
  bgm = bgmfile(here('Geometry','neus_tmerc_RM2.bgm'))
  # bgm = bgmfile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm')
  
  # Read boxes shape file
  neus.shp = rgdal::readOGR(here('Geometry','Neus_ll_0p01.shp'))
  # neus.shp = rgdal::readOGR('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/Neus_ll_0p01.shp')
  
  
  # Read in ROMS Output  -----------------------------------------------------
  setwd(roms.dir)
  # roms
  roms_files = dir(roms.dir,pattern = roms.prefix)
  
  #Creates table of ROMS output files: For daily files (don't need RM old function, all have ocean_time length of 1)
  file_db = tibble(fullname = roms_files,band_level = 1:length(roms_files))
  
  
  # Format and Summarize Box and Shape Files --------------------------------------------
  
  # rbgm::boxSpatial returns spatial polygons as a spatial object
  boxes = boxSpatial(bgm)
  
  # Determine boundary box for each neus box
  spatialbbox=list()
  for (i in 1:length(neus.shp@data$BOX_ID)){
    spatialbbox[[i]]=bbox(neus.shp@polygons[[i]])
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
    test=bbox(neus.shp@polygons[[i]])
    # distMeeus calculates shortest distance between two points on defined ellipsoid (Meeus method)
    #NS distance in meters
    bgm_bbox$dist_NS[i]=distMeeus(test[,1], cbind(test[1,1],test[2,2]), a=6378137, f=1/298.577223563) 
    #EW distance in meters
    bgm_bbox$dist_EW[i]=distMeeus(test[,1], cbind(test[1,2],test[2,1]), a=6378137, f=1/298.577223563)
  }
  
  # save(bgm,file='C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_boxes_info.R')
  
  # Create Table similar to Isaac's for hydroconstruct (unused) ----------------------
  
  # test=data.frame(bgm$boxes$.bx0)
  # colnames(test)='polygon'
  # test2=left_join(test, bgm$facesXboxes, by=c('polygon'='.bx0'))
  # # test3=left_join(test2, d)
  # z3=read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/NEUSdepth.csv', header=T, stringsAsFactors = F)
  # try=list()
  # lev=list()
  # for(i in 1:length(z3$Box)){
  #   if (z3$numlayers[i] > 0 & !is.na(z3$numlayers[i])){
  #     try[[i]]=rep(z3$Box[i], z3$numlayers[i])
  #     lev[[i]]=seq(from=1, to=z3$numlayers[i], by=1)
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
  # # final$iface=final$iface+1
  # write.table(final, file='C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/NEUSlookup.csv', col.names = F, sep=',', row.names = F)
  # 
  
  # Convert NEUS boxes to ROMS coordinates ----------------------------------
  
  #Using rho points for all export variables. Rho is in center and u- and v-points on verticles of cells.
  #Not as percise, but on scale on Atlantis boxes, point deviations shouldn't matter (can revisit if necessary)
  
  
  # Obtain lat/lon for rho-points
  roms_ll_rho<- romscoords(roms_files[[1]], transpose = TRUE, spatial = c("lon_rho", "lat_rho"))
  # Rotate longitude from (0-,360) to (-180,180 )
  roms_ll_rho$longitude.of.RHO.points = ((roms_ll_rho$longitude.of.RHO.points+180) %% 360) - 180
  # extent(roms_ll_rho) = c(-180,180,0,161)
  # obtain find nearest-neighbor of bgm faces on ROMS grid
  roms_face_rho <- romsmap(project_to(faceSpatial(bgm), "+init=epsg:4326"), roms_ll_rho)
  
  #get coordinates of faces in terms of ROMS XY
  bgm_lstXY = coordinates(roms_face_rho)
  #creates matrix of face coordinates
  bgmXY = matrix(ncol = length(bgm_lstXY), nrow = 4, unlist(bgm_lstXY))
  #IMPORTANT ORDER FOR GEOMETRY! Point 1 = x1,y1; Point 2 = x2,y2
  rownames(bgmXY) = c('x1','x2','y1','y2')
  #Turn into long dataframe with roms grid X and Y coordinates, to use with romes_ll_...
  bgmXY = data.frame(t(bgmXY))
  #Add empty values for fill for lat/lon of face coords
  bgmXY$lat1=NA
  bgmXY$lat2=NA
  bgmXY$lon1=NA
  bgmXY$lon2=NA
  
  ### Find lat and lon for the BGM shape with extractions by roms XY coordinates
  p1 = cellFromXY(roms_ll_rho,cbind(bgmXY[,1],bgmXY[,3]))
  p2 = cellFromXY(roms_ll_rho,cbind(bgmXY[,2],bgmXY[,4]))
  bgmXY$lat1 = roms_ll_rho[p1][,2]
  bgmXY$lat2 = roms_ll_rho[p2][,2]
  bgmXY$lon1 = roms_ll_rho[p1][,1]
  bgmXY$lon2 = roms_ll_rho[p2][,1]
  
  #Create face ID. Starts at 0 not 1
  bgmXY$face = seq(nrow(bgmXY))-1
  
  
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
  angles$bearingRhumb = bearingRhumb(p1 = bearmat[,1:2],p2 = bearmat[,3:4])
  # convert bearing into polar XY coordinates where 0 is east (x) and 90 is North(y)
  angles$XYbearingRhumb = ((360-angles$bearingRhumb)+90)%%360
  
  ### note - 1 point results in an NA bearing (face 31) because lat and long points are the same in BGM file. ###
  
  #Plot boxes and Rhumb line to check
  # ggplot()+geom_segment(data=angles,aes(x= lon1,xend = lon2,y = lat1,yend = lat2))
  # angles$distRhumb = distRhumb(p1 = bearmat[,1:2],p2 = bearmat[,3:4])
  # data(merc)
  # data(wrld)
  # plot(wrld, type='l', xlim=c(-68,-65), ylim=c(44,45), main='Equirectangular',lty=NA)
  # for(i in 1:nrow(angles)){
  #   pr=gcIntermediate(angles[i,c(5,7)],angles[i,c(6,8)])
  #   lines(pr[,2:1])
  # }
  # points(bearmat[32,1],bearmat[32,2],cex = 2,col=2,pch=16)
  
  
  # Build Index for the ROMS Cell of NEUS Boxes and Faces -------------------
  
  #For each face, which cells does it traverse
  box_roms_rhoindex = index_box(boxes, roms_ll_rho)
  
  #raster::cellFromLine determines which cells each face line intersects (u and v)
  ind_face_u = cellFromLine(romsdata(roms_files[1],'u'),roms_face_rho)
  ind_face_v = cellFromLine(romsdata(roms_files[1],'v'),roms_face_rho)
  
  # creates table of face cell index for u and v points
  face_roms_vindex <- tibble(face = roms_face_rho$label[rep(seq_len(nrow(roms_face_rho)), lengths(ind_face_v))], 
                             cell = unlist(ind_face_v))
  face_roms_uindex <- tibble(face = roms_face_rho$label[rep(seq_len(nrow(roms_face_rho)), lengths(ind_face_u))], 
                             cell = unlist(ind_face_u))
  
  
  # Create Index between ROMS and NEUS Levels -------------------------------
  
  #Read cell depths from ROMS
  h <- readAll(raster(roms_files[1], varname = "h", ncdf=T))
  
  ## Cs_r is the S-coord stretching (length is num layers from -1 to 0 describing what portion of the w.c. each layer spans)
  ### CANNOT NC_OPEN WITHIN FUNCTION. INDEFINITELY KEEPS CONNECTION OPEN
  dumm = nc_open(roms_files[1])
  Cs_r = ncvar_get(dumm,'Cs_r')
  nc_close(dumm)
  # Cs_r <- rawdata(roms_files[1], "Cs_r")
  
  #Atlantis depths
  max_depth <- 500 # max(extract(h, unique(box_roms_index$cell)))
  # Specific to the NEUS model
  atlantis_depths <- -rev(cumsum(c(0, rev(rbgm::build_dz(-max_depth, zlayers = c(-500, -300, -120, -50, 0))))))
  
  # add data for max numlayers for Atlantis model
  countZLayer=apply(dz_box[,2:5], c(1,2), function(x) any(is.finite(x)))
  NEUSz=data.frame(dz_box[,1]); colnames(NEUSz)='.bx0'
  NEUSz$NEUSlevels=rowSums(countZLayer)
  NEUSz$zmax = apply(dz_box[,2:5],1,function(x) sum(x,na.rm=T))
  
  ## build the level index between Atlantis and ROMS
  list_nc_z_rhoindex <- vector('list', nrow(box_roms_rhoindex))
  list_nc_z_uindex <- vector('list', nrow(face_roms_uindex))
  list_nc_z_vindex <- vector('list', nrow(face_roms_vindex))
  
  # Loop through each cell, identify depths of ROMS layers, identify corresponding NEUS layers
  for (i in seq_len(nrow(box_roms_rhoindex))) {
    
    # retreives depth of each cell and applies coord-stretching
    rl <- roms_level(Cs_r, h, box_roms_rhoindex$cell[i])
    
    focal.box = bgm$boxes$.bx0[bgm$boxes$label == box_roms_rhoindex$box[i]]
    
    if(!(focal.box %in% c(23,24))){
      z.box = NEUSz$zmax[NEUSz$.bx0 == focal.box]
      rl = rl[rl >= -z.box]
    }
    
    # implicit 0 at the surface, and implicit bottom based on ROMS
    # Identifies where ROMS depths fit in NEUS intervals and reverses order (NEUS: 1 at bottom, 4 at surface)
    list_nc_z_rhoindex[[i]] <- length(atlantis_depths) - findInterval(rl, atlantis_depths, all.inside = F) # + 1
    list_nc_z_rhoindex[[i]][which(list_nc_z_rhoindex[[i]]==5)]=NA ### remove depths greater than 500
    
    if (i %% 1000 == 0) print(i)
  }
  gc()
  
  # join the box-xy-index to the level index using rho coordinates
  box_z_index <- bind_rows(lapply(list_nc_z_rhoindex, 
                                  function(x) tibble(atlantis_level = pmax(1, x), roms_level = seq_along(x))), 
                           .id = "cell_index") %>% 
    filter(!is.na(atlantis_level)) %>%
    inner_join(mutate(box_roms_rhoindex, cell_index = as.character(row_number()))) %>% 
    select(-cell_index)
  
  # Repeat same process for U and V indices
  for (i in seq_len(nrow(face_roms_uindex))) {
    rl_u = roms_level(Cs_r, h, face_roms_uindex$cell[i])
    rl_v = roms_level(Cs_r, h, face_roms_vindex$cell[i])
    
    uface = face_roms_uindex$face[i]
    vface = face_roms_vindex$face[i]
    
    box.left = bgm$faces$left[bgm$faces$label == uface]
    box.right = bgm$faces$right[bgm$faces$label == uface]
    
    if(!(box.left %in% c(23,24) | box.right %in% c(23,24))){
      
      z.left = NEUSz$zmax[NEUSz$.bx0 == box.left]
      z.right = NEUSz$zmax[NEUSz$.bx0 == box.right]
      
      box.min = ifelse(z.left<=z.right,box.left,box.right)
      box.max = ifelse(z.left>z.right,box.left,box.right)
      
      z.min = min(c(z.left,z.right))
      z.max = max(c(z.left,z.right))
      
      rl_u = rl_u[rl_u >= -z.min]
      rl_v = rl_v[rl_v >= -z.min]
    }
 
    
    ## implicit 0 at the surface, and implicit bottom based on ROMS
    list_nc_z_uindex[[i]] <- length(atlantis_depths) -findInterval(rl_u, atlantis_depths) #+ 1
    list_nc_z_uindex[[i]][which(list_nc_z_uindex[[i]]==5)]=NA ### remove depths greater than 500
    
    list_nc_z_vindex[[i]] <- length(atlantis_depths) -findInterval(rl_v, atlantis_depths) #+ 1
    list_nc_z_vindex[[i]][which(list_nc_z_vindex[[i]]==5)]=NA ### remove depths greater than 500
    
    if (i %% 1000 == 0) print(i)
  }
  
  gc()
  rm(h)
  
  ## join the face-xy-index to the level index
  face_z_uindex <- bind_rows(lapply(list_nc_z_uindex, 
                                    function(x) tibble(atlantis_level = x, roms_level = seq_along(x))), 
                             .id = "cell_index")  %>%
    filter(!is.na(atlantis_level)) %>%
    inner_join(mutate(face_roms_uindex, cell_index = as.character(row_number()))) %>%
    select(-cell_index)
  # face_z_uindex = face_z_uindex[which(!is.na(face_z_uindex$atlantis_level)),]
  
  face_z_vindex <- bind_rows(lapply(list_nc_z_vindex, 
                                    function(x) tibble(atlantis_level = x, roms_level = seq_along(x))), 
                             .id = "cell_index") %>% 
    filter(!is.na(atlantis_level)) %>%
    inner_join(mutate(face_roms_vindex, cell_index = as.character(row_number()))) %>% 
    select(-cell_index)
  # face_z_vindex = face_z_vindex[which(!is.na(face_z_vindex$atlantis_level)),]
  
  # Loop over ROMS files and calculate state variables and fluxes -----------
  
  # creating placeholders
  box_props <- box_props_cob <- face_props <- face_props_sum <- vector("list", nrow(file_db))
  i_timeslice <- 1
  
  face_cell_u  = dplyr::rename(face_z_uindex, level = roms_level, cell = cell)
  face_cell_v = dplyr::rename(face_z_vindex, level = roms_level, cell = cell)
  box_cell = dplyr::rename(box_z_index, level = roms_level, cell = cell)
  ocean_time = numeric(nrow(file_db))
  
  # for(i in 1:nrow(file_db)){ocean_time[i] = ncvar_get(nc_open(file_db$fullname[i]),'ocean_time')}
  
  
  for (i_timeslice in seq(nrow(file_db))) {
  # for(i_timeslice in 1:2){
  # tic()
    print(i_timeslice)
    
    #roms file name and band name
    roms_file <- file_db$fullname[i_timeslice]
    
    roms.nc = nc_open(paste0(roms.dir,'/',roms_file))
    ocean_time[i_timeslice] = ncvar_get(roms.nc,'ocean_time')
    nc_close(roms.nc)
    # level <- file_db$band_level[i_timeslice]
    level = 1
  
    #Sets extent of u,v,w, temp, and salt
    r_u <- set_indextent(brick(roms_file, varname = "u", lvar = 4, level = level, ncdf=T))
    r_v <- set_indextent(brick(roms_file, varname = "v", lvar = 4, level = level, ncdf=T))
    r_w <- set_indextent(brick(roms_file, varname = "w", lvar = 4, level = level, ncdf=T))
    r_temp <- set_indextent(brick(roms_file, varname = "temp", lvar = 4, level = level, ncdf=T))
    r_salt <- set_indextent(brick(roms_file, varname = "salt", lvar = 4, level = level, ncdf=T))
    
    #COBALT PARAMS
    r_rho <- set_indextent(brick(roms_file, varname = "rho", lvar = 4, level = level, ncdf=T)) #Fluid Density Anomaly
    r_ndi <- set_indextent(brick(roms_file, varname = "ndi", lvar = 4, level = level, ncdf=T)) #Diazotroph N
    r_nlg <- set_indextent(brick(roms_file, varname = "nlg", lvar = 4, level = level, ncdf=T)) #Large Phyto N
    r_nlgz <- set_indextent(brick(roms_file, varname = "nlgz", lvar = 4, level = level, ncdf=T)) #Large Zoo N
    r_nmdz <- set_indextent(brick(roms_file, varname = "nmdz", lvar = 4, level = level, ncdf=T)) #Med Zoo N
    r_nsm <- set_indextent(brick(roms_file, varname = "nsm", lvar = 4, level = level, ncdf=T)) #Small Phtyo N
    r_nsmz <- set_indextent(brick(roms_file, varname = "nsmz", lvar = 4, level = level, ncdf=T)) #Small Zoo N
    r_silg <- set_indextent(brick(roms_file, varname = "silg", lvar = 4, level = level, ncdf=T)) #Large Phyto Si
    r_nbact <- set_indextent(brick(roms_file, varname = "nbact", lvar = 4, level = level, ncdf=T)) #Bacterial N
    
    # tic()
    
    face_z_uindex$ue <- extract_at_level(readAll(r_u), face_cell_u); rm(r_u)
    face_z_vindex$vn <- extract_at_level(readAll(r_v), face_cell_v); rm(r_v)
    box_z_index$w <- extract_at_level(readAll(r_w),box_cell ); rm(r_w)
    box_z_index$temp <- extract_at_level(readAll(r_temp), box_cell); rm(r_temp)
    box_z_index$salt <- extract_at_level(readAll(r_salt), box_cell); rm(r_salt)
    
    #COBALT PARAMS
    box_z_index$rho <- extract_at_level(readAll(r_rho), box_cell)+1000; rm(r_rho)
    #convert biological groups from molN/kg to mgN/m3
    rho_scale = box_z_index$rho*14.0067*1E3
    
    box_z_index$ndi <- extract_at_level(readAll(r_ndi), box_cell)*rho_scale; rm(r_ndi)
    box_z_index$nlg <- extract_at_level(readAll(r_nlg), box_cell)*rho_scale; rm(r_nlg)
    box_z_index$nlgz <- extract_at_level(readAll(r_nlgz), box_cell)*rho_scale; rm(r_nlgz)
    box_z_index$nmdz <- extract_at_level(readAll(r_nmdz), box_cell)*rho_scale; rm(r_nmdz)
    box_z_index$nsm <- extract_at_level(readAll(r_nsm), box_cell)*rho_scale; rm(r_nsm)
    box_z_index$nsmz <- extract_at_level(readAll(r_nsmz), box_cell)*rho_scale; rm(r_nsmz)
    box_z_index$silg <- extract_at_level(readAll(r_silg), box_cell)*box_z_index$rho*1E3*28.0855; rm(r_silg)
    box_z_index$nbact <- extract_at_level(readAll(r_nbact), box_cell)*rho_scale; rm(r_nbact)
    
    # toc()
    
    ### added to get missing data back in as NA dimensions should be 30x4=120 for each date
    
    ##WHICH ONE?
    # note - ungroup and complete (both vars) needed to get to desired dimension, works now
    ## add proper box number to sort on
    box_z_index2=left_join(box_z_index, bgm$boxes[c("label", ".bx0")], by=c("box"="label")) 
    
    ### RM 20180320 drop data (set NA) in boxes deeper than atlantis_depth by box numberusing NEUSz (above)
    ##Add number of total NEUS Atlantis levels per box
    box_z_index2=left_join(box_z_index2, NEUSz, by='.bx0') 
    
    # Index where number of levels in roms is greater than atlantis box depth
    ## WHERE DOES TEST COME FROM?
    # idx=test$roms_level>test$atlantis_level
    # idx = box_z_index2$roms_level > box_z_index2$atlantis_level
    # box_z_index2[idx,'w']=NA
    # box_z_index2[idx,'salt']=NA
    # box_z_index2[idx,'temp']=NA
    # 
    # box_z_index2[idx,'rho'] = NA
    # box_z_index2[idx,'ndi'] = NA
    # box_z_index2[idx,'nlg'] = NA
    # box_z_index2[idx,'nlgz'] = NA
    # box_z_index2[idx,'nmdz'] = NA
    # box_z_index2[idx,'nsm'] = NA
    # box_z_index2[idx,'nsmz'] = NA
    # box_z_index2[idx,'silg'] = NA
    # box_z_index2[idx,'nbact'] = NA
    
    #box_props: summary of box-wide variables, grouped by box, atl_level, where means are used across cells
    box_props[[i_timeslice]] <- box_z_index2 %>% group_by(atlantis_level, .bx0) %>% 
      summarize(temp = mean(temp, na.rm = TRUE), salt = mean(salt ,na.rm = TRUE), vertflux=mean(w, na.rm=T)) %>% 
      ungroup(box_z_index2)%>%
      complete(atlantis_level, .bx0)
    box_props[[i_timeslice]]$band_level = file_db$band_level[i_timeslice]
      # mutate(band_level = level)
    
    # For biological parameters, summarize by MEAN (units are density (mol/kg)) * maybe convert to mgN per m^3
    box_props_cob[[i_timeslice]] <- box_z_index2 %>% group_by(atlantis_level, .bx0) %>%
      summarize(rho = mean(rho,na.rm=T),ndi = mean(ndi,na.rm=T), nlg = mean(nlg,na.rm=T), nlgz = mean(nlgz, na.rm=T), nmdz = mean(nmdz,na.rm=T),
                nsm = mean(nsm,na.rm=T), nsmz = mean(nsmz,na.rm=T), silg = mean(silg,na.rm=T), nbact = mean(nbact, na.rm=T)) %>%
      ungroup(box_z_index2) %>%
      complete(atlantis_level,.bx0)
    box_props_cob[[i_timeslice]]$band_level = file_db$band_level[i_timeslice]
    
    #Summarize horizontal flows
    face_z_uv_index=left_join(face_z_uindex, face_z_vindex, by = c("atlantis_level", "roms_level", "face")) # join u and v indices together
    face_z_uv_index2=left_join(face_z_uv_index, bgm$faces[c("label", ".fx0")], by=c("face"="label")) ## add proper box number to sort on
    
    ### RM mod 20180320 *** MEAN *** -> good, direction is same as previous, drop complete cases to reduce NAs -> 379(5) per time
    #face_props: summary of face-level flow magnitudes and direction, grouped by box/faceID with magnitude mean velocity as stat. and direction
    # Might revisit summary statistic (mean,median, sum,etc.)
    face_props[[i_timeslice]] <-  face_z_uv_index2 %>% group_by(atlantis_level, .fx0) %>% 
      summarize(velocity = sqrt((mean(ue, na.rm=T)^2) + (mean(vn, na.rm = TRUE)^2)), 
                dir.uv=atan2(mean(vn, na.rm=T),mean(ue, na.rm=T)), na.rm=T) %>% 
      ungroup(face_z_uvindex2) %>%
      complete(atlantis_level, .fx0) 
      # mutate(band_level = level)
      face_props[[i_timeslice]]$band_level = file_db$band_level[i_timeslice]
      
    # toc()
  }
  
  # save(box_props,box_props_cob,face_props,file = 'Test Dump.R')
  # load(paste0(roms.dir,'Test Dump.R'))
  
  # Combine box and face properties
  box_props <- bind_rows(box_props)
  box_props_cob <- bind_rows(box_props_cob)
  face_props <- bind_rows(face_props)
  
  
  
  #Remove data from islands (Boxes 23 and 24)
  box_props[which(box_props$.bx0==23 | box_props$.bx0==24), c('temp', 'salt', 'vertflux')]=-999 #islands
  box_props[is.na(box_props$vertflux),c('temp', 'salt', 'vertflux')]=-999 # change NA to fill value
  
  box_props_cob[which(box_props_cob$.bx0==23 | box_props_cob$.bx0==24), c('temp', 'salt', 'vertflux')]=-999 #islands
  box_props_cob[is.na(box_props_cob$vertflux),c('temp', 'salt', 'vertflux')]=-999 # change NA to fill value
  
  # Determine direction of horizontal fluxes --------------------------------
  
  # Need horizontal flux perpendicular to faces (rhumb line)
  # NOTE positive cos(theta) indicates flows from left to right; negative cos(theta) is flow from right to left -> destination box
  # may be hemispere local... this is for northern hemisphere
  
  # convert rhumb bearing to radians
  angles2=data.frame(angles$XYbearingRhumb)*(pi/180) 
  angles2$face=angles$face
  # in meters, WGS84 default
  angles2$dist=distMeeus(cbind(angles$lon1, angles$lat1), cbind(angles$lon2, angles$lat2))
  
  ### Fix direction of currents from face_props2 to positive radians (atan2 func gives -pi:pi; change -> 0:2pi)
  face_props$dir.uv[which(!is.na(face_props$dir.uv) & face_props$dir.uv<0)]=face_props$dir.uv[which(!is.na(face_props$dir.uv) & face_props$dir.uv<0)]+2*pi # normalize 0 to 2pi
  
  ### get quadrant of face to use to determine which direction flow is going (left:right or right:left)
  # angles2$quadrant=NA
  # for (i in 1:length(angles2$quadrant)){
  #   if (is.na(angles2$angles.XYbearingRhumb[i])){
  #     next
  #   } else if (angles2$angles.XYbearingRhumb[i] <= (90*pi/180)){
  #     angles2$quadrant[i]=1
  #   } else if (angles2$angles.XYbearingRhumb[i] <=(180*pi/180)){
  #     angles2$quadrant[i]=2
  #   } else if (angles2$angles.XYbearingRhumb[i] <=(270*pi/180)){
  #     angles2$quadrant[i]=3
  #   } else if (angles2$angles.XYbearingRhumb[i] <=(360*pi/180)){
  #     angles2$quadrant[i]=4
  #   }
  # }
  
  ### add some BGM box info, used later on merge and for hyperdiffusion correction
  box.area=data.frame(bgm$boxes$area)
  box.area$box=bgm$boxes$.bx0
  box.area$nconn=bgm$boxes$nconn  # used to determine max number of connections in .NC hydro forcing file
  box.area$z=bgm$boxes$botz
  box.area$volume=box.area$bgm.boxes.area*box.area$z*-1
  
  #add BGM info on faces gets left right boxes looking from p1 to p2 to use with 'angles'
  face_props2=left_join(face_props, bgm$faces, by=".fx0") # 
  
  # https://stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
  ### Note that u_east and v_north were used for direction calculation, so no rotation is necessary for current direction
  face_props2=left_join(face_props2, angles2, by=c(".fx0"="face"))
  
  ### THIS DETERMINES DIRECTION OF FLOW: pos values flow R:L, neg flows L:R from p1 looking to p2 of a face
  ### This is correct 20180322, the relative angle method below works and is correct. Positive rel_angle indicates flow 
  ### from R:L across a face, and L:R has a negative rel_angle. THE CONVERSION TO 0:2pi is NOT NEEDED AND MAY BE WRONG
  face_props2$rel_angle=atan2(sin(face_props2$dir.uv -face_props2$angles.XYbearingRhumb), cos(face_props2$dir.uv -face_props2$angles.XYbearingRhumb))
  
  # 20180322 Updata: copy prior to crazyness below... THIS MAY NOT BE CORRECT
  ### CHECK ON ADDITION OF 90 Degrees... 20180322
  face_props2$cos_theta=abs(cos(face_props2$rel_angle + pi/2)) ### this is the scalar for flux velocity across face, plus 90 degrees rotation
  
  ### direction of flow depends on quadrant the face lies in: (this may be hemisphere dependent???)
  ### where x=current direction in XY coords, y=bearing of face from p1 to p2 (rhumbline) in XY coords (all in radians)
  # If x=y | x -y = +-pi then parallel (no flux)
  # If Q I/II : 
        #if x > y | x + pi < y  Flux R:L (pos)
        #if x < y | x + pi > y  Flux L:R (neg)
  # If Q III/IV:
        #if x > y | x - pi < y  Flux R:L (pos)
        #if x < y | x - pi > y  Flux L:R
  # for (i in 1:length(face_props2$left)){
  #   if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$atlantis_level)*100, '%'))
  #   x=face_props2$dir.uv[i]
  #   y=face_props2$angles.XYbearingRhumb[i]
  #   if (is.na(face_props2$cos_theta[i]) | is.na(face_props2$quadrant[i])){
  #     next
  #   } else if(x == y | (x-y) == pi | (y-x) == pi){
  #     y = face_props2$fluxsign[i] = 0
  #     next
  #   } else if (face_props2$quadrant[i]==1 | face_props2$quadrant[i]==2){
  #     if (x > y | (x+pi) < y){
  #       face_props2$destbox[i]=face_props2$left[i]
  #       face_props2$fluxsign[i]=1 #positive flux R:L
  #       face_props2$destbox.lr[i]='l'
  #     } else if(x < y | (x+pi)>y){
  #       face_props2$destbox[i]=face_props2$right[i]
  #       face_props2$fluxsign[i]=-1 #negative flux L:R
  #       face_props2$destbox.lr[i]='r'
  #     } else {
  #       next
  #     }
  #   } else if (face_props2$quadrant[i]==3 | face_props2$quadrant[i]==4){
  #     if ( x>y | (x-pi) <y){
  #       face_props2$destbox[i]=face_props2$left[i]
  #       face_props2$fluxsign[i]=1 
  #       face_props2$destbox.lr[i]='l'
  #     } else if(x<y | (x-pi)>y){
  #       face_props2$destbox[i]=face_props2$right[i]
  #       face_props2$fluxsign[i]=-1 #positve flux R:L
  #       face_props2$destbox.lr[i]='r'
  #     } else {
  #       next
  #     }
  #   }
  # }
  
  # add area -> length of face x depth from dz file... need to merge on dz_box "polygon"="left" (same for "right" later) && "l1"="atlantis_level==1"
  face_props2=left_join(face_props2, dz_box, by=c("left"="polygon")) ### l1.x-l4.x is boxleft
  face_props2=left_join(face_props2, dz_box, by=c("right"="polygon")) ### l1.y-l4.y is boxright
  face_props2$Sediment_1.x=NULL
  face_props2$Sediment_1.y=NULL
  
  ### try this instead: 20180323
  face_props2$destbox.new=ifelse(face_props2$rel_angle>0, face_props2$left, face_props2$right) #R to L is pos flow, LtoR neg
  face_props2$origbox.new=ifelse(face_props2$rel_angle<0, face_props2$left, face_props2$right) #R to L is pos flow, LtoR neg
  face_props2$destbox.lr.new=ifelse(face_props2$rel_angle>0, 'l', 'r') # probably not needed
  face_props2$fluxsign.new=ifelse(face_props2$rel_angle>0, 1, -1)
  
  ### add number of levels for box left (x) and boxright(y)
  face_props2=left_join(face_props2, NEUSz, by=c("destbox.new"=".bx0")) ### NEUSlevels.x is for destination box
  face_props2=left_join(face_props2, NEUSz, by=c("origbox.new"=".bx0"),suffix = c("_dest", "_orig")) # rename dest and orig
  
  ### manually try this... 20180323, this works, slow, could make bottom more like top, also add face area...
  # face_props2$orig_z=ifelse((!is.na(face_props2$fluxsign) & face_props2$atlantis_level==1), 1, NA)
  # face_props2$dest_z=ifelse((!is.na(face_props2$fluxsign) & face_props2$atlantis_level==1), 1, NA) # misses areas with NA (islands...) ->fixed
  # face_props2$facearea=NA 
  
  ### origin level - added 20180325
  face_props2$orig_z=ifelse(face_props2$atlantis_level <= face_props2$NEUSlevels_orig, face_props2$atlantis_level, face_props2$NEUSlevels_orig)
  
  ### destination level - added 20180325
  face_props2$dest_z=ifelse(face_props2$atlantis_level <= face_props2$NEUSlevels_dest, face_props2$atlantis_level, face_props2$NEUSlevels_dest)
  # get depths of boxes, reverse order 1=shallow, 4=deep NEUS ONLY
  dz_box2=dz_box[,c(5,4,3,2,1)] 
  
  ### depth of origin box * length of face added 20180325
  face_props2$facearea=NA
  for (i in 1:length(face_props2$atlantis_level)){
    if (is.na(face_props2$fluxsign.new[i])|is.na(face_props2$atlantis_level[i])){
      next
    } else {
      face_props2$facearea[i]=dz_box2[(face_props2$origbox.new[i]+1) ,face_props2$orig_z[i]]*face_props2$length[i]
    }
  }
  
  #### flux= velocity*cos(theta)*area in meters
  face_props2$flux=NA
  for (i in 1:length(face_props2$rel_angle)){
    if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$rel_angle)*100, '%'))
    if (is.na(face_props2$destbox.lr.new[i])){
      next
    } 
    face_props2$flux[i]=face_props2$cos_theta[i] *face_props2$fluxsign.new[i] * face_props2$velocity[i] *face_props2$facearea[i]
  }
  ### add time back in to daily mean flows (not sure which to use yet 20180325)
  face_props2$fluxtime=face_props2$flux *86400
  
  
  ### Define dimensions for the two NetCDF files that go into hydroconstruct:
  nboxes=length(unique(box_props$.bx0)) #30
  ntimes=length(unique(box_props$band_level))
  nlevel=length(unique(box_props$atlantis_level))
  atl.level=unique(box_props$atlantis_level)
  nfaces=length(unique(face_props2$.fx0))
  
  ### fix time dimension - needs to be from 1964 for NEUS
  # ynum=0 # use for 2008 data (first year of data)
  # # ynum=365 # use for 2009 data (1 year already processed)
  # # ynum=730 # use for 2010 data (2 years already processed)
  # 
  # # ocean_time=ncdf4::ncvar_get(ncdf4::nc_open(roms_files[1]), varid='ocean_time') 
  # ocean_time = sapply(roms_files,function(x) ncvar_get(nc_open(x),varid='ocean_time'))
  # ### MAKE SURE TO SELECT ABOVE ynum CORRECTLY
  # t_start=min(unique(face_props2$band_level))+ynum 
  # # seconds in one day
  # dt=86400 
  # ### MAKE SURE TO SELECT ABOVE ynum CORRECTLY
  # # t_stop = ocean_time[1]+(dim(exch_nc)[4]-1)*86400
  # t_stop=max(unique(face_props2$band_level))+ynum 
  # t_tot=seq(t_start*dt,t_stop*dt,dt)
  
  #subtract seconds between 1-1-1964 and 1-1-1900 
  #ROMS is 1900, NEUS is 1964 reference point
  t_tot = ocean_time  - 2019600000
  dt=86400 # seconds in one day
  # t_tot = seq(ocean_time[1],by = dt, length.out = length(roms_files))
  
  
  ## variables in transport file:
  faces=angles$face
  pt1_x=angles$lon1
  pt2_x=angles$lon2
  pt1_y=angles$lat1
  pt2_y=angles$lat2
  dest_boxid=bgm$faces$left
  # positive flow is right to left across face from p1 to p2
  source_boxid=bgm$faces$right 
  
  year = last(strsplit(getwd(),'/')[[1]])
  
  ### create 3d array of transport vals
  transport=array(NA, dim=c(nlevel, nfaces, ntimes))
  for (i in 1:length(face_props2$flux)){
    if (i %% 10000 ==0) print(paste('progress:',i/length(face_props2$flux)*100, '%'))
    j=face_props2$band_level[i] #time
    k=face_props2$.fx0[i]+1 ### Face NOTE added 1 because index cannot be 0, must remove later (maybe not?)
    l=face_props2$atlantis_level[i]# depth
    transport[l,k,j]=face_props2$fluxtime[i] # time now added back in (flux per day in seconds)
  }
  
  #Write transport array as R object
  save('transport',file = paste0(out.dir,name.out,'transport_',year,'.R'))
  
  ### create vars for box structure data
  ### NOTE added 1 because index cannot be 0, must remove 1 later w/ ncks
  box.boxes=bgm$boxes$.bx0 
  salinity=array(NA, dim=c(nlevel,nboxes, ntimes))
  temperature=array(NA, dim=c(nlevel,nboxes,ntimes))
  vertical_flux=array(NA, dim=c(nlevel,nboxes,ntimes))
  
  ##COBALT vars
  ndi=array(NA, dim=c(nlevel,nboxes, ntimes))
  nlg=array(NA, dim=c(nlevel,nboxes, ntimes))
  nlgz=array(NA, dim=c(nlevel,nboxes, ntimes))
  nmdz=array(NA, dim=c(nlevel,nboxes, ntimes))
  nsm=array(NA, dim=c(nlevel,nboxes, ntimes))
  nsmz=array(NA, dim=c(nlevel,nboxes, ntimes))
  silg=array(NA, dim=c(nlevel,nboxes, ntimes))
  nbact=array(NA, dim=c(nlevel,nboxes, ntimes))
  
  for (i in 1:length(box_props$temp)){
    if (i %% 5000 ==0) print(paste('progress:',i/length(box_props$temp)*100, '%'))
    #time
    j=box_props$band_level[i]
    ### box NOTE added 1 because index cannot be 0, must remove later
    k=(box_props$.bx0[i])+1 
    # depth
    l=box_props$atlantis_level[i]
    
    vertical_flux[l,k,j]=box_props$vertflux[i]
    temperature[l,k,j]=box_props$temp[i]
    salinity[l,k,j]=box_props$salt[i]
    
    #cobalt
    ndi[l,k,j]=box_props_cob$ndi[i]
    nlg[l,k,j]=box_props_cob$nlg[i]
    nlgz[l,k,j]=box_props_cob$nlgz[i]
    nmdz[l,k,j]=box_props_cob$nmdz[i]
    nsm[l,k,j]=box_props_cob$nsm[i]
    nsmz[l,k,j]=box_props_cob$nsmz[i]
    silg[l,k,j]=box_props_cob$silg[i]
    nbact[l,k,j]=box_props_cob$nbact[i]
  }
  
  save('vertical_flux','temperature','salinity',file = paste0(out.dir,name.out,'statevars_',year,'.R'))
  save('ndi','nlg','nlgz','nmdz','nsm','nsmz','silg','nbact',file = paste0(out.dir,name.out,'ltl_statevars_',year,'.R'))
  
  ### FOR TRANSPORT NC FILE
  filename=paste0(out.dir,name.out,'transport_2hydro.nc')
  
  #define dimensions
  timedim=ncdim_def("time", "", 1:length(t_tot), unlim=T, create_dimvar = F) #as.double(t_tot)
  leveldim=ncdim_def("level", "", 1:nlevel, create_dimvar = F)
  facesdim=ncdim_def("faces", "", 1:nfaces, create_dimvar = F)
  
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
  ncvar_put(nc_transp,var.lev,atl.level)
  ncvar_put(nc_transp,var.face,faces)
  ncvar_put(nc_transp,var.destb,dest_boxid)
  ncvar_put(nc_transp,var.pt1x,pt1_x)
  ncvar_put(nc_transp,var.pt1y,pt1_y)
  ncvar_put(nc_transp,var.pt2x,pt2_x)
  ncvar_put(nc_transp,var.pt2y,pt2_y)
  ncvar_put(nc_transp,var.sourceb,source_boxid)
  
  nc_close(nc_transp)
  
  # x = nc_open(filename)
  
  ### For T, S, Vertical Flux NC file
  filename=paste0(out.dir,name.out,'statevars_2hydro.nc')
  
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
  ncvar_put(nc_varfile,var.time,t_tot,verbose = T)
  ncvar_put(nc_varfile,var.lev,atl.level)
  ncvar_put(nc_varfile,var.salt,salinity, count=c(nlevel,nboxes, ntimes))
  ncvar_put(nc_varfile,var.temp,temperature, count=c(nlevel,nboxes, ntimes))
  ncvar_put(nc_varfile,var.box,box.boxes)
  
  nc_close(nc_varfile)
  # x = nc_open(filename)
  
  ### For COBALT LTL variables
  filename=paste0(out.dir,name.out,'ltl_statevars_2hydro.nc')

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
  var.ndi=ncvar_def('ndi','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Diazotroph Nitrogen',prec='float')
  var.nlg=ncvar_def('nlg','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Phyotplankton Nitrogen',prec='float')
  var.nlgz=ncvar_def('nlgz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Zooplankton Nitrogen',prec='float')
  var.nmdz=ncvar_def('nmdz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Medium Zooplankton Nitrogen',prec='float')
  var.nsm=ncvar_def('nsm','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Small Phytoplankton Nitrogen',prec='float')
  var.nsmz=ncvar_def('nsmz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Small Zooplankton Nitrogen',prec='float')
  var.silg=ncvar_def('silg','mg Si / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Phytoplankton Silicon',prec='float')
  var.nbact=ncvar_def('nbact','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Bacterial Nitrogen',prec='float')

  nc_varfile=nc_create(filename,list(var.time,var.box, var.lev,
                                     var.ndi,var.nlg,var.nlgz,
                                     var.nmdz,var.nsm,var.nsmz,
                                     var.silg,var.nbact))
  #assign global attributes to file
  ncatt_put(nc_varfile,0,"title","Box averaged properties file, NEUS")
  ncatt_put(nc_varfile,0,"geometry","neus_tmerc_RM.bgm")
  ncatt_put(nc_varfile,0,"parameters","")

  #assign attributes to variables
  ncatt_put(nc_varfile,var.time,"dt",86400,prec="double")

  #assign variables to file
  ncvar_put(nc_varfile,var.ndi,ndi,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.time,t_tot,verbose = T)
  ncvar_put(nc_varfile,var.lev,atl.level)
  ncvar_put(nc_varfile,var.nlg,nlg,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.nlgz,nlgz,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.nmdz,nmdz,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.nsm,nsm,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.nsmz,nsmz,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.silg,silg,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.nbact,nbact,count = c(nlevel,nboxes,ntimes))
  ncvar_put(nc_varfile,var.box,box.boxes)
  
  nc_close(nc_varfile)
  # x = nc_open(filename)
  # x$dim$time


}

# Roms2Hydro(roms.dir,roms.prefix,out.dir,name.out)
