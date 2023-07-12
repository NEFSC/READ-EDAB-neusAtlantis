#' Converts raw phys-formatted output into the form used by hydroconstruct
#' 
#' Modified from original script used to process phys-Doppio output into Atlantis.
#' Reads in physical and biological float variables on phys grid structure and transforms
#' into Atlantis boxes/faces. Then uses phys vertical stretching coordinates to match to
#' Atlantis layers. State variables (temperature, salinity, biological groups, etc.) are
#' aggregated across all phys cells within an Atlantis box. All horizontal fluxes (flows)
#' are aggregated across phys cells intersecting Atlantis faces. Output is organized in arrays
#' and exported as .nc files to be further processed by Hydroconstruct. 
#' 
#' This script is based on ROMS_COBALT physical and lower trophic level variables. Must ensure correct names are used
#' for different output.Function is also hard-coded to NEUS Atlantis model 
#' 
#' @param phys.dir String. Defines path to phys output. All .nc files within path must be glorys output
#' @param glorys.prefix String. glorys output prefix to pattern match
#' @param out.dir String. Path for output files
#' @param name.out String. Prefix for output (arrays and .nc) files
#' 
#' @return Output is a 4D array [NEUS level,Box/Face, Time, Var] for each processed variable as well as well as
#' .nc files for horizontal exchanges, phyical state variables, and biological state variables
#' 
#' #' Created by R. Morse and modified by J. Caracappa
# 
phys.dir = 'C:/Users/joseph.caracappa/Documents/Cobia/data/'
phys.prefix = ''
phys.files = list.files(phys.dir,phys.prefix)
out.dir = 'C:/Users/joseph.caracappa/Documents/Cobia/Atlantis_Format/'
dz.file = here::here('Geometry','dz.csv')
bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
bgm.ll.file = here::here('Geometry','neus_ll_WGS84.bgm')
shp.file = here::here('Geometry','gis','Neus_ll_0p01.shp')
name.out = 'CM2_6_Atlantis_'
make.hflux = F
make.physvars = T
start.year = 2022
lat.min = 34
lat.max = 47
lon.min = -78
lon.max = -62

make_phys_files = function(phys.dir,
                           phys.prefix,
                           phys.files,
                           out.dir,
                           dz.file,
                           bgm.file,
                           bgm.ll.file,
                           shp.file,
                           name.out,
                           make.hflux,
                           make.physvars,
                           rerun){
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
  
  # Read in External Data Files ---------------------------------------------
  # Read box_depth data (shows depth of each layer in each box)
  
  dz_box = read.csv(dz.file,header=T)
  
  # Read BGM file
  bgm = rbgm::bgmfile(bgm.file)
  bgm.ll = rbgm::bgmfile(bgm.ll.file)
  
  # Read boxes shape file
  neus.shp = rgdal::readOGR(shp.file)
  
  #make file_db
  file_db_ls = list()
  for(f in 1:length(phys.files)){
    file.nc = ncdf4::nc_open(paste0(phys.dir,phys.files[f]))
    model_time = floor(file.nc$dim$TIME$vals)
    model_date=as.character(as.POSIXct(model_time*86400,origin = '1900-01-01 00:00:00',tz = 'UTC'))
    file_db_ls[[f]] = data.frame(fullname = phys.files[f],
                                 date = model_date,
                                 model_time = model_time)
    file_db_ls[[f]]$band_level = 1:nrow(file_db_ls[[f]])
    ncdf4::nc_close(file.nc)
    print(f)
  }
  file_db = dplyr::bind_rows(file_db_ls)
  file_db = dplyr::arrange(file_db,model_time)
  file_db$time_id = 1:nrow(file_db)
  
  # Read in phys Output  -----------------------------------------------------

  #Creates table of phys output files: For daily files (don't need RM old function, all have model_time length of 1)
  #Might still need if dailies are concatenated
  
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
  
    # Convert NEUS boxes to phys coordinates ----------------------------------
  
  #Using rho points for all export variables. Rho is in center and u- and v-points on verticles of cells.
  #Not as percise, but on scale on Atlantis boxes, point deviations shouldn't matter (can revisit if necessary)
  
  
  # Obtain lat/lon for rho-points
  #Read in grid file
  grid.file = 'C:/USers/joseph.caracappa/Documents/Cobia/data/1.nc'
  phys_grid = nc_open(grid.file) 
  
  #Get Lat/lon cood brick
  phys_lat = phys_grid$dim$YT_OCEAN1439_1825$vals
  phys_lon = phys_grid$dim$XT_OCEAN1901_2300$vals
  
  #Format to cell array for raster
  phys_lat_mat = matrix(rep(rev(phys_lat),length(phys_lon)),ncol = length(phys_lon), nrow = length(phys_lat),byrow = F)
  phys_lon_mat = matrix(rep(phys_lon,length(phys_lat)),ncol = length(phys_lon), nrow = length(phys_lat),byrow = T)
  
  phys_lat_rs = raster(phys_lat_mat)
  phys_lon_rs = raster(phys_lon_mat)
  
  coord.ls = list(longitude = phys_lon_rs,latitude = phys_lat_rs)
  coord.ls = lapply(coord.ls, function(x) setExtent(x, extent(0, nrow(x), 0, ncol(x))))
  phys_coords = stack(coord.ls)
  
  #Create face ID. Starts at 0 not 1

  #Build Index for the phys Cell of NEUS Boxes and Faces -------------------
  
  #For each face, which cells does it traverse
  box_index = index_box(boxes, phys_coords)
  
  #Combinde box and face into one df for processing (split out later)
  colnames(box_index) = c('ID','cell')
  box_index$type = 'box'
  full_index = box_index
  
  #Retreive cell dimensions from coordinates file
  
  # Create Index between phys and NEUS Levels for all box and face cells -------------------------------
  
  #Read cell depths from phys: These are cell mid points
  t.z = c(0,phys_grid$dim$ST_OCEAN$vals)
  t.z = t.z + c(diff(t.z)/2,0)

  #Vertical weights from depth bin intervals (changes with cell though)
  
  #Atlantis depths
  max_depth <- 500 # max(extract(h, unique(box_roms_index$cell)))
  # Specific to the NEUS model
  atlantis_depths <- cumsum(c(0, rev(rbgm::build_dz(-max_depth, zlayers = c(-500, -300, -120, -50, 0)))))
  
  # add data for max numlayers for Atlantis model
  countZLayer=apply(dz_box[,2:5], c(1,2), function(x) any(is.finite(x)))
  NEUSz=data.frame(dz_box[,1]); colnames(NEUSz)='.bx0'
  NEUSz$NEUSlevels=rowSums(countZLayer)
  NEUSz$zmax = apply(dz_box[,2:5],1,function(x) sum(x,na.rm=T))
  
  ## build the level index between Atlantis and phys
  list_nc_z_index = vector('list', nrow(full_index))

  # Loop through each cell, identify depths of phys layers, identify corresponding NEUS layers
  for (i in seq_len(nrow(full_index))) {
  
    #Retreives depths at each cell, and calculates cell volume (for volume-based weights)
    nlev = length(t.z)
    na.flag = ifelse(is.na(nlev),T,F)
    
    #identify box and/or face ID as well as whether island box
    box.id = bgm$boxes$.bx0[bgm$boxes$label == full_index$ID[i]]
    island.flag = ifelse(box.id %in% c(23,24),T,F)

    #Fill placeholder for cells in island or with no output values
    if(na.flag | island.flag){
      list_nc_z_index[[i]] = data.frame(ID = full_index$ID[i],
                                        atlantis_levels = NA,
                                        phys_levels = NA,
                                        cell.z = NA,
                                        cell = full_index$cell[i])
    }else{

      #Calculate total z intervals
      cell.w = t.z
      
      #If bathymetry is less than bottom of the deepest depth level, take halfway between 2nd bottom and bathy
      cell.z = t.z[1:nlev]
   
      if(full_index$type[i] == 'box'){
        z.box = NEUSz$zmax[NEUSz$.bx0 == box.id]
        
        if(cell.z[nlev] > z.box){
          z.ind = which(findInterval(cell.z,z.box)==1)[1]
          cell.z = cell.z[1:z.ind]
        }else{
          cell.z = cell.z[cell.z <= z.box]
        }
        
        neus.max = NEUSz$NEUSlevels[which(NEUSz$.bx0 == box.id)]
      }
      
      # length(atlantis_depths) - 
      z_index <- findInterval(cell.z, atlantis_depths, all.inside = F) # + 1
      z.ls = list()
      # for(L in 1:length(atlantis_depths)){
      #   dumm1 = cell.dz[which(z_index==L)]
      #   z.ls[[L]] = dumm1/sum(dumm1)
      # }
      z_index[which(z_index==5)]=NA ### remove depths greater than 500
      z_index[which(z_index > neus.max)] = NA
      
      list_nc_z_index[[i]] = data.frame(ID = full_index$ID[i],
                                        atlantis_levels = z_index,
                                        phys_levels = 1:length(z_index),
                                        cell.z = cell.z,
                                        cell = full_index$cell[i])
    }

    
    if (i %% 1000 == 0) print(i)
  
  }
  gc()
  
  # join the box-xy-index to the level index using rho coordinates
  full_z_index =  bind_rows(list_nc_z_index) %>% inner_join(full_index, by = c('ID','cell')) 

  #test z.wgts sum to 1 per cell/level
  
  #Split full index into box and face index
  box_z_index = full_z_index %>% filter(type == 'box') %>% select (-type) %>% rename(box = ID)
  
  # Loop over phys files and calculate state variables and fluxes -----------
  
  # creating placeholders
  box_props <- face_props <- face_props_sum <- vector("list", nrow(file_db))
  i_timeslice <- 1
  
  box_cell = dplyr::rename(box_z_index, level = phys_levels)  
  
  model_time = numeric(nrow(file_db))
  
  # for(i in 1:nrow(file_db)){model_time[i] = ncvar_get(nc_open(file_db$fullname[i]),'model_time')}
  
  # if(rerun | !file.exists(paste0(phys.dir,'Test Dump.R'))){
    for (i_timeslice in seq(nrow(file_db))) {
      # for(i_timeslice in 300){
      
      print(i_timeslice)
      
      #phys file name and band name
      phys_file <-file_db$fullname[i_timeslice]
      
      model_time[i_timeslice] = file_db$time[i_timeslice]
      
      level <- file_db$band_level[i_timeslice]
      
      #Sets extent of u,v,w, temp, and salt
      if(make.physvars){
        r_temp <- set_indextent(brick(paste0(phys.dir,phys_file), varname = "DELTA_TEMP", lvar = 4, level = level, ncdf=T, stopIfNotEqualSpaced = F))
      }
      
       if(make.physvars){
        box_z_index$temp <- extract_at_level(readAll(r_temp), box_cell)
        rm(r_temp)
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
        
        #aggregate over boxes and weight by cell area
        box_props[[i_timeslice]] <- box_z_index1 %>% 
          group_by(atlantis_levels, .bx0) %>% 
          summarize(temp = mean(temp, na.rm = TRUE)) %>% 
          ungroup()%>%
          complete(atlantis_levels, .bx0) %>%
          filter(!is.na(atlantis_levels))
        box_props[[i_timeslice]]$band_level = file_db$time_id[i_timeslice]
      }
    }
    
    saveRDS(box_props,file = paste0(out.dir,'delta_temp_atlantis.rds'))
  # }
  
  
  # load(paste0(phys.dir,'Test Dump.R'))
  
  # Combine box and face properties
  if(make.physvars){
    box_props <- bind_rows(box_props)  
    
    #Remove data from islands (Boxes 23 and 24)
    box_props[which(box_props$.bx0==23 | box_props$.bx0==24), 'temp']=NA #islands
    box_props[is.na(box_props$temp),'temp'] =NA # change NA to fill value
  }

  
  nit = length(box_props$temp)
  bands = box_props$band_level
  box.id = box_props$.bx0
  level.id = box_props$atlantis_levels
  
  
  ### Define dimensions for the two NetCDF files that go into hydroconstruct:
  nboxes = length(unique(box.id))
  ntimes = length(file_db$band_level)
  nlevel = length(unique(level.id))
  atl.level = unique(level.id)
  
  #phys is 1950, NEUS is 1964 reference point
  phys_time = as.POSIXct(file_db$model_time*86400, origin = '1900-01-01 00:00:00',tz = 'UTC')
  #convert to numeric
  t_tot = as.numeric(difftime(phys_time,as.POSIXct('1964-01-01 00:00:00',tz = 'UTC'),units = 'secs'))
  
  dt=86400 # seconds in one day

  year = format(as.Date(file_db$date), format = '%Y')
  
  ### create vars for box structure data
  ### NOTE added 1 because index cannot be 0, must remove 1 later w/ ncks
  box.boxes=bgm$boxes$.bx0 
  
  if(make.physvars){
    temperature=array(NA, dim=c(nlevel,nboxes,ntimes))
  }

  for (i in 1:nit){
    if (i %% 5000 ==0) print(paste('progress:',i/length(box_props$temp)*100, '%'))
    #time
    j=bands[i]
    ### box NOTE added 1 because index cannot be 0, must remove later
    k=box.id[i]+1 
    # depth
    l=level.id[i]
    
    if(make.physvars){
      temperature[l,k,j]=box_props$temp[i]
    }

  }
  
  saveRDS(temperature,file = paste0(out.dir,name.out,'temperature_delta.rds'))  
  saveRDS(file_db,file = paste0(out.dir,name.out,'temperature_delta_db.rds'))  
  
}


