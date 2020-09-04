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

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Test_Output/TestYear/'
# roms.dir = 'D:/NWA_Revised/2012/'
# roms.prefix = 'neusNWA_Cob10_avg_2012_*'
# roms.files = list.files(roms.dir,roms.prefix)
# # out.dir = 'D:/OUtput/1980/'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/PreAggregated Temperature/'
# dz.file = here::here('Geometry','dz.csv')
# bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
# shp.file = here::here('Geometry','Neus_ll_0p01.shp')
# # name.out = 'roms_cobalt_'
# year.id = 2012


make_ROMS_files_output_preaggregation = function(roms.dir,
                           roms.prefix,
                           roms.files,
                           out.dir,
                           dz.file,
                           bgm.file,
                           shp.file,
                           name.out,
                           year.id
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
  library(tictoc) #just for timing
  
  #Make sure package conflicts are sorted out
  complete = tidyr::complete
  select = dplyr::select
  `%>%` = dplyr::`%>%`
  
  # Convenience Functions ---------------------------------------------------
  
  # Transforms map projection
  project_to <- function(x, to) {
    sp::spTransform(x, sp::CRS(raster::projection(to)))
  }
  
  # Determine which box each point falls within
  index_box = function(box_sp, roms_ll){
    ind <- sp::over(project_to(angstroms::coords_points(roms_ll), box_sp) , as(box_sp, "SpatialPolygons"))
    dplyr::tibble(box = box_sp$label[ind], cell = seq_len(raster::ncell(roms_ll))) %>% 
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
      values[asub] <- raster::extract(x[[ulevel[ul]]], 
                              cell_level$cell[asub])
    }
    values
  }
  
  # sets the extent of indexed data
  set_indextent <- function(x) {
    raster::setExtent(x, extent(0, ncol(x), 0, nrow(x)))
  }
  
  #Get date from file name
  date_from_file = function(file){
    x = strsplit(file, paste0(roms.prefix,'|.nc'))[[1]][2]
    return(x)
    # return(as.Date(x,format = '%Y-%m-%d'))
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
  

  # Read in ROMS Output  -----------------------------------------------------
  # setwd(roms.dir)
  # roms.files = dir(roms.dir,pattern = roms.prefix)
  
  #Creates table of ROMS output files: For daily files (don't need RM old function, all have ocean_time length of 1)
  #Might still need if dailies are concatenated
  
  file_db_ls = list()
  for(f in 1:length(roms.files)){
    file.nc = ncdf4::nc_open(paste0(roms.dir,roms.files[f]))
    file_db_ls[[f]] = dplyr::tibble(fullname = roms.files[f],
                                    # date = as.Date(date_from_file(roms.files[f])),
                                    ocean_time = ncdf4::ncvar_get(file.nc,'ocean_time'))
    file_db_ls[[f]]$band_level = 1:nrow(file_db_ls[[f]])
    ncdf4::nc_close(file.nc)
    print(f)
  }
  file_db = dplyr::bind_rows(file_db_ls)
  file_db = dplyr::arrange(file_db,ocean_time)
  file_db$time_id = 1:nrow(file_db)
  file_db$date = as.POSIXct(file_db$ocean_time, origin = '1900-01-01 00:00:00', tz = 'UTC')
  file_db$month = format(file_db$date, format = '%m')
  file_db$year = format(file_db$date, format = '%Y')
  

  # Format and Summarize Box and Shape Files --------------------------------------------
  
  # rbgm::boxSpatial returns spatial polygons as a spatial object
  boxes = rbgm::boxSpatial(bgm)
  
  # Determine boundary box for each neus box
  spatialbbox=list()
  for (i in 1:length(neus.shp@data$BOX_ID)){
    spatialbbox[[i]]=raster::bbox(neus.shp@polygons[[i]])
  }
  spbox=rbind(spatialbbox)
  
 
  # save(bgm,file='C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_boxes_info.R')
  
  # Convert NEUS boxes to ROMS coordinates ----------------------------------
  
  #Using rho points for all export variables. Rho is in center and u- and v-points on verticles of cells.
  #Not as percise, but on scale on Atlantis boxes, point deviations shouldn't matter (can revisit if necessary)
  
  
  # Obtain lat/lon for rho-points
  roms_ll_rho<- angstroms::romscoords(paste0(roms.dir,roms.files[[1]]), transpose = TRUE, spatial = c("lon_rho", "lat_rho"))
  # Rotate longitude from (0-,360) to (-180,180 )
  roms_ll_rho$longitude.of.RHO.points = ((roms_ll_rho$longitude.of.RHO.points+180) %% 360) - 180

   # Build Index for the ROMS Cell of NEUS Boxes and Faces -------------------
  
  #For each face, which cells does it traverse
  box_roms_rhoindex = index_box(boxes, roms_ll_rho)
 
  # Create Index between ROMS and NEUS Levels -------------------------------
  
  #Read cell depths from ROMS
  h <- readAll(raster(paste0(roms.dir,roms.files[1]), varname = "h", ncdf=T))
  
  ## Cs_r is the S-coord stretching (length is num layers from -1 to 0 describing what portion of the w.c. each layer spans)
  ### CANNOT NC_OPEN WITHIN FUNCTION. INDEFINITELY KEEPS CONNECTION OPEN
  dumm = nc_open(paste0(roms.dir,roms.files[1]))
  Cs_r = ncvar_get(dumm,'Cs_r')
  nc_close(dumm)
  # Cs_r <- rawdata(roms.files[1], "Cs_r")
  
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
  list_nc_depth = vector('list',nrow(box_roms_rhoindex))

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
    box_z_index = cbind(box_z_index,extract(roms_ll_rho,box_z_index$cell))
    colnames(box_z_index)[5:6] = c('lon.rho','lat.rho')
    
    box_z_index$maxz = extract(h,box_z_index$cell)
    box_z_index$z = Cs_r[box_z_index$roms_level]*-box_z_index$maxz
    

  # Loop over ROMS files and calculate state variables and fluxes -----------
  
  # creating placeholders
  box_props <- vector("list", nrow(file_db))
  i_timeslice <- 215
  
  box_cell = dplyr::rename(box_z_index, level = roms_level, cell = cell)
  ocean_time = numeric(nrow(file_db))
  
  # for(i in 1:nrow(file_db)){ocean_time[i] = ncvar_get(nc_open(file_db$fullname[i]),'ocean_time')}
  
  
  for (i_timeslice in seq(nrow(file_db))) {

    print(i_timeslice)
    
    #roms file name and band name
    roms_file <-file_db$fullname[i_timeslice]

    ocean_time[i_timeslice] = file_db$ocean_time[i_timeslice]
    
    level <- file_db$band_level[i_timeslice]
    
    #Sets extent of u,v,w, temp, and salt
    # r_w <- set_indextent(brick(paste0(roms.dir,roms_file), varname = "w", lvar = 4, level = level, ncdf=T))
    r_temp <- set_indextent(brick(paste0(roms.dir,roms_file), varname = "temp", lvar = 4, level = level, ncdf=T))
    # r_salt <- set_indextent(brick(paste0(roms.dir,roms_file), varname = "salt", lvar = 4, level = level, ncdf=T))
  
    # box_z_index$w <- extract_at_level(readAll(r_w),box_cell ); rm(r_w)
    box_z_index$temp <- extract_at_level(readAll(r_temp), box_cell)
    # box_z_index$salt <- extract_at_level(readAll(r_salt), box_cell); rm(r_salt)
    
    # note - ungroup and complete (both vars) needed to get to desired dimension, works now
    ## add proper box number to sort on
    box_z_index2=left_join(box_z_index, bgm$boxes[c("label", ".bx0")], by=c("box"="label")) 
    
    ### RM 20180320 drop data (set NA) in boxes deeper than atlantis_depth by box numberusing NEUSz (above)
    ##Add number of total NEUS Atlantis levels per box
    box_z_index2=left_join(box_z_index2, NEUSz, by='.bx0') 
    
    
    box_z_index2$band_level = file_db$time_id[i_timeslice]
    box_z_index2$date = file_db$date[i_timeslice]
    box_z_index2$month = file_db$month[i_timeslice]
    
    test = filter(box_z_index2, cell == 12626)
    new.seq = seq(1,500,1)
    fun = splinefun(test$z, test$temp,method = 'natural')
    curve(fun,0,120)
    new.temp = fun(new.seq)
  
    # plot(new.temp~new.seq)
    # plot(temp~z,test)
    
    #Are boxes being matched to correctly. 
    # test = filter(box_z_index2, .bx0 == 11) %>% group_by(cell,lon.rho,lat.rho)
    # ggplot()+
    #   annotation_map(map_data('worldHires'),fill = 'grey70')+
    #   geom_point(data = test2,aes(x=lon.rho,y=lat.rho))+
    #   xlim(-80,-65)+
    #   ylim(35,45)
    # 
    # test2 = box_z_index2 %>% filter(.bx0 == 3 & cell == 12626) %>% 
    #   arrange(atlantis_level,roms_level,cell) %>%
    #   select(atlantis_level, roms_level,cell, lon.rho,lat.rho,z,maxz,zmax,temp)
    # 
    
    box_z_index3 = box_z_index2 %>% select(band_level,.bx0,cell,roms_level,atlantis_level,NEUSlevels,temp,z,maxz) %>%
      filter( roms_level %in% c(1,2,3,40))
    # box_z_index3 = box_z_index2 %>% select(band_level,.bx0,cell,roms_level,atlantis_level,NEUSlevels,temp,z,maxz) %>%
    #   filter(z <=500) %>%
    #   group_by(band_level, .bx0,atlantis_level,NEUSlevels,roms_level) %>%
    #   summarize(mean.temp = mean(temp,na.rm=T),
    #             med.temp = median(temp,na.rm=T),
    #             mean.z = mean(z,na.rm=T),
    #             med.z = median(z,na.rm = T),
    #             maxz = mean(maxz)
      # )
                
    
    # box_z_index3 = box_z_index2 %>% group_by(month, date,band_level, .bx0, atlantis_level, roms_level) %>%
    #   summarize(mean.)
    
    box_props[[i_timeslice]] = box_z_index3
  }
  
  # Combine box and face properties
  box_props <- bind_rows(box_props)  
  
  #Remove data from islands (Boxes 23 and 24)
  box_props[which(box_props$.bx0==23 | box_props$.bx0==24), 'temp']=NA #islands
  
  save(box_props, file = paste0(out.dir, 'PreAggregated_Temperature_',year.id,'.R'))
}

# Roms2Hydro(roms.dir,roms.prefix,out.dir,name.out)
