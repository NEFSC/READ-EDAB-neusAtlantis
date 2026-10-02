#' Generate .ts files for Irradiance using ROMS output
#' 
#' Function works on daily ROMS output (netCDF) files and outputs daily
#' mean irradiance (shortwave radiative flux) in Watts m^-2. For irradiance
#' .ts files, the assumption is that irradiance is uniform across domain (i.e. 
#' only a single value for eachd day). This function calculates the average
#' shortwave radiative flux over the Atlantis boxes. 
#' 
#' @roms.dir String. Full path for ROMS output file
#' @roms.files String. Names of ROMS output files
#' @bgm.at.file string. Full path for Atlantis .bgm file in Atlantis units
#' @start.date string. Date of the first Atlantis model day Format '%y-%m-%d' 
#' @reference.date string. Date of the model reference date, format '%y-%m-%d'
#' 
#' @return list containing date, days since model start, irradiance
#' 
#' Author: J. Carcappa

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Test_Output/1980/'
# file.names = c('RM_NWA-SZ.HCob05T_avg_1980-02-01.nc','RM_NWA-SZ.HCob05T_avg_1980-02-02.nc')
# bgm.at.file = 'neus_tmerc_RM2.bgm'
# start.date = '1980-02-01'
# reference.date = '1964-01-01'

make_force_light = function(roms.dir,file.names,bgm.at.file,bgm.ll.file,start.date,reference.date){
  
  `%>%` = dplyr::`%>%`
  
  roms.files = paste0(roms.dir,file.names)
  
  bgm = rbgm::bgmfile(here::here('Geometry',bgm.at.file))
  
  boxes = rbgm::boxSpatial(bgm)
  
  #lat/lon for roms rho_points
  roms_ll_rho = angstroms::romscoords(roms.files[1],transpose = T,spatial = c('lon_rho','lat_rho'))
  roms_ll_rho$longitude.of.RHO.points = ((roms_ll_rho$longitude.of.RHO.points+180) %% 360) - 180
  
  index_box = function(box_sp, roms_ll){
    ind <- sp::over(project_to(angstroms::coords_points(roms_ll), box_sp) , as(box_sp, "SpatialPolygons"))
    dplyr::tibble(box = box_sp$label[ind], cell = seq_len(raster::ncell(roms_ll))) %>% 
      dplyr::filter(!is.na(box))
  }
  project_to <- function(x, to) {
    sp::spTransform(x, sp::CRS(raster::projection(to)))
  }

  set_indextent <- function(x) {
    raster::setExtent(x, raster::extent(0, ncol(x), 0, nrow(x)))
  }
  
  box_roms_rhoindex = index_box(boxes,roms_ll_rho)
  
  out.df = data.frame(file = file.names, ocean_time = NA,date =NA,day.from.start=NA,irradiance=NA)
  
  for(f in 1:length(roms.files)){
    
    nc.file = ncdf4::nc_open(roms.files[f])
    ocean_time = ncdf4::ncvar_get(nc.file,'ocean_time')
    out.df$ocean_time[f] = ocean_time
    date = as.POSIXct(ocean_time,origin = '1900-01-01 00:00:00', tz = 'GMT')
    out.df$date[f] = as.character.Date(date)
    out.df$day.from.start[f] = as.Date(date)-as.Date(start.date)+1
    
    r_swrad = set_indextent(raster::brick(roms.files[f],varname = 'swrad', lvar = 4, level = 1, ncdf = T))
    box_roms_rhoindex$swrad = raster::extract(raster::readAll(r_swrad),box_roms_rhoindex$cell)
    out.df$irradiance[f] = mean(box_roms_rhoindex$swrad,na.rm=T)
    
    ncdf4::nc_close(nc.file)
  }
  
  return(out.df)
  
}

# daily.irradiance(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Test_Output/1980/',
#                  file.names = c('RM_NWA-SZ.HCob05T_avg_1980-02-01.nc','RM_NWA-SZ.HCob05T_avg_1980-02-02.nc'),
#                  bgm.at.file = 'neus_tmerc_RM2.bgm',
#                  start.date = '1980-02-01',
#                  reference.date = '1964-01-01')
