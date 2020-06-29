#Function that indentifies cell to EPU index as well as roms_level to depth bin

# grd.file = 'D:/NWA_grd_NEUS_2.nc'
# epu.file = here::here('Geometry','EPU_NOESTUARIES.shp')
# depth.bins = seq(0,500,5)

make_ROMS_cellindex_epu = function(grd.file,epu.file,depth.bins){
  
  epu.shp = rgdal::readOGR(epu.file)
  
  # Obtain lat/lon for rho-points
  roms_ll_rho<- angstroms::romscoords(grd.file, transpose = TRUE, spatial = c("lon_rho", "lat_rho"))
  # Rotate longitude from (0-,360) to (-180,180 )
  roms_ll_rho$longitude.of.RHO.points = ((roms_ll_rho$longitude.of.RHO.points+180) %% 360) - 180
  
  # Build Index for the ROMS Cell of NEUS Boxes and Faces -------------------
  
  #For each face, which cells does it traverse
  proj.1 = spTransform( angstroms::coords_points(roms_ll_rho), CRS(raster::projection(epu.shp)))
  ind = over(proj.1, as(epu.shp,'SpatialPolygons'))
  roms_rhoindex = dplyr::tibble(epu = epu.shp$EPU[ind], cell = seq_len(raster::ncell(roms_ll_rho))) %>%
    filter(!is.na(epu))
  
  #Pull bathymetry
  h <- readAll(raster(grd.file, varname = "h", ncdf=T))
  
  #Pull coord stretching
  dumm = nc_open(grd.file)
  Cs_r = ncvar_get(dumm,'Cs_r')
  nc_close(dumm)
  
  #create space for box index
  list_z_rhoindex <- vector('list', nrow(roms_rhoindex))
  list_depth = vector('list',nrow(roms_rhoindex))
  
  # Loop through each cell, identify depths of ROMS layers, identify corresponding NEUS layers
  for (i in seq_len(nrow(roms_rhoindex))) {
    
    # retreives depth of each cell and applies coord-stretching
    rl = raster::extract(h,roms_rhoindex$cell[i])*Cs_r

        # focal.box = bgm$boxes$.bx0[bgm$boxes$label == roms_rhoindex$box[i]]
    focal.epu = roms_rhoindex$epu[i]
    
    depth.sort = sort(-1*depth.bins)
    depth.match = findInterval(rl,depth.sort,all.inside = T)
    # depth.sort[depth.match]
    
    list_z_rhoindex[[i]] = depth.match
    list_depth[[i]] = depth.sort[depth.match]

    if (i %% 1000 == 0) print(i)
  }

  # join the box-xy-index to the level index using rho coordinates
  box_z_index = bind_rows(lapply(list_z_rhoindex,
                                 function(x) tibble(depth.bin.id = length(depth.bins)-x,
                                                    roms_level = seq_along(x))),
                          .id = 'cell_index') %>%
    filter(!is.na(depth.bin.id)) %>%
    inner_join(mutate(roms_rhoindex,cell_index = as.character(row_number()))) %>%
    dplyr::select(-cell_index)
  
  box_z_index = cbind(box_z_index,extract(roms_ll_rho,box_z_index$cell))
  colnames(box_z_index)[5:6] = c('lon.rho','lat.rho')
  
  box_z_index$maxz = extract(h,box_z_index$cell)
  box_z_index$z = Cs_r[box_z_index$roms_level]*-box_z_index$maxz
  box_z_index$depth.bin = depth.bins[box_z_index$depth.bin.id]
  
  return(box_z_index)
  
}