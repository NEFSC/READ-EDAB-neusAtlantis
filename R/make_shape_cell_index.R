#' Function to create a cell-spatial area index for gridded files
#' 
#' @grid.file string. Full path to grid file (doesn't need data just coordinates)
#' 
# 
# GLORYS.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/'
# shape.file= paste0(GLORYS.dir,'Shape_Files/EPU_NO_ESTUARIES/EPU_NOESTUARIES.shp')
# grid.file = paste0(GLORYS.dir,'GLORYS_coords_neus.nc')

make_shape_cell_index = function(grid.file){
  
  #Read in shape file
  # shp = sf::read_sf(shape.file)
  
  #Read in grid file
  grid = ncdf4::nc_open(grid.file)
  
  #Read in dum var for cell IDs
  x.length = raster::raster(grid.file,ncdf = T,varname = 'e1t' )
  y.length = raster::raster(grid.file,ncdf = T,varname = 'e2t' )
  
  #extract coordinates
  lon = grid$dim$longitude$vals
  lat = grid$dim$latitude$vals
  
  #convert coords to cell index
  cell.index = data.frame(cell = 1:raster::ncell(x.length),raster::rowColFromCell(x.length,1:raster::ncell(x.length)))
  cell.index$lon = lon[cell.index$col]
  cell.index$lat = lat[cell.index$row]
  cell.index$cell.area = raster::extract(x.length,cell.index$cell) * raster::extract(y.length,cell.index$cell)
  
  return(cell.index)
}