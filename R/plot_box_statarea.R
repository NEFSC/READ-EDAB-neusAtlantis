#' plots NEUS Atlantis box structure overlaying statistical areas 
#' 
#'@param crs Integer scalar. coordinate reference system EPSG number
#'
#'@return A shapefle resulting in the intersection of the NEUS box and statistical areas. 
#'A figure is also plotted to the Plots window
#'
#'
#'

library(magrittr)
###################################################
# you'll need to install sf, here, ggplot2
###################################################

plot_box_statarea <- function(crs=4269L){
  
  options(warn = -1)
  
  #read in statistical areas
  statAreas <- sf::st_read(here::here("Geometry","gis"),layer="Statistical_Areas_2010_withNames",quiet=T)
  # set crs and transform
  sf::st_crs(statAreas) <- crs
  statAreas <- sf::st_transform(statAreas,crs)
  # calculate the centroid of each region and bind to shapefile
  centroids <- sf::st_coordinates(sf::st_centroid(statAreas))
  statAreas <- cbind(statAreas,centroids)
  
  # read in neus Box. repeat steps as for stat areas
  neusBox <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)
  neusBox <- sf::st_transform(neusBox,crs)
  centroids <- sf::st_coordinates(sf::st_centroid(neusBox))
  neusBox <- cbind(neusBox,centroids)
  
  # stat areas intersecting with box layers
  statAreasIntersection <- statAreas[neusBox,]
  
  # plots stat areas with neus box
  message("Please wait ... plotting map")
  p <- ggplot2::ggplot(data = statAreasIntersection) +
    ggplot2::geom_sf(mapping=ggplot2::aes(fill=Id),fill="lightgrey",color="grey") +
    # add centroid text for stat areas
    ggplot2::geom_text(ggplot2::aes(x=X,y=Y,label=Id),size=2,color="darkgrey") +
    # plot neus box
    ggplot2::geom_sf(data = neusBox, ggplot2::aes(fill = BOX_ID),fill = "grey", alpha = 0) +
    # add centroid text for neusbox
    ggplot2::geom_text(data = neusBox,ggplot2::aes(x=X,y=Y,label=BOX_ID),size=2,color="black") +
    # resize output window
    ggplot2::coord_sf(xlim = c(-78, -62), ylim = c(34,47), expand = FALSE)
    
  print(p)
  

  return(statAreasIntersection)

  
  
}
