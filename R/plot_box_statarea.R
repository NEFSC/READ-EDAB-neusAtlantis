#' plots NEUS Atlantis box structure overlaying statistical areas 
#' 
#'@param crs Integer scalar. coordinate reference system EPSG number
#'
#'@return A shapefle resulting in the intersection of the NEUS box and statistical areas. 
#'A figure is also plotted to the Plots window
#'
#' Beet
#'

library(magrittr)
###################################################
# you'll need to install sf, here, ggplot2
###################################################

plot_box_statarea <- function(crs=4269L,saveFigure = F){
  
  options(warn = -1)
  sf::sf_use_s2(FALSE)
  # define EPUs
  mab <- data.frame(Id = cfdbs::EPUs$data$MAB,EPU = "MAB")
  gb <- data.frame(Id = cfdbs::EPUs$data$GB, EPU = "GB")
  gom <- data.frame(Id = cfdbs::EPUs$data$GOM, EPU = "GOM")
  ss <- data.frame(Id = cfdbs::EPUs$data$SS, EPU = "SS")

  neusEPUs <- rbind(mab,gb,ss,gom)
  # define NAFO
  neusNAFO <- NEFSCspatial::NAFO_Divisions_2021_poly_clipped %>%
    dplyr::filter(Division %in% c("4X","5Y","5Z","6A","6B","6C")) %>%
    dplyr::filter(!Label == "5Ze")
  neusNAFO <- sf::st_transform(neusNAFO,crs)

  #read in statistical areas
  #statAreas <- sf::st_read(here::here("Geometry","gis"),layer="Statistical_Areas_2010_withNames",quiet=T)
  statAreas <- NEFSCspatial::Statistical_Areas_2010_withNames
  # set crs and transform
  sf::st_crs(statAreas) <- crs
  statAreas <- sf::st_transform(statAreas,crs)
  # calculate the centroid of each region and bind to shapefile
  centroids <- sf::st_coordinates(sf::st_centroid(statAreas))
  statAreas <- cbind(statAreas,centroids)
  
  statAreas <- statAreas %>% 
    dplyr::left_join(.,neusEPUs, by="Id")
  
  
  # read in neus Box. repeat steps as for stat areas
  neusBox <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)
  neusBox <- sf::st_transform(neusBox,crs)
  centroids <- sf::st_coordinates(sf::st_centroid(neusBox))
  neusBox <- cbind(neusBox,centroids) 

  
  # stat areas intersecting with box layers
  statAreasIntersection <- statAreas[neusBox,]
  
  # plots stat areas with neus box
  message("Please wait ... plotting map")
  p1 <- ggplot2::ggplot(data  = statAreasIntersection) +
    ggplot2::geom_sf(mapping=ggplot2::aes(fill=EPU), alpha = .3, color = "grey" ) +
    # add centroid text for stat areas
    ggplot2::geom_text(ggplot2::aes(x=X,y=Y,label=Id),size=2, color="black") +
    # plot neus box
    ggplot2::geom_sf(data = neusBox,
                     ggplot2::aes(fill = BOX_ID),
                     fill = "grey", 
                     alpha = 0,
                     color="black") +
    # add centroid text for neusbox
    #ggplot2::geom_text(data = neusBox,ggplot2::aes(x=X,y=Y,label=BOX_ID),size=2,color="black") +
    # resize output window
    ggplot2::coord_sf(xlim = c(-78, -63), ylim = c(34,46.5), expand = FALSE) +
    ggplot2::ggtitle("Statistical Areas") + 
    ggplot2::ylab("") +
    ggplot2::xlab("") 

  p2 <- ggplot2::ggplot(data = neusNAFO) +
    ggplot2::geom_sf( ggplot2::aes(fill = Label), alpha = .3, color = "grey" ) +
    ggplot2::geom_text(ggplot2::aes(x=X,y=Y,label=Label),size=2,color="black") +
    
    ggplot2::geom_sf(data = neusBox, 
                     ggplot2::aes(fill = BOX_ID),
                     fill = "grey",
                     alpha = 0,
                     color="black") +
    
    ggplot2::coord_sf(xlim = c(-78, -63), ylim = c(34,46.5), expand = FALSE) +
    ggplot2::ggtitle("NAFO Sub Divisions") + 
    ggplot2::ylab("") +
    ggplot2::xlab("") 
  
  
  
  p <- cowplot::plot_grid(p1, p2)
  print(p)

  if (saveFigure) {
    cowplot::save_plot(here::here("Geometry/spatialFootprints.png"),p,base_height=7,base_width=12)
  }
  
  

  return(list(p1,p2))

  
  
}
