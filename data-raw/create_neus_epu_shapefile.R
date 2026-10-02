#' add EPUs to shapefile
#' 
#' takes current shapefile, adds EPU designation, X,Y centroid values, Area of box and 
#' strips Z dimension
#' 
#' @param crs integer. coordinate reference system
#' @param showPlot boolean. plot fields. Default = F
#' @param overwrite boolean. overwrite saved version with new version. Default = F
#' 
#' @return sf object
#' 

library(magrittr)
create_neus_epu_shapefile <- function(crs=4269L,showPlot=F,overwrite=F){
  
  options(warn = -1)
  sf::sf_use_s2(FALSE)
  # define EPUs
   epus <- readr::read_csv("https://raw.githubusercontent.com/NOAA-EDAB/neus-atlantis/master/Geometry/box2epu.csv") %>%
     dplyr::filter(!is.na(epu))

  # read in neus Box. repeat steps as for stat areas
  neusBox <- sf::st_read(here::here("Geometry/gis/Neus_ll_0p01.shp"),quiet=T)
  eusBox <- sf::st_transform(neusBox,crs)
  centroids <- sf::st_coordinates(sf::st_centroid(neusBox))
  neusBox <- cbind(neusBox,centroids)
  
  #join 
  neusEPU <- neusBox %>% 
    dplyr::mutate(BOX_ID = as.numeric(BOX_ID)) %>%
    dplyr::left_join(.,epus, by=c("BOX_ID"="box"))  %>% 
    dplyr::mutate(Area = sf::st_area(.)) 
  
  if (showPlot) {
    # plots stat areas with neus box
    message("Please wait ... plotting map")
    p1 <- ggplot2::ggplot(data  = neusEPU) +
      ggplot2::geom_sf(mapping=ggplot2::aes(fill=epu), alpha = .3, color = "grey" ) +
      # add centroid text for stat areas
      #ggplot2::geom_text(ggplot2::aes(x=X,y=Y,label=Id),size=2, color="black") +
      # plot neus box
      ggplot2::geom_sf(data = neusEPU,
                       ggplot2::aes(fill = BOX_ID),
                       fill = "grey", 
                       alpha = 0,
                       color="black") +
      # add centroid text for neusbox
      #ggplot2::geom_text(data = neusBox,ggplot2::aes(x=X,y=Y,label=BOX_ID),size=2,color="black") +
      # resize output window
      ggplot2::coord_sf(xlim = c(-78, -63), ylim = c(34,46.5), expand = FALSE) +
      ggplot2::ggtitle("NEUS EPUs mapped to Atlantis Boxes") + 
      ggplot2::ylab("") +
      ggplot2::xlab("") 
    print(p1)
  }
  
  if (overwrite) {
    sf::st_zm(neusEPU,drop=TRUE) %>%
      sf::write_sf(.,
                   dsn=here::here("Geometry/gis/Neus_ll_0p01_epu.shp"))
  }
  
  return(neusEPU)
  
}
