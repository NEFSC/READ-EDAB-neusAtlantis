#' obtain swept are biomass by neus box
#' 
#' pulls data from survdat then estimates swept area biomass in 
#' each neus defined box
#' 
#' Note: assumes catchability, q = 1 for all species, 
#' and assumes a constant tow area over space and time

library(magrittr)

neus <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)

boxes <- neus %>% 
  dplyr::filter(BOX_ID != c("23","24") ) %>%
  dplyr::pull(BOX_ID)
  

channel <- dbutils::connect_to_database("server","user")
survdatData <- survdat::get_survdat_data(channel)

neusBiomass <- NULL

for (box in boxes) {
    
  boxbio <- survdat::calc_swept_area(survdatData$survdat,
                         areaPolygon = neus,
                         filterByArea = box,
                         areaDescription="BOX_ID",
                         filterBySeason = "all",
                         tidy=T)
  boxbio$box <- box
  neusBiomass <- rbind(neusBiomass,boxbio)
  
}


saveRDS(neusBiomass,file=here::here("data-raw","neusBiomass.RDS"))
