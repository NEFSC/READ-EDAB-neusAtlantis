#' obtain swept are biomass by neus box
#' 
#' pulls data from survdat then estimates swept area biomass in 
#' each neus defined box
#' 
#' Note: assumes catchability, q = 1 for all species, 
#' and assumes a constant tow area over space and time

neus <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)


channel <- dbutils::connect_to_database("server","user")
survdatData <- survdat::get_survdat_data(channel)
a <- survdat::calc_swept_area(survdatData$survdat,
                         areaPolygon = neus,
                         areaDescription="BOX_ID",
                         filterBySeason = "all",
                         tidy=T)

saveRDS(a,file=here::here("data-raw","neusBiomass.RDS"))
