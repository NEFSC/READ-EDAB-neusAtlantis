#' compare swept area biomass based spatial domain
#' 
#' NEFSC EPUS, Atlantis domain, Atlantis boxes 1-22 (excluding boundary)
#' 
#' 
#' 
#' 

library(magrittr)

# plot both domains
neusBox <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)
neusEPU <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T) # EPU shape file

p <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data=neusBox) +
  ggplot2::geom_sf(data=neusEPU)
print(p)

# read in three datas sets
# EPU
epu <- readRDS(file = here::here("data","sweptAreaBiomassEPU.RDS")) %>% 
  dplyr::filter(variable == "tot.biomass") %>%
  dplyr::group_by(SVSPP) %>% 
  dplyr::summarise(totalEPU=sum(value)) 

# ALl domain
neus <- readRDS(file = here::here("data","sweptAreaBiomassNEUS.RDS")) %>% 
  dplyr::filter(variable == "tot.biomass") %>%
  dplyr::group_by(SVSPP) %>% 
  dplyr::summarise(totalNEUS=sum(value)) 

# boxes 1-22 of Atlantis domain
box <- readRDS(file = here::here("data","sweptAreaBiomassNEUSBox.RDS")) %>% 
  dplyr::filter(variable == "tot.biomass") %>%
  dplyr::filter(box %in% 1:22) %>%
  dplyr::group_by(SVSPP) %>% 
  dplyr::summarise(totalBox=sum(value)) 

#join all together
df <- epu %>% 
  dplyr::left_join(.,box,by="SVSPP") %>%
  dplyr::left_join(.,neus,by="SVSPP") %>% 
  print(n=111)
  

