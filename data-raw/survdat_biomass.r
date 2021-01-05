#' Estimate swept area biomass over whole shelf for species in Atlantis present in survey
#'
#'

pullFromDB <- F
uid <- "username" # change to your username

# pull survey data
# either pull raw data 
if (pullFromDB) {
  channel <- dbutils::connect_to_database("sole",uid)
  survey <- survdat::get_survdat_data(channel)
} else { # or read in previous pull
  survey <- readRDS("C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/survdat/survdat.RDS")
}


data <- survey$survdat # data pull
neus <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T) # EPU shape file

# read in functional group/species relationship
speciesList <- readr::read_csv(file=here::here("data-raw","functionalGroupNames.csv"))
atlantisSpecies <- as.vector(na.omit(unique(speciesList$SVSPP)))
# unique list of species codes in atlantis and survey formatted as a 3 character string
atlantisSpecies <- sprintf("%03d",atlantisSpecies)

# estimate swept area biomass for entire region
biomass <- survdat::swept_area_biomass(data=data,areaPolygon = neus,areaDescription="EPU",filterByArea = c("MAB","SS","GB","GOM"),filterBySeason = "FALL", species=atlantisSpecies)

# pull out total bio, abund with standard error for each species over time
sweptAreaBiomass <- biomass %>% dplyr::select(YEAR,SVSPP,tot.biomass,tot.bio.SE,tot.abundance,tot.abund.SE) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  units::drop_units() %>% 
  tibble::as_tibble()

saveRDS(sweptAreasBiomass,file = here::here("data-raw","sweptAreaBiomass.RDS"))

#sweptarea Biomass by NEUS BOx
neusBox <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)

neusBox$BOX_ID <- as.numeric(levels(neusBox$BOX_ID))[neusBox$BOX_ID]

boxids <- neusBox %>% 
  sf::st_drop_geometry() %>%
  tibble::as_tibble() %>%
  dplyr::distinct(BOX_ID) %>%
  unlist() %>%
  as.numeric()
  
#### WAIT FOR SEAN TO MERGE PULL REQUEST. CURRENTLY post_strat is BROKEN
biomass <- NULL
for (boxid in boxids[1]){ 
  biomassBox <- survdat::swept_area_biomass(data=data,areaPolygon = neusBox, areaDescription="BOX_ID", filterByArea = boxid, filterBySeason = "FALL", species=atlantisSpecies)
  biomass <- rbind(biomass,biomassBox)
}

sweptAreaBiomass <- biomass %>% dplyr::select(YEAR,SVSPP,tot.biomass,tot.bio.SE,tot.abundance,tot.abund.SE) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  units::drop_units() %>% 
  tibble::as_tibble()

saveRDS(sweptAreasBiomass,file = here::here("data-raw","sweptAreaBiomass_byBox.RDS"))




        