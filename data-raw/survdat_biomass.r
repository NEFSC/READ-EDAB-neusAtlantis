#' Estimate swept area biomass over whole shelf for species in Atlantis present in survey
#'
#'

pullFromDB <- F
uid <- "abeet" # change to your username

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

# estimate swept area biomass
biomass <- survdat::swept_area_biomass(data=data,areaPolygon = neus,areaDescription="EPU",filterByArea = c("MAB","SS","GB","GOM"),filterBySeason = "FALL", species=atlantisSpecies)

# pull out total bio, abund with standard error for each species over time
sweptAreaBiomass <- biomass %>% dplyr::select(YEAR,SVSPP,tot.biomass,tot.bio.SE,tot.abundance,tot.abund.SE) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  units::drop_units() %>% 
  tibble::as_tibble()

saveRDS(sweptAreasBiomass,file = here::here("data-raw","sweptAreaBiomass.RDS"))

        