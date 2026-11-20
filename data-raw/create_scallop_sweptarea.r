#' Creates swept area biomass estimates by box for scallop
#'
#' Uses the scallop survey and survdat sweptare to estmate by atlantis box
#'
#' Creates RDS files saved in data folder:
#' sweptAreaBiomassScallop.RDS
#'
#' ALL BIOMASS VALUES ARE IN KG.

library(magrittr)
pullFromDB <- F
uid <- "" # change to your username
server <- "" # select server

# pull survey data
# either pull raw data
if (pullFromDB) {
  channel <- dbutils::connect_to_database(server, uid)
  scallop <- survdat::get_survdat_scallop_data(channel)
} else {
  # or read in previous pull
  # eventually this will reside on Github in version controlled package
  scallop <- readRDS(here::here("data-raw/scallop2025.rds"))
}

data <- scallop$survdat
##############################################################################
######################## USE domain from NEUS shape file #####################
##############################################################################

#### Do similar thing but for each box in Atlantis
# sweptarea Biomass by NEUS BOx
neusBox <- sf::st_read(
  here::here("Geometry", "gis"),
  layer = "Neus_ll_0p01",
  quiet = T
)

# select boxes. remove islands
boxids <- neusBox |>
  dplyr::filter(BOX_ID != c("23", "24")) |>
  dplyr::pull(BOX_ID)

#FALL
biomassNEUSfall <- NULL
for (boxid in boxids) {
  biomassBox <- survdat::calc_swept_area(
    surveyData = data,
    areaPolygon = neusBox,
    areaDescription = "BOX_ID",
    filterByArea = boxid,
    filterBySeason = "FALL",
    tidy = T,
    q = 0.4,
    a = .004515
  )
  biomassBox$box <- boxid
  biomassNEUSfall <- rbind(biomassNEUSfall, biomassBox)
}

#SPRING
biomassNEUSspring <- NULL
for (boxid in boxids) {
  biomassBox <- survdat::calc_swept_area(
    surveyData = data,
    areaPolygon = neusBox,
    areaDescription = "BOX_ID",
    filterByArea = boxid,
    filterBySeason = "SPRING",
    tidy = T,
    q = 0.4,
    a = .004515
  )
  biomassBox$box <- boxid
  biomassNEUSspring <- rbind(biomassNEUSspring, biomassBox)
}

biomassNEUSspring <- biomassNEUSspring |>
  dplyr::mutate(season = "SPRING")
biomassNEUSfall <- biomassNEUSfall |>
  dplyr::mutate(season = "FALL")

biomassNEUS <- rbind(biomassNEUSspring, biomassNEUSfall)

sweptAreaBiomassBox <- biomassNEUS %>%
  dplyr::filter(
    variable %in%
      c("tot.biomass", "tot.bio.var", "tot.abundance", "tot.abundance.var")
  ) %>%
  tibble::as_tibble()

saveRDS(
  sweptAreaBiomassBox,
  file = here::here("data", "sweptAreaBiomassScallop.RDS")
)
