#' Extracts Landings and Biomass from comlandr, survdat
#'
#' \code{comlandr}: \link{github.com/NOAA-EDAB/comlandr} is an r package that aggregates landings (not catch) by EPU or stat area. You will need additional priviledges to access the underlying data tables. \code{survdat}:\link{github.com/NOAA-EDAB/survdat} is an r package that estimates survey biomass by expanding swept area. The latest data can be found in the repo \code{ecodata}: \link{github.com/NOAA-EDAB/ecodata}
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param species data frame: Contains common name of species, SVSPP, NESPP3 codes
#'
#'@section Functional groups:
#'
#' Species list is created from result of mapping species to functional groups as designated in the Atlantis wiki. See \link{\code{map_functional_group}}
#'
#'@examples
#'\dontrun{
#'
#'library(magrittr)
#' # connect to the database
#'channel <- dbutils::connect_to_database("sole","user")
#' # list species of interest
#'species <- tolower(c("Yellowtail flounder","haddock","winter flounder","silver hake","goosefish","winter skate","spiny dogfish","atlantic cod","atlantic herring","atlantic mackerel"))
#'extract_landings_biomass(channel,speciesNames = species)
#'
#'}


extract_landings_biomass <- function(channel,species) {

  # pull or process commercial landings pull from data base
  #comlandData <- comlandr::comland(channel,use.existing = "y",out.dir = here::here("other"))
  comlandData <- readRDS(here::here("other","comland_meatwt_deflated_EPU.RDS"))

  # extract species of interest that have NESPP3 codes. time series of landings by species  
  landings <- comlandData %>% 
    dplyr::filter(NESPP3 %in% as.numeric(species$NESPP3),EPU != "OTHER") %>%
    dplyr::left_join(species,by="NESPP3") %>%
    dplyr::group_by(YEAR,Species,EPU) %>%
    dplyr::summarise(VALUE = sum(SPPLIVMT)) %>%
    dplyr::ungroup()
  landings$SEASON <- NA
  landings$TYPE <- "landings"
  landings$UNITS <- "mt"
  
  # unique list of EPUs
  EPUs <- unique(landings$EPU)
  
  # pull survey data and apply conversion factors (requires permissions and takes ~15 mins)
  # surveyData <- survdat::get_survdat_data(channel)
  # or 
  surveyData <- readRDS(here::here("other","survdat.rds"))
  surveyData$survdat$SVSPP <- as.numeric(surveyData$survdat$SVSPP)
  area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"), quiet=T)
  seasons <- unique(surveyData$survdat$SEASON)
  surveyStrat <- NULL
  
  for (aseason in seasons) {
    for (aregion in EPUs) {
      message(paste0("Region = ",aregion, ". Season = ",aseason))
      surveyRegion <- survdat::swept_area_biomass(data = surveyData$survdat, areaPolygon = area,areaDescription = "EPU", filterByArea = aregion, filterBySeason = aseason, species = unique(species$SVSPP))
      surveyRegion$EPU <- aregion
      surveyRegion$SEASON <- aseason
      
      #combine each region
      surveyStrat <- rbind(surveyStrat,surveyRegion)
    }
  }
  
  survey <- surveyStrat %>%
    dplyr::group_by(YEAR,SVSPP,SEASON,EPU) %>% 
    dplyr::summarise(VALUE = sum(tot.biomass)) %>%
    dplyr::left_join(species,by=c("SVSPP")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-SVSPP,-NESPP3) %>% 
    dplyr::mutate(TYPE = "survey",UNITS="biomass, mt") %>%
    dplyr::select(names(landings))
    
  data <- rbind(landings,survey)
  
  # expand grid (all species by all years) to include Zeros
  #completeGrid <- expand.grid(YEAR=min(landings$YEAR,survey$YEAR):max(landings$YEAR,survey$YEAR),COMNAME=unique(landings$COMNAME,survey$COMNAME),stringsAsFactors = F)
  #landings <- dplyr::as_tibble(dplyr::left_join(completeGrid,landings,by=c("YEAR","COMNAME")))
  
  return(data)

}


