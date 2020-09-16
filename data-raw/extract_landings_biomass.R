#' Extracts Landings and Biomass from comlandr, survdat
#'
#' \code{comlandr}: \link{github.com/NOAA-EDAB/comlandr} is an r package that aggregates landings (not catch) by EPU or stat area. You will need additional priviledges to access the underlying data tables. \code{Survdat}:\link{github.com/slucey/Rsurvey/Survdat} is an r package that estimates survey biomass by expanding swept area. The latest data can be found on the repo. 
#'
#'@param speciesNames Character string. common name of species (all lower case)
#'
#'@section Functional groups:
#'
#' Species list is created from result of mapping species to functional groups as designated in the Atlantis wiki.
#' See \link{\code{map_functional_group}}
#'
#'@examples
#'\dontrun{
#'
#'library(magrittr)
#' # connect to the database
#'channel <- dbutils::connect_to_database("sole","abeet")
#' # list species of interest
#'species <- tolower(c("Yellowtail flounder","haddock","winter flounder","silver hake","goosefish","winter skate","spiny dogfish","atlantic cod"))
#'extract_landings_biomass(userName="abeet",speciesNames = species)
#'
#'}



extract_landings_biomass <- function(channel,species = speciesName) {

  # grab all species in cfdbs and get unique list
  allSpecies <- cfdbs::get_species(channel)
  NESPP3species <- allSpecies$data %>% 
    dplyr::select(NESPP3) %>% 
    unique() %>%
    unlist() %>%
    as.numeric()
  # find corresponding SVSPP codes
  lookupTable <- dbutils::create_species_lookup(channel,species = NESPP3species ,speciesType = "NESPP3")
  # list species we want to get data for

  # get species NESPP3 and SVSPP3 codes for species of interest
  tableOfSpecies <- lookupTable$data %>%
    dplyr::select(NESPP3,COMNAME,SVSPPsv) %>% 
    dplyr::filter(tolower(COMNAME) %in% species) %>%
    dplyr::distinct() 
  tableOfSpecies$NESPP3 <- as.numeric(tableOfSpecies$NESPP3)
  
  # pull or process commercial landings pull from data base
  #comlandData <- comlandr::comland(channel,use.existing = "y",out.dir = here::here("other"))
  comlandData <- readRDS(here::here("other","comland_meatwt_deflated_EPU.RDS"))

  # time series of landings by species  
  landings <- comlandData %>% 
    dplyr::filter(NESPP3 %in% as.numeric(tableOfSpecies$NESPP3),EPU != "OTHER") %>%
    dplyr::left_join(tableOfSpecies,by="NESPP3") %>%
    dplyr::group_by(YEAR,COMNAME) %>%
    dplyr::summarise(total = sum(SPPLIVMT)) %>%
    dplyr::ungroup()
  
  # expand grid (all species by all years) to include Zeros
  completeGrid <- expand.grid(YEAR=min(landings$YEAR):max(landings$YEAR),COMNAME=unique(landings$COMNAME),stringsAsFactors = F)
  landings <- dplyr::as_tibble(dplyr::left_join(completeGrid,landings,by=c("YEAR","COMNAME")))
  
  return(landings)
  
  # now do same for survey
  #surveyDataPath <- system.file("extdata","Survdat.RData",package="comlandr")
  #load(surveyDataPath)
  
  
  
    
}


