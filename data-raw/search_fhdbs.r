#' functions to explore the food habits data
#' 
library(magrittr)
# location of downloaded file
file <- here::here("data-raw/data","foodHabitData.rdata")
download.file("https://github.com/NOAA-EDAB/ECSA/raw/master/data/allfhsg.RData",destfile=file)


#' Filter prey species based on predator
#'
#' Uses the FHDBS (Food habits database) to identify prey item of a given species
#'
#' @param datafile dataframe. The foods habit data pull. (See `file` above)
#' @param predator Numeric. SVSPP code to identify predator
#' @param area Boolean. Break down prey by area predator was caught
#' 
#' @return tibble  nx 2
#' \item{pynam}{Name of prey species}
#' \item{garea}{Region predator was caught}
#' 
#' @examples 
#' \dontrun{
#' # pulls prey for Smooth dogfish (SVSPP = 13)
#' find_prey(predator=13) 
#' pulls prey for Smooth dogfish by area
#' find_prey(predator=13,area =T)
#' }
#'
find_prey <- function(datafile=file,predator,area=F) {
  # read in downloaded file
  load(datafile) # called allfhsg
  
  prey <- allfhsg %>%
    dplyr::filter(svspp == predator) %>% 
    dplyr::select(pynam,garea) %>%
    dplyr::distinct(pynam,garea) %>% 
    dplyr::arrange(pynam) %>%
    tibble::as_tibble()
  
  if (!area) {
    prey <- prey %>% 
      dplyr::select(pynam) %>%
      dplyr::distinct(pynam)
  }
  
  return(prey)

}


#' Finds list of all predators in the food habits database
#'
#' @param datafile dataframe. The foods habit data pull. (See `file` above)
#'
#' @return tibble n x 7
#' \item{SVSPP}{species code for survey data}
#' \item{Code}{Atlantis functional group code}
#' \item{Functional_Group}{Atlantis functional group name}
#' \item{Species}{Common name for species}
#' \item{Scientific_Name}{Scientific name for species}
#' \item{NESPP3}{species code for commercial fishing data}
#' \item{Speciec_Itis}{Global species code}
#' 
#' #' @examples 
#' \dontrun{
#' # pulls all predators infor found in the FHDBS
#' find_predators()
#' }

find_predators <- function(datafile=file) {
  load(datafile) # load fh data
  # read in functional group data
  fg <- readr::read_csv(here::here("data","functionalGroupNames.csv"))
  preds <- allfhsg %>% 
    dplyr::distinct(svspp) %>%
    dplyr::rename(SVSPP = svspp) %>%
    dplyr::left_join(.,fg,by="SVSPP") %>% 
    dplyr::arrange(SVSPP) %>% 
    tibble::as_tibble()
  
  return(preds)
  
}
