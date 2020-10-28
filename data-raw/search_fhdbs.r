#' functions to explore the food habits data
#' 
library(magrittr)
# location of downloaded file
file <- here::here("data-raw","foodHabitData.rdata")
download.file("https://github.com/NOAA-EDAB/ECSA/raw/master/data/allfhsg.RData",destfile=file)


#' Filter prey species based on predator
#'
#' Uses the FHDBS (Food habits database) to identify prey item of a given species
#'
#' @param predator Numeric. SVSPP code to identify predator
#' @param area Boolean. Break down prey by area predator was caught
#' 
#' @return tibble  nx 2
#' \item{pynam}{Name of prey species}
#' \item{garea}{Region predator was caught}
#'
find_prey <- function(file,predator,area=F) {
  # read in downloaded file
  load(file) # called allfhsg
  
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
#' @return tibble n x 7
#' \item{SVSPP}{species code for survey data}
#' \item{Code}{Atlantis functional group code}
#' \item{Functional_Group}{Atlantis functional group name}
#' \item{Species}{Common name for species}
#' \item{Scientific_Name}{Scientific name for species}
#' \item{NESPP3}{species code for commercial fishing data}
#' \item{Speciec_Itis}{Global species code}

find_predators <- function(file) {
  load(file) # load fh data
  # read in functional group data
  fg <- readr::read_csv(here::here("data-raw","functionalGroupNames.csv"))
  preds <- allfhsg %>% 
    dplyr::distinct(svspp) %>%
    dplyr::rename(SVSPP = svspp) %>%
    dplyr::left_join(.,fg,by="SVSPP") %>% 
    dplyr::arrange(SVSPP) %>% 
    tibble::as_tibble()
  
  return(preds)
  
}