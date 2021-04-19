#' Combines the stock SMART data with the Atlantis functional group codes
#'
#' Filters out all species that reside on the east coast from stock smart, and assign them with an atlantis functional group code
#'
#' saves RDS file
#' 
#' stockSMARTData.Rds"

library(magrittr)

stockSMART_data <- function() {

  # pulls in all of stockSMART data
  saData <- dplyr::as_tibble(assessmentdata::stockAssessmentData)
  # pulls in atlantis functional group codes and species names
  atlantis <- readr::read_csv(here::here("data-raw","functionalGroupNames.csv"))
  
  # join the two and filter. Species must have an NESPP3 code and be in NE region
  stockData <- saData %>%
    dplyr::left_join(.,atlantis,by=c("StockName"="Species")) %>% 
    dplyr::filter(!is.na(Code),!is.na(NESPP3)) %>%
    dplyr::filter(!grepl("acific",StockArea)) %>%
    dplyr::filter(!grepl("ering",StockArea)) %>%
    dplyr::filter(!grepl("exico",StockArea))
  
  saveRDS(stockData,here::here("data","stockSMARTData.Rds"))
          
}
