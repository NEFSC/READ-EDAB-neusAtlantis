#' Filters stock smart data by Atlantis species
#' 
#' Needed for QC for fishing inputs and possibly for reasonability checks
#' Filters out all species that reside on the east coast from stock smart
#' 
#' saves RDS file
#' 
#' stockSMARTData.Rds"

library(magrittr)

create_stockSMART_data <- function() {

  # pulls in all of stockSMART data
  saData <- dplyr::as_tibble(stocksmart::stockAssessmentData)
  # pulls in atlantis functional group codes and species names
  atlantis <- readr::read_csv(here::here("data","functionalGroupNames.csv"))
  
  # join the two and filter. Species must have an NESPP3 code and be in NE region
  stockData <- saData %>%
    dplyr::left_join(.,atlantis,by=c("StockName"="Species")) %>% 
    dplyr::filter(!is.na(Code),!is.na(NESPP3)) %>%
    dplyr::filter(!grepl("acific",StockArea)) %>%
    dplyr::filter(!grepl("ering",StockArea)) %>%
    dplyr::filter(!grepl("exico",StockArea))
  
  saveRDS(stockData,here::here("data","stockSMARTData.Rds"))
          
}

create_stockSMART_data()
