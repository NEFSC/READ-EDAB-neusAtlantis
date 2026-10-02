#' Filters stock smart data by Atlantis species
#' 
#' Filters all species that reside on the north east coast from stock smart
#' 
#' saves RDS file neusStockSMARTCatchData.Rds"
#' 
#' @return data frame
#' \item{YEAR}{}
#' \item{SVSPP}{NEFSC species survey code}
#' \item{variable}{Defining the type of data, eg. Catch}
#' \item{value}{}
#' \item{units}{}
#' \item{Code}{Atlantis group code}
#' \item{Functional_Group}{Atlantis functional group}
#' \item{Species}{Species name}
#' \item{Scientific_Name}{}
#' \item{Species_Itis}{Itis code}
#' \item{Description}{Description of Catch data}
#' \item{StockArea}{Defines area of stock}
#' 

library(magrittr)

create_stockSMART_catch_data <- function(exportFile = F) {

  # pulls in all the most recent Abundance data from stockSMART data
  saData <- stocksmart::get_latest_metrics(metrics = "Catch")

    # pulls in atlantis functional group codes and species names
  atlantis <- readr::read_csv(here::here("data","functionalGroupNames.csv"))
  # duplicate ITIS since some species have multiple NESPP3s
  atlantisITIS <- atlantis %>% 
    dplyr::select(-NESPP3) %>% 
    dplyr::distinct()
  
  # join the two and filter.
  poundtomt <- .000453592 # conversion from lbs to mt

  stockData <- atlantisITIS %>% 
    dplyr::inner_join(.,saData$data,by=c("Species_Itis"="ITIS"),na_matches = "never") %>% 
    dplyr::filter(RegionalEcosystem %in% c("Northeast Shelf","Atlantic Highly Migratory","Northeast Shelf / Southeast Shelf")) %>%
    dplyr::select(Year,SVSPP,Metric,Value,Units, Code,Functional_Group,Species,ScientificName,Species_Itis,Description,StockArea) %>%
    dplyr::mutate(Value=dplyr::case_when(Units=="Thousand Metric Tons" ~ Value*1000, TRUE ~ Value)) %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="Thousand Metric Tons" ~ "Metric Tons", TRUE ~ Units)) %>%
    dplyr::mutate(Value=dplyr::case_when(Units=="Thousand Pounds" ~ Value*poundtomt*1000, TRUE ~ Value))  %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="Thousand Pounds" ~ "Metric Tons", TRUE ~ Units)) %>%
    dplyr::mutate(Value=dplyr::case_when(Units=="1000 mt" ~ Value*1000, TRUE ~ Value))  %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="1000 mt" ~ "Metric Tons", TRUE ~ Units)) %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="mt" ~ "Metric Tons", TRUE ~ Units)) %>% 
    dplyr::mutate(Units=dplyr::case_when(Units=="Thousand Kilograms" ~ "Metric Tons", TRUE ~ Units)) %>% 
    dplyr::filter(Units == "Metric Tons") %>%
    # rename variables to be consistent with survdat
    dplyr::rename(YEAR = Year, value = Value, variable = Metric, units=Units,Scientific_Name=ScientificName)
  

    

  
  if (exportFile){
    saveRDS(stockData,here::here("data","neusStockSMARTCatchData.Rds"))
  } 
  
  return(stockData)
  
}

#create_stockSMART_catch_data(exportFile = F)
