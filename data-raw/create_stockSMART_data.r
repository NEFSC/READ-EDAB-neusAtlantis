#' Filters stock smart data by Atlantis species
#' 
#' Needed for QC for fishing inputs and possibly for reasonability checks
#' Filters out all species that reside on the east coast from stock smart
#' 
#' saves RDS file
#' 
#' stockSMARTData.Rds"

library(magrittr)

create_stockSMART_data <- function(exportFile = F) {

  # pulls in all the most recent Abundance data from stockSMART data
  saData <- stocksmart::get_latest_metrics(metrics = "Abundance")

    # pulls in atlantis functional group codes and species names
  atlantis <- readr::read_csv(here::here("data","functionalGroupNames.csv"))
  
  # join the two and filter.
  poundtomt <- .000453592 # conversion from lbs to mt

  stockData <- atlantis %>% 
    dplyr::inner_join(.,saData$data,by=c("Species_Itis"="ITIS"),na_matches = "never") %>% 
    dplyr::filter(RegionalEcosystem %in% c("Northeast Shelf","Atlantic Highly Migratory")) %>%
    # dplyr::filter(!grepl("acific",StockArea)) %>% # Pacific coast
    # dplyr::filter(!grepl("ering",StockArea)) %>% # Bering sea
    # dplyr::filter(!grepl("exico",StockArea)) %>% # Mexico
    # dplyr::filter(!grepl("awaii",StockName)) %>% # Hawaii
    dplyr::select(Year,SVSPP,Metric,Value,Units, Code,Functional_Group,Species,ScientificName,Species_Itis,Description,StockArea) %>%
    dplyr::mutate(Value=dplyr::case_when(Units=="Thousand Metric Tons" ~ Value*1000, TRUE ~ Value)) %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="Thousand Metric Tons" ~ "Metric Tons", TRUE ~ Units)) %>%
    dplyr::mutate(Value=dplyr::case_when(Units=="Thousand Pounds" ~ Value*poundtomt*1000, TRUE ~ Value))  %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="Thousand Pounds" ~ "Metric Tons", TRUE ~ Units)) %>%
    dplyr::mutate(Units=dplyr::case_when(Units=="mt" ~ "Metric Tons", TRUE ~ Units)) %>% 
    dplyr::filter(Units == "Metric Tons") %>%
    # rename variables to be constent with survdat
    dplyr::rename(YEAR = Year, value = Value, variable = Metric, units=Units,Scientific_Name=ScientificName) %>%
    dplyr::mutate(variable = dplyr::case_when(variable == "Abundance" ~ "biomass", TRUE ~ variable)) %>%
    dplyr::mutate(isFishedSpecies = T)
    

  
  if (exportFile){
    saveRDS(stockData,here::here("data","stockSMARTData.Rds"))
  } 
  
  return(stockData)
  
}

create_stockSMART_data()
