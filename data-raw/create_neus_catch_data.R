#' Reads in sources of catch data and creates a master catch file
#'
#' Uses comlandr data to extend stockSMART time series where needed
#' Comlandr(USA) + NAFO (!USA) = atlantis catch for groups not using stocksmart
#' 
#' Run the following using argument 
#' 1. Run create_stockSMART_catch_data.r     --> neusStockSMARTCatchData.rds
#' 2. Run create_nafo_21a_catch_data.r       --> neusNAFOCatchData.rds
#' 3. Run create_comlandr_US_catch_data      --> neusComlandrCatchData.rds
#' 
#' @return Data frame and RDS file
#' \item{}{}

library(magrittr)

source(here::here("data-raw/impute_catch.R"))
source(here::here("data-raw/impute_catch2.R"))
source(here::here("data-raw/select_source.R"))

create_neus_catch_data <- function(exportFile = F){

  myenv <- environment()
  # read in atlantis codes
  atlantis <- read.csv(here::here("data/functionalGroupNames.csv")) %>%
    dplyr::select(Code,NESPP3,Species) 
  # read in the csv outlining which data source catch data should come from
  dataSource <- read.csv(here::here("data/functionalGroupNames_catchsource.csv")) %>%
      dplyr::distinct(Code, .keep_all = TRUE)
    
  # create a full grid of year codes
  fullgrid <- expand.grid(YEAR = 1985:2021,Code = unique(atlantis$Code))
  
  # read in stocksmart data for available species
  ss <- readRDS(here::here("data/neusStockSMARTCatchData.rds")) %>% 
    dplyr::group_by(YEAR,Code,units) %>% 
    dplyr::summarise(value = sum(value),.groups="drop") %>%
    dplyr::select(YEAR,Code,value,units) %>%
    dplyr::filter(YEAR >= 1985)
  # require all years even if no catch
  ss <- fullgrid %>% 
    dplyr::left_join(.,ss,by=c("YEAR","Code")) %>%
    dplyr::mutate(units = "Metric Tons",
                  source = "stocksmart")
  
  ## NAFO data for available species
  # NAFO already filtered to 1985
  nafo <- readRDS(here::here("data/neusNAFOCatchData.rds")) %>%
    dplyr::mutate(source="21A-nafo") 
  
  # read in comlandr data prepped for atlantis. Already filtered to 1985
  comlandrData <- readRDS(here::here("data/neusComlandrCatchData.rds"))
  comlandrmeat <- comlandrData$comlandrmeat
  comlandrlive <- comlandrData$comlandrlive
  
  ## for stocksmart stocks that have catch data missing at start or
  # end of time series 1985-2020. We either use comlandr data to scale or 
  # take the mean of last n yrs
  ssComlandr <- atlantis %>% 
    dplyr::distinct(Code) %>% 
    dplyr::left_join(.,rbind(ss,comlandrmeat),by = "Code")
  
  # number of years to take mean of
  nyrs <- 3
  # impute missing stocksmart catch from comlandr
  a <- ssComlandr  %>%
    tidyr::pivot_wider(.,id_cols = c("YEAR","Code"),names_from = source,values_from = value)  %>%
    dplyr::left_join(.,dataSource,by = "Code") %>% 
    dplyr::rename(comlandr = "comlandr meat") %>%
    dplyr::arrange(YEAR,Code,Catch_Source) %>%
    dplyr::group_by(Code) %>%
    dplyr::mutate(newvalue = impute_catch(Code,YEAR,comlandr,stocksmart,Catch_Source,Imputation_Type,nyrs))
  
  # plot the extended time series with the original stocksmart and comlandr
  p <- a %>% 
    dplyr::select(YEAR,Code,stocksmart,comlandr,newvalue) %>%
    tidyr::pivot_longer(.,cols = c(-YEAR,-Code),names_to = "source",values_to = "value") %>%
    ggplot2::ggplot(data=.) +
    ggplot2::geom_line(ggplot2::aes(x=YEAR,y=value,color = source)) +
    ggplot2::facet_wrap(ggplot2::vars(Code),scales = "free_y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  
  if(exportFile){
    ggplot2::ggsave(here::here("data-raw/figures/imputedCatchComaprison.png"),height=7,width=12)
  }else {
    print(p)
  }
  
  p1 <- a %>% 
    dplyr::select(YEAR,Code,stocksmart,comlandr,newvalue) %>%
    tidyr::pivot_longer(.,cols = c(-YEAR,-Code),names_to = "source",values_to = "value") %>%
    dplyr::filter(source == "newvalue") %>%
    ggplot2::ggplot(data=.) +
    ggplot2::geom_line(ggplot2::aes(x=YEAR,y=value)) +
    ggplot2::facet_wrap(ggplot2::vars(Code),scales = "free_y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  
  if(exportFile){
    ggplot2::ggsave(here::here("data-raw/figures/imputedCatch2.png"),height=7,width=12)
  }else {
    print(p1)
  }
  
  
  ## Scale nafo data to meat weight for shellfish
  
  # obtain ratio of meat to live weight
  comlandData <- rbind(comlandrmeat,comlandrlive)

  scalarts <- comlandData %>% 
    dplyr::filter(Code %in% c("CLA","QHG","SCA","BFF")) %>%
    tidyr::pivot_wider(.,id_cols = c(YEAR,Code),names_from = "source",values_from = "value") %>% 
    dplyr::mutate(scalar = `comlandr meat`/`comlandr live`) %>%
    tidyr::pivot_longer(.,cols = c(-YEAR,-Code),names_to = "source",values_to = "value") %>%
    dplyr::filter(source == "scalar") 
  
  
  p2 <- scalarts %>%
    ggplot2::ggplot(.) +
    ggplot2::geom_line(ggplot2::aes(x=YEAR,y=value)) + 
    ggplot2::facet_wrap(ggplot2::vars(Code),scales="free_y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) 
  
  
  if(exportFile){
    ggplot2::ggsave(here::here("data-raw/figures/scalarts.png"),height=7,width=12)
  }else {
    print(p2)
  }
  
  
  
  # obtain mean scalar (over time) for each shellfish Code
  scalars <- scalarts %>%
    dplyr::group_by(Code) %>% 
    dplyr::summarise(meanScalar=mean(value))
  
  # adjust nafo landings for shellfish groups
  nafoAdjusted <- nafo %>%
    dplyr::left_join(.,scalars,by = "Code") %>%
    dplyr::mutate(nafo = dplyr::case_when(Code %in% c("CLA","QHG","SCA","BFF") ~ value*meanScalar,
                                           TRUE ~ value)) %>% 
    dplyr::select(-meanScalar,-units,-source,-value)
  
  # Combine nafo and comland data and select data source for each species
  nafoAdjustedData <- a %>% 
    dplyr::left_join(.,nafoAdjusted,by = c("YEAR","Code")) %>%
    dplyr::group_by(YEAR,Code) %>%
    dplyr::mutate(value = select_source(stocksmart,comlandr,newvalue,nafo,Catch_Source)) %>%
    dplyr::select(YEAR,Code,value)
  
  p3 <- ggplot2::ggplot(nafoAdjustedData) +
    ggplot2::geom_line(ggplot2::aes(x=YEAR,y=value)) + 
    ggplot2::facet_wrap(ggplot2::vars(Code),scales="free_y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) 
  
  
  
  
  if(exportFile) {
    ggplot2::ggsave(here::here("data-raw/figures/2nafoAdjusted.png"),height=7,width=12)
  } else {
    print(p3)
  }
  
  
  # Interpolate missing data as specified by NA
  finalData <- nafoAdjustedData %>% 
    dplyr::group_by(Code) %>% 
    dplyr::mutate(val = wql::interpTs(value,type = "linear")) %>%
    dplyr::group_by(Code) %>% 
    dplyr::mutate(value = impute_catch2(Code,YEAR,val,"mean",3)) %>%
    dplyr::select(-val) 
  
  p4 <- ggplot2::ggplot(finalData) +
    ggplot2::geom_line(ggplot2::aes(x=YEAR,y=value)) + 
    ggplot2::facet_wrap(ggplot2::vars(Code),scales="free_y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) 
  
  
  if(exportFile) {
    ggplot2::ggsave(here::here("data-raw/figures/3finalNeusCatch.png"),height=7,width=12)
  } else {
    print(p4)
  }
  
  
  if (exportFile) {
    saveRDS(finalData,here::here("data/neusCatchData.rds"))
  }
  
  return(finalData)
  
  
  return()
  
}

###################

# 
# missingspp <- comlandrlive %>% 
#   dplyr::left_join(.,atlantis,by = "NESPP3") %>% 
#   dplyr::filter(YEAR >= 1985,
#                 is.na(Code)) %>% 
#   dplyr::distinct(NESPP3) %>% 
#   dplyr::pull()
# 
# 
# 
# a <- comlandrlive %>% 
#   dplyr::left_join(.,atlantis,by = "NESPP3") %>% 
#   dplyr::filter(YEAR >= 1985,
#                 is.na(Code)) %>% 
#   dplyr::filter(NESPP3 <= 700) %>%
#   dplyr::mutate(NESPP3 = as.factor(NESPP3)) %>% 
# 
#   dplyr::group_by(YEAR,NESPP3) %>% 
#   dplyr::summarise(tot = sum(value),.groups="drop") %>%
#   ggplot2::ggplot(.,ggplot2::aes(x=YEAR,y=tot,fill=NESPP3)) +
#   ggiraph::geom_bar_interactive(width = 0.95, stat = "identity", show.legend = TRUE,
#                                 ggplot2::aes(tooltip = NESPP3, data_id = NESPP3)) 
#   #ggplot2::geom_col() 
# 
# ggiraph::ggiraph(code=print(a))
# 
# 
# 
# 
# channel <- dbutils::connect_to_database("sole","abeet")
# gg <- comlandr::get_species(channel,c(748,763,781,789,805))$data %>%
#   dplyr::distinct(NESPP3,SPPNM) 
# 
# readr::write_csv(gg,here::here("data/temp.csv"))
