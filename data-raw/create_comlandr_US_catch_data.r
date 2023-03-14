#' Reads in comlandr data and process
#'
#' Join comlandr with atlantis to get landings by groups
#' Live and meat weight
#' 
#' @return list of data frames
#' \item{meatArea}{meat weight by filtered stat area for US only}
#' \item{meatSpp}{meat weight aggregated over stat area and joined with atlantis Codes}
#' \item{meatAll}{meat weight aggregated to atlantis codes}
#' \item{comlandrmeat}{meat weight filtered by start year with NA Codes removed. Species in data but not mapped to atlantis}
#' \item{same set of data frames but for live weight}{differnces in CLA, QHG, SCA, BFF}
#' 

library(magrittr)

create_comlandr_US_catch_data <- function(exportFile = F) {
  myenv <- environment()
  # read in atlantis codes
  atlantis <- read.csv(here::here("data/functionalGroupNames.csv")) %>%
    dplyr::select(Code,NESPP3,Species) 
  
  # specify stat areas that define neus.
  # we will use data from these areas from comlandr data pull
  mab <- c(cfdbs::EPUs$data$MAB,533,534,541) # 611,625 remove???
  gb <- cfdbs::EPUs$data$GB
  gom <- cfdbs::EPUs$data$GOM
  scots <- cfdbs::EPUs$data$SS
  
  allareas <- c(mab,scots,gb,gom)
  
  startYear <- 1985
  
  # comlandr meat weight data for all other species not defined in stocksmart
  comlandrmeat <- readRDS(here::here("data/comlandr_stat_area_meat.rds"))$comland %>% 
    dplyr::filter(US == TRUE,
                  AREA %in% allareas) %>%
    assign("meatArea",.,envir = myenv) %>%
    dplyr::group_by(YEAR,NESPP3) %>%
    dplyr::summarise(value = sum(SPPLIVMT),.groups="drop") %>% 
    dplyr::left_join(.,atlantis,by = "NESPP3") %>% 
    assign("meatSpp",.,envir = myenv) %>%
    dplyr::group_by(YEAR,Code) %>% 
    dplyr::summarise(value = sum(value),.groups="drop") %>% 
    dplyr::mutate(units = "meat wt (Metric Tons)") %>%
    assign("meatAll",.,envir = myenv) %>%
    dplyr::filter(YEAR >= startYear,
                  !is.na(Code))  %>% 
    dplyr::mutate(source="comlandr meat")
  
    
  # comlandr live weight data for all other species not defined in stocksmart
  comlandrlive <- readRDS(here::here("data/comlandr_stat_area_live.rds"))$comland %>%
    dplyr::filter(US == TRUE,
                  AREA %in% allareas) %>%
    assign("liveArea",.,envir = myenv) %>%
    dplyr::group_by(YEAR,NESPP3) %>%
    dplyr::summarise(value = sum(SPPLIVMT),.groups="drop") %>% 
    dplyr::left_join(.,atlantis,by = "NESPP3") %>% 
    assign("liveSpp",.,envir = myenv) %>%
    dplyr::group_by(YEAR,Code) %>% 
    dplyr::summarise(value = sum(value),.groups="drop") %>% 
    dplyr::mutate(units = "live wt (Metric Tons)") %>% 
    assign("liveAll",.,envir = myenv) %>%
    dplyr::filter(YEAR >= startYear,
                  !is.na(Code))  %>% 
    dplyr::mutate(source="comlandr live")
  
    
  comlandData <- list(meatAll=meatAll,meatArea=meatArea,meatSpp=meatSpp,comlandrmeat=comlandrmeat,
                      liveAll=liveAll,liveArea=liveArea,liveSpp=liveSpp,comlandrlive=comlandrlive)
 
  if (exportFile)   {
    saveRDS(comlandData,here::here("data/neusComlandrCatchData.rds"))
  }
  
  return(comlandData)

  ################### ISSUES with DATA ######################
  
  # 200 species in comlandr not accounted for in atlantis
  missingspp <- liveSpp %>% 
    dplyr::filter(YEAR >= 1985,
                  is.na(Code)) %>% 
    dplyr::distinct(NESPP3) %>% 
    dplyr::pull()
  
  
  
  a <- liveSpp %>% 
    dplyr::filter(YEAR >= 1985,
                  is.na(Code)) %>% 
    dplyr::filter(NESPP3 >= 700) %>%
    dplyr::mutate(NESPP3 = as.factor(NESPP3)) %>% 
    
    dplyr::group_by(YEAR,NESPP3) %>% 
    dplyr::summarise(tot = sum(value),.groups="drop") %>%
    ggplot2::ggplot(.,ggplot2::aes(x=YEAR,y=tot,fill=NESPP3)) +
    ggiraph::geom_bar_interactive(width = 0.95, stat = "identity", show.legend = TRUE,
                                  ggplot2::aes(tooltip = NESPP3, data_id = NESPP3)) 
  #ggplot2::geom_col() 
  
  ggiraph::ggiraph(code=print(a))
  
  
  channel <- dbutils::connect_to_database("sole","abeet")
  gg <- comlandr::get_species(channel,missingspp)$data %>%
    dplyr::distinct(NESPP3,SPPNM) 
  
  #readr::write_csv(gg,here::here("data/temp.csv"))
  
  
  
}

