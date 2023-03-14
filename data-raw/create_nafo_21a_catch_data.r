#' Process NAFO 21A for NEUS atlantis
#' 
#' 
#' https://www.nafo.int/Data/STATLANT-21A
#' 
#' 
#' 
#' @return data frame
#' \item{YEAR}{}
#' \item{Code}{Atlantis code}
#' \item{value}{}
#' \item{units}{}
#' 



library(magrittr)

create_nafo21a_catch_data <- function(exportFile = F){
  
  NEUSDivs <- c(47, 51:56, 61:63)
  
  temp <- tempfile()
  download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-18.zip",destfile=temp,quiet=TRUE)
  
  # division codes. supporting table
  divCodes <- readr::read_csv(unz(temp,"NAFO-21B-2010-18/divisions.txt"),col_names = T,show_col_types = F) %>%
    magrittr::set_colnames(c("Divcode","Division"))
  
  # species codes - supporting table
  speciesCodes <- readr::read_tsv(unz(temp,"NAFO-21B-2010-18/species.txt"),col_names = T,show_col_types = F) %>%
    dplyr::slice(-(1:3)) %>%
    dplyr::mutate(Speciesname = paste0(Longname," - ",Abbreviation)) %>%
    dplyr::rename(Speciescode = Code) %>%
    dplyr::mutate(source = "nafo") %>%
    dplyr::select(Speciescode,Speciesname,source) 
  
  db21a <- readr::read_csv(here::here("data-raw/data/NAFO_21A_20230306.csv")) %>%
    dplyr::rename(Speciesname = `Species Name`) %>%
    dplyr::rename(MetricTonnes = `Metric Tonnes`) %>%
    dplyr::left_join(.,speciesCodes, by = "Speciesname") %>%
    dplyr::left_join(.,divCodes,by = "Division") %>%
    dplyr::filter(Divcode %in% NEUSDivs) %>%
    dplyr::select(-Divcode) %>%
    dplyr::mutate(Division = as.factor(Division)) %>%
    dplyr::filter(!grepl("USA",Country)) %>% 
    dplyr::group_by(Year,Division,Speciesname,Speciescode) %>%
    dplyr::summarise(mt = sum(MetricTonnes),.groups="drop")
  
  # nafo codes in data set
  nafoCodes <- db21a %>%
    dplyr::filter(Year >=1985) %>% 
    dplyr::distinct(Speciescode,Speciesname)
  
  #reads in species which have data post 1984
  nafoneus <- readr::read_csv(here::here("data/nafo_neus_codes.csv")) %>% 
    dplyr::filter(post1984 == T)
  
  # groups with not specified catch (catch all groups)
  nafoneus %>%
    dplyr::filter(is.na(Code)) %>%
    dplyr::left_join(.,db21a) %>%
    dplyr::filter(Year >=1985) %>%
    ggplot2::ggplot(.) +
    ggplot2::geom_line(ggplot2::aes(x=Year,y=mt,color=Division)) +
    ggplot2::geom_point(ggplot2::aes(x=Year,y=mt,color=Division),size=0.5) +
    ggplot2::facet_wrap(ggplot2::vars(Speciesname))
  
  ggplot2::ggsave(here::here("data/nafo_ns_catch.png"),height=7,width=12)
  
  # filter out codes that can't be mapped
  nafoneuscodes <- nafoneus %>%
    dplyr::filter(!is.na(Code)) 
  
  # joint catch with atlantis codes
  # aggregate species with same atlantis code 
  # remove data from catch all groups (insignificant tonage)
  # and filter  data from 1985 on
  nafo <- db21a %>% 
    dplyr::left_join(.,nafoneuscodes,by = c("Speciesname","Speciescode")) %>%
    dplyr::filter(!is.na(Code)) %>%
    dplyr::group_by(Year,Division,Code) %>%
    dplyr::summarise(mt = sum(mt),.groups = "drop") %>%
    dplyr::filter(Year >=1985)
  
  nafo %>%
    ggplot2::ggplot(.) +
    ggplot2::geom_line(ggplot2::aes(x=Year,y=mt,color=Division)) +
    ggplot2::geom_point(ggplot2::aes(x=Year,y=mt,color=Division),size=0.5) +
    ggplot2::facet_wrap(ggplot2::vars(Code),scales = "free_y")
  
  ggplot2::ggsave(here::here("data/nafo_catch.png"),height=7,width=12)
  
  nafoExport <- nafo %>% 
    dplyr::rename(YEAR = Year) %>%
    dplyr::group_by(YEAR,Code) %>% 
    dplyr::summarise(value = sum(mt),.groups="drop") %>% 
    dplyr::mutate(units = "Metric Tons")
  
  if (exportFile) {
    saveRDS(nafoExport,here::here("data/neusNAFOCatchData.rds"))
  } 
  
  return(nafoExport)
  
  
}