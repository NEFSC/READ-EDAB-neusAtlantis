#' Clean EMax data to join Atlantis species codes with survey q's
#'
#'
#'
#'
#'

library(magrittr)
# read in atlantis groups
fgs <- readr::read_csv(here::here("data/functionalGroupNames.csv")) %>% 
  dplyr::select(Code,Functional_Group,Species,SVSPP,NESPP3)

# read in EMax data

load(here::here("data-raw/Emax_Catchability.Rdata"))
spp %>% tibble::as_tibble()

# filter and join
emax <- spp %>% 
  dplyr::select(SVSPP,NESPP3,COMNAME,SCINAME,Fall.q,Spring.q) %>% 
  tibble::as_tibble()

basedonsvspp <- fgs %>% 
  dplyr::left_join(emax,by = ("SVSPP")) %>% 
  dplyr::filter(!is.na(SVSPP)) %>% 
  dplyr::filter(!is.na(Fall.q)) %>% 
  dplyr::filter(!is.na(Spring.q)) %>%
  dplyr::select(-dplyr::starts_with("NESPP3")) %>%
  dplyr::distinct()
  

basedonnespp3 <- fgs %>% 
  dplyr::left_join(emax,by = ("NESPP3")) %>% 
  dplyr::filter(!is.na(NESPP3)) %>%
  dplyr::filter(!is.na(Fall.q)) %>% 
  dplyr::filter(!is.na(Spring.q)) 

res <- rbind(basedonsvspp,basedonnespp3) %>%
  dplyr::distinct()
