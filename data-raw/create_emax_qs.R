#' Clean EMax data to join Atlantis species codes with survey q's
#'
#' Result is RDS file with Atlantis Code and Fall and spring qs
#'
#'

library(magrittr)
# read in atlantis groups
fgs <- readr::read_csv(here::here("data/functionalGroupNames.csv")) %>%
  dplyr::mutate(Scientific_Name= trimws(Scientific_Name))

# read in EMax data and retain select columns
# remove NAs in q and codes
emax <- readRDS(here::here("data-raw/Emax_Catchability.Rds")) %>%
  dplyr::select(SVSPP,NESPP3,COMNAME,SCINAME,Fall.q,Spring.q) %>% 
  dplyr::mutate(SCINAME = trimws(as.character(SCINAME))) %>%
  dplyr::mutate(COMNAME = trimws(as.character(COMNAME))) %>%
  #dplyr::filter(!is.na(Fall.q)) %>% 
  #dplyr::filter(!is.na(Spring.q)) %>%
  #dplyr::mutate(NESPP3 = dplyr::case_when(NESPP3 != 526 ~ NESPP3) )%>%
  tibble::as_tibble()


# loop over each row and pull corresponding q's
#fgs <- fgs[1,]
keep <- tibble::tibble()
uniqueSVSPP <- fgs %>% 
  dplyr::distinct(SVSPP) %>%
  tidyr::drop_na() %>% 
  dplyr::pull()

for (irow in 1:length(uniqueSVSPP)) {
  row <- fgs %>% dplyr::filter(SVSPP == uniqueSVSPP[irow]) %>%
    dplyr::slice(1)
  emaxdata <- emax %>% 
    dplyr::filter(SVSPP == row$SVSPP)
  
  if(nrow(emaxdata) == 0) { # look based on NESPP3
    emaxdata <- emax %>% 
      dplyr::filter(NESPP3 == row$NESPP3)
  }
   
  if(nrow(emaxdata) == 0) { # look based on sciname
    emaxdata <- emax %>% 
      dplyr::filter(SCINAME == row$Scientific_Name)
  }
  
  if(nrow(emaxdata) == 0){ next}
  
  data <- emaxdata %>% dplyr::select(SVSPP,NESPP3,COMNAME,Fall.q,Spring.q) %>% 
    dplyr::rename(SVSPPE=SVSPP,NESPP3E=NESPP3)
  

  if(nrow(emaxdata) == 1) {
    keep <- rbind(keep,cbind(row,data))
  } else {
    newd <- emaxdata %>% 
      dplyr::mutate(NESPP3 = min(NESPP3)) %>% 
      dplyr::distinct()
    if(nrow(newd) == 1) {
      # duplicate NESPP3 codes
      data <- newd %>% dplyr::select(SVSPP,NESPP3,COMNAME,Fall.q,Spring.q) %>% 
        dplyr::rename(SVSPPE=SVSPP,NESPP3E=NESPP3)
      keep <- rbind(keep,cbind(row,data))
      
    } else {
      # NAs for qs
      newd <- emaxdata %>% tidyr::drop_na()
      data <- newd %>% dplyr::select(SVSPP,NESPP3,COMNAME,Fall.q,Spring.q) %>% 
        dplyr::rename(SVSPPE=SVSPP,NESPP3E=NESPP3)
      keep <- rbind(keep,cbind(row,data))
    }
  }


}
emaxq <- keep %>% 
  tibble::as_tibble()

saveRDS(emaxq,here::here("data/emax_qs.rds"))

#keep %>% duplicated()


