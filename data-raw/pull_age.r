
#connect to db
channel <- dbutils::connect_to_database("sole","jcaracappa")
# pull data using survdat package
a <- survdat::get_survdat_data(channel,getBio=T,getLengths = T,getWeightLength = F)

library(magrittr)

# read in functional group/species 
speciesList <- readr::read_csv(file=here::here("data","functionalGroupNames.csv")) %>%
  dplyr::filter(isFishedSpecies == T) %>%
  dplyr::select(-NESPP3) %>% 
  dplyr::distinct()

atlantisSpecies <- as.vector(na.omit(unique(speciesList$SVSPP)))

# filter atlantis species from data and join with atlantis groups
data <- a$survdat %>% 
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  dplyr::select(YEAR,SEASON,INDWT,LENGTH,SEX,MATURITY,AGE, Code,Functional_Group,Species) %>%
  dplyr::filter(!is.na(INDWT)) %>%
  tibble::as_tibble()
