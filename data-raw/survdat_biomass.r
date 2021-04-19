#' Estimate swept area biomass over whole shelf for species in Atlantis present in survey
#' 
#' Creats RDS files:
#' sweptAreaBiomassEPU.RDS
#' sweptAreaBiomassNEUS_Box.RDS
#' sweptAreaBiomassNEUS.RDS
#'

library(magrittr)
pullFromDB <- F
uid <- "username" # change to your username
server <- "server" # select server

# pull survey data
# either pull raw data 
if (pullFromDB) {
  channel <- dbutils::connect_to_database(server,uid)
  survey <- survdat::get_survdat_data(channel)
} else { # or read in previous pull
  survey <- readRDS("C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/survdat/survdat.RDS")
}

### read in atlantic surfclam data. Poorly sampled in bottom trawl survey

clam <- readr::read_csv(file=here::here("data-raw","surfclam403Biomass.csv")) # from Dan Hennen swept are biomass
clam <- clam %>%
  dplyr::select(Yr,Value,StdDev) %>% 
  dplyr::rename(YEAR = Yr) %>%
  dplyr::mutate(tot.biomass = 1000*Value) %>%
  dplyr::mutate(SVSPP=403) %>%
  dplyr::mutate(tot.biomass.var = StdDev*2) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-StdDev,-Value) %>% 
  tidyr::pivot_longer(.,cols= c("tot.biomass","tot.biomass.var"),names_to = "variable",values_to = "value") %>%
  dplyr::mutate(variable = as.factor(variable))


##############################################################################
######################## USE domain from EPU shape file #####################
##############################################################################

## Read in EPU shape file for NEUS wide biomass
data <- survey$survdat # data pull
neusEPU <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T) # EPU shape file

# read in functional group/species relationship
speciesList <- readr::read_csv(file=here::here("data-raw","functionalGroupNames.csv"))
atlantisSpecies <- as.vector(na.omit(unique(speciesList$SVSPP)))
# unique list of species codes in atlantis and survey formatted as a 3 character string
atlantisSpecies <- sprintf("%03d",atlantisSpecies)

# estimate swept area biomass for entire region
biomassEPU <- survdat::calc_swept_area(surveyData=data,
                                    areaPolygon = neusEPU,
                                    areaDescription="EPU",
                                    filterByArea = c("MAB","SS","GB","GOM"),
                                    filterBySeason = "FALL",
                                    tidy=T) %>%
  tibble::as_tibble()
  

# remove clams
biomassEPU <-  biomassEPU %>% dplyr::filter(SVSPP != 403)

## join clam data from assessment
biomassEPU <- rbind(biomassEPU,clam)

# pull out total bio, abund with standard error for each species over time
sweptAreaBiomassEPU <- biomassEPU %>% 
  #dplyr::select(YEAR,SVSPP,tot.biomass,tot.bio.SE,tot.abundance,tot.abund.SE) %>%
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  #units::drop_units() %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassEPU,file = here::here("data","sweptAreaBiomassEPU.RDS"))

##############################################################################
######################## USE domain from NEUS shape file #####################
##############################################################################

#### Do similar thing but for each box in Atlantis
# sweptarea Biomass by NEUS BOx
neusBox <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)

# select boxes. remove islands
boxids <- neusBox %>% 
  dplyr::filter(BOX_ID != c("23","24") ) %>%
  dplyr::pull(BOX_ID)

#### WAIT FOR SEAN TO MERGE PULL REQUEST. CURRENTLY post_strat is BROKEN
biomassNEUS <- NULL
for (boxid in boxids){ 
  biomassBox <- survdat::calc_swept_area(surveyData=data,
                                         areaPolygon = neusBox,
                                         areaDescription="BOX_ID", 
                                         filterByArea = boxid, 
                                         filterBySeason = "FALL",
                                         tidy=T)
  biomassBox$box <- boxid
  biomassNEUS <- rbind(biomassNEUS,biomassBox)
}

sweptAreaBiomassBox <- biomassNEUS %>% 
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassBox,file = here::here("data","sweptAreaBiomassNEUSBox.RDS"))


#############################################################################
################## Aggreate over Box but add in clams #######################
#############################################################################

biomass <- survdat::calc_swept_area(surveyData=data,
                                       areaPolygon = neusBox,
                                       areaDescription="BOX_ID", 
                                       filterByArea = boxids, 
                                       filterBySeason = "FALL",
                                       tidy=T)
# remove clams from survdat since poorly sampled in bottom trawl
biomass <-  biomass %>% dplyr::filter(SVSPP != 403)

## join clam data from assessment
biomassAllNEUS <- rbind(biomass,clam)

sweptAreaBiomassNEUS <- biomassAllNEUS %>% 
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassNEUS,file = here::here("data","sweptAreaBiomassNEUS.RDS"))

        