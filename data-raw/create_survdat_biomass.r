#' Creates biomass datafiles needed for reasonability checks
#' 
#'  
#' Estimate swept area biomass (with uncertainty measures) over whole shelf for species in Atlantis
#' Bottom trawl Survey data is used (survdat) with 3 custom files for scallop, quahogs, surfclams
#' 
#' Creates RDS files saved in data folder:
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
  # eventually this will reside on Github in version controlled package
  survey <- readRDS("C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/survdat/testing/survdat2021.RDS")
}

### read in atlantic surfclam data. Poorly sampled in bottom trawl survey

clam <- readr::read_csv(file=here::here("data-raw/data","surfclam403Biomass.csv"),skip=8) 
# from Dan Hennen swept are biomass
clam <- clam %>%
  dplyr::select(Yr,Value,StdDev) %>% 
  dplyr::rename(YEAR = Yr) %>%
  dplyr::mutate(tot.biomass = 1000*Value) %>%
  dplyr::mutate(SVSPP=403) %>%
  dplyr::mutate(tot.biomass.var = StdDev^2) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-StdDev,-Value) %>% 
  tidyr::pivot_longer(.,cols= c("tot.biomass","tot.biomass.var"),names_to = "variable",values_to = "value") %>%
  dplyr::mutate(variable = as.factor(variable))

### read in ocean quahog data. Poorly sampled in bottom trawl survey

quahog <- readr::read_csv(file=here::here("data-raw/data","quahog754Biomass.csv"),skip=8) 
# from Dan Hennen swept are biomass
quahog <- quahog %>%
  dplyr::select(Yr,Value,StdDev) %>% 
  dplyr::rename(YEAR = Yr) %>%
  dplyr::mutate(tot.biomass = 1000*Value) %>%
  dplyr::mutate(SVSPP=754) %>%
  dplyr::mutate(tot.biomass.var = StdDev^2) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-StdDev,-Value) %>% 
  tidyr::pivot_longer(.,cols= c("tot.biomass","tot.biomass.var"),names_to = "variable",values_to = "value") %>%
  dplyr::mutate(variable = as.factor(variable))

#### scallop data from stock smart
# scallop <- assessmentdata::stockAssessmentData %>% 
#   dplyr::filter(ITIS==79718, AssessmentYear == 2018) %>% 
#   dplyr::select(Year,Value,Metric, Description, Units)
# # from 65 Stock assessment table A9.4 p80

scallop <- readr::read_csv(file=here::here("data-raw/data","scallop401Biomass.csv"),skip=9) 

scallops <- scallop %>%
  dplyr::select(Year,Bms,CV_2) %>% 
  dplyr::rename(YEAR = Year) %>%
  dplyr::mutate(tot.biomass = 1000*Bms) %>%
  dplyr::mutate(SVSPP=401) %>%
  dplyr::mutate(tot.biomass.var = (Bms*CV_2)^2) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-Bms,-CV_2) %>% 
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
  

# remove clams, quahogs, scallops
biomassEPU <-  biomassEPU %>% dplyr::filter(!(SVSPP %in% c(403,754,401)))

## join clam, quahog, scallop data from assessment
biomassEPU <- rbind(biomassEPU,clam)
biomassEPU <- rbind(biomassEPU,quahog)
biomassEPU <- rbind(biomassEPU,scallops)

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
# remove clams from survdat since poorly sampled in bottom trawl
biomassNEUS <-  biomassNEUS %>% dplyr::filter(!(SVSPP %in% c(403,754,401)))

## join clam, quahog , scallop data from assessment
biomassNEUS <- rbind(biomassNEUS,clam)
biomassNEUS <- rbind(biomassNEUS,quahog)
biomassNEUS <- rbind(biomassNEUS,scallops)


sweptAreaBiomassBox <- biomassNEUS %>% 
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassBox,file = here::here("data","sweptAreaBiomassNEUSBox.RDS"))


#############################################################################
######### Aggreate over Box but add in clams, quahog, scallop################
#############################################################################

biomass <- survdat::calc_swept_area(surveyData=data,
                                       areaPolygon = neusBox,
                                       areaDescription="BOX_ID", 
                                       filterByArea = boxids, 
                                       filterBySeason = "FALL",
                                       tidy=T)
# remove clams from survdat since poorly sampled in bottom trawl
biomassAllNEUS <-  biomass %>% dplyr::filter(!(SVSPP %in% c(403,754,401)))

## join clam, quahog, scallop data from assessment
biomassAllNEUS <- rbind(biomassAllNEUS,clam)
biomassAllNEUS <- rbind(biomassAllNEUS,quahog)
biomassAllNEUS <- rbind(biomassAllNEUS,scallops)

sweptAreaBiomassNEUS <- biomassAllNEUS %>% 
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassNEUS,file = here::here("data","sweptAreaBiomassNEUS.RDS"))

        

ggplot2::ggplot(data = quahog) +
  ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=value)) +
  ggplot2::facet_wrap(~variable,scales="free")
                       