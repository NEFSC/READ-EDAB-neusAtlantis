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
#'
#' ALL BIOMASS VALUES ARE IN KG. 

library(magrittr)
pullFromDB <- F
uid <- "" # change to your username
server <- "" # select server

# pull survey data
# either pull raw data 
if (pullFromDB) {
  channel <- dbutils::connect_to_database(server,uid)
  survey <- survdat::get_survdat_data(channel)
} else { # or read in previous pull
  # eventually this will reside on Github in version controlled package
  survey <- readRDS(here::here("data-raw/survdat2021.rds"))
}

### read in atlantic surfclam data. Poorly sampled in bottom trawl survey

clam <- readr::read_csv(file=here::here("data-raw","surfclam403Biomass.csv"),skip=8) 
# from Dan Hennen swept are biomass
clam <- clam %>%
  dplyr::select(Yr,Value,StdDev) %>% 
  dplyr::rename(YEAR = Yr) %>%
  dplyr::mutate(tot.biomass = Value*1000) %>% # convert to kg since all survdata is in kg
  dplyr::mutate(SVSPP=403) %>%
  dplyr::mutate(tot.bio.var = 1e6*(StdDev^2)) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-StdDev,-Value) %>% 
  tidyr::pivot_longer(.,cols= c("tot.biomass","tot.bio.var"),names_to = "variable",values_to = "value") %>%
  dplyr::mutate(variable = as.factor(variable))

### read in ocean quahog data. Poorly sampled in bottom trawl survey

quahog <- readr::read_csv(file=here::here("data-raw","quahog754Biomass.csv"),skip=8) 
# from Dan Hennen swept are biomass
quahog <- quahog %>%
  dplyr::select(Yr,Value,StdDev) %>% 
  dplyr::rename(YEAR = Yr) %>%
  dplyr::mutate(tot.biomass = 1000*Value) %>%
  dplyr::mutate(SVSPP=409) %>%
  dplyr::mutate(tot.bio.var = 1e6*(StdDev^2)) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-StdDev,-Value) %>% 
  tidyr::pivot_longer(.,cols= c("tot.biomass","tot.bio.var"),names_to = "variable",values_to = "value") %>%
  dplyr::mutate(variable = as.factor(variable))

#### scallop data from stock smart
# scallop <- assessmentdata::stockAssessmentData %>% 
#   dplyr::filter(ITIS==79718, AssessmentYear == 2018) %>% 
#   dplyr::select(Year,Value,Metric, Description, Units)
# # from 65 Stock assessment table A9.4 p80

scallop <- readr::read_csv(file=here::here("data-raw","scallop401Biomass.csv"),skip=9) 

scallops <- scallop %>%
  dplyr::select(Year,Bms,CV_2) %>% 
  dplyr::rename(YEAR = Year) %>%
  dplyr::mutate(tot.biomass = 1000*Bms) %>%
  dplyr::mutate(SVSPP=401) %>%
  dplyr::mutate(tot.bio.var = 1e6*((Bms*CV_2)^2)) %>%
  dplyr::mutate(units = "kg") %>%
  dplyr::select(-Bms,-CV_2) %>% 
  tidyr::pivot_longer(.,cols= c("tot.biomass","tot.bio.var"),names_to = "variable",values_to = "value") %>%
  dplyr::mutate(variable = as.factor(variable))

#### Bluefish data from ASMFC_2015

bluefish = readRDS(here::here('data-raw','BLF_asfmc_biomass.rds'))%>%
  mutate(biomass = biomass*1E3)

#Create variance that is based on a CV = 0.3 so sigma = 0.3*mu
bluefish.var = bluefish %>%
  mutate(value = (biomass*0.3)^2,
         variable = 'tot.bio.var',
         units = 'kg^2')%>%
  select(year,variable,units,value)
  
bluefish = bluefish %>%
  rename(value = 'biomass')%>%
  mutate(variable = 'tot.biomass',units = 'kg')%>%
  bind_rows(bluefish.var)%>%
  mutate(SVSPP = 135)%>%
  rename(YEAR = 'year')

### Menhaden data from ASFMC_2020

menhaden =  readRDS(here::here('data-raw','MEN_asfmc_biomass.rds'))%>%
  mutate(biomass = biomass*1E3)

#Create variance that is based on a CV = 0.3 so sigma = 0.3*mu
menhaden.var = menhaden %>%
  mutate(value = (biomass*0.3)^2,
         variable = 'tot.bio.var',
         units = 'kg^2')%>%
  select(year,variable,units,value)

menhaden = menhaden %>%
  rename(value = 'biomass')%>%
  mutate(variable = 'tot.biomass',units = 'kg')%>%
  bind_rows(menhaden.var)%>%
  mutate(SVSPP = 36)%>%
  rename(YEAR = 'year')

### SummerFlounder data from ASFMC_2020

summer.flounder =  readRDS(here::here('data-raw','SUF_asfmc_biomass.rds'))%>%
  mutate(biomass = biomass*1E3)

#Create variance that is based on a CV = 0.3 so sigma = 0.3*mu
summer.flounder.var = summer.flounder %>%
  mutate(value = (biomass*0.3)^2,
         variable = 'tot.bio.var',
         units = 'kg^2')%>%
  select(year,variable,units,value)

summer.flounder = summer.flounder %>%
  rename(value = 'biomass')%>%
  mutate(variable = 'tot.biomass',units = 'kg')%>%
  bind_rows(summer.flounder.var)%>%
  mutate(SVSPP = 103)%>%
  rename(YEAR = 'year')

### Striped Bass data from ASFMC_2020

striped.bass =  readRDS(here::here('data-raw','STB_asfmc_biomass.rds'))%>%
  mutate(biomass = as.numeric(biomass)*1E3)

#Create variance that is based on a CV = 0.3 so sigma = 0.3*mu
striped.bass.var = striped.bass %>%
  mutate(value = (biomass*0.3)^2,
         variable = 'tot.bio.var',
         units = 'kg^2')%>%
  select(year,variable,units,value)

striped.bass = striped.bass %>%
  rename(value = 'biomass')%>%
  mutate(variable = 'tot.biomass',units = 'kg')%>%
  bind_rows(striped.bass.var)%>%
  mutate(SVSPP = 139)%>%
  rename(YEAR = 'year')


##############################################################################
######################## USE domain from EPU shape file #####################
##############################################################################

## Read in EPU shape file for NEUS wide biomass
data <- survey$survdat # data pull
neusEPU <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T) # EPU shape file

# read in functional group/species relationship
speciesList <- readr::read_csv(file=here::here("data","functionalGroupNames.csv")) %>%
  dplyr::select(-NESPP3) %>% 
  dplyr::distinct()
atlantisSpecies <- as.vector(na.omit(unique(speciesList$SVSPP)))
# unique list of species codes in atlantis and survey formatted as a 3 character string
#atlantisSpecies <- sprintf("%03d",atlantisSpecies)

# estimate swept area biomass for entire region
biomassEPU <- survdat::calc_swept_area(surveyData=data,
                                    areaPolygon = neusEPU,
                                    areaDescription="EPU",
                                    filterByArea = c("MAB","SS","GB","GOM"),
                                    filterBySeason = "FALL",
                                    tidy=T) %>%
  tibble::as_tibble()
  

# remove clams, quahogs, scallops
biomassEPU <-  biomassEPU %>% dplyr::filter(!(SVSPP %in% c(403,409,401,36,135,103,139)))

## join clam, quahog, scallop data from assessment
biomassEPU <- rbind(biomassEPU,clam)
biomassEPU <- rbind(biomassEPU,quahog)
biomassEPU <- rbind(biomassEPU,scallops)
biomassEPU <- rbind(biomassEPU,menhaden)
biomassEPU <- rbind(biomassEPU,bluefish)
biomassEPU <- rbind(biomassEPU,summer.flounder)
biomassEPU <- rbind(biomassEPU,striped.bass)

# pull out total bio, abund with standard error for each species over time
sweptAreaBiomassEPU <- biomassEPU %>% 
  #dplyr::select(YEAR,SVSPP,tot.biomass,tot.bio.SE,tot.abundance,tot.abund.SE) %>%
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  #dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
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
biomassNEUS <-  biomassNEUS %>% dplyr::filter(!(SVSPP %in%c(403,409,401,36,135,103,139)))

## join clam, quahog , scallop data from assessment
clambox <- clam %>% dplyr::mutate(box=NA)
quahogbox <- quahog %>% dplyr::mutate(box=NA)
scallopbox <- scallops %>% dplyr::mutate(box=NA)
menhadenbox = menhaden %>% dplyr::mutate(box = NA)
summer.flounderbox = summer.flounder %>% dplyr::mutate(box = NA)
bluefishbox = bluefish %>% dplyr::mutate(box = NA)
striped.bassbox = striped.bass %>% dplyr::mutate(box = NA)


biomassNEUS <- rbind(biomassNEUS,clambox)
biomassNEUS <- rbind(biomassNEUS,quahogbox)
biomassNEUS <- rbind(biomassNEUS,scallopbox)
biomassNEUS <- rbind(biomassNEUS,menhadenbox)
biomassNEUS <- rbind(biomassNEUS,bluefishbox)
biomassNEUS <- rbind(biomassNEUS,summer.flounderbox)
biomassNEUS <- rbind(biomassNEUS,striped.bassbox)


sweptAreaBiomassBox <- biomassNEUS %>% 
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  #dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassBox,file = here::here("data","sweptAreaBiomassNEUSBox.RDS"))


#############################################################################
######### Aggregate over Box but add in clams, quahog, scallop################
#############################################################################

biomass <- survdat::calc_swept_area(surveyData=data,
                                       areaPolygon = neusBox,
                                       areaDescription="BOX_ID", 
                                       filterByArea = boxids, 
                                       filterBySeason = "FALL",
                                       tidy=T)
# remove clams from survdat since poorly sampled in bottom trawl
biomassAllNEUS <-  biomass %>% dplyr::filter(!(SVSPP %in%c(403,409,401,36,135,103,139)))

## join clam, quahog, scallop data from assessment
biomassAllNEUS <- rbind(biomassAllNEUS,clam)
biomassAllNEUS <- rbind(biomassAllNEUS,quahog)
biomassAllNEUS <- rbind(biomassAllNEUS,scallops)
biomassAllNEUS <- rbind(biomassAllNEUS,menhaden)
biomassAllNEUS <- rbind(biomassAllNEUS,bluefish)
biomassAllNEUS <- rbind(biomassAllNEUS,summer.flounder)
biomassAllNEUS <- rbind(biomassAllNEUS,striped.bass)

sweptAreaBiomassNEUS <- biomassAllNEUS %>% 
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var","tot.abundance","tot.abundance.var")) %>%
  dplyr::filter(SVSPP %in% atlantisSpecies) %>%
  #dplyr::mutate(SVSPP=as.numeric(SVSPP)) %>%
  dplyr::inner_join(.,speciesList,by="SVSPP") %>% 
  tibble::as_tibble()

saveRDS(sweptAreaBiomassNEUS,file = here::here("data","sweptAreaBiomassNEUS.RDS"))

        
# 
# ggplot2::ggplot(data = quahog) +
#   ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=value)) +
#   ggplot2::facet_wrap(~variable,scales="free")
#                        
