## Script to run Atlantis Diagnostics

# Read in Package Names
library(atlantisdiagnostics)
library(atlantisom)
library(dplyr)

#### CHANGE THIS FOR EACH RUN ###
#Set the "Name" of the run and the directory of output files
run.name = 'homog_sp_off'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name)
####_________________________####


#Read in functional groups and filter out groups turn off
fgs = atlantisom::load_fgs(dir = here::here('currentVersion'),'neus_groups.csv') %>% 
  dplyr::filter(IsTurnedOn == T)

#Read in neus_outputBiomIndx file with atlantisom
modelBiomass <- atlantisom::load_bioind(run.dir,'neus_outputBiomIndx.txt',fgs)
speciesCodes = fgs$Code

#Read in Survdat biomass data
realBiomass <- readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)
surveyBounds = c(1,100)
initBioBounds = c(0.5,2)

#Run Persistence
persist =diag_persistence(modelBiomass,
                          speciesCodes= NULL,
                          nYrs = 40,
                          floor = 0
)

#Run Stability
stable = diag_stability(modelBiomass,
                        speciesCodes=NULL,
                        nYrs = 10,
                        sigTest = 0.1) 
#Run Reasonability
reasonable = diag_reasonability(modelBiomass=modelBiomass,
                                initialYr = 1964,
                                speciesCodes =NULL,
                                realBiomass=realBiomass,
                                surveyBounds = c(1,100),
                                initBioBounds = c(0.5,2))

#Combine 3 outputs together
diag.all = diag_combine(persist,stable,reasonable)
diag.all$test.sum = apply(select(diag.all,persistence,stability,reasonability),1,sum,na.rm=T)
