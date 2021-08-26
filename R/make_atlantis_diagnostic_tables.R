## Script to run Atlantis Diagnostics

# Read in Package Names
library(atlantisdiagnostics)
library(atlantisom)
library(dplyr)

#### CHANGE THIS FOR EACH RUN ###
#Set the "Name" of the run and the directory of output files
run.name = 'ZM_Spatial_3'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name)
####_________________________####

priority = read.csv(here::here('Diagnostics','neus_atlantis_group_priority.csv'))

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
surveyBounds = c(0.5,2)
initBioBounds = c(0.5,2)

#Run Persistence
persist =diag_persistence(modelBiomass,
                          speciesCodes= NULL,
                          nYrs = 40,
                          floor = 0.1
)

test = modelBiomass %>% 
  group_by(code) %>%
  filter(time == 0) %>% 
  select(code,atoutput) 

persist.test = persist %>% left_join(test, by = 'code') %>% select(code,initialBiomass,atoutput)

#Run Stability
stable = diag_stability(modelBiomass,
                        speciesCodes=NULL,
                        nYrs = 20,
                        relChangeThreshold = .05) 
#Run Reasonability
reasonable = diag_reasonability(modelBiomass=modelBiomass,
                                initialYr = 1964,
                                speciesCodes =NULL,
                                realBiomass=realBiomass,
                                surveyBounds = c(1,10),
                                initBioBounds = c(0.1,10))
reasonable.fail = reasonable %>%
  filter(pass == F) %>%
  arrange(maxExceedance) %>%
  mutate(sine = ifelse(maxExceedance > 1, '+','-')) %>%
  left_join(priority, by = 'code') %>%
  # select(code,species,maxExceedance,priority.overall) %>%
  filter(priority.overall == 'H')

#Combine 3 outputs together
diag.all = diag_combine(persist,stable,reasonable)
diag.all$test.sum = apply(select(diag.all,persistence,stability,reasonability),1,sum,na.rm=T)

save(persist,stable,reasonable, file = paste0(run.dir,'/Post_Processed/diagnostics.Rdata'))

diag.fail = diag.all %>% filter(stability == F | reasonability == F)