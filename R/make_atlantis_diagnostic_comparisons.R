## Script to run Atlantis Diagnostics

# Read in Package Names
library(atlantisdiagnostics)
library(atlantisom)
library(dplyr)

#### CHANGE THIS FOR EACH RUN ###
#Set the "Name" of the run and the directory of output files
run.name1 = 'InvertPersist_2'
run.dir1 = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name1)

run.name2 = 'InvertPersist_3'
run.dir2 = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name2)

#Timing
persist.nyr = 40
stable.nyr = 20

#Reasonability bounds
surveyBounds = c(1,100)
initBioBounds = c(0.5,2)
####_________________________####


#Read in functional groups and filter out groups turn off
fgs = atlantisom::load_fgs(dir = here::here('currentVersion'),'neus_groups.csv') %>% 
  dplyr::filter(IsTurnedOn == T)

#Read in neus_outputBiomIndx file with atlantisom
modelBiomass1 <- atlantisom::load_bioind(run.dir1,'neus_outputBiomIndx.txt',fgs)
modelBiomass2 <- atlantisom::load_bioind(run.dir2,'neus_outputBiomIndx.txt',fgs)

speciesCodes = fgs$Code

#Read in Survdat biomass data
realBiomass <- readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)

#Run Persistence

##Run1
persist1 =diag_persistence(modelBiomass1,
                          speciesCodes= NULL,
                          nYrs = persist.nyr,
                          floor = 0
)
##Run2
persist2 =diag_persistence(modelBiomass2,
                           speciesCodes= NULL,
                           nYrs = persist.nyr,
                           floor = 0
)

#Run Stability

##Run1
stable1 = diag_stability(modelBiomass1,
                        speciesCodes=NULL,
                        nYrs = stable.nyr,
                        sigTest = 0.1) 
##Run2
stable2 = diag_stability(modelBiomass2,
                         speciesCodes=NULL,
                         nYrs = stable.nyr,
                         sigTest = 0.1) 

#Run Reasonability

##Run1
reasonable1 = diag_reasonability(modelBiomass=modelBiomass1,
                                initialYr = 1964,
                                speciesCodes =NULL,
                                realBiomass=realBiomass,
                                surveyBounds = c(1,100),
                                initBioBounds = c(0.5,2))
##Run2
reasonable2 = diag_reasonability(modelBiomass=modelBiomass2,
                                initialYr = 1964,
                                speciesCodes =NULL,
                                realBiomass=realBiomass,
                                surveyBounds = c(1,100),
                                initBioBounds = c(0.5,2))

#Combine 3 outputs together
diag.all.1 = diag_combine(persist1,stable1,reasonable1)
colnames(diag.all.1)[3:length(colnames(diag.all.1))] = paste0(colnames(diag.all.1)[3:length(colnames(diag.all.1))],'.1')
# diag.all.1$test.sum = apply(select(diag.all.1,persistence,stability,reasonability),1,sum,na.rm=T)

diag.all.2 = diag_combine(persist2,stable2,reasonable2)
colnames(diag.all.2)[3:length(colnames(diag.all.2))] = paste0(colnames(diag.all.2)[3:length(colnames(diag.all.2))],'.2')

diag.compare = diag.all.1 %>%
  left_join(diag.all.2) %>%
  select(code, species, persistence.1, persistence.2, stability.1, stability.2, reasonability.1, reasonability.2) %>%
  mutate(persistence.change = ifelse(persistence.1 == persistence.2,F,T),
         stability.change = ifelse(stability.1 == stability.2,F,T),
         reasonability.change = ifelse(reasonability.1 == reasonability.2, F, T))%>% 
  filter(persistence.change==T|stability.change==T|reasonability.change==T) %>%
  arrange(persistence.change, stability.change, reasonability.change)


