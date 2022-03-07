## Script to run Atlantis Diagnostics

# Read in Package Names
library(atlantisdiagnostics)
library(atlantisom)
library(dplyr)

source(here::here('R','make_recruit_diagnostics.R'))
#### CHANGE THIS FOR EACH RUN ###
#Set the "Name" of the run and the directory of output files
run.name = 'PL_DF_SlowSink_4'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/')
####_________________________####

#Set Parameter file names

#Group Priority
priority.file =here::here('Diagnostics','neus_atlantis_group_priority.csv')
#Functional Group Definitions
fgs.file = here::here('currentVersion','neus_groups.csv')
#BiomIndx
biomind.file = paste0(run.dir,'/neus_outputBiomIndx.txt')

#Read in Survdat biomass data
realBiomass.survdat <- readRDS(here::here("data/sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var")) %>%
  dplyr::mutate(variable = ifelse(as.character(variable)=="tot.biomass","biomass",as.character(variable))) %>%
  dplyr::mutate(variable = ifelse(as.character(variable)=="tot.bio.var","var",as.character(variable))) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::mutate(value=ifelse(grepl("kg\\^2$",units),value/1e6,value)) %>%
  dplyr::select(-units)

realBiomass.stockSmart = readRDS(here::here('data/StockSMARTData.Rds'))%>%
  dplyr::filter(variable == 'biomass')%>%
  group_by(Code,YEAR,Species,Functional_Group,variable,isFishedSpecies)%>%
  summarise(value = sum(value,na.rm=T))
  

# realBiomass %>%
#   filter(Code == 'MEN',variable == 'biomass')%>%View()
surveyBounds = c(0.1,1)
initBioBounds = c(0.1,10)

#Run Persistence
persist =diag_persistence(fgs = fgs.file,
                          biomind = biomind.file,
                          speciesCodes= NULL,
                          nYrs = 40,
                          floor = 0.1)%>%
  rename(pass.persist = 'pass')

#Run Stability
stable = diag_stability(fgs = fgs.file,
                        biomind = biomind.file,
                        initialYr = 1964,
                        speciesCodes=NULL,
                        nYrs = 20,
                        relChangeThreshold = .05)%>%
  rename(pass.stable = 'pass')

#Run Reasonability
reasonable.survdat <- atlantisdiagnostics::diag_reasonability(fgs.file,
                                                     biomind.file, 
                                                     initialYr = 1964, 
                                                     speciesCodes=NULL, 
                                                     realBiomass=realBiomass.survdat,
                                                     useVariance = F,
                                                     nYrs = 20,
                                                     surveyBounds=surveyBounds,
                                                     initBioBounds = initBioBounds)%>%
  rename(pass.reasonable.SD = 'pass',
         maxExceedance.SD = 'maxExceedance')

reasonable.stocksmart <- atlantisdiagnostics::diag_reasonability(fgs = fgs.file,
                                                              biomind = biomind.file, 
                                                              initialYr = 1964, 
                                                              speciesCodes=NULL, 
                                                              realBiomass=realBiomass.stockSmart,
                                                              useVariance = F,
                                                              nYrs = 20,
                                                              surveyBounds=surveyBounds,
                                                              initBioBounds = initBioBounds)%>%
  rename(pass.reasonable.SS = 'pass',
         maxExceedance.SS = 'maxExceedance')

reasonable.all = select(reasonable.survdat,code,species,maxExceedance.SD,pass.reasonable.SD,test) %>%
  left_join(select(reasonable.stocksmart,code,species,maxExceedance.SS,pass.reasonable.SS))

write.csv(reasonable.all, file = paste0(run.dir,'Post_Processed/',run.name,'_reasonability.csv'),row.names = F)
#Run Cohort Test
  cohort = diag_cohortBiomass(fgs = fgs.file,
                              mortality = paste0(run.dir,'neus_outputMort.txt'),
                              agebiomind = paste0(run.dir,'neus_outputAgeBiomIndx.txt'),
                              neusPriority =here::here('Diagnostics','neus_atlantis_group_priority.csv') )%>%
    rename(pass.cohort = 'pass')

#Run recruit diagnostic
recruit = make_recruit_diagnostics(run.dir = paste0(run.dir,'/'))%>%
  mutate(pass.recruit = ifelse(spawn.check == 0, 'TRUE','FALSE'))%>%
  rename(code = 'group')

#Combine to single table
diag.all = select(persist, code,initialBiomass, pass.persist) %>%
  left_join(select(stable,code,pass.stable),by = 'code')%>%
  left_join(select(reasonable, code, minBiomass,maxBiomass,test,pass.reasonable),by = 'code')%>%
  left_join(select(cohort, code, maxCohort, pass.cohort),by = 'code')%>%
  # left_join(select(recruit,code,mean.recruit,pass.recruit), by = 'code')%>%
  # select(code, initialBiomass,minBiomass,maxBiomass,maxCohort,test,mean.recruit,pass.persist,pass.stable,pass.reasonable,pass.cohort,pass.recruit)
  select(code, initialBiomass,minBiomass,maxBiomass,maxCohort,test,pass.persist,pass.stable,pass.reasonable,pass.cohort)

write.csv(diag.all,file = paste0(run.dir,'Post_Processed/',run.name,'_diagnostics.csv'),row.names = F)

