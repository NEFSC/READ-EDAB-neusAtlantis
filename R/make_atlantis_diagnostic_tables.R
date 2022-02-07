## Script to run Atlantis Diagnostics

# Read in Package Names
library(atlantisdiagnostics)
library(atlantisom)
library(dplyr)

source(here::here('R','make_recruit_diagnostics.R'))
#### CHANGE THIS FOR EACH RUN ###
#Set the "Name" of the run and the directory of output files
run.name = 'Pre_Merge_MumC_4_Winners'
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
realBiomass <- readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)

surveyBounds = c(0.5,2)
initBioBounds = c(0.5,10)

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
reasonable = diag_reasonability(fgs = fgs.file,
                                biomind = biomind.file,
                                speciesCodes = NULL,
                                realBiomass = realBiomass,
                                surveyBounds = surveyBounds,
                                initBioBounds = initBioBounds)%>%
  rename(pass.reasonable = 'pass')

#Run Cohort Test
  cohort = diag_cohortBiomass(mortFile = paste0(run.dir,'neus_outputMort.txt'),
                            agebiomind = paste0(run.dir,'neus_outputAgeBiomIndx.txt'),
                            neusPriority =here::here('Diagnostics','neus_atlantis_group_priority.csv') )%>%
    rename(pass.cohort = 'Status', code = 'Code')

#Run recruit diagnostic
recruit = make_recruit_diagnostics(run.dir = paste0(run.dir,'/'))%>%
  mutate(pass.recruit = ifelse(spawn.check == 0, 'TRUE','FALSE'))%>%
  rename(code = 'group')

#Combine to single table
diag.all = select(persist, code,initialBiomass, pass.persist) %>%
  left_join(select(stable,code,pass.stable),by = 'code')%>%
  left_join(select(reasonable, code, minBiomass,maxBiomass,test,pass.reasonable),by = 'code')%>%
  left_join(select(cohort, code, Max_Cohort, pass.cohort),by = 'code')%>%
  left_join(select(recruit,code,mean.recruit,pass.recruit), by = 'code')%>%
  select(code, initialBiomass,minBiomass,maxBiomass,Max_Cohort,test,mean.recruit,pass.persist,pass.stable,pass.reasonable,pass.cohort,pass.recruit)


