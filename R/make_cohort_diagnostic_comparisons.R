#Script to make comparison table bettween cohort diagnostic of multiple runs

library(atlantisdiagnostics)

set.dir = here::here('Atlantis_Runs','Batcher_MaxCohort_1','misc_mumC_shape','')

run.names = list.files(set.dir)

for(i in 1:length(run.names)){
  
  run.cohort = diag_cohortBiomass(fgs = here::here('currentVersion','neus_groups.csv'),
                              mortality = paste0(run.dir,'neus_outputMort.txt'),
                              agebiomind = paste0(run.dir,'neus_outputAgeBiomIndx.txt'),
                              neusPriority =here::here('diagnostics','neus_atlantis_group_priority.csv') )%>%
    rename(pass.cohort = 'pass')
}