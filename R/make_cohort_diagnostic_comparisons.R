#Script to make comparison table bettween cohort diagnostic of multiple runs

library(atlantisdiagnostics)
library(dplyr)

set.dir = here::here('Atlantis_Runs','Batcher_MaxCohort_3','MaxCohort_Fix_2','')

run.names = list.files(set.dir)

i=1
cohort.diag.ls = list()
for(i in 1:length(run.names)){
  
  run.dir = paste0(set.dir,run.names[i],'/')
  cohort.diag.ls[[i]] = diag_cohortBiomass(fgs = here::here('currentVersion','neus_groups.csv'),
                                  mortality = paste0(run.dir,'neus_outputMort.txt'),
                                  agebiomind = paste0(run.dir,'neus_outputAgeBiomIndx.txt'),
                                  neusPriority =here::here('diagnostics','neus_atlantis_group_priority.csv') )%>%
    filter(fishing <=2)%>%
    rename(pass.cohort = 'pass')%>%
    mutate(run.name = run.names[i])%>%
    select(code,maxCohort,pass.cohort,run.name)
  
}
cohort.diag = bind_rows(cohort.diag.ls)

cohort.diag.max = cohort.diag %>%
  select(-pass.cohort)%>%
  tidyr::spread(run.name,maxCohort)

cohort.diag.max$mean.max = apply(cohort.diag.max[,2:ncol(cohort.diag.max)],1,mean,na.rm=T)

arrange(cohort.diag.max,-mean.max)

cohort.diag.pass = cohort.diag %>%
  select(-maxCohort)%>%
  tidyr::spread(run.name,pass.cohort)

cohort.diag.pass$n.pass = apply(cohort.diag.pass[,2:ncol(cohort.diag.pass)],1,sum,na.rm=T)

arrange(cohort.diag.pass,n.pass)


