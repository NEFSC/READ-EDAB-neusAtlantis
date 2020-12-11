#Script to generate the init_scalars in run.prm based on survdat data

library(dplyr)
library(ncdf4)

source(here::here('R','edit_param_init_scalar.R'))

##Read in parameter and data files

#run file
run.prm = here::here('currentVersion','at_run.prm')

#initial conditions file
init.file = here::here('currentVersion','neus_init.nc')
init.nc = ncdf4::nc_open(init.file)
init.varnames = names(init.nc$var)

#groups file
groups.file = here::here('currentVersion','neus_groups.csv')
fgs = read.csv(groups.file,stringsAsFactors = F)
groups.inverts = fgs$Code[which(fgs$NumCohorts != 10)]
fgs.fullname = select(fgs, Code, Name, LongName)

#bgm.file 
bgm.file = here::here('currentVersion','neus_tmerc_RM2.bgm')

#AgeBiomIndx for run with init_scalars set to 1
bio.age.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_NoInitScalar/neus_outputAgeBiomIndx.txt'
init.biomass = get_init_biomass(bio.age.file,groups.file,write.output = F)
colnames(init.biomass) = c('Code','init.biomass.tonnes')


#Read in swept area data
survdat =readRDS(here::here('data-raw','sweptAreaBiomass.RDS'))
survdat$YEAR = as.numeric(survdat$YEAR)

#Define Output Directory
output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/'

#Missing Cohorts
#missing_cohort column defines first N cohorts that are "missing" from survdat data
missing.cohorts = read.csv(paste0(output.dir,'missing_survdat_cohorts.csv'),stringsAsFactors = F)

#svspp to neus index
spp_neus = read.csv(here::here('data-raw','Atlantis_1_5_groups_svspp_nespp3.csv'),header = T) %>%
  select(-NESPP3)

#Catchability EMAX data
#Filter to only NEUS matching groups, aggregate by NEUS code (median)
#Use fall Q only
load(here::here('data-raw','Emax_Catchability.RData'))
emax_catch = spp %>%
  filter(!is.na(SVSPP)) %>%
  left_join(spp_neus, by = 'SVSPP') %>%
  filter(!is.na(Code)) %>%
  group_by(Code) %>%
  summarize(q = median(Fall.q))

#Filter to first 5 years of each group's data
survdat.agg=survdat %>%
  group_by(Code, YEAR) %>%
  summarize(tot.biomass = sum(tot.biomass,na.rm=T),
            tot.bio.SE = mean(tot.bio.SE,na.rm=T),
            tot.abundance = sum(tot.abundance,na.rm=T),
            tot.abund.SE = mean(tot.abund.SE,na.rm=T)) %>%
  group_by(Code) %>%
  mutate(year.id = YEAR-YEAR[1]) %>%
  # mutate(year.id = YEAR-1964)
  filter(year.id <= 10) %>%
  summarize(tot.biomass = mean(tot.biomass,na.rm=T),
            tot.bio.SE = mean(tot.bio.SE,na.rm=T),
            tot.abundance = mean(tot.abundance,na.rm=T),
            tot.abund.SE = mean(tot.abund.SE,na.rm=T)) %>%
  filter(!Code %in% groups.inverts) %>%
  left_join(missing.cohorts) %>%
  left_join(fgs.fullname)  %>%
  left_join(init.biomass) %>% 
  select(Code, Name, LongName,missing_cohorts,tot.biomass,tot.bio.SE, tot.abundance, tot.abund.SE, init.biomass.tonnes)

#Get numbers at age, total numbers, and fraction for cohorts missing from survdat from init.nc
survdat.agg$init.numbers = NA
survdat.agg$group.scale.fract = NA
for( i in 1:nrow(survdat.agg)){
  
  #Find matching init.nc varnames for each group in survdat.agg
  var.match = init.varnames[grep(paste0(survdat.agg$Name[i],'.*_Nums'), init.varnames)]
  
  #Loop through all cohorts and retreive domain-wide numbers in init.nc
  group.nums.df = data.frame(varname = var.match, Name = survdat.agg$Name[i], cohort= NA, nums = NA)
  for(j in 1:length(var.match)){
    group.nums.df$cohort[j] = as.numeric(strsplit(var.match[j],paste0(survdat.agg$Name[i],'|_Nums'))[[1]][2])
    group.nums.df$nums[j] = sum(ncdf4::ncvar_get(init.nc,var.match[j]),na.rm=T)
  }
  group.nums.tot = sum(group.nums.df$nums)
  
  
  group.nums.df = group.nums.df %>%
    mutate(num.tot = group.nums.tot,
           cohort.prop = nums/num.tot) %>%
    arrange(cohort) %>% 
    filter(cohort <= survdat.agg$missing_cohorts[i])
  
  survdat.agg$init.numbers[i] = group.nums.tot
  survdat.agg$group.scale.fract[i] = 1-sum(group.nums.df$cohort.prop)
  
}


#join in catchability
#set to 1 if Emax q value not available for group
survdat.agg = survdat.agg %>%
  left_join(emax_catch, by = 'Code') %>%
  units::drop_units() %>%
  mutate(tot.abundance.adj = (as.numeric(tot.abundance)/group.scale.fract)/q,
         new.init.scalar = tot.abundance.adj/init.numbers)

# write.csv(survdat.agg, file = paste0(output.dir,'survdat_initial_conditions.csv'),row.names = F)
write.csv(survdat.agg, file = here::here('data-raw','survdat_initial_conditions.csv'),row.names = F)

  
