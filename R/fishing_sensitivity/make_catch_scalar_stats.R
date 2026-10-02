library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggrepel)


data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fscale_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/fscale_combined/'

#make some fake data based on the run.index
experiment.id = 'fscale_combined'


ref.run.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/'
data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary.rds'))
proj.start = 20805

fgs.file = here::here('currentVersion','neus_groups.csv')


fgs = read.csv(fgs.file,as.is =T) %>%select(Code,LongName,IsTurnedOn)

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)

setup.df = setup.df %>% select(run.id,scalar,target.species)

data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary.rds'))%>%
  rename(biomass.age.mean.ref = 'biomass.age.mean')

#Read in biomass and numbers data
biomass = readRDS(paste0(data.dir,'scenario_stats_biomass.rds'))
numbers = readRDS(paste0(data.dir,'scenario_stats_numbers.rds'))

data.run = biomass %>%
  left_join(numbers)%>%
  tidyr::separate(run.name,c('dum','ID'),'_',remove =F )%>%
  mutate(ID = as.numeric(ID))%>%
  rename(Code = 'species')%>%
  left_join(setup.df,by = c('run.name' = 'run.id'))%>%
  left_join(fgs)%>%
  filter(IsTurnedOn == 1)

#Combine run output and ref data
data.all = data.run %>% 
  left_join(data.ref, by = 'Code') %>%
  mutate(biomass.rel = biomass.mean/biomass.ref,
         number.rel = number.mean/number.ref)%>%
  left_join(fgs)%>%
  filter(IsTurnedOn == 1)

spp.names = sort(unique(data.run$Code))

biomass.plot.name = paste0(figure.dir,'biomass_catch_scalar_species.pdf')
number.plot.name = paste0(figure.dir,'numbers_catch_scalar_species.pdf')
biomass.number.plot.name = paste0(figure.dir,'biomass_v_numbers_catch_scalar_species.pdf')
number.age.plot.name = paste0(figure.dir,'numbers_age_catch_scalar_species.pdf')
biomass.age.plot.name = paste0(figure.dir,'biomass_age_catch_scalar_species.pdf')

biomass.plot = number.plot = biomass.number.plot = number.age.plot = biomass.age.plot = list()

biomass.numbers = data.frame(Code = spp.names,biomass.slope = NA,biomass.int = NA, number.slope = NA, number.int = NA)

i=1
for(i in 1:length(spp.names)){
  
  data.species = data.all %>% 
    filter(Code == spp.names[i] & Code == target.species)%>%
    arrange(scalar)%>%
    mutate(scalar.log = log(scalar),
           biomass.rel.log = log(biomass.rel),
           number.rel.log = log(number.rel))
  
  data.species.nozero = data.species %>%
    filter(scalar >0)%>%
    filter(is.finite(biomass.rel.log))
  
  if(nrow(data.species)== 0| all(is.na(data.species$biomass.ref))){
    biomass.plot[[i]] = NULL
    biomass.age.plot[[i]] = NULL
    number.age.plot[[i]] = NULL
    number.plot[[i]] = NULL
    next()
  }else{
    #fit a linear model to biomass and numbers vs fishing scalar and extract the slopes
    b.lm = lm(biomass.rel.log~scalar.log,data.species.nozero)
    biomass.numbers$biomass.slope[i] = coef(b.lm)[2]
    biomass.numbers$biomass.int[i] = coef(b.lm)[1]
  }
  
  if(all(is.na(data.species$number.ref))){
    biomass.numbers$number.slope[i] = NA
    biomass.age.plot[[i]] = NULL
    number.age.plot[[i]] = NULL
    number.plot[[i]] = NULL
    next()
  }else{
    
    n.lm = lm(number.rel.log~scalar.log,data =data.species.nozero)
    biomass.numbers$number.slope[i] = coef(n.lm)[2]
    biomass.numbers$number.int[i] = coef(n.lm)[1]
  }
    

}

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)


data.slope = biomass.numbers %>% 
  filter(!is.na(number.slope))%>%
  left_join(spp2guild)%>%
  left_join(data.ref)
saveRDS(data.slope,paste0(data.dir,'robustness_',experiment.id,'.rds'))

#plot maximum scalar
spp.names2 = sort(unique(data.all$target.species))
guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv')) %>%
  select(Code,LongName,Guild)


data.thresh = data.frame(Code = spp.names2,max.scalar = NA)
for(i in 1:length(spp.names2)){
  
  data.spp = data.all %>% 
    filter(target.species == spp.names2[i] & Code == spp.names2[i])%>%
    mutate(bio.prop = biomass.mean/biomass.ref)%>%
    filter(bio.prop >= 0.1)
  
  data.thresh$max.scalar[i] =  max(data.spp$scalar,na.rm=T)
}
data.thresh = data.thresh %>%
  left_join(fgs)%>%
  left_join(guild2spp)%>%
  left_join(data.ref)

saveRDS(data.thresh, paste0(data.dir,'threshold_scalar_',experiment.id,'.rds'))

thresh.prop = data.thresh %>%
  group_by(max.scalar)%>%
  summarise(N =n())%>%
  mutate(pct = N/nrow(data.thresh))

thresh.prop$cum.pct = cumsum(thresh.prop$pct)

nrow(data.thresh %>% filter(exploit.prop < 0.01))/nrow(data.thresh)