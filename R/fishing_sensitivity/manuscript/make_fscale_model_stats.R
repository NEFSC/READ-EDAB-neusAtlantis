#Stats related to fitting catch scalar decay function to Rel_Biomass ~ Scalar

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
data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary_20805_28105.rds'))
proj.start = 20805

fgs.file = here::here('currentVersion','neus_groups.csv')


  
fgs = read.csv(fgs.file,as.is =T) %>%select(Code,LongName,IsTurnedOn)

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)

# setup.df$target.species = sapply(setup.df$Run,function(x) return(strsplit(x,split=paste0(experiment.id,'|_'))[[1]][3]),USE.NAMES = F)
mig.groups = read.csv(here::here('currentVersion','neus_migrations_orig.csv'),as.is=T)$GroupCode

setup.df = setup.df %>% select(run.id,scalar,target.species)

data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary_20805_28105.rds'))%>%
  rename(biomass.age.mean.ref = 'biomass.age.mean')%>%
  mutate(biomass.ref.plot = ifelse(Code %in% mig.groups,biomass.ref.max,biomass.ref.mean),
         number.ref.plot = ifelse(Code %in% mig.groups,number.ref.max,number.ref.mean))

#Read in biomass and numbers data

biomass = readRDS(paste0(data.dir,'scenario_stats_biomass.rds'))%>%
  mutate(biomass.plot = ifelse(species %in% mig.groups,biomass.max,biomass.mean))

numbers = readRDS(paste0(data.dir,'scenario_stats_numbers.rds'))%>%
  mutate(number.plot = ifelse(species %in% mig.groups,number.max,number.mean))

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
  mutate(biomass.rel = biomass.plot/biomass.ref.plot,
         number.rel = number.plot/number.ref.plot)%>%
  left_join(fgs)%>%
  filter(IsTurnedOn == 1)%>%
  filter(scalar %in% c(2,5,10,25,50,100))

spp.names = sort(unique(data.run$Code))
  
#Fit 1)lm , 2)log, 3)complex power function and compare

data.model.comp = data.frame(Code = spp.names,bio.1.AIC = NA, bio.2.AIC = NA, bio.3.AIC = NA, 
                             num.1.AIC = NA, num.2.AIC = NA, num.3.AIC = NA)
i=6
for(i in 1:length(spp.names)){
  
  data.species = data.all %>% 
    filter(Code == spp.names[i] & Code == target.species)%>%
    arrange(scalar)
  
  if(nrow(data.species)== 0){
    next()
  }
  
  data.species.nozero = data.species %>%
    filter(scalar !=0)%>%
    select(Code,target.species,LongName,scalar,biomass.rel,number.rel)%>%
    mutate(biomass.rel.log = log(biomass.rel),
           number.rel.log = log(number.rel),
           scalar.log = log(scalar))
  
  dum.line = data.species.nozero[1,]
  dum.line$scalar = 1
  dum.line$biomass.rel = 1
  dum.line$number.rel = 1
  dum.line$biomass.rel.log = 0
  dum.line$number.rel.log = 0
  
  data.species.nozero = bind_rows(dum.line,data.species.nozero)
  
  bio.model.1 = lm(biomass.rel~scalar,data.species.nozero)
  bio.model.2 = lm(biomass.rel.log~scalar.log,data.species.nozero)
  bio.model.3 = try(nls(biomass.rel ~ (1/(a+b*scalar^c))+d,data.species.nozero,
                    start = list(a = 1,b = 0.1, c= 2,d=min(data.species.nozero$biomass.rel)),
                    lower = c(0,0,0,0),
                    alg = 'port'), silent = T)

  data.model.comp$bio.1.AIC[i]=AIC(bio.model.1)
  data.model.comp$bio.2.AIC[i]=AIC(bio.model.2)
  if(class(bio.model.3) != 'try-error'){data.model.comp$bio.3.AIC[i]=AIC(bio.model.3)}
  
  if(any(is.na(data.species.nozero$number.rel))){
    next()
  }
  num.model.1 = lm(number.rel~scalar,data.species.nozero)
  num.model.2 = lm(number.rel.log~scalar.log,data.species.nozero)
  num.model.3 = try(nls(number.rel ~ (1/(a+b*scalar^c))+d,
                        data.species.nozero,
                        start = list(a = 1,b = 0.1, c= 2,d = min(data.species.nozero$number.rel)),
                        lower = c(0,0,0,0),
                        alg = 'port'), silent = T)
  
  data.model.comp$num.1.AIC[i]=AIC(num.model.1)
  data.model.comp$num.2.AIC[i]=AIC(num.model.2)
  if(class(num.model.3) != 'try-error'){data.model.comp$num.3.AIC[i]=AIC(num.model.3)}
}

write.csv(data.model.comp, '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/tables/catch_scalar_model_comparisons.csv',row.names =F)

data.model.comp2 =data.model.comp %>%
  tidyr::gather(dum,AIC,-Code)%>%
  tidyr::separate(dum,c('variable','model','dum2'))%>%
  filter(!is.na(AIC))%>%
  group_by(Code,variable)%>%
  mutate(is.min = AIC == min(AIC,na.rm=T))%>%
  filter(is.min == T)%>%
  arrange(Code,variable)

write.csv(data.model.comp2, '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/tables/catch_scalar_model_selected.csv',row.names =F)

data.model.comp2 %>%
  select(Code,variable,model,is.min)%>%
  group_by(variable,model)%>%
  summarise(count = n())%>%
  tidyr::spread(model,count)

