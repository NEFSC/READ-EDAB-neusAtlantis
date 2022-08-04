#Makes a table with reference biomass (converted to numbers) for all groups with data
library(dplyr)
library(ncdf4)

#Get StockSmart Data units and make conversion table 
# readRDS(here::here('data-raw/stockData.Rds'))%>%
#   filter(Metric == 'Abundance')%>%
#   select(Species,Code,Description,Units)%>%
#   distinct()%>%
#   write.csv(here::here('data','StockSmart_Abundance_Units.csv'),row.names = F)

#Read in FSPB
source(here::here('R','edit_param_FSPB.R'))
fspb = get_param_FSPB(bio.prm = here::here('currentVersion','at_biology.prm'))
#Groups file
fgs = read.csv(here::here('currentVersion','neus_groups.csv'))%>%
  select(LongName,Code,Name,NumCohorts)
#StockSmart unit conversions
ss.unit.cases = read.csv(here::here('data','StockSmart_Abundance_Units_Conversion.csv'))
ss.unit.conv = read.csv(here::here('data-raw','StockSmart_Conversions.csv'))
#Box proportions
box.props = read.csv(here::here('diagnostics','Group_Box_Proportions.csv'))
#Menhaden data
men.dat = read.csv(here::here('data-raw','data','MenhadenBiomass.csv'),as.is =T)%>%rename(Value = 'Biomass')%>%mutate(Code = 'MEN')
#Striped Bass data
stb.dat = read.csv(here::here('data-raw','data','StripedBassBiomass.csv'),as.is =T)%>%rename(Value = 'Biomass') %>% mutate(Code = 'STB')
#Lobster data
lob.dat = read.csv(here::here('data-raw','data','LobsterBiomass.csv'),as.is =T)%>%rename(Value = 'Biomass') %>% mutate(Code = 'LOB')
non.ss.all = bind_rows(men.dat,stb.dat,lob.dat)

####Flag for predefined size at age####
prescribed.age.scale = T
age.scale = c(0.005,0.03,0.2,0.2,0.175,0.15,0.1,0.075,0.05,0.015)
####

# run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/New_LengthAge_Revised/Post_Processed/Data/'
run.dir = here::here('Atlantis_Runs','Dev_07282022','Post_Processed','Data/')

#Get initial biomass in mT
init.biomass.age = readRDS(paste0(run.dir,'biomass_age.rds'))%>%
  filter(time ==0)%>%
  select(-time)%>%
  rename(biomass.mt = 'atoutput')

init.biomass.invert = readRDS(paste0(run.dir,'biomass_age_invert.rds'))%>%
  filter(time == 0)%>%
  mutate(agecl = 1)%>%
  select(-time)%>%
  rename(biomass.mt = 'atoutput')
init.biomass.all = bind_rows(init.biomass.age,init.biomass.invert)%>%
  left_join(fgs,by = c('species'='LongName'))

#Get initial RN and SN for age groups
# init.rn = readRDS(paste0(run.dir,'RN_age_mean.rds'))%>%
#   filter(time == 0)%>%
#   rename(rn = 'atoutput')
# init.sn = readRDS(paste0(run.dir,'sN_age_mean.rds'))%>%
#   filter(time == 0)%>%
#   rename(sn = 'atoutput')
# init.rn.sn = init.rn %>%
#   left_join(init.sn)%>%
#   mutate(tot.n = rn+sn)
init.rn.sn = read.csv(here::here('diagnostics','Initial_Size_Age.csv'))%>%
  left_join(fgs)%>%
  mutate(tot.n = rn + sn,
         ratio = rn/sn)



# init.rn.sn = read.csv( here::here('diagnostics','Initial_RN_SN_from_reference_length_age.csv')) %>%
#   left_join(fgs)%>%
#   select(LongName,agecl,RN,SN)%>%
#   rename(species = 'LongName',rn = 'RN',sn = 'SN')%>%
#   mutate(tot.n = rn + sn)
#     

ss.neus = readr::read_csv(here::here("data","functionalGroupNames.csv"))
# data.ss.raw = ss.neus %>%
#   dplyr::inner_join(stocksmart::stockAssessmentData,by=c("Species_Itis"="ITIS"),na_matches = "never") %>% 
#   filter(Metric == 'Abundance'& RegionalEcosystem %in% c("Northeast Shelf","Atlantic Highly Migratory"))%>%
#   select(Code,Description,Units,AssessmentYear,Year)%>%
#   group_by(Code,Description,Units,AssessmentYear)%>%
#   summarise(year.min = min(Year),year.max = max(Year))%>%
#   ungroup()%>%
#   arrange(Code,AssessmentYear)

ss.raw = stocksmart::stockAssessmentData %>%
  filter(Metric == 'Abundance' & RegionalEcosystem %in% c("Northeast Shelf","Atlantic Highly Migratory") )

# write.csv(data.ss.raw, file = here::here('diagnostics','StockSmart_Assessment_Summary.csv'),row.names = F)
data.no.ss = ss.unit.cases %>%
  filter(in.stocksmart == F)%>%
  left_join(ss.neus)%>%
  left_join(non.ss.all)

data.ss = ss.unit.cases %>%
  filter(in.stocksmart == T)%>%
  left_join(ss.neus)%>%
  left_join(ss.raw)%>%
  bind_rows(data.no.ss)%>%
  filter(Year >= 1990)%>%
  group_by(Code,Units,AssessmentYear,Case,min.cohort,sex.ratio.mf)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  filter(Case %in% c('Total','SSB','Adult','Female'))%>%
  left_join(ss.unit.conv)%>%
  mutate(Value.new = Value*conversion)
  
data.ss$numbers = NA
data.ss$biomass.tot = NA

i=1
for(i in 1:nrow(data.ss)){
  
  #Convert different cases to biomass (except for numbers)
  if(fgs$NumCohorts[which(fgs$Code == data.ss$Code[i])] == 1){
    data.ss$biomass.tot[i] = data.ss$Value.new[i]
    next()
  }
  
  if(data.ss$Case[i] == 'Total' & data.ss$new.units[i] == 'mt'){
    data.ss$biomass.tot[i] = data.ss$Value.new[i]
  }else if(data.ss$Case[i] == 'SSB'){
    
    fspb.group = fspb %>%
      filter(group == data.ss$Code[i])%>%
      reshape2::melt(id.vars = 'group',value.name = 'fspb')%>%
      tidyr::separate(variable,c('dum','agecl'))%>%
      mutate(agecl = as.numeric(agecl),
             fspb = as.numeric(fspb))
    
    init.bio.group = init.biomass.all %>%
      filter(Code == data.ss$Code[i])%>%
      left_join(fspb.group)%>%
      mutate(ssb = biomass.mt * fspb)
    
    init.bio.group.tot = init.bio.group %>%
      group_by(species)%>%
      summarise(biomass.mt.tot = sum(biomass.mt,na.rm=T),
                ssb.tot = sum(ssb,na.rm=T))%>%
      mutate(ssb.prop = biomass.mt.tot/ssb.tot)
    
    data.ss$biomass.tot[i] = data.ss$Value.new[i] * init.bio.group.tot$ssb.prop[1]
  }else if(data.ss$Case[i] == 'Adult'){
    
    init.bio.group = init.biomass.all %>%
      filter(Code == data.ss$Code[i])
    
    init.bio.age = init.bio.group %>%
      filter(agecl >= data.ss$min.cohort[i])%>%
      group_by(Code)%>%
      summarise(biomass.mt.age.tot = sum(biomass.mt,na.rm=T))
    
    init.bio.age.prop = init.bio.group%>%
      group_by(Code)%>%
      summarise(biomass.mt.tot = sum(biomass.mt,na.rm=T))%>%
      left_join(init.bio.age)%>%
      
      mutate(biomass.age.prop = biomass.mt.tot/biomass.mt.age.tot)
    
    data.ss$biomass.tot[i] = data.ss$Value.new[i] * init.bio.age.prop$biomass.age.prop[1]
  }else if(data.ss$Case[i] == 'Female'){
    data.ss$biomass.tot[i] = data.ss$Value.new[i]*2
  }else if(data.ss$Case[i] == 'Total' & data.ss$new.units[i] == 'num'){
    data.ss$numbers[i] = data.ss$Value.new[i]
  }

}


#Clean up SS data reference table
data.ss.final = data.ss %>%
  group_by(Code)%>%
  summarise(numbers = mean(numbers,na.rm=T),
            biomass.tot = mean(biomass.tot,na.rm=T))
write.csv(data.ss.final,file = here::here('diagnostics','StockSmart_Adjusted_Abundance.csv'),row.names = F)


#Read in survdat reference biomass
# sdBiomass <- readRDS(here::here("data/sweptAreaBiomassNEUS.rds")) %>%
#   dplyr::filter(variable %in% c("tot.biomass") & YEAR >= 1990) %>%
#   dplyr::mutate(variable = ifelse(as.character(variable)=="tot.biomass","survdatBio",as.character(variable))) %>%
#   dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
#   dplyr::group_by(Code,variable) %>% 
#   dplyr::summarize(biomass.tot = sum(value),.groups="drop")%>%
#   mutate(source = 'survdat')%>%
#   select(Code,source,biomass.tot)%>%
#   filter(!(Code %in% data.ss.final$Code))

#Combine survdat and stocksmart biomass estimates
# data.combined = data.ss.final %>%
#   mutate(source = 'StockSmart')%>%
#   bind_rows(sdBiomass)%>%
#   left_join(fgs)

data.combined = data.ss.final %>%
  left_join(fgs)
  
##Separate by group type
#Inverts
data.invert = data.combined %>% filter(NumCohorts ==1)%>%
  mutate(biomass.N = biomass.tot * 1E9 /20/5.7)

#Age-structured inverts
data.invert.age = data.combined %>% filter(NumCohorts ==2)

#Age-structered verts
data.age = data.combined %>%
  filter(NumCohorts ==10)%>%
  mutate(biomass.N = biomass.tot * 1E9/20/5.7)

init.nc = nc_open(here::here('currentVersion','neus_init.nc'))
varnames = names(init.nc$var)

#Pull proportion at age and the size at age from initial conditions and apply to the reference biomass.n

ref.data.ls = list()
for(i in 1:nrow(data.age)){
  
  ind.n = init.rn.sn %>% filter(LongName == data.age$LongName[i]) 
  ind.n.age = ind.n$tot.n
  
  #Get prop at age
  dat.age = numeric()
  for(j in 1:10){
    group.name = grep(paste0(data.age$Name[i],j,'_Nums'),varnames,value = T)
    dat.age[j] = sum(ncvar_get(init.nc,group.name)[1,],na.rm=T)
  }
  
  #option to use fixed age-structure
  if(prescribed.age.scale == T){
    dat.age.prop = age.scale
  }else{
    dat.age.prop = dat.age/sum(dat.age)
  }

  if(is.na(data.age$biomass.N[i])){
    ref.nums = round(data.age$numbers[i]*dat.age.prop)
  }else{
    ref.nums = round((data.age$biomass.N[i]/ind.n.age)*dat.age.prop)  
  }
  
  ref.data.ls[[i]] = data.frame(Code = data.age$Code[i],agecl = 1:10,ref.nums = ref.nums)

}
nc_close(init.nc)
ref.num.age = bind_rows(ref.data.ls)

write.csv(ref.num.age, file = here::here('diagnostics','StockSmart_Initial_Numbers.csv'),row.names = F)

#Calculate box-level numbers based on box proportions
age.groups = unique(ref.num.age$Code)
num.box.age.all.ls = list()
for(i in 1:length(age.groups)){
  group.num = ref.num.age %>%
    filter(Code == age.groups[i])
  group.box.prop = box.props[,as.character(age.groups[i])]
  
  num.box.age.group = as.data.frame(group.num$ref.nums %*% t(group.box.prop))
  colnames(num.box.age.group) = 0:29
  num.box.age.group$agecl = 1:10
  num.box.age.all.ls[[i]] = num.box.age.group %>%
    tidyr::gather(key = 'box',value = 'measurement',-agecl)%>%
    mutate(Code = age.groups[i])
}

num.box.age.all = bind_rows(num.box.age.all.ls)
# x = num.box.age.all %>% filter(Code == 'COD')%>%group_by(agecl)%>%summarise(ref.num = sum(measurement))

write.csv(num.box.age.all, here::here('diagnostics','StockSmart_Numbers_Initial_Conditions.csv'),row.names = F)

#Reset Init Scalar to 1 for changed groups
source(here::here('R','edit_param_init_scalar.R'))
init.scalar = get_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                      groups.file = here::here('currentVersion','neus_groups.csv'),
                      write.output = F)
new.init.scalar = init.scalar
for( i in 1:length(age.groups)){new.init.scalar$init.scalar[which(as.character(new.init.scalar$group) == as.character(age.groups[i]))] = 1 }
new.init.scalar$init.scalar = as.numeric(as.character(new.init.scalar$init.scalar))
edit_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                       groups.file = here::here('currentVersion','neus_groups.csv'),
                       new.init.scalar = new.init.scalar,
                       overwrite = T
                       )

#Write new box-age numbers values to init.nc
init.nc = nc_open(here::here('currentVersion','neus_init.nc'),write = T)
varnames = names(init.nc$var)

for(i in 1:length(age.groups)){

  for(j in 1:10){
    num.age.group = num.box.age.all %>%
      filter(Code == age.groups[i],agecl == j)%>%
      left_join(fgs)

    nc.name = grep(paste0(num.age.group$Name[i],j,'_Num'),varnames,value = T)

    new.init = matrix(NA,nrow = 5, ncol = 30)
    new.init[1,] = num.age.group$measurement
    ncvar_put(nc = init.nc,varid = nc.name, vals = new.init)
    # ncvar_get(init.nc,nc.name)
  }
}
nc_close(init.nc)


#Check
# test = num.box.age.all %>% filter(Code == 'COD')%>% group_by(agecl) %>% summarise(ref.nums = sum(measurement))
# size = init.rn.sn %>% filter(LongName == 'Atlantic cod')
# sum(test$ref.nums*size$tot.n*5.7*20*1E-9)
