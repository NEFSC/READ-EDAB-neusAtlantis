#Function to edit initial age distribution

# bio.prm =  here::here('currentVersion','at_biology.prm')
# fgs.file = here::here('currentVersion','neus_groups.csv')
# init.file = here::here('currentVersion','neus_init.nc')
# ss.cat.file = here::here('data','StockSmart_Abundance_Units_Conversion.csv')
# ss.conv.file = here::here('data-raw','StockSmart_Conversions.csv')
# box.prop.file = here::here('diagnostics','Group_Box_Proportions.csv')
# peak.age = 3
# steepness = 1.5
# ref.run.dir = here::here('Atlantis_Runs','Dev_07282022','Post_Processed','Data/')
# prescribed.age.scale = T
# age.scale =  c(0.005,0.03,0.2,0.2,0.175,0.15,0.1,0.075,0.05,0.015)
# # init.size.age =  here::here('currentVersion','vertebrate_init_length_cm.csv')
# init.size.age = here::here('diagnostics','Initial_Size_Age.csv')
# ss.adj.abund.file =  here::here('diagnostics','StockSmart_Adjusted_Abundance.csv')
# overwrite = F
# new.init.file = here::here('currentVersion','neus_init_test.nc')

edit_init_age_distribution = function(bio.prm,
                                      fgs.file,
                                      ss.conv.file,
                                      ss.cat.file,
                                      box.prop.file,
                                      peak.age,
                                      steepness,
                                      prescribed.age.scale = F,
                                      age.scale = NA,
                                      ref.run.dir,
                                      init.size.age,
                                      ss.adj.abund.file,
                                      groups = NA,
                                      init.file,
                                      overwrite,
                                      new.init.file){
    
  library(dplyr)
  library(ncdf4)
  
  #Read in FSPB
  source(here::here('R','edit_param_FSPB.R'))
  source(here::here('R','make_age_distribution.R'))
  source(here::here('R','make_size_age_reference.R'))
  source(here::here('R','make_init_size_age.R'))
  fspb = get_param_FSPB(bio.prm)
  #Groups file
  fgs = read.csv(fgs.file)%>%
    select(LongName,Code,Name,NumCohorts)
  #StockSmart unit conversions
  ss.unit.cases = read.csv(ss.cat.file)
  ss.unit.conv = read.csv(ss.conv.file)
  #Box proportions
  box.props = read.csv(box.prop.file)
  #Menhaden data
  men.dat = read.csv(here::here('data-raw','data','MenhadenBiomass.csv'),as.is =T)%>%rename(Value = 'Biomass')%>%mutate(Code = 'MEN')
  #Striped Bass data
  stb.dat = read.csv(here::here('data-raw','data','StripedBassBiomass.csv'),as.is =T)%>%rename(Value = 'Biomass') %>% mutate(Code = 'STB')
  #Lobster data
  lob.dat = read.csv(here::here('data-raw','data','LobsterBiomass.csv'),as.is =T)%>%rename(Value = 'Biomass') %>% mutate(Code = 'LOB')
  non.ss.all = bind_rows(men.dat,stb.dat,lob.dat)
  
  ####Flag for predefined size at age####
  
  if(is.na(age.scale)){
    age.scale = make_age_distribution(peak.age,steepness)
  }
  
  # plot(ages.scale,type='l')
  # age.scale = c(0.005,0.03,0.2,0.2,0.175,0.15,0.1,0.075,0.05,0.015)
  ####
  
  # ref.run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/New_LengthAge_Revised/Post_Processed/Data/'
  
  #Get initial biomass in mT
  init.biomass.age = readRDS(paste0(ref.run.dir,'biomass_age.rds'))%>%
    filter(time ==0)%>%
    select(-time)%>%
    rename(biomass.mt = 'atoutput')
  
  init.biomass.invert = readRDS(paste0(ref.run.dir,'biomass_age_invert.rds'))%>%
    filter(time == 0)%>%
    mutate(agecl = 1)%>%
    select(-time)%>%
    rename(biomass.mt = 'atoutput')
  init.biomass.all = bind_rows(init.biomass.age,init.biomass.invert)%>%
    left_join(fgs,by = c('species'='LongName'))
  
  #Get initial RN and SN for age groups
  make_init_size_age(init.file = init.file,fgs.file = fgs.file,out.file = init.size.age)
  init.rn.sn = read.csv(init.size.age)%>%
    left_join(fgs)%>%
    mutate(tot.n = rn + sn,
           ratio = rn/sn)
  
  ss.neus = readr::read_csv(here::here("data","functionalGroupNames.csv"))
  
  ss.raw = stocksmart::stockAssessmentData %>%
    filter(Metric == 'Abundance' & RegionalEcosystem %in% c("Northeast Shelf","Atlantic Highly Migratory") )
  
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
  # write.csv(data.ss.final,file = ss.adj.abund.file,row.names = F)
  
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
  
  if(overwrite == T){
    init.nc = nc_open(init.file)  
  }else{
    file.copy(init.file,new.init.file,overwrite = T)
    init.nc = nc_open(new.init.file)
  }
  
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
  
  # write.csv(ref.num.age, file = here::here('diagnostics','StockSmart_Initial_Numbers.csv'),row.names = F)
  
  #Calculate box-level numbers based on box proportions
  if(is.na(groups)){
    age.groups = unique(ref.num.age$Code)  
  }else{
    age.groups = groups
  }
  
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
  
  # write.csv(num.box.age.all, here::here('diagnostics','StockSmart_Numbers_Initial_Conditions.csv'),row.names = F)
  
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
  
  if(overwrite==T){
    init.nc = nc_open(init.file,write = T)
  }else{
    init.nc = nc_open(new.init.file,write = T)
  }
  varnames = names(init.nc$var)
  
  for(i in 1:length(age.groups)){
  
    for(j in 1:10){
      num.age.group = num.box.age.all %>%
        filter(Code == age.groups[i],agecl == j)%>%
        left_join(fgs)
  
      nc.name = grep(paste0(num.age.group$Name[i],j,'_Nums'),varnames,value = T)
  
      new.init = matrix(NA,nrow = 5, ncol = 30)
      new.init[1,] = num.age.group$measurement
      ncvar_put(nc = init.nc,varid = nc.name, vals = new.init)
      # ncvar_get(init.nc,nc.name)
    }
  }
  nc_close(init.nc)
  
}
