#Script to create spatial biomass and abundance reference points using
# A) Survdat data pull over a certain time range
# B) Initial model conditions
# C) Catch spatial reference (from comlandr pull)
library(dplyr)
library(atlantistools)

###
start.year = 2011
stop.year =2021
ref.years = start.year:stop.year
###

#Read input data
fgs.file = here::here('currentVersion','neus_groups.csv')
fgs = read.csv(fgs.file,as.is = T) %>%
  select(Code,Name,LongName,NumCohorts)


survdat = readRDS(here::here('data','sweptAreaBiomassNEUSBoxSpringandFall.RDS'))


init.file = here::here('currentVersion','neus_init.nc')
init.nc = ncdf4::nc_open(init.file)


#Get box info
bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
bgm = rbgm::bgmfile(bgm.file)
bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm.file))
box.area = bgm$boxes$area

box.l1 = bgm$boxes$botz*-1
box.l1[which(box.l1>50)] = 50
box.l1.vol = box.area*box.l1

bio.conv  = atlantistools::get_conv_mgnbiot(prm_biol = here::here('currentVersion','at_biology.prm'))

#All groups extracted (names, age-structured, biopools, and codes)
group.names = atlantistools::get_groups(fgs.file)
groups.age = atlantistools::get_age_groups(fgs.file)
groups.bp = group.names[!group.names %in% groups.age]
groups.epi = atlantistools::load_bps(fgs = fgs.file,init = init.file)


#Make blank reference format
blank.ref = data.frame(Code = rep(fgs$Code, each = 30),
                       LongName = rep(fgs$LongName,each =30),
           box = rep(0:29, nrow(fgs)))

#A) Survdat-based ref biomass & abundance

survdat.mean = survdat %>%
  filter(YEAR %in% ref.years & variable %in% c('tot.biomass','tot.abundance'))%>%

  group_by(Code,variable,season,box)%>%
  summarise(mean.value = mean(value,na.rm=T))%>%
  group_by(Code,variable,box)%>%
  summarise(mean.value = mean(mean.value,na.rm=T))%>%

  ungroup()

survdat.mean$mean.value[which(survdat.mean$box %in% bboxes)] = NA

survdat.mean = survdat.mean %>%
  group_by(Code,variable)%>%
  mutate(tot.value = sum(mean.value,na.rm=T))%>%
  mutate(prop.value = mean.value/ tot.value,
         box = as.numeric(box))

survdat.num.ref = survdat.mean %>%
  filter(variable == 'tot.abundance')%>%
  right_join(blank.ref)%>%
  ungroup()%>%
  rename(polygon = 'box',
         species = 'LongName',
         var.name = 'variable',
         value = 'mean.value',
         proportion = 'prop.value')%>%
  mutate(var.name = 'abundance')%>%
  select(species,polygon, var.name,proportion,value)%>%
  arrange(species,polygon)%>%
  tidyr::gather('statistic','ref.value',-polygon, -species, -var.name)

survdat.bio.ref = survdat.mean %>%
  filter(variable == 'tot.biomass')%>%
  right_join(blank.ref)%>%
  ungroup()%>%
  rename(polygon = 'box',
         species = 'LongName',
         var.name = 'variable',
         value = 'mean.value',
         proportion = 'prop.value')%>%
  mutate(var.name = 'biomass')%>%
  select(species,polygon, var.name,proportion,value)%>%
  arrange(species,polygon)%>%
  tidyr::gather('statistic','ref.value',-polygon, -species, -var.name)
         
survdat.ref = bind_rows(survdat.bio.ref,survdat.num.ref)

saveRDS(survdat.ref,here::here('data',paste0('spatial_reference_survdat_',start.year,'_',stop.year,'.rds')))

#B) initial conditions based spatial reference

i=1
# init.num.ref = blank.ref%>%
#   left_join(fgs)%>%
#   mutate(value = NA)%>%
#   rename(polygon = 'box',
#          species = 'LongName')

init.ref.ls = list()
for(i in 1:nrow(fgs)){
  
  blank.ref.spp = data.frame(species = fgs$Name[i],polygon = 0:29, abundance.value = NA, abundance.propoprtion = NA, biomass.value = NA, biomass.proportion = NA)
  
  spp.age = fgs$NumCohorts[i]
  
  if(fgs$Name[i] %in% groups.age){
    
     # init.num = load_init_age(init = init.file,
     #               fgs = fgs.file,
     #               bboxes = bboxes,
     #               select_variable = 'Nums',
     #               select_groups = fgs$Name[i])%>%
     #  group_by(species,polygon)%>%
     #  summarise(value = sum(atoutput,na.rm=T))%>%
     #  arrange(polygon)
     
     # blank.ref.spp$num[match(init.num$polygon,blank.ref.spp$polygon )] = init.num$value
    #get ResN and StructN from attribute fill
     
    spp.num.age = matrix(NA,nrow = spp.age,ncol = 30)
    spp.bio.age = matrix(NA,nrow = spp.age,ncol = 30)
    
    j=1
    for(j in 1:spp.age){
      
      var.prefix = paste0(fgs$Name[i],j)
      
      num.var.name = paste0(var.prefix,'_Nums')
      rn.var.name = paste0(var.prefix,'_ResN')
      sn.var.name = paste0(var.prefix,'_StructN')
      
      rn.val = ncdf4::ncatt_get(init.nc,rn.var.name,'_FillValue')$value
      sn.val = ncdf4::ncatt_get(init.nc,sn.var.name,'_FillValue')$value
      
      num.val = colSums(ncdf4::ncvar_get(init.nc,num.var.name),na.rm=T)
      
      biomass.mt = num.val * (rn.val+sn.val) * bio.conv
      
      spp.num.age[j,] = num.val
      spp.bio.age[j,] = biomass.mt
      
    }
    
    spp.num = colSums(spp.num.age,na.rm=T)
    spp.bio = colSums(spp.bio.age,na.rm=T)
    
    spp.num.tot = sum(spp.num,na.rm=T)
    spp.bio.tot = sum(spp.bio,na.rm=T)
    
    spp.num.prop = spp.num/spp.num.tot
    spp.bio.prop = spp.bio/spp.bio.tot
    
    blank.ref.spp$abundance.value = spp.num
    blank.ref.spp$abundance.propoprtion =spp.num.prop
    blank.ref.spp$biomass.value = spp.bio
    blank.ref.spp$biomass.proportion = spp.bio.prop
  }else if(fgs$Name[i] %in% groups.bp){
    
    if(spp.age >1){
      
      spp.bio.age = matrix(NA,nrow = spp.age,ncol = 30)
      
      for(j in 1:spp.age){
        
        n.var.name = paste0(fgs$Name[i],'_N',j)
        
        n.val = colMeans(ncdf4::ncvar_get(init.nc,n.var.name),na.rm=T)
        
        spp.bio.age[j,] = n.val * box.l1.vol * bio.conv
      }
      
      bio.val = colSums(spp.bio.age)
      bio.val.tot = sum(bio.val,na.rm=T)
      bio.val.prop = bio.val/bio.val.tot
      
      blank.ref.spp$biomass.value = bio.val
      blank.ref.spp$biomass.proportion = bio.val.prop
      
    }else if(fgs$Name[i] %in% groups.epi){
      
      n.var.name = paste0(fgs$Name[i],'_N')
      
      n.val = ncdf4::ncvar_get(init.nc,n.var.name)
      
      bio.val = n.val * box.area * bio.conv
      bio.val.tot = sum(bio.val,na.rm=T)
      bio.val.prop = bio.val/bio.val.tot
      
      blank.ref.spp$biomass.value = bio.val
      blank.ref.spp$biomass.proportion = bio.val.prop      
    }else{
    
      n.var.name = paste0(fgs$Name[i],'_N')
      
      n.val = colSums(ncdf4::ncvar_get(init.nc,n.var.name),na.rm=T)
      
      bio.val = n.val * box.l1.vol * bio.conv
      bio.val.tot = sum(bio.val,na.rm=T)
      bio.val.prop = bio.val/bio.val.tot
      
      blank.ref.spp$biomass.value = bio.val
      blank.ref.spp$biomass.proportion = bio.val.prop
      
    }
  }
  
  init.ref.ls[[i]] = blank.ref.spp
  
}

init.ref = bind_rows(init.ref.ls) %>%
  tidyr::gather('variable','init.value',-species,-polygon)%>%
  dplyr::mutate(init.value = ifelse(init.value == 0, NA, init.value))%>%
  tidyr::separate('variable',c('var.name','statistic'))%>%
  left_join(fgs, by = c('species' = 'Name'))%>%
  select(LongName,polygon,var.name,statistic,init.value)%>%
  rename(species = 'LongName')


ncdf4::nc_close(init.nc)
  

saveRDS(init.ref,here::here('data',paste0('spatial_reference_initial_conditions.rds')))


#Make Catch Reference
catch.box = readRDS(here::here('data-raw','landings_by_box_species.rds'))

