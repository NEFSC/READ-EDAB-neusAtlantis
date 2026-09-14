#Function to calculate the available catch based on the population, fishing footprints, and minimum size
library(dplyr)
library(XML)
library(xml2)
run.name = 'fleet_calibration_5'
ref.years = c(30,60)


run.dir = here::here('Atlantis_Runs',run.name,'')
figure.dir = paste0(run.dir,'Post_Processed/')
param.ls = atlantisprocessing::get_atl_paramfiles(param.dir = here::here('currentVersion',''),
                              atl.dir=run.dir,
                              run.prefix = 'neus_output',
                              include_catch=T)

#Functional groups
fgs.orig = read.csv(param.ls$groups.file)
fgs.fished = fgs.orig %>%
  dplyr::filter(isFished == 1 & IsTurnedOn == 1 & NumCohorts >2)%>%
  dplyr::arrange(LongName)

fleet.param = read.csv(param.ls$fishery.prm)
fleet.names = fleet.param$Code
gf.fleet.names = grep('^gf',fleet.names,value = T)

#Function to extract attributes from harvest.xml
extract_subattribute <- function(xml_file, attribute_name) {
  # Parse the XML file
  xml_data <- read_xml(xml_file)
  
  # Find the Attribute node matching the provided attribute name
  attribute_node <- xml_find_first(
    xml_data,
    sprintf(".//Attribute[@AttributeName='%s']", attribute_name)
  )
  
  if (is.na(attribute_node)) {
    stop(sprintf("Attribute '%s' not found in the XML structure.", attribute_name))
  }
  
  # Extract FisheryValue nodes within the matched Attribute
  if(grepl('FisheryValue',attribute_node)){
    fishery_values <- xml_find_all(attribute_node, ".//FisheryValue")  
    data <- tibble(
      FisheryName = xml_attr(fishery_values, "FisheryName"),
      AttributeValue = xml_attr(fishery_values, "AttributeValue")
    )
  }else if(grepl('GroupValue',attribute_node)){
    group_values <- xml_find_all(attribute_node, ".//GroupValue")
    data <- tibble(
      GroupName = xml_attr(group_values, "GroupName"),
      AttributeValue = xml_attr(group_values, "AttributeValue")
    )
  }else{
    stop('Attribute type only works if GroupValue or FisheryValue')
  }
  
  return(data)
}

#Get fleet targets
fleet.target.xml = extract_subattribute(xml_file = paste0(run.dir,'at_harvest.xml'),attribute_name =  "FisheryTargetSpecies")
i =1
fleet.target.ls = list()
for(i in 1:nrow(fleet.target.xml)){
  target.str = fleet.target.xml$AttributeValue[i]
  target.val = as.numeric(strsplit(target.str,split = ' |\t')[[1]])
  fleet.target.ls[[i]] = data.frame(fleet = fleet.target.xml$FisheryName[i], Code = fgs.orig$Code, target = target.val)
}
fleet.target = bind_rows(fleet.target.ls)

# get fleet minimum size
fleet.minsize.xml <- extract_subattribute(xml_file = paste0(run.dir,'at_harvest.xml'),attribute_name= "SizeBasedDiscarding")
i = 1
fleet.minsize.ls = list()
for(i in 1:nrow(fleet.minsize.xml)){
  minsize.str = fleet.minsize.xml$AttributeValue[i]
  minsize.val = as.numeric(strsplit(minsize.str,split = ' |\t')[[1]])
  fleet.minsize.ls[[i]] = data.frame(Code = fleet.minsize.xml$GroupName[i], fleet = fleet.param$Code, minsize = minsize.val)
}
fleet.minsize = bind_rows(fleet.minsize.ls) %>%
  select(fleet,Code,minsize)

#Get model output RN and SN
model.nc = ncdf4::nc_open(param.ls$main.nc)
model.nc.names = names(model.nc$var)


#Get model output catch.fleet
model.catch = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))
catch.nc = ncdf4::nc_open(paste0(run.dir,'/neus_outputCATCH.nc'))
model.time = catch.nc$dim$t$vals/86400/365
which.time = which(model.time >= ref.years[1] & model.time <= ref.years[2])
model.time = model.time[which.time]
#Read bio.prm lines
bio.lines = readLines(param.ls$biol.prm)

#get wetdry conversion
wetdry = grep('wetdry',bio.lines,value =T)
wetdry = strsplit(wetdry,'\t| ')[[1]]
wetdry = as.numeric(wetdry[wetdry != ''][2])

#Get CN conversion
CN = grep('X_CN',bio.lines,value =T)
CN = strsplit(CN,'\t| ')[[1]]
CN = as.numeric(CN[CN != ''][2])

spp.len.ls = list()
spp.bio.ls = list()
s=1
for(s in 1:length(fgs.fished$Code)){
  
  #Get model length-weight conversion
  spp.lia = grep(paste0('li_a_',fgs.fished$Code[s]),bio.lines,value =T)
  spp.lia = strsplit(spp.lia,paste0('li_a_|',fgs.fished$Code[s],'|\t'))[[1]]
  spp.lia = as.numeric(spp.lia[spp.lia != ''][1])
  spp.lib = grep(paste0('li_b_',fgs.fished$Code[s]),bio.lines,value =T)
  spp.lib = strsplit(spp.lib,paste0('li_b_|',fgs.fished$Code[s],'|\t'))[[1]]
  spp.lib = as.numeric(spp.lib[spp.lib != ''][1])
  
  #Get model population
  
  spp.ages = fgs.fished$NumCohorts[s]
  
  spp.age.len.ls = list()
  spp.age.bio.ls= list()
  for(a in 1:spp.ages){
    spp.sn.age = ncdf4::ncvar_get(model.nc,paste0(fgs.fished$Name[s],a,'_StructN'))[,,which.time]
    spp.rn.age = ncdf4::ncvar_get(model.nc,paste0(fgs.fished$Name[s],a,'_ResN'))[,,which.time]
    spp.num.age = ncdf4::ncvar_get(model.nc,paste0(fgs.fished$Name[s],a,'_Nums'))[,,which.time]
    spp.catch.age = ncdf4::ncvar_get(catch.nc,paste0(fgs.fished$Name[s],a,'_Catch'))[,which.time]
    
    spp.wgt.age = (spp.sn.age + spp.rn.age)* wetdry * CN /1000
    spp.wgt.age[spp.wgt.age == 0] = NA
    spp.wgt.age.mean = apply(spp.wgt.age,c(2,3),mean,na.rm=T)
    spp.len.age.mean = (spp.wgt.age.mean/spp.lia)^(1/spp.lib)
    spp.num.age[spp.num.age == 0] = NA
    spp.num.age.sum = apply(spp.num.age,c(2,3),sum,na.rm=T)
    
    spp.len.age = (spp.wgt.age/spp.lia)^(1/spp.lib)
    
    spp.age.bio.ls[[a]] = reshape2::melt(spp.wgt.age)%>%
      rename(layer = 'Var1',polygon = 'Var2',time = 'Var3',biomass = 'value')%>%
      filter(!is.na(biomass))%>%
      group_by(polygon,time)%>%
      summarise(biomass = sum(biomass))%>%
      mutate(species = fgs.fished$Name[s],
             agecl = a)%>%
      select(species,agecl,time,polygon,biomass)
      
    
    spp.len.age.long = reshape2::melt(spp.len.age)%>%
      rename(layer = 'Var1',polygon = 'Var2',time = 'Var3',length = 'value')%>%
      filter(!is.na(length))%>%
      group_by(polygon,time)%>%
      summarise(length = mean(length))%>%
      mutate(species = fgs.fished$Name[s],
             agecl = a)%>%
      select(species,agecl,time,polygon,length)

    spp.age.len.ls[[a]] = spp.len.age.long
  }
  spp.len.ls[[s]] = bind_rows(spp.age.len.ls)
  spp.bio.ls[[s]] = bind_rows(spp.age.bio.ls)
}
spp.len.df = bind_rows(spp.len.ls) 
spp.bio.df = bind_rows(spp.bio.ls) 
saveRDS(spp.len.df,paste0(run.dir,'Post_Processed/Data/length_age_box.rds') )
saveRDS(spp.bio.df,paste0(run.dir,'Post_Processed/Data/biomass_age_box.rds') )

#Get fishing footprints
fleet.ref = readRDS(here::here('data-raw','data','groundfishFleetData.rds'))$effort%>%
  mutate(fleet = paste0('gf',gsub(' ','',tolower(newport))))%>%
  select(fleet,Box, effort)%>%
  group_by(fleet,Box)%>%
  summarise(effort = sum(effort,na.rm=T))

footprint.mat = matrix(0, nrow = length(gf.fleet.names), ncol = 30)
for(i in 1:length(gf.fleet.names)){
  fleet.boxes = filter(fleet.ref, fleet == gf.fleet.names[i]) %>%
    arrange(Box)
  f.eff = round(fleet.boxes$effort/sum(fleet.boxes$effort),2)
  footprint.mat[i,(fleet.boxes$Box+1)] = f.eff
}  
  
#Zero out boundary boxes
footprint.mat.nobound = footprint.mat
footprint.mat.nobound[,c(1,22:30)] = 0
footprint.mat.nobound = footprint.mat.nobound/rowSums(footprint.mat.nobound)

footprint.df = reshape2::melt(footprint.mat.nobound)%>%
  rename(Index = 'Var1',polygon = 'Var2',eff.wgt = 'value')%>%
  mutate(Index = Index + 4)%>%
  left_join(fleet.param)%>%
  rename(fleet.name = 'Code')%>%
  mutate(eff.flag = ifelse(eff.wgt>0,1,0))%>%
  select(fleet.name,polygon,eff.wgt,eff.flag)

#Flag which species/box overlap with footprint
#Need to merge fleet x spp x box
spp.df = spp.bio.df %>%
  left_join(spp.len.df)%>%
  rename(Name = 'species')%>%
  left_join(select(fgs.orig, Code, Name))

hal = spp.df %>% 
  filter(Code == 'HAL')%>%
  group_by(time, polygon)%>%
  summarise(biomass = sum(biomass,na.rm=T))
ggplot(hal,aes(x= time, y = biomass))+geom_line()+facet_wrap(~polygon)+ylim(0,3E6)

fleet.df =  fleet.minsize.df %>%
  left_join(fleet.target.df)

#loop through fleet
f=1
spp.avail.ls = list()
pdf(paste0(figure.dir,run.name,'_fleet_catch_availability.pdf'), width = 12,height = 12)
for(f in 1:length(gf.fleet.names)){
  
  fleet.target = fleet.df %>% 
    filter(fleet.name == gf.fleet.names[f] & target == 1)
  
  fleet.footprint = footprint.df %>%
    filter(fleet.name == gf.fleet.names[f] & eff.flag == 1)
  
  spp.target = spp.df %>%
    filter(Code %in% fleet.target$Code)
  
  spp.bio.tot = spp.target %>%
    filter(polygon %in% fleet.footprint$polygon)%>%
    group_by(Code, time)%>%
    summarise(biomass.tot = sum(biomass,na.rm=T))
  
  spp.avail = spp.target %>%
    left_join(fleet.target)%>%
    mutate(available = ifelse(length > minsize, 1, 0))%>%
    filter(available == 1 & polygon %in% fleet.footprint$polygon)%>%
    group_by(Code, time)%>%
    summarise(biomass = sum(biomass,na.rm=T))%>%
    left_join(spp.bio.tot)%>%
    mutate(bio.avail.pct = biomass/biomass.tot,
           year = model.time[time])

   p = ggplot(spp.avail, aes( x= year, y = bio.avail.pct))+
    geom_line()+
    ylab('Biomass Availability to Fleet')+
    facet_wrap(~Code)+
     ggtitle(gf.fleet.names[f])+
     theme_bw()
   
   gridExtra::grid.arrange(p)
   
   spp.avail.ls[[f]] = spp.avail
}
dev.off()

spp.avail.df = bind_rows(spp.avail.ls)
saveRDS(spp.avail.df,paste0(figure.dir,'Data/',run.name,'_fleet_catch_availability.rds'))

#Calculate proportion of spp age distr above the minsize threshold
j=1
spp.minsize.ls = list()
for(j in 1:length(gf.fleet.names)){

  spp.target = fleet.df %>%
    filter(fleet.name == gf.fleet.names[j],target ==1)%>%
    mutate(pct.below.min = NA)
  
  fleet.spp = spp.target$Code
  
  k=1
  for(k in 1:length(fleet.spp)){
    this.minsize = spp.target$minsize[which(spp.target$Code == fleet.spp[k])]
    spp.l = spp.df %>%
      filter(Code == fleet.spp[k])%>%
      mutate(below.minsize = ifelse(length<this.minsize,1,0))
    
    spp.target$pct.below.min[which(spp.target$Code == fleet.spp[k])] = signif(sum(spp.l$below.minsize)/nrow(spp.l),2)
             
  }
  
  spp.minsize.ls[[j]] = spp.target
}
spp.minsize = bind_rows(spp.minsize.ls)
write.csv(spp.minsize,paste0(figure.dir,'Data/',run.name,'_proportion_discarded.csv'),row.names =F)

