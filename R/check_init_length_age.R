#Check that the initial conditions length-at-age distribution matches what is in the reference data

library(dplyr)
library(ncdf4)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

age.key = data.frame(variable = paste0('X',1:10),agecl = 1:10)

length.age.ref = read.csv(here::here('currentVersion','vertebrate_init_length_cm.csv')) %>%
  select(Long.Name:X10)%>%
  reshape2::melt(id.vars = 'Long.Name')%>%
  left_join(age.key)%>%
  rename(species = 'Long.Name',
         length.ref = 'value')%>%
  select(species,agecl,length.ref,-variable)%>%
  arrange(species,agecl,length.ref)
  

run.name = 'New_LengthAge_Init'
run.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/Post_Processed/Data/')

#Read in length-age data from run and filter initial conditions (same as init.nc)
length.age.init = readRDS(paste0(run.dir,'length_age.rds'))%>% filter(time == 0)

#Combine and plot
length.age = length.age.init %>% 
  rename('length.age.init' = 'atoutput')%>%
  left_join(length.age.ref)%>%
  select(-time)%>%
  mutate(diff = ifelse( length.age.init < length.ref*1.05 & length.age.init > length.ref*0.95,0,1))

spp.names = unique(length.age$species)

pdf(file =  paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/Post_Processed/Length_Age_Init_Diagnostic.pdf'),width = 8, height = 5)
for(i in 1:length(spp.names)){
  
  length.age.spp = length.age %>% filter(species == spp.names[i])
  
  y.range = range(c(length.age.spp$length.age.init, length.age.spp$length.ref),na.rm=T)
  
  plot(length.ref~agecl,data = length.age.spp, type = 'p',col = 2, pch = 17, cex = 2, ylim = c(y.range[1]*0.8, y.range[2]*1.2),xlab = 'Age Class',ylab = 'Length cm', main = spp.names[i])
  points(length.age.init~agecl,data = length.age.spp, col = 3, pch = 16, cex = 2)
  
} 

dev.off()


#Make length-age initial conditions in terms of RN and SN

#Read in li_a and li_b
bio.lines = readLines(here::here('currentVersion','at_biology.prm'))
li_a.lines = grep('li_a_',bio.lines,value = T)
li_b.lines = grep('li_b_',bio.lines,value = T)
li_a.vals = sapply(li_a.lines,function(x) return(as.numeric(strsplit(x,'\t| ')[[1]][2])),USE.NAMES = F)
li_a.names = sapply(li_a.lines,function(x) return(strsplit(x,'_|\t| ')[[1]][3]),USE.NAMES = F)
li_b.vals = sapply(li_b.lines,function(x) return(as.numeric(strsplit(x,'\t| ')[[1]][2])),USE.NAMES = F)
li_b.names = sapply(li_b.lines,function(x) return(strsplit(x,'_|\t| ')[[1]][3]),USE.NAMES = F)
a.df = data.frame(Code = li_a.names,li_a = li_a.vals)
b.df = data.frame(Code = li_b.names, li_b = li_b.vals)

nochange = c('Atlantic menhaden','Baleen whales','Butterfish','Marine turtles','Migratory mesopelagic fish','Miscellaneous demersal fish','Other benthopelagic fish',
             'Other flatfish','Right whales','Shallow demersal fish','Silver hake','Small toothed whales','Toothed whales','White hake','Yellowfail flounder')

length.age.init.new = length.age.ref %>%
  left_join(select(fgs,Code,LongName,Name), by = c('species' = 'LongName'))%>%
  left_join(a.df)%>%
  left_join(b.df)%>%
  mutate(
    wgt = li_a * length.ref^li_b,
    tot.N =  (wgt * 1000)/(5.7*20),
    RN = 2.65 * tot.N / 3.65,
    SN = tot.N / 3.65
  )%>%
  left_join(dplyr::select(length.age,species,agecl,diff))%>%
  filter(diff==1 & !(species %in% nochange))%>%
  tidyr::unite(nc.name,c('Name','agecl'),sep = '')%>%
  select(nc.name,Code,RN,SN)

#Loop through species/agecl and edit init.nc fill attributes
init.nc = nc_open(here::here('currentVersion','neus_init.nc'),write = T)

for(i in 1:nrow(length.age.init.new)){
  
  rn.name = paste0(length.age.init.new$nc.name[i],'_ResN')
  sn.name = paste0(length.age.init.new$nc.name[i],'_StructN')
  
  # ncatt_get(init.nc,rn.name,'_FillValue')$value
  ncatt_put(init.nc,rn.name,'_FillValue',round(length.age.init.new$RN[i],2),prec = 'double',verbose = T)
  # ncatt_get(init.nc,sn.name,'_FillValue')$value
  ncatt_put(init.nc,sn.name,'_FillValue',round(length.age.init.new$SN[i],2),verbose = F)
  
}

nc_close(init.nc)

# write.csv(length.age.init.new, file = here::here('diagnostics','Initial_RN_SN_from_reference_length_age.csv'),row.names = F)
