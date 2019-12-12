library(readxl)
library("atlantistools")
library("dplyr")
library("rfishbase")

d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved
d2='E:/AtlantisRun/20161103/tes/20190404dta'

fgs="C:/Users/ryan.morse/Documents/GitHub/atneus_RM/NeusGroups_v15_unix_RM.csv"
fgs2 <- load_fgs(fgs)
fgs2=fgs2[,c('Code', 'LongName')]

nm=read_xlsx(paste(d1,'/R/length_weight_params.xlsx', sep=''), sheet='names', col_names=F)
nm2=nm[complete.cases(nm),]
species=unlist(nm2[,3])
spp=as.character(species)

fish=validate_names(spp)
lw=length_weight(species_list = fish)
test=lw[,c('SpecCode', 'Species', 'a', 'b')]
t=aggregate(.~Species, test, mean, na.rm=T)

# get maturity data for habitat mapping (includes ALL fish - need to sort for region - locality?)
loc=unique(xx$Locality)
spp=gnms$spp
spp=unlist(spp)
spp=spp[2:14]
library(Hmisc)
spp=tolower(spp)
spp=capitalize(spp)
validate_names(spp)
xx=maturity(spp)
loc=unique(xx$Locality) # locations
t2=aggregate(Lm~Species, xx, mean, na.rm=T)
t2sd=aggregate(Lm~Species, xx, sd, na.rm=T)
t2med=aggregate(Lm~Species, xx, median.default, na.rm=T)
t2min=aggregate(Lm~Species, xx, min, na.rm=T)
t2max=aggregate(Lm~Species, xx, max, na.rm=T)
t2$sd=t2sd$Lm
t2$med=t2med$Lm
t2$min=t4$Lm
t2$max=t5$Lm
t2ra=t2max$Lm-t2min$Lm
t2geom=aggregate(Lm~Species, xx, function(x) exp(mean(log(x))))
t2$geoMean=t2geom$Lm

