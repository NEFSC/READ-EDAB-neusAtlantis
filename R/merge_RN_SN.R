library(readxl)
library(dplyr)

setwd(choose.dir(default=getwd())) # where run data are saved
d2=getwd()
# d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R' #where (PRM, bgm, group data) are saved
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved

setwd(d1)

# 
# #linux
d1='/home/ryan/Git/atneus_RM'
d2='/home/ryan/AtlRuns/20180510a'
setwd(d2)


t=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RNSN.xlsx', sheet='Sheet1')
c=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RNSN.xlsx', sheet='Sheet2')
v=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RNSN.xlsx', sheet='Sheet3')


colnames(c)=c('Code', 'Species')
tt=merge(t, c, by=('Species'))
colnames(v)=c('Code', 'li_a', 'li_b')
tt2=merge(tt, v, by='Code')


tt2$length=(tt2$`weight (g)`/tt2$li_a)^(1/tt2$li_b)
tt3=tt2[with(tt2, order(Code, Cohort)),]
write.csv(tt3, file='length_weight_v15.csv', col.names = T, row.names = F, sep=',')

spp=unique(tt3$Species)
pdf(nc='length_weight.pdf',paper='A4r',width=11, height=8)
for(i in 1:length(spp)){

  plot( tt3$length[which(tt3$Species==spp[i])]~tt3$Cohort[which(tt3$Species==spp[i])], type='b', ylab='length (cm)', xlab='cohort', main=spp[i],
        ylim=c(0, max(tt3$length[which(tt3$Species==spp[i])])))
  plot( tt3$`weight (kg)`[which(tt3$Species==spp[i])]~tt3$Cohort[which(tt3$Species==spp[i])], type='b', ylab='weight (kg)', xlab='cohort', main=spp[i],
        ylim=c(0, max(tt3$`weight (kg)`[which(tt3$Species==spp[i])])))
  
}
dev.off()

cr=read_xlsx('RNSN.xlsx', sheet='Sheet4')
tt=read.table('length_weight_v15.csv', header = T, sep=',')

## missing entries for MPF, BPF, FDE, FDF... look into this.


## Read in RN and SN values, xlsx updated 20180709, '____data' tab has values copied from '___calc_doc' tab, which has documentation and calculations
## (von Bertalanffy model, Atlantis calcs for RN SN weight conversions, length_weight relationships, etc.)
# x=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/length_weight_v15.xlsx', sheet='length_weight_v15_data')
x=read_xlsx(paste(d1, '/R/length_weight_v15.xlsx', sep=''), sheet='length_weight_v15_data')
x$Rname=paste(x$Species, x$Cohort, "_ResN", sep="") #make name same as variable name in netcdf file
x$Sname=paste(x$Species, x$Cohort, "_StructN", sep="") #make name same as variable name in netcdf file
# ## Subset above to use for replacement in initial conditions netcdf file
# xR=data.frame(Variables=x$Rname,RN=x$RN_mg) # this is the new calculated reserve nitrogen value
# xS=data.frame(Variables=x$Sname,SN=x$SN_mg) # this is the new calculated structural nitrogen value

### edit RN and SN values to compensate for long-lived species being at old age for cohort 1 (e.g. whales, sharks, etc)
newX=x
newX[,c('SN', 'RN', 'weight_g')]=NULL
newX[,c('weight_kg', 'weight_t', 'weight_lbs', 'length_cm')]=NULL
### weight of inidividual fish in a cohort should be mean size (age 1.5 in cohort 1 of a fish that spends 2 years in a cohort)
### The recruit weight is then that of an age 1 fish
newX$interval=apply(newX[,'numyrs'], 1, function(x) median(seq(1:x))) # use to get mean age of cohort
newX$newage=newX$age*newX$interval # this is updated mean age of cohort to use in von Bert calcs instead of below
# newX$newage=newX$age-(newX$numyrs-1) # use this for von Bertalannfy calculations, li_a li_b (offsets values to start at 1)
newX$vbert_cm2=newX$Linf*(1-exp(-newX$K*(newX$newage-newX$To)))
newX$grams2=newX$li_a*(newX$vbert_cm2^newX$li_b)
newX$inches=newX$vbert_cm2*0.393701
newX$lbs=newX$grams2*0.00220462
newX$recruit_cm=newX$Linf*(1-exp(-newX$K*(1-newX$To))) # age 1 fish
newX$recruit_grams=newX$li_a*(newX$recruit_cm^newX$li_b)

## plot length vs weight to make sure thinks look OK
nmc=unique(newX$Code)
pdf(file='weight_length_US.pdf')
for(i in 1:length(nmc)){
  ii=nmc[i]
plot(newX$inches[newX$Code==ii]~newX$lbs[newX$Code==ii], ylab='inches', xlab='lbs', main=ii)
}
dev.off()
##

### USE THESE VALUES TO REPLACE RN AND SN IN INITIAL CONDITIONS FILE ### updated 20180711
newX$SN_2=newX$grams2/20/5.7/3.65*1000 # convert grams wet weight to mg SN
newX$RN_2=newX$SN_2*2.65 # convert SN to RN (mg)
### for recruits, use in KWRR_xxx KWSS_xxx in biol file
newX$recruitSN=newX$recruit_grams /20/5.7/3.65*1000 
newX$recruitRN=newX$recruitSN *2.65 

## Subset above to use for replacement in initial conditions netcdf file
xR=data.frame(Variables=newX$Rname,RN=newX$RN_2) # this is the new calculated reserve nitrogen value
xS=data.frame(Variables=newX$Sname,SN=newX$SN_2) # this is the new calculated structural nitrogen value

### reshape and cast long to wide to get individual vertebrate weight by cohort (grams)
library(reshape)
t=newX[,c("Code", "grams2", "Cohort")]
t2=melt.data.frame(t, id.vars=c('Code', 'Cohort'), measure.vars = 'grams2')
t3=cast(t2, Code ~ Cohort)
write.table(t3, file='vertebrate_weights_grams.csv', sep=',', col.names = T, row.names = F)

t=newX[,c("Code", "Cohort")]
t$RNSN=newX$RN_2 + newX$SN_2
# t2=melt.data.frame(t, id.vars=c("Code", "Cohort")) #, measure.vars = c('RN_2', 'SN_2'))
t3=cast(t, Code ~ Cohort)
write.table(t3, file='vertebrate_sumRNSN.csv', sep=',', col.names = T, row.names = F)

## write out RN and SN by cohort
t=newX[,c("Code", "Cohort", "SN_2")]
t3=cast(t, Code ~ Cohort)
write.table(t3, file='vertebrate_SN.csv', sep=',', col.names = T, row.names = F)

### get C biomass (mg C per individual), RN+SN, convert to C; used for tuning mum and C
t=newX[,c("Code", "Cohort")]
t$biomass=(newX$RN_2 + newX$SN_2)*20*5.7
t2=melt.data.frame(t, id.vars=c('Code', 'Cohort'), measure.vars = 'biomass')
t3=cast(t2, Code ~ Cohort)
write.table(t3, file='vertebrate_biomass_mgC.csv', sep=',', col.names = T, row.names = F)


### open initial conditions nc
library(ncdf4)
# nc=nc_open('RMinit_newvalues2017.nc', write=T) # old init, old _FillValues, (used to create file below)
# nc=nc_open('20180710init.nc', write=T) # edited 20180710, new _FillValues, need to update data with _FillValues
# 
# a=data.frame(attributes(nc$var), stringsAsFactors = F) # dataframe
# aa=attributes(nc$var) #list
# print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"Netcdf attributes"))
# 
# ## this looks promising...
# # ncvar_change_missval( nc, varid, missval )
# # aa$names[1]
# # aaR=aa$names[which("_ResN" %in% aa$names)] 
# # ncvar_change_missval( nc, avarid, missval )
# 
# 
# # library(stringr)
# # str_locate_all(aa$names[2], "_ResN")
# 
# # grep("_ResN", aa$names[2]) # search 
# # grep("_StructN", aa$names[2])
# aaR=data.frame(nm=a$names[grep("_ResN", a$names)]) # split names with ResN
# aaS=data.frame(nm=a$names[grep("_StructN", a$names)]) # split names with SN
# # aaR=aa$names[which(grep("_ResN", aa$names)==1)]
# 
# aaR=data.frame(nm=a$names[grep("_ResN", a$names)]) # split names with ResN
# aaS=data.frame(nm=a$names[grep("_StructN", a$names)]) # split names with SN
# 
# aaRS=rbind(aaR, aaS)
# b=which(a$names %in% aaRS$nm)
# c=a[b,1]
# ## can manually edit the initial conditions nc like this...
# # nc$var$Anchovies10_ResN$missval=x$RN_mg[10]
# ## and with indexing like this...
# # nc$var[[2]]$missval # second variable entry of 1917
# # nc$var[[2]]$name # use to match aaR/aaS
# 
# # grep("_ResN", nc$var[[2]]$name) # search 
# # 
# # nc$var[[i]]$missval=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
# # nc$var[[i]]$missval=xS$SN[which(xS$Variables==nc$var[[i]]$name)]
# 
# ## reassign missing values for RN and SN, this works now, but need to replace values for all non vert entries below yellowtail flounder
# ## as this routine causes the _FillValue to be replaced with " " (copy and paste old vals from cdf file, then do ncgen...)
# for (i in 1:dim(a)[1]){ #1:10){
#   # print(nc$var[[i]]$name) # use to match aaR/aaS
#   testNum=grep("_Nums", nc$var[[i]]$name) # search for Nums, skip to next
#   testN=grep("_N", nc$var[[i]]$name)
#   test2=grep("_ResN", nc$var[[i]]$name) # search for RN
#   if (length(testNum) ==1){
#     next
#   } else if (length(testN) ==1){
#     next
#   }  else if (length(test2) == 1) {
#     # print(nc$var[[i]]$missval)
#     # print(xR$RN[which(xR$Variables==nc$var[[i]]$name)])
#     # nc$var[[i]]$missval=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
#     # ncvar_change_missval(nc, a[i,1],xR$RN[which(xR$Variables==nc$var[[i]]$name)]) #nc$var[[i]]$name)])
#     ncatt_put(nc,a[i,1],"_FillValue", xR$RN[which(xR$Variables==nc$var[[i]]$name)])
#     ncvar_put(nc,a[i,1], rep(xR$RN[which(xR$Variables==nc$var[[i]]$name)],150)) # replicate 5x30
#     # print(nc$var[[i]]$missval) 
#   } else if (length(test2) != 1) {
#     # print(nc$var[[i]]$missval)
#     # print(xS$SN[which(xS$Variables==nc$var[[i]]$name)])
#     # nc$var[[i]]$missval=xS$SN[which(xS$Variables==nc$var[[i]]$name)]
#     # ncvar_change_missval(nc, a[i,1],xS$SN[which(xS$Variables==nc$var[[i]]$name)])
#     ncatt_put(nc,a[i,1],"_FillValue", xS$SN[which(xS$Variables==nc$var[[i]]$name)])
#     # print(nc$var[[i]]$missval) #
#     ncvar_put(nc,a[i,1], rep(xS$SN[which(xS$Variables==nc$var[[i]]$name)],150))
#     
#   }
#   nc_sync(nc)
# }
# nc_sync(nc)
# nc_close(nc)


#### 20180711 update - used this to write data to variables for ResN and StructN
nc=nc_open('RMinit_2018.nc', write=T) # 
a=data.frame(attributes(nc$var), stringsAsFactors = F) # dataframe

aaR=data.frame(nm=a$names[grep("_ResN", a$names)], stringsAsFactors = F) # split names with ResN
aaS=data.frame(nm=a$names[grep("_StructN", a$names)], stringsAsFactors = F) # split names with SN
aaNum=data.frame(nm=a$names[grep("_Nums$", a$names)], stringsAsFactors = F) # split names with Nums
aaN=data.frame(nm=a$names[grep("_N$", a$names)], stringsAsFactors = F) # split names with _N
aaN1=data.frame(nm=a$names[grep("_N1", a$names)], stringsAsFactors = F) # split names with _N
aaN2=data.frame(nm=a$names[grep("_N2", a$names)], stringsAsFactors = F) # split names with _N

aaRS=rbind(aaR, aaS, stringsasFactors=F)
b=which(a$names %in% aaRS$nm)
c=a[b,1]

for (i in 1:length(b)){
  vv=as.numeric(b[i]) # index of ResN and StructN in entire list `a` of nc file
  testSN=grep("_StructN", a[vv,1]) 
  testRN=grep("_ResN", a[vv,1])
  
  if (length(testRN) ==1){
    ncatt_put(nc,a[vv,1],"_FillValue", xR$RN[which(xR$Variables==a[vv,1])])
    ncvar_put(nc,a[vv,1], rep(xR$RN[which(xR$Variables==a[vv,1])],150)) # replicate 5x30
  } else if (length(testSN) ==1){
    ncatt_put(nc,a[vv,1],"_FillValue", xS$SN[which(xS$Variables==a[vv,1])])
    ncvar_put(nc,a[vv,1], rep(xS$SN[which(xS$Variables==a[vv,1])],150))
  }
  nc_sync(nc)
}
nc_sync(nc)
nc_close(nc)

# then in terminal:
# ncatted -O -a _FillValue,,d,, RMinit_2018.nc RMinitnofill_2018.nc



### check new values in RM_NEUS.r initial conditions
library(shinyrAtlantis)
library(tidyverse)
library(stringr)
library(rbgm)
library(bgmfiles)
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
bgm.file <- ("neus_tmerc_RM.bgm")
# NEUS_15_init=make.sh.init.object(bgm.file, '20180710init2.nc') #'RMinit_newvalues2017.nc')
NEUS_15_init=make.sh.init.object(bgm.file, 'RMinit_2018.nc') #'RMinit_newvalues2017.nc')
sh.init(NEUS_15_init)
newN=NEUS_15_init$df.nitrogen
write.csv(t, file='benthic_species_N.csv', sep=',', col.names = T, row.names = F)

# t2=NEUS_15_init$species.3.data
# t=t(t2)
# colnames(t)=NEUS_15_init$species.2.names.full
# write.csv(t, file='benthic_species_N.csv', sep=',', col.names = T, row.names = F)
