library(readxl)
library(dplyr)

setwd(choose.dir(default=getwd())) # where run data are saved
d2=getwd()
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R' #where (PRM, bgm, group data) are saved
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
write.csv(tt3, nc='length_weight_v15.csv', col.names = T, row.names = F, sep=',')

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
x=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/length_weight_v15.xlsx', sheet='length_weight_v15_data')
x$Rname=paste(x$Species, x$Cohort, "_ResN", sep="") #make name same as variable name in netcdf nc
x$Sname=paste(x$Species, x$Cohort, "_StructN", sep="") #make name same as variable name in netcdf nc
## Subset above to use for replacement in initial conditions netcdf nc
xR=data.frame(Variables=x$Rname,RN=x$RN_mg) # this is the new calculated reserve nitrogen value
xS=data.frame(Variables=x$Sname,SN=x$SN_mg) # this is the new calculated structural nitrogen value

### open initial conditions nc
library(ncdf4)
nc=nc_open('RMinit_newvalues2017.nc', write=T)
a=data.frame(attributes(nc$var)) # dataframe
aa=attributes(nc$var) #list
print(paste("The nc has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"Netnc attributes"))

## this looks promising...
# ncvar_change_missval( nc, varid, missval )
# aa$names[1]
# aaR=aa$names[which("_ResN" %in% aa$names)] 
# ncvar_change_missval( nc, avarid, missval )


# library(stringr)
# str_locate_all(aa$names[2], "_ResN")

# grep("_ResN", aa$names[2]) # search 
# grep("_StructN", aa$names[2])
aaR=data.frame(nm=a$names[grep("_ResN", a$names)]) # split names with ResN
aaS=data.frame(nm=a$names[grep("_StructN", a$names)]) # split names with SN
# aaR=aa$names[which(grep("_ResN", aa$names)==1)]

## can manually edit the initial conditions nc like this...
# nc$var$Anchovies10_ResN$missval=x$RN_mg[10]
## and with indexing like this...
# nc$var[[2]]$missval # second variable entry of 1917
# nc$var[[2]]$name # use to match aaR/aaS

# grep("_ResN", nc$var[[2]]$name) # search 
# 
# nc$var[[i]]$missval=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
# nc$var[[i]]$missval=xS$SN[which(xS$Variables==nc$var[[i]]$name)]

## reassign missing values for RN and SN
for (i in 1:dim(a)[1]){ #1:10){
# print(nc$var[[i]]$name) # use to match aaR/aaS
test=grep("_Nums", nc$var[[i]]$name) # search for Nums, skip to next
if (length(test) ==1){
  next
}
test2=grep("_ResN", nc$var[[i]]$name) # search for RN
if (length(test2) == 1) {
  # print(nc$var[[i]]$missval) #=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
  # print(xR$RN[which(xR$Variables==nc$var[[i]]$name)])
  nc$var[[i]]$missval=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
  # print(nc$var[[i]]$missval) #=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
  
} else if (length(test2) != 1) {
  # print(nc$var[[i]]$missval)
  # print(xS$SN[which(xS$Variables==nc$var[[i]]$name)])
  nc$var[[i]]$missval=xS$SN[which(xS$Variables==nc$var[[i]]$name)]
  # print(nc$var[[i]]$missval) #=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
  
}
}
nc_sync(nc)
nc_close(nc)
