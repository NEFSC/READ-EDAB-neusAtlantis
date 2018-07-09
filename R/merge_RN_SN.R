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
write.csv(tt3, file='length_weight_v15.csv', col.names = T, row.names = F, sep=',')

spp=unique(tt3$Species)
pdf(file='length_weight.pdf',paper='A4r',width=11, height=8)
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


# Read in new updated values 20180709
x=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/length_weight_v15.xlsx', sheet='length_weight_v15_data')

# library(makecdfenv)
# cdf=read.cdffile('RMinit_newvalues2017.cdf',compress=F)

library(ncdf4)
cdf=nc_open('RMinit_newvalues2017.nc', write=T)
a=data.frame(attributes(cdf$var))
aa=attributes(cdf$var)

print(paste("The file has",cdf$nvars,"variables,",cdf$ndims,"dimensions and",cdf$natts,"NetCDF attributes"))

## this looks promising...
# ncvar_change_missval( nc, varid, missval )
aa$names[1]
# aaR=aa$names[which("_ResN" %in% aa$names)] 
ncvar_change_missval( cdf, avarid, missval )


library(stringr)
str_locate_all(aa$names[2], "_ResN")

grep("_ResN", aa$names[2])
grep("_StructN", aa$names[2])
aaR=data.frame(nm=a$names[grep("_ResN", a$names)]) # split names with ResN

# aaR=aa$names[which(grep("_ResN", aa$names)==1)]
