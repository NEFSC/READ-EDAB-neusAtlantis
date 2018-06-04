library(readxl)
library(gplots)
library(stringr)
# setwd("/media/ryan/TOSHIBA EXT/1 RM/10 ATLANTIS transfer/") # linux
# setwd("I:/1 RM/10 ATLANTIS transfer/") #win
d1='/home/ryan/Git/atneus_RM/R'
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R' #where (PRM, bgm, group data) are saved
setwd(d1)
# d1=getwd()

# Read original pPrey data from Gavin
# diet=read.csv('atneus_diet_RM.csv', header = F)
diet=read.csv('C:/Users/ryan.morse/Desktop/NEUS Atl files/10 Atlantis transfer files/atneus_diet_RM_edited.csv', header = F) # 20170424 added stages to shrimps and squid
diet2=diet[complete.cases(diet[,2]),] # drops blank rows between entries
diet.nms=diet2[!complete.cases(diet2[,3]),] # drops data
diet.data=diet2[complete.cases(diet2),]
d=diet.data
table(floor(log10(d[,2])))
d[,1]=as.numeric(as.character(d[,1]))
d2=as.matrix(d)
'C:/Users/ryan.morse/Desktop/NEUS Atl files/0 Atlantis transfer files'

#plot min and max
heatmap.2(d2, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = NA)
cmax=apply(d2, 1, function(x) max(x, na.rm=T))
plot(cmax, main='original max')
d2.test=d2
d2.test[d2==0]=NA
cmin=apply(d2.test, 1, function(x) min(x, na.rm=T))
plot(cmin, main='original min')


### raise smallest values in d to something more reasonable (scale is from 0-1)
for (i in 1:length(d)){
# print(paste('old',i, sep=' '))
# print(table(floor(log10(d[,i]))))
d[which(floor(log10(d[,i]))==-15),i]=d[which(floor(log10(d[,i]))==-15),i]*1e12 # scale to e-3
d[which(floor(log10(d[,i]))==-14),i]=d[which(floor(log10(d[,i]))==-14),i]*1e11 # scale to e-3
d[which(floor(log10(d[,i]))==-13),i]=d[which(floor(log10(d[,i]))==-13),i]*1e10 # scale tp e-3
d[which(floor(log10(d[,i]))==-12),i]=d[which(floor(log10(d[,i]))==-12),i]*1e9 # scale to e-3
d[which(floor(log10(d[,i]))==-11),i]=d[which(floor(log10(d[,i]))==-11),i]*1e8 # scale to e-3
d[which(floor(log10(d[,i]))==-10),i]=d[which(floor(log10(d[,i]))==-10),i]*1e7 # scale to e-3
d[which(floor(log10(d[,i]))==-9),i]=d[which(floor(log10(d[,i]))==-9),i]*1e7 # scale to e-2
# print(paste('new', i, sep=' '))
# print(table(floor(log10(d[,i]))))
}
# plot min and max after raising low values
d.mat=as.matrix(d)
heatmap.2(d.mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = NA)
cmax=apply(d, 1, function(x) max(x, na.rm=T))
plot(cmax, main='raised vals max')
d.test=d
d.test[d==0]=NA
cmin=apply(d.test, 1, function(x) min(x, na.rm=T))
plot(cmin, main='raised vals min')


### Scale original d2 ala Rob
d3=(d2/max(d2, na.rm=T))*0.9
d3.mat=as.matrix(d3)
heatmap.2(d3.mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = NA)
cmax=apply(d3, 1, function(x) max(x, na.rm=T))
plot(cmax, main='scaled rob, max')
d.test=d3
d.test[d3==0]=NA
cmin=apply(d.test, 1, function(x) min(x, na.rm=T))
plot(cmin, main='scaled rob, min')


### scale d2 version 2
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
d4=range01(d2)
d4.mat=as.matrix(d4)
heatmap.2(d4.mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = NA)
cmax=apply(d4, 1, function(x) max(x, na.rm=T))
plot(cmax, main='scaled func max')
d.test=d4
d.test[d4==0]=NA
cmin=apply(d.test, 1, function(x) min(x, na.rm=T))
plot(cmin, main='scaled func min')


### Scale raised vals d ala Rob
d5=(d/max(d, na.rm=T))*0.9
d5.mat=as.matrix(d5)
heatmap.2(d5.mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = NA)
cmax=apply(d5, 1, function(x) max(x, na.rm=T))
plot(cmax, main='raised scaled rob, max')
d.test=d5
d.test[d5==0]=NA
cmin=apply(d.test, 1, function(x) min(x, na.rm=T))
plot(cmin, main='raised scaled rob, min')


hist(as.matrix(d1.0), ylim=c(0,500), xlim=c(0,1),main='original v 1.0')
hist(as.matrix(d2), ylim=c(0,500), xlim=c(0,1), main='original v1.5')
hist(as.matrix(d), ylim=c(0,500), xlim=c(0,1),main='raised')
hist(as.matrix(d3), ylim=c(0,500), xlim=c(0,1),main='scaled 0.9')
hist(as.matrix(d5), ylim=c(0,500), xlim=c(0,1),main='raised and scaled 0.9')
hist(as.matrix(d4), ylim=c(0,500), xlim=c(0,1),main='scaled 1')


### write output for NEUS, paste into biol.prm


nms=as.character(diet.nms[,1])
size=as.numeric(diet.nms[,2])
diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
diet3[rownames(diet.nms),1]=nms
diet3[rownames(diet.nms),2]=size
diet3[rownames(d),]=d5
write.table(diet3, file='raised_scaled_diet_2_RM.csv',row.names=F, sep=",")


# 3/31/17 Read in raised and scaled pPrey from result of this file and scaling to rowmax in excel
# 'v15_pPrey_scaled_min_0_3_20170331.csv'
file=list.files(path=d1, pattern='.csv')
data=file[13]
d6=read.csv(file.path(d1, data))
nms=as.character(diet.nms[,1])
size=as.numeric(diet.nms[,2])
diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
diet3[rownames(diet.nms),1]=nms
diet3[rownames(diet.nms),2]=size
diet3[rownames(d),]=d6[,3:94]
write.table(diet3, file='20170331_scaled_pPrey_RM.csv',row.names=F, sep=",")

### check sorting to make sure these are same as original file for indexing!
newP=read_excel('C:/Users/ryan.morse/Dropbox/editing_v15_pPrey.xlsx', sheet='20170413dt2', col_names = T)
newP=read_excel('C:/Users/ryan.morse/Dropbox/v15_pPrey_edited to Link_v10.xlsx', sheet='20170424', col_names = T)
newP=read_excel('/home/ryan/Dropbox/editing_v15_pPrey.xlsx', sheet='20170406', col_names = T)
rownames(newP)=newP[,1]
newP[,1]=NULL
newP2=newP[order(rownames(newP)),]
P=data.frame(lapply(newP2, function(y) if(is.numeric(y)) round(y, 4) else y)) 
diet.nms2=diet.nms[order(diet.nms$V1),]
nms=as.character(diet.nms2[,1])
size=as.numeric(diet.nms2[,2])
# diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
diet3=as.data.frame(matrix(NA,nrow=779, ncol=92))

diet3[rownames(diet.nms),1]=nms
diet3[rownames(diet.nms),2]=size
diet3[rownames(d),]=P
write.table(diet3, file='20170424_from_JLink_pPrey_RM.csv',row.names=F, col.names=F, sep=",")



# x=list()
# for (i in 2:length(d)){
#   x[[i]]=(table(floor(log10(d[,i]))))
# }

write.csv(diet3, file='pPrey_scale2v1_20170406.csv')

diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
diet3[rownames(diet.nms),]=diet.nms
diet3[rownames(d),]=d

### _________________________
# NEUS v1.0
diet_v1.0=read.csv('I:/1 RM/10 ATLANTIS transfer/atneus_diet_v1_0_gamble.csv', header=F)
diet_v1.0_2=diet_v1.0[complete.cases(diet_v1.0[,2]),] # drops blank rows between entries
diet_v1.nms=diet_v1.0_2[!complete.cases(diet_v1.0_2[,3]),] # drops data
diet_v1.data=diet_v1.0_2[complete.cases(diet_v1.0_2),]
d1.0=diet_v1.data
d1.0[,1]=as.numeric(as.character(d1.0[,1]))
table(floor(log10(d1.0[,1])))
# nums <- sapply(diet_v1.data, is.numeric)

hist(as.matrix(d1.0))
cmax=apply(d1.0, 1, function(x) max(x, na.rm=T))
plot(cmax)

hist(as.matrix(d1.0), ylim=c(0,500), xlim=c(0,1),main='original v 1.0')
d1.0.mat=as.matrix(d1.0)
heatmap.2(d1.0.mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = NA)


#### mod V1 treatment 20180516
# p.neus.num=read_excel('pPrey_workbook.xlsx', sheet='20170517prm_20180213', col_names = T, trim_ws = T, col_types = 'numeric')
v1.num=read_excel('atneus_diet_v1_0_gamble.xlsx', sheet='atneus_diet_v1_0_gamble', col_names = T, trim_ws = T, col_types = 'numeric')
# p.neus.data=p.neus.num[complete.cases(p.neus.num[,3]),] #data only
v1.data=v1.num[complete.cases(v1.num[,3]),] # drops blank rows between entries
# p.neus=read_excel('pPrey_workbook.xlsx', sheet='20170517prm_20180213', col_names = T, trim_ws = T) # keep character 1st col
v1.neus=read_excel('atneus_diet_v1_0_gamble.xlsx', sheet='atneus_diet_v1_0_gamble', col_names = T, trim_ws = T)
# p.neus2=p.neus[complete.cases(p.neus[,2]),] # drops blank rows between entries
v1.neus2=v1.neus[complete.cases(v1.neus[,2]),]
# p.neus.nms=p.neus2[which(is.na(p.neus2[,3])),1:2] # drops data, keeps names
v1.nms=v1.neus2[which(is.na(v1.neus2[,3])),1:2]
# pnms=data.frame(p.neus.nms)
v1.nms=data.frame(v1.nms)

### Split names and reorder...
test=gsub("(.{5})", "\\1 ", v1.nms[,1]) #split after 5 chars (drop pPREY)
test2=strsplit(test, ' ') #split on space
r=sapply(test2, "[[", 2) #keep second group to drop pPREY
v1.nms$nm=r
v1.nms$name=(str_extract(v1.nms$nm, "[aA-zz]+")) # just names, drop numbers
v1.nms$type=as.numeric(gsub("[^0-9]", "", v1.nms$nm))  #just numbers, drop letters

#### now order by name and type (1xxx1, 1xxx2, 2xxx1, 2xxx2)
v1.ordrd.data=v1.data[order(v1.nms$name, v1.nms$type, na.last=T),]
v1.ordrd.nms=v1.nms[order(v1.nms$name, v1.nms$type, na.last=T),]
v1.ordrd.nms$intx=NA
v1.ordrd.nms$intx[which(v1.ordrd.nms$type==11)]='Juv Pred - Juv Prey'
v1.ordrd.nms$intx[which(v1.ordrd.nms$type==12)]='Juv Pred - Adt Prey'
v1.ordrd.nms$intx[which(v1.ordrd.nms$type==21)]='Adt Pred - Juv Prey'
v1.ordrd.nms$intx[which(v1.ordrd.nms$type==22)]='Adt Pred - Adt Prey'
v1.ordrd.nms$intx[which(is.na(v1.ordrd.nms$type))]='Invert Pred'

d=as.data.frame(v1.ordrd.data)
colnames(d)=c('FPL','FPO','FPS','FVD','FVV','FVS','FVB','FVT','FVO','FMM','FMN','FBP','FDD',
              'FDE','FDS','FDM','FDP','FDB','FDC','FDO','FDF','SHB','SHD','SHC','SHP','SHR',
              'SSK','SB','SP','PIN','REP','WHB','WHS','WHT','WDG','CEP','BFS','BFF','BFD','BG',
              'BMD','BML','BMS','PWN','ZL','BD','MA','MB','SG','BC','ZG','PL','DF','PS','ZM','ZS',
              'PB','BB','BO','DL','DR','DC','Dlsed','Drsed','Dcsed','jCEP','jPWN')

wd3='C:/Users/ryan.morse/Desktop/NEUS Atl files'
filename=paste('pPrey_v1_values_20180515',".pdf", sep="")
mypath=file.path(wd3, filename)
pdf(file=mypath)
for(i in 1:length(v1.ordrd.nms$name)){
  if(length(as.numeric(d[i,which(d[i,]>0)]))==0){
    next
  }
  barplot(as.numeric(d[i,which(d[i,]>0)]), names.arg=colnames(d[i,which(d[i,]>0)]), main=paste(v1.ordrd.nms[i,c('name', 'intx')]), las=2,
          col=ifelse(is.na(v1.ordrd.nms$type[match(colnames(d[i,which(d[i,]>0)]), v1.ordrd.nms$name)]), 'red', 'gray'))
}
dev.off()


# colnames(d[i,which(d[i,]>0)])
# any(is.na(pnms$type[which(pnms$name=='BC')]))
# is.na(ordrd.nms$type[match(colnames(d[i,which(d[i,]>0)]), ordrd.nms$name)])

write.csv(d, file='pPrey_v1_ordrd_data_20180516.csv', row.names = F, col.names = T)
write.csv(v1.ordrd.nms, file='pPrey_v1_ordrd_nms_20180516.csv', row.names = F, col.names = T)

v1.ordrd.data=read.csv('pPrey_v1_ordrd_data_20180516.csv')
v1.ordrd.nms=read.csv('pPrey_v1_ordrd_nms_20180516.csv')



nmsX=as.character(p.neus[,1])
sizeX=as.numeric(diet.nms2[,2])
# diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
diet3=as.data.frame(matrix(NA,nrow=779, ncol=92))

diet3[rownames(diet.nms),1]=nms
diet3[rownames(diet.nms),2]=size
diet3[rownames(d),]=P
write.table(diet3, file='20170424_from_JLink_pPrey_RM.csv',row.names=F, col.names=F, sep=",")


###________________________________________________________________________________________
# read in original, non-scaled
# sorted v1.0 and v1.5 pPrey spreadsheets for better comparison of lifestage with heatmaps
# sorted by name alphabetically, juvenile vertebrates at top, adult verts below )equal number
# inverts at bottom (only 1 age)


v1.0=read.csv('NEUS_v10_sorted_pPrey.csv', header=T)
v1.0_data=v1.0[1:159,5:71]
v1.0_data[,1]=as.numeric(as.character(v1.0_data[,1]))
v10mat=as.matrix(v1.0_data)
heatmap.2(v10mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = 'v1.0', rowsep = c(70, 140)) #juv 1-70, adlt 71-140, nvrt 142-159


v1.5=read.csv('NEUS_v15_sorted_pPrey.csv', header=T)
v1.5_data=v1.5[1:256,5:96]
v1.5_data[,1]=as.numeric(as.character(v1.5_data[,1]))
v15mat=as.matrix(v1.5_data)
heatmap.2(v15mat, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75,
          key.title = 'v1.5', rowsep = c(118,236)) #juv 1-118, adlt 119-236, invert 237-256


#### READ in pPrey from at_biol_...20170517_lowMUM.prm (mod 2018/2/13)
### note this file only has 1 entry for 1 stage (of 2) for ISQ LSQ NSH OSH
# p.neus.num=read_excel('pPrey_workbook.xlsx', sheet='20170517prm_20180213', col_names = T, trim_ws = T, col_types = 'numeric')
p.neus.num=read_excel('pPrey_workbook.xlsx', sheet='at_biol_20180419', col_names = T, trim_ws = T, col_types = 'numeric') #updated for inverts w/ age
p.neus.data=p.neus.num[complete.cases(p.neus.num[,3]),] #data only
# p.neus=read_excel('pPrey_workbook.xlsx', sheet='20170517prm_20180213', col_names = T, trim_ws = T) # keep character 1st col
p.neus=read_excel('pPrey_workbook.xlsx', sheet='at_biol_20180419', col_names = T, trim_ws = T) # updated for inverts with age structure
p.neus2=p.neus[complete.cases(p.neus[,2]),] # drops blank rows between entries
p.neus.nms=p.neus2[which(is.na(p.neus2[,3])),1:2] # drops data, keeps names
pnms=data.frame(p.neus.nms)
cnames=read_excel('pPrey_workbook.xlsx', sheet='colnames', col_names = F) # get column names for pPrey matrix
cnames=data.frame(cnames)

d=p.neus.data
table(floor(log10(d[,2])))
# d[,1]=as.numeric(as.character(d[,1]))
d2=as.matrix(d)
row.names(d2)=pnms[,1]
# colnames(d2)=cnames[,1]
# pdf(file='neus_pPrey_heatmap_20170517.pdf', paper='USr') #margins too small
# heatmap.2(d2, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75, key.title = 'v1.5')
# dev.off()
d2.min=apply(d2, 1, FUN=function(x) {min(x[x > 0])}) #range) # get range for all columns
d2.max=apply(d2, 1, FUN=function(x) {max(x)}) #range) # get range for all columns
table(d2.min)
table(d2.max)
# d2.rng=apply(d2, 1, range)
# table(d2.rng[2,]) #max value


### Split names and reorder...
test=gsub("(.{5})", "\\1 ", pnms[,1]) #split after 5 chars (drop pPREY)
test2=strsplit(test, ' ') #split on space
r=sapply(test2, "[[", 2) #keep second group to drop pPREY
pnms$nm=r
pnms$name=(str_extract(pnms$nm, "[aA-zz]+")) # just names, drop numbers
pnms$type=as.numeric(gsub("[^0-9]", "", pnms$nm))  #just numbers, drop letters

# now order by name and type (1xxx1, 1xxx2, 2xxx1, 2xxx2)
ordrd.data=p.neus.data[order(pnms$name, pnms$type, na.last=T),]
ordrd.nms=pnms[order(pnms$name, pnms$type, na.last=T),]
ordrd.nms$intx=NA
ordrd.nms$intx[which(ordrd.nms$type==11)]='Juv Pred - Juv Prey'
ordrd.nms$intx[which(ordrd.nms$type==12)]='Juv Pred - Adt Prey'
ordrd.nms$intx[which(ordrd.nms$type==21)]='Adt Pred - Juv Prey'
ordrd.nms$intx[which(ordrd.nms$type==22)]='Adt Pred - Adt Prey'
ordrd.nms$intx[which(is.na(ordrd.nms$type))]='Invert Pred'


### NOW plot ordered diet contribution; highlight inverts in red
d=as.data.frame(ordrd.data)
wd3='C:/Users/ryan.morse/Desktop/NEUS Atl files'
filename=paste('pPrey_VAlues_20180515',".pdf", sep="")
mypath=file.path(wd3, filename)
pdf(file=mypath)
for(i in 1:length(ordrd.nms$name)){
  barplot(as.numeric(d[i,which(d[i,]>0)]), names.arg=colnames(d[i,which(d[i,]>0)]), main=paste(ordrd.nms[i,c('name', 'intx')]), las=2,
          col=ifelse(is.na(ordrd.nms$type[match(colnames(d[i,which(d[i,]>0)]), ordrd.nms$name)]), 'red', 'gray'))
}
dev.off()


# colnames(d[i,which(d[i,]>0)])
# any(is.na(pnms$type[which(pnms$name=='BC')]))
# is.na(ordrd.nms$type[match(colnames(d[i,which(d[i,]>0)]), ordrd.nms$name)])

write.csv(ordrd.data, file='pPrey_ordrd_data_20180516.csv', row.names = F, col.names = T)
write.csv(ordrd.nms, file='pPrey_ordrd_nms_20180516.csv', row.names = F, col.names = T)
### read in saved data
ordrd.data=read.csv('pPrey_ordrd_data_20180516.csv')
ordrd.nms=read.csv('pPrey_ordrd_nms_20180516.csv')



### read in v15 pPrey edited @ NEUS v1 Gamble version
gamble.p.neus.num=read_excel('pPrey_workbook.xlsx', sheet='20180419_@Gamble_final', col_names = T, trim_ws = T, col_types = 'numeric')
gamble.p.neus.data=gamble.p.neus.num[complete.cases(gamble.p.neus.num[,3]),] #data only
gamble.p.neus=read_excel('pPrey_workbook.xlsx', sheet='20180419_@Gamble_final', col_names = T, trim_ws = T) # keep character 1st col
gamble.p.neus2=gamble.p.neus[complete.cases(gamble.p.neus[,2]),] # drops blank rows between entries
gamble.p.neus.nms=gamble.p.neus2[which(is.na(gamble.p.neus2[,3])),1:2] # drops data, keeps names
pnms=data.frame(gamble.p.neus.nms)

g.ordrd.data=gamble.p.neus.data[order(pnms$name, pnms$type, na.last=T),]
g.ordrd.nms=pnms[order(pnms$name, pnms$type, na.last=T),]
g.ordrd.nms$intx=NA
g.ordrd.nms$intx[which(g.ordrd.nms$type==11)]='Juv Pred - Juv Prey'
g.ordrd.nms$intx[which(g.ordrd.nms$type==12)]='Juv Pred - Adt Prey'
g.ordrd.nms$intx[which(g.ordrd.nms$type==21)]='Adt Pred - Juv Prey'
g.ordrd.nms$intx[which(g.ordrd.nms$type==22)]='Adt Pred - Adt Prey'
g.ordrd.nms$intx[which(is.na(g.ordrd.nms$type))]='Invert Pred'




# A=as.matrix(gamble.p.neus.data[,2:93])
A=as.matrix(g.ordrd.data[,2:93]) #20180419@Gamble ordered
B=as.matrix(ordrd.data[,1:92]) #20180419
C=pmax(A,B) # select largest value from Gamble/v15 20180419 pPrey

setwd('C:/Users/ryan.morse/Documents')
png(filename='NEUS_v15_edited_At_Gamble.png', width=1440, height=960)
heatmap.2(A, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75, key.title = NA) # Gamble v1 -> v15
dev.off()
png(filename='NEUS_v15_20180419.png', width=1440, height=960)
heatmap.2(B, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75, key.title = NA) # v15
dev.off()
png(filename='NEUS_v15_merged_Gamble20180419.png', width=1440, height=960)
heatmap.2(C, Rowv=NULL, Colv = NULL, dendrogram = 'none', trace='none', keysize=0.75, key.title = NA) # merge on max (v15, Gamble)
dev.off()

### read in code relations file
# cnvrt=read_excel('coderelations.xls', col_names = T, trim_ws = T, col_types = 'numeric')
cnvrt=read.csv('coderelations.csv')
xx=cnvrt$Parent[order(cnvrt$Parent_index)] # factor
xx=as.vector(cnvrt$Parent[order(cnvrt$Parent_index)]) #vector

yy=as.vector(ordrd.nms$name)
# yy=as.vector(v1.ordrd.nms$name)

### this works
map=setNames(as.vector(cnvrt$Parent), as.vector(cnvrt$Child))
yy3=map[yy]
test=data.frame(yy3, yy)

test$v15_intx=ordrd.nms$intx
test$v15_type=ordrd.nms$type
#__________________________________________________________________________________________________________
#### UPDATE 5/24/2018 newest biol file pPREY matrix with mods for labile detritus and LTL
#### read in most recent diet copied from biol file 20180524
#### READ in pPrey from at_biol_...20180419.prm with 4 entries for inverts with multiple stages 1xxx1, 1xxx2, 2xxx1, 2xxx2
p.neus.num=read_excel('pPrey_workbook.xlsx', sheet='at_biol_20180419', col_names = T, trim_ws = T, col_types = 'numeric')
p.neus.data=p.neus.num[complete.cases(p.neus.num[,3]),] #data only
p.neus=read_excel('pPrey_workbook.xlsx', sheet='at_biol_20180419', col_names = T, trim_ws = T) # keep character 1st col

p.neus2=p.neus[complete.cases(p.neus[,2]),] # drops blank rows between entries
p.neus.nms=p.neus2[which(is.na(p.neus2[,3])),] # drops data, keeps names

pnms=data.frame(p.neus.nms)
cnames=read_excel('pPrey_workbook.xlsx', sheet='colnames', col_names = F) # get column names for pPrey matrix
cnames=data.frame(cnames)
test=gsub("(.{5})", "\\1 ", pnms[,1]) #split after 5 chars (drop pPREY)
test2=strsplit(test, ' ') #split on space
r=sapply(test2, "[[", 2) #keep second group to drop pPREY
pnms$nm=r
pnms$name=(str_extract(pnms$nm, "[aA-zz]+")) # just names, drop numbers
pnms$type=as.numeric(gsub("[^0-9]", "", pnms$nm))  #just numbers, drop letters

# now order by name and type (1xxx1, 1xxx2, 2xxx1, 2xxx2)
ordrd.data=p.neus.data[order(pnms$name, pnms$type, na.last=T),]
ordrd.nms=pnms[order(pnms$name, pnms$type, na.last=T),]
ordrd.nms$intx=NA
ordrd.nms$intx[which(ordrd.nms$type==11)]='Juv Pred - Juv Prey'
ordrd.nms$intx[which(ordrd.nms$type==12)]='Juv Pred - Adt Prey'
ordrd.nms$intx[which(ordrd.nms$type==21)]='Adt Pred - Juv Prey'
ordrd.nms$intx[which(ordrd.nms$type==22)]='Adt Pred - Adt Prey'
ordrd.nms$intx[which(is.na(ordrd.nms$type))]='Invert Pred'


### NOW plot ordered diet contribution; highlight inverts in red
d=as.data.frame(ordrd.data)
wd3='C:/Users/ryan.morse/Desktop/NEUS Atl files'
filename=paste('pPrey_VAlues_20180515',".pdf", sep="")
mypath=file.path(wd3, filename)
pdf(file=mypath)
for(i in 1:length(ordrd.nms$name)){
  barplot(as.numeric(d[i,which(d[i,]>0)]), names.arg=colnames(d[i,which(d[i,]>0)]), main=paste(ordrd.nms[i,c('name', 'intx')]), las=2,
          col=ifelse(is.na(ordrd.nms$type[match(colnames(d[i,which(d[i,]>0)]), ordrd.nms$name)]), 'red', 'gray'))
}
dev.off()


### use this to read in modified versions to paste into shape for at_biology prm file (do all above, then read this, then continue)
ordrd.data=read_excel('pPrey_workbook.xlsx', sheet='20180419_@Gamble_final', col_names = T, trim_ws = T, col_types = 'numeric') # 20180419 with v1 replacements (same for all children)


### NOW put back into original format to allow pasting into biology prm file ###
test <- as.matrix(ordrd.data[,2:93])
ind <- (1:nrow(test)*3 - 1) # - 1 b/c you want to insert rows after, not before, existing rows
test_new <- matrix(rep(NA, (nrow(test)*3*ncol(test))), ncol=ncol(test))
test_new[ind,] <- test
ind2=seq(from=1, to=nrow(test_new),by=3)
test_new[ind2,1:2]=as.matrix(ordrd.nms[1:2])
write.table(test_new, file='20180419_@Gamble.csv',row.names=F, col.names=F, sep=" ", na="")




### (OLD) _______________________________________________________________________________________________________
### CREATE VERSIONS OF pPREY MATRIX TO TEST EFFECTS ON GROWTH 20180213 ###
d2[(d2<0.01)&(d2>0)]=0.01 ### make lowest value (where entered) = Xxx
### multiply values to increase values for different scenarios, keep max entry per row to < 1
d2.10pct=d2*0.1 # 10 percent of value
d2.120pct=d2*1.2; #120 pct
d2.150pct=d2*1.5 #150 pct
d2.200pct=d2*2
d2.400pct=d2*4
# keep rowmax below 1:
d2.120pct[d2.120pct>1]=1
d2.150pct[d2.150pct>1]=1
d2.200pct[d2.200pct>1]=1
d2.400pct[d2.400pct>1]=1

# nms=as.character(diet.nms[,1])
# size=as.numeric(diet.nms[,2])
# diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
# diet3[rownames(diet.nms),1]=nms
# diet3[rownames(diet.nms),2]=size
# diet3[rownames(d),]=d5
# write.table(diet3, file='raised_scaled_diet_2_RM.csv',row.names=F, sep=",")

diet=read.csv('atneus_diet_RM_edited.csv', header = F) # 20170424 added stages to shrimps and squid
diet2=diet[complete.cases(diet[,2]),] # drops blank rows between entries
diet.nms=diet2[!complete.cases(diet2[,3]),] # drops data
diet.data=diet2[complete.cases(diet2),]
d=diet.data
table(floor(log10(d[,2])))
d[,1]=as.numeric(as.character(d[,1]))
d2=as.matrix(d)
d6=read.csv(file.path(d1, data))

nms=as.character(diet.nms[,1])
size=as.numeric(diet.nms[,2])
diet3=as.data.frame(matrix(NA,nrow=768, ncol=92))
diet3[rownames(diet.nms),1]=nms
diet3[rownames(diet.nms),2]=size
diet3[rownames(d),]=d6[,3:94]
write.table(d2.10pct, file='20180213_0517prm_scaled_10pct.csv',row.names=F, col.names=F, sep=",")

### 20180227 READ IN EDITED VERSION - CHANGES TO pPREY for DL, DR, DC, Dlsed, Drsed, Dcsed for many groups, based on Jason Link's early version for v1.0,
# also changed structure of LSQ and ISQ from 1 to 4 entries (pPREY1ISQ1, 1ISQ2, 2ISQ1, 2ISQ2, etc.) RM 2/27/2018
# new=read_excel('pPrey_workbook.xlsx', sheet='20170517prm_20180213edited@Link', col_names = T,col_types = 'numeric')
# new.2=new[complete.cases(new[,2]),] # drops blank rows between entries
# new.data=new[complete.cases(new[,3]),] #data only
# new.nms=new.2[!complete.cases(new.2[,3]),] # drops data
# new.col=read_excel('pPrey_workbook.xlsx', sheet='20170517prm_20180213edited@Link', col_names = T, trim_ws = T) # keep character 1st col
# new.col2=new.col[complete.cases(new.col[,2]),] # drops blank rows between entries
# new.nms=new.col2[which(is.na(new.col2[,3])),1:2] # drops data, keeps names
# pnms=data.frame(new.nms)
# cnames=read_excel('pPrey_workbook.xlsx', sheet='colnames', col_names = F) # get column names for pPrey matrix
# cnames=data.frame(cnames)
# d=new.data
# table(floor(log10(d[,2])))
# # d[,1]=as.numeric(as.character(d[,1]))
# d2=as.matrix(d)
# row.names(d2)=pnms[,1]
# diet3=as.data.frame(matrix(NA,nrow=dim(new)[1], ncol=dim(new)[2]))
# test=new.2$HER==92
# test2=seq(1:length(nms))
# diet=read.csv('atneus_diet_RM_edited.csv', header = F) # 20170424 added stages to shrimps and squid### original 
diet=read.csv('pPrey_workbook.csv', header = F) # 20180227 new pPrey for ISQ, LSQ, add detritus groups to benthos
diet2=diet[complete.cases(diet[,2]),] # drops blank rows between entries
diet.nms=diet2[!complete.cases(diet2[,3]),] # drops data
diet.data=diet2[complete.cases(diet2),]
d=diet.data
table(floor(log10(d[,2])))
d[,1]=as.numeric(as.character(d[,1]))
d2=as.matrix(d)
### write output for NEUS, paste into biol.prm
nms=as.character(diet.nms[,1])
size=as.numeric(diet.nms[,2])
diet3=as.data.frame(matrix(NA,nrow=dim(diet)[1], ncol=dim(diet)[2]))
diet3[rownames(diet.nms),1]=nms
diet3[rownames(diet.nms),2]=size
diet3[rownames(d),]=d2
write.table(diet3, file='20180227pPreyRM_2.csv',row.names=F, sep=" ")



