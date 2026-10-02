####
# Used to create pdfs of atlantis netcdf output of production and numbers/N
# Using Alex Keth's atlantistoools to view output from NEUS v1.5
# RM 20170103


library(atlantistools)
library(ggplot2)
library(dplyr)
library(mapdata)
library(ncdf) #ncdf

# get old ncdf package
# url <- "https://cran.r-project.org/src/contrib/Archive/ncdf/ncdf_1.6.8.tar.gz"
# pkgFile <- "ncdf_1.6.8.tar.gz"
# download.file(url = url, destfile = pkgFile)
# ## Install dependencies
# ## install.packages(c("ada", "ipred", "evd"))
# # Install package
# install.packages(pkgs=pkgFile, type="source", repos=NULL)
# # Delete package tarball
# unlink(pkgFile)
# library(ncdf)


# d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
setwd(choose.dir(default=getwd())) # where run data are saved
d2=getwd()
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved

#linux
d1='/home/ryan/Git/atneus_RM'
d2='/home/ryan/AtlRuns/20180321b'
setwd(d2)

fname=sapply(strsplit(as.character(d2), "/"), tail, 1) # grab last chars of folder

files=list.files(path=d2, pattern='.nc')
NEUSgroups=read.csv(paste(d1,"/NeusGroups_v15_unix.csv",sep=""))
NEUSnames=lapply(NEUSgroups$Name, as.character)
fgs='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/NeusGroups_v15_unix.csv'
bps <- load_bps(fgs =file.path(d1, "NeusGroups_v15_unix.csv"), init=file.path(d1, "RMinit_newvalues2017.nc"))

nc <- files[2] #"atneus_v15_Test2_.nc"

xml.files=list.files(path=d2, pattern='.xml')
xml.str=strsplit(xml.files, '.xml')
# xml.str2=strsplit(xml.str[[1]], '_')
prm_run <- file.path(d1, paste(xml.str[[2]], '.prm',sep=''))
prm_run #make sure
prm_biol <- file.path(d1, paste(xml.str[[1]], '.prm',sep=''))
prm_biol #make sure
# prm_run = 'C:/Users/ryan.morse/Documents/GitHub/atneus_RM/at_run_neus_v15_RM.prm'
bboxes <- get_boundary(boxinfo = load_box(bgm = file.path(d1,"neus_tmerc_RM.bgm")))


### Production sums ###
res.dir <- d2 # "C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort/"
res.blurb <- files[[3]] #'timeseries_plots_prod.pdf'
my.ncdf <- open.ncdf(paste(res.dir,'/',res.blurb,sep=""))
var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name
mydat <- NULL
for (ivar in 1:my.ncdf$nvar){
  temp <- get.var.ncdf(my.ncdf,var.names[ivar])
  mydat <- cbind(mydat,apply(temp,length(dim(temp)),sum,na.rm=TRUE))
  print(ivar)
}
colnames(mydat) <- var.names
test=matrix(var.names)
# ivar=26

pdf(file=paste(fname,'_timeseries_plots_PROD.pdf', sep=''))
par(mfrow=c(2,2),oma=c(1,1,1,1),mar=c(1,1,1,1))
for (ivar in 11:length(var.names)){
  plot(mydat[,ivar],type='l',axes=T,ylab="",xlab="",
       ylim=c(0,1.02*max(mydat[,ivar],na.rm=TRUE)),cex.main=0.8)
  # es<-plotrix::emptyspace(mydat[,ivar], seq(1:length(mydat[,ivar])))
  # plotrix::boxed.labels(es,labels=var.names[ivar], bty='n', bg="transparent")
  legend('topright', legend=var.names[ivar])#, bty='n')
  # box()
}
dev.off()

#### sum of numbers, N ####
res.dir <- d2 # "C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort/"
res.blurb <- files[[1]] # windows
res.blurb <- files[[2]] #linux
my.ncdf <- open.ncdf(paste(res.dir,'/',res.blurb,sep=""))

var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name

mydat <- NULL
for (ivar in 1:my.ncdf$nvar)
{
  temp <- get.var.ncdf(my.ncdf,var.names[ivar])
  mydat <- cbind(mydat,apply(temp,length(dim(temp)),sum,na.rm=TRUE))
  print(ivar)
}
colnames(mydat) <- var.names
test=matrix(var.names)
# ivar=26

pdf(file=paste(fname,'_timeseries_plots.pdf', sep=''))
par(mfrow=c(2,2),oma=c(1,1,1,1),mar=c(1,1,1,1))
for (ivar in 1:length(var.names)){
  plot(mydat[,ivar],type='l',axes=T,ylab="",xlab="",
       ylim=c(0,1.02*max(mydat[,ivar],na.rm=TRUE)),cex.main=0.8)
  # es<-plotrix::emptyspace(mydat[,ivar], seq(1:length(mydat[,ivar])))
  # plotrix::boxed.labels(es,labels=var.names[ivar], bty='n', bg="transparent")
  legend('topright', legend=var.names[ivar])#, bty='n')
  # box()
}
dev.off()
#_______________________________________________________________
#### Plotting NEUS v1.0 data for comparison ####
###
setwd('C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort')
d2=getwd()
fgs=load_fgs(dir=getwd(),fgs='functionalGroups.csv')# C:/Users/ryan.morse/Documents/GitHub/atneus_RM/NeusGroups_v15_unix.csv'
bps <- load_bps(dir = d2, fgs = 'functionalGroups.csv', init = "inneus_2007.nc")
nc <- "neusDynEffort_Base_Effort_.nc"
nc <- files[8]
prm_run = 'at_run_neus_DE_50yearFC2.prm'
bboxes <- get_boundary(boxinfo = load_box(dir = d2, bgm = "neus30_2012.bgm"))
# NEUSgroupsv1.0=read.csv('C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort/functionalGroups.csv',check.names = F)
NEUSgroupsv1.0=read.csv('functionalGroups.csv',check.names = F)
NEUSnamesv1.0=lapply(NEUSgroupsv1.0$Name, as.character)

test <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnamesv1.0[1:62],
                select_variable = "N")

# "C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort"
res.dir <- "C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort/"
res.blurb <- "neusDynEffort_Base_Effort_PROD"
my.ncdf <- open.ncdf(paste(res.dir,res.blurb,".nc",sep=""))

var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name

mydat <- NULL
for (ivar in 1:my.ncdf$nvar)
{
  temp <- get.var.ncdf(my.ncdf,var.names[ivar])
  mydat <- cbind(mydat,apply(temp,length(dim(temp)),sum,na.rm=TRUE))
}
colnames(mydat) <- var.names
test=matrix(var.names)
ivar=26

pdf(file='timeseries_plots_prod.pdf')
par(mfrow=c(2,2),oma=c(1,1,1,1),mar=c(1,1,1,1))
for (ivar in 11:length(var.names)){
  plot(mydat[,ivar],type='l',axes=T,ylab="",xlab="",
       ylim=c(0,1.02*max(mydat[,ivar],na.rm=TRUE)),cex.main=0.8)
  # es<-plotrix::emptyspace(mydat[,ivar], seq(1:length(mydat[,ivar])))
  # plotrix::boxed.labels(es,labels=var.names[ivar], bty='n', bg="transparent")
  legend('topright', legend=var.names[ivar])#, bty='n')
  # box()
}
dev.off()

#_______________________________________________________________________
setwd(choose.dir(default=getwd())) # where run data are saved
d2=getwd()
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/' #where (PRM, bgm, group data) are saved


files=list.files(path=d2, pattern='.nc')
NEUSgroups=read.csv('functionalGroups.csv',check.names = F)
NEUSnames=lapply(NEUSgroups$Name, as.character)
fgs='functionalGroups.csv'
bps <- load_bps(dir = d1, fgs = "NeusGroups_v15_unix.csv", init = "RMinit_2.nc")
nc <- files[1] #"atneus_v15_Test2_.nc"
prm_run = 'C:/Users/ryan.morse/Documents/GitHub/atneus_RM/at_run_neus_v15_RM.prm'
bboxes <- get_boundary(boxinfo = load_box(dir = d1, bgm = "neus30_v15.bgm"))


### Production sums ###
res.dir <- d2 # "C:/Users/ryan.morse/AtlantisRun/NEUS v1 Base Effort/"
res.blurb <- files[[3]] #'timeseries_plots_prod.pdf'
my.ncdf <- open.ncdf(paste(res.dir,'/',res.blurb,sep=""))
var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name
mydat <- NULL
for (ivar in 1:my.ncdf$nvar){
  temp <- get.var.ncdf(my.ncdf,var.names[ivar])
  mydat <- cbind(mydat,apply(temp,length(dim(temp)),sum,na.rm=TRUE))
  print(ivar)
}
colnames(mydat) <- var.names
test=matrix(var.names)
# ivar=26

pdf(file='timeseries_plots_prod_prod.pdf') 
par(mfrow=c(2,2),oma=c(1,1,1,1),mar=c(1,1,1,1))
for (ivar in 11:length(var.names)){
  plot(mydat[,ivar],type='l',axes=T,ylab="",xlab="",
       ylim=c(0,1.02*max(mydat[,ivar],na.rm=TRUE)),cex.main=0.8)
  # es<-plotrix::emptyspace(mydat[,ivar], seq(1:length(mydat[,ivar])))
  # plotrix::boxed.labels(es,labels=var.names[ivar], bty='n', bg="transparent")
  legend('topright', legend=var.names[ivar])#, bty='n')
  # box()
}
dev.off()


#_______________________________________________________________________________

### load_nc() Only N, Nums, ResN, StructN, Eat, Growth, Prodn, Grazing, Catch can be selected as 'select_variable'

#linux
fgs="/home/ryan/Git/atneus_RM/NeusGroups_v15_unix.csv"
prm_run='/home/ryan/Git/atneus_RM/at_run_neus_v15_RM.prm'


NN <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "N")

Nums <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "Nums")

ResN <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "ResN")

StrcN <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "StructN")

nc <- "atneus_v15_Test2_PROD.nc"

Grwth <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "Growth")

Graz <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "Grazing")

Eat <- load_nc(dir = NULL, nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
                select_groups = NEUSnames,
                select_variable = "Eat")


### Plotting example from Alex____________________________________________
# convert bgm to dataframe with transformed coordinates
setwd(d1)
df <- convert_bgm(bgm = "neus30_v15.bgm")
plot_boxes(df)
# setwd('D:/AtlantisRun/20161103/tes/20170103')
# load("dogfish.rdata", verbose = T)
# test <- filter(dogfish, time < 0.5) # otherwise the plotting takes ages... :)


test=Graz[which(Graz$species=='Carnivorous zooplankton'),]
test=Graz[which(Graz$species=='Mesozooplankton'),]

test <- filter(test, time < 2) # otherwise the plotting takes ages... :)

# add timesteps and layers to df to add empty polygons in case no data is present
df2 <- merge(df, unique(select(test, time, agecl)))
# df2 <- merge(df, unique(select(test, time, layer)))

# combine data and map-layout
plot_test <- full_join(test, df2)

# Set your Rectangle here!
lon_min <- -76
lon_max <- -65
lat_min <- 35
lat_max <- 45

# Dont load the whole map... Use xlim/ylim to select a specific area.
# By default the WHOLE regions inside this area are extracted...
cm <- fortify(maps::map(database = "worldHires", plot = FALSE, fill = T, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)))

# ggplot2::ggplot(plot_test, aes(x = long, y = lat)) +
#   ggplot2::geom_polygon(aes(fill = atoutput, group = factor(polygon)), colour = "black") +
#   ggplot2::facet_grid(layer ~ time) +
#   geom_polygon(data = cm, aes(group = group)) + # add land
#   coord_map(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) # zoom to rectangle

ggplot2::ggplot(plot_test, aes(x = long, y = lat)) +
  ggplot2::geom_polygon(aes(fill = atoutput, group = factor(polygon)), colour = "black") +
  ggplot2::facet_grid(agecl ~ time) +
  geom_polygon(data = cm, aes(group = group)) + # add land
  coord_map(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) # zoom to rectangle

###______________________________________________________________________



names=matrix(unique(NN$species))
i=14
plot_line(NN[which(NN$species==names[i]),])
pdf(file='NN.pdf')
for(i in 1:length(names)){
  p=plot_line(NN[which(NN$species==names[i]),])
  print(p)
}
dev.off()

names=matrix(unique(Nums$species))
pdf(file='Nums.pdf')
for(i in 1:length(names)){
  p=plot_line(Nums[which(Nums$species==names[i]),])
  print(p)
}
dev.off()


names=matrix(unique(Nums$species))
i=23
plot_line(Nums[which(Nums$species==names[i]),])
pdf(file='Nums.pdf')
for(i in 1:3){
  plot_line(Nums[which(Nums$species==names[i]),]); i=i+1
  Sys.sleep(0) 
}
dev.off()

names=matrix(unique(Nums$species))
pdf(file='Nums.pdf')
for(i in 1:length(names)){
  p=plot_line(Nums[which(Nums$species==names[i]),])
  print(p)
}
dev.off()

names=matrix(unique(ResN$species))
plot_line(ResN[which(ResN$species==names[i]),])

names=matrix(unique(StrcN$species))
plot_line(StrcN[which(StrcN$species==names[i]),])

names=matrix(unique(Grwth$species))
i=19
test=Grwth[which(Grwth$species==names[i]),]
test <- filter(test, time < 0.5) 
plot_line(test)

plot_line(Grwth[which(Grwth$species==names[i]),])

names=matrix(unique(Graz$species))
plot_line(Graz[which(Graz$species==names[i]),])

names=matrix(unique(Eat$species))
plot_line(Eat[which(Eat$species==names[i]),])

for(i in 1:2){
plot_line(test[which(test$species==names[i]),])
}

i=77
plot_line(Grwth[which(Grwth$species==names[i]),])





#_____________________________________________________________
library(vat)
create_vadt(outdir='C:/Users/ryan.morse/AtlantisRun/20161103/tes/20161121_3/', 
            fgfile='C:/Users/ryan.morse/Documents/GitHub/atneus_gavdev/atneus/NeusGroups_v15_unix.csv',
            biolprm='C:/Users/ryan.morse/AtlantisRun/20161103/tes/20161121_3/at_biol_neus_v15_working.prm', 
            ncout='atneus_v15_Test1_', 
            startyear=1964, toutinc=365, diet = TRUE)

outdir = 'C:/Users/ryan.morse/AtlantisRun/20161103/tes/20161121_3/'
fgfile = 'C:/Users/ryan.morse/Documents/GitHub/atneus_gavdev/atneus/NeusGroups_v15_unix.csv'
biolprm = 'C:/Users/ryan.morse/AtlantisRun/20161103/tes/20161121_3/at_biol_neus_v15_working.prm'
# ncout = 'atneus_v15_Test1_.nc'
ncout='atneus_v15_Test1_'
startyear = 1964
toutinc = 30
diet = F
obj <- create_vadt(outdir, fgfile, biolprm, ncout, startyear, toutinc, diet)

ncx=nc_open(myfiles[1])