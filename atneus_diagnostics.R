###

res.dir <- "test3/"
res.blurb <- "atneus_v15_Test1_"

library(ncdf)
library(reshape2)
library(ggplot2)

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


#plots

#for vertebrates
# 1. Time series of biomass
# 2. Spatial distribution - seasonal if it is different
# 3. Numbers at age over time (perhaps a bubble plot single panel)
# 4. size at age (maybe one panel with growth curve)
# 5. size by age over time (1 panel per age class - or diff lines in 1 panel)
# 6. mortality by age over time (bubble?)  M, M2, F
# 7. diet composition (juv and adult separate - params same but realized will be diff)

# for inverts
# 1. Time series of biomass
# 2. Spatial distribution - seasonal if it is different
# 3. mortality over time (bubble?)  M, M2, F
# 4. diet composition (juv and adult separate - params same but realized will be diff)

# for physical tracers, nutrients, etc.
# 1. Time series of abundance / density
# 2. Spatial distribution - seasonal if it is different
# 3. time series by box, scale - to id if going to zero anywhere.


mydat2 <- melt(mydat,id.vars=1:length(var.names))

pdf(file='timeseries_plots.pdf')
par(mfrow=c(5,5),oma=c(1,1,1,1),mar=c(1,1,1,1))
for (ivar in 1:length(var.names))
{
  plot(mydat[,ivar],type='b',axes=F,ylab="",xlab="",main=var.names[ivar],
       ylim=c(0,1.02*max(mydat[,ivar],na.rm=TRUE)),cex.main=0.8)
  box()
}
dev.off()

# Probability of overfishing
p5 <- ggplot(mydat,aes(betas,F_FMSY)) #,colour=factor(SEX)))

layout(matrix(c(1,2,3,0), 2, 2, byrow = TRUE))

group <- "Pollock"
pdf(paste(group,".pdf",sep=""))
par(mfrow=c(2,2),oma=c(1,1,1,1),mar=c(2,2,1,1))
vertbiomassplot(group)
vertbiomassplot(group)
sizeatageplot(group)
spatialbioplot(group)
numbersatageplot(group)
dev.off()


source('multiplot.r')

##### BIOMASS TIME SERIES PLOT #####
vertbiomassplot <- function(group)
{
setwd('~/Atlantis/atneus')
group.names <- read.csv("NeusGroups_v15.csv")
groups <- group.names$Name

res.dir <- "test3/"
res.blurb <- "atneus_v15_Test1_"

library(ncdf)
#library(reshape2)
#library(ggplot2)

my.ncdf <- open.ncdf(paste(res.dir,res.blurb,".nc",sep=""))

var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name

#group <- "Pollock"

#get total N
var <- paste(group,"_N",sep="") 
groupN <- get.var.ncdf(my.ncdf,var)
vol <- get.var.ncdf(my.ncdf,"volume")
TotN <- apply(groupN*vol,3,sum,na.rm=TRUE)
for (age in 1:10)
{
  var <- paste(group,age,"_ResN",sep="")
  SRN <- get.var.ncdf(my.ncdf,var)
  var <- paste(group,age,"_StructN",sep="")
  SRN <- SRN + get.var.ncdf(my.ncdf,var)
  var <- paste(group,age,"_Nums",sep="")
  Nums <- get.var.ncdf(my.ncdf,var)
  if (age==1) TheN <- SRN*Nums
  if (age>1) TheN <- TheN + SRN*Nums
}
Kwet = 20.0
Redfield_CN = 5.7
#biomass <- Kwet*Redfield_CN*TotN/10^9
biomass <- Kwet*Redfield_CN*apply(TheN,3,sum,na.rm=TRUE)/10^9

times <- my.ncdf$var[[which(var.names==var)]]$dim[[3]]$vals/(60*60*24*365)

plot(times,biomass,type='b',axes=F,ylab="time (y)",xlab="Biomass (t)",
     main=paste(group," biomass (t)",sep=""),
     ylim=c(0,1.02*max(biomass,na.rm=TRUE)),cex.main=0.8)
box()
axis(2)
axis(1,at=seq(0,floor(times[length(times)]),1))

spatial.bio <- Kwet*Redfield_CN*apply(TheN,2,colSums,na.rm=TRUE)/10^9
close.ncdf(my.ncdf)
}
########## END BIOMASS TIME SERIES PLOT


########### NUMBERS AT AGE PLOT
numbersatageplot <- function(group)
{
setwd('~/Atlantis/atneus')
group.names <- read.csv("NeusGroups_v15.csv")
groups <- group.names$Name

res.dir <- "test3/"
res.blurb <- "atneus_v15_Test1_"

library(ncdf)
#library(reshape2)
#library(ggplot2)

my.ncdf <- open.ncdf(paste(res.dir,res.blurb,".nc",sep=""))

var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name

#group <- "Pollock"

for (age in 1:10)
{
  var <- paste(group,age,"_Nums",sep="")
  Nums <- get.var.ncdf(my.ncdf,var)
  if (age==1) TheNums <- array(NA,dim=c(dim(Nums)[3],10))
  TheNums[,age] <- apply(Nums,3,sum,na.rm=TRUE)
}
Kwet = 20.0
Redfield_CN = 5.7
#biomass <- Kwet*Redfield_CN*TotN/10^9
biomass <- Kwet*Redfield_CN*apply(TheN,3,sum,na.rm=TRUE)/10^9

times <- my.ncdf$var[[which(var.names==var)]]$dim[[3]]$vals/(60*60*24*365)

#plot(times,biomass,type='b',axes=F,ylab="",xlab="",
#     main=paste(group," biomass (t)",sep=""),
#     ylim=c(0,1.02*max(biomass,na.rm=TRUE)),cex.main=0.8)
#box()
#axis(2)
#axis(1,at=seq(0,floor(times[length(times)]),1))

colnames(TheNums) <- 1:10
mydat <- cbind(stack(as.data.frame(TheNums)),rep(times,10))
mydat[,2] <- as.numeric(as.character(mydat[,2]))
colnames(mydat) <- c("Numbers","Age","Time")
#mydat2 <- melt(mydat,id.vars=1:3)

p <- ggplot(mydat, aes(Time, Age)) + 
  geom_point(aes(size = Numbers, alpha=Numbers)) + scale_size(range=c(0,20))
#p + geom_point(aes(size = log(Numbers), alpha=log(Numbers)))
#p + geom_point(aes(size = Numbers, alpha=Numbers)) + scale_size(range=c(0,20))
return(p)
close.ncdf(my.ncdf)
}
######### End Numbers at age plot ###########


##### SPATIAL BIOMASS PLOT #####
spatialbioplot <- function(group)
{
setwd('~/Atlantis/atneus')
group.names <- read.csv("NeusGroups_v15.csv")
groups <- group.names$Name

res.dir <- "test3/"
res.blurb <- "atneus_v15_Test1_"

library(ncdf)
#library(reshape2)
#library(ggplot2)

my.ncdf <- open.ncdf(paste(res.dir,res.blurb,".nc",sep=""))

var.names <- rep(NA,my.ncdf$nvar)
for (i in 1:length(var.names)) var.names[i] <- my.ncdf$var[[i]]$name

#group <- "Pollock"

#get total N
var <- paste(group,"_N",sep="") 
groupN <- get.var.ncdf(my.ncdf,var)
vol <- get.var.ncdf(my.ncdf,"volume")
TotN <- apply(groupN*vol,3,sum,na.rm=TRUE)
for (age in 1:10)
{
  var <- paste(group,age,"_ResN",sep="")
  SRN <- get.var.ncdf(my.ncdf,var)
  var <- paste(group,age,"_StructN",sep="")
  SRN <- SRN + get.var.ncdf(my.ncdf,var)
  var <- paste(group,age,"_Nums",sep="")
  Nums <- get.var.ncdf(my.ncdf,var)
  if (age==1) TheN <- SRN*Nums
  if (age>1) TheN <- TheN + SRN*Nums
}
Kwet = 20.0
Redfield_CN = 5.7
#biomass <- Kwet*Redfield_CN*TotN/10^9
spatial.bio <- Kwet*Redfield_CN*apply(TheN,2,colSums,na.rm=TRUE)/10^9

times <- my.ncdf$var[[which(var.names==var)]]$dim[[3]]$vals/(60*60*24*365)

#plot(times,biomass,type='b',axes=F,ylab="",xlab="",
#     main=paste(group," biomass (t)",sep=""),
#     ylim=c(0,1.02*max(biomass,na.rm=TRUE)),cex.main=0.8)
#box()
#axis(2)
#axis(1,at=seq(0,floor(times[length(times)]),1))

#get the coords of the boxes
bgm.file <- read.table("neus30_v15.bgm",col.names=1:100,comment.char="",
                       fill=TRUE,header=FALSE)
boxes <- NULL
for (box in 0:29)
{
  label <- paste("box",box,".vert",sep="")
  pick <- grep(label,bgm.file[,1])
  boxes <- rbind(boxes,cbind(box,bgm.file[pick[-1],2:3]))
}
colnames(boxes) <- c("box","X","Y")
boxes[,2] <- as.numeric(as.character(boxes[,2]))
boxes[,3] <- as.numeric(as.character(boxes[,3]))



#read in biology file
TheData <- read.table("at_biol_neus_v15_working.prm",col.names=1:100,comment.char="",fill=TRUE,header=FALSE)
#find the spatial distribution parameters from the prm file.
pick <- c(grep("_S1",TheData[,1]),grep("_S2",TheData[,1]),
          grep("_S3",TheData[,1]),grep("_S4",TheData[,1]))
hdistrib <- TheData[pick+1,1:30]
hdistrib <- cbind(TheData[pick,1],hdistrib)
pick <- grep(as.character(group.names$Code[which(group.names$Name==group)]),hdistrib[,1])
pick2 <- pick[-grep("juv",hdistrib[pick,1])]
#pick2 <- pick[-grep("#",hdistrib[pick2,1])]
#hdistrib <- read.table("hdistrib.out",header=FALSE)
coords <- boxes
coords.2 <- coords
#pick <- grep(group.names$Code[which(group.names$Name==group)],hdistrib[,1])
#pick2 <- pick[-grep("juv",hdistrib[pick,1])]
c2 <- NULL
for (irow in pick2)
{
  coords$fillpts <- as.numeric(t(hdistrib[irow,as.numeric(coords$box)+2]))/
    sum(as.numeric(t(hdistrib[irow,as.numeric(coords$box)+2])),na.rm=TRUE)
  #coords$fillpts <- as.numeric(t(hdistrib[irow,as.numeric(coords$id)+2])) 
  coords$season <- rep(paste("Season ",which(pick2==irow),sep=""),nrow(coords))
  c2 <- rbind(c2,coords)
}
c2$season <- factor(c2$season)

# Map <- ggplot(c2, aes(long,lat, group = id, fill=fillpts)) + 
#  geom_polygon() + coord_equal() + labs(x = "Longitude", y = "Latitude") + 
#  scale_fill_gradient() + facet_wrap(~season) + #,nrow=1,ncol=4) + 
#  ggtitle(strsplit(as.character(hdistrib[pick2[1],1]),split="_",
#                   fixed=TRUE)[[1]][1])
# Map
Map1 <- ggplot(c2, aes(X,Y, group = box, fill=fillpts)) + 
  geom_polygon() + coord_equal() + labs(x = "Longitude", y = "Latitude") + 
  scale_fill_gradient() + facet_wrap(~season) + #,nrow=1,ncol=4) + 
  ggtitle(group)
#Map

c3 <- NULL
rows <- floor(seq(1,dim(spatial.bio)[1],length.out=4))
for (irow in rows)
{
  coords.2$fillpts <- as.numeric(t(spatial.bio[irow,as.numeric(coords.2$box)+1]))/
    sum(as.numeric(t(spatial.bio[irow,as.numeric(coords.2$box)+1])),na.rm=TRUE)
  coords.2$season <- rep(paste("t = ",format(times[irow],digits=2)," y",sep=""),nrow(coords.2))
  c3 <- rbind(c3,coords.2)
}
c3$season <- factor(c3$season)

c4 <- rbind(c2,c3)

Map2 <- ggplot(c3, aes(X,Y, group = box, fill=fillpts)) + 
  geom_polygon() + coord_equal() + labs(x = "Longitude", y = "Latitude") + 
  scale_fill_gradient() + facet_wrap(~season) + #,nrow=1,ncol=4) + 
  ggtitle(group)
#Map

Map <- ggplot(c4, aes(X,Y, group = box, fill=fillpts)) + 
  geom_polygon() + coord_equal() + labs(x = "Longitude", y = "Latitude") + 
  scale_fill_gradient() + facet_wrap(~season,nrow=2) + #,nrow=1,ncol=4) + 
  ggtitle(group)
return(Map)

close.ncdf(my.ncdf)
########## END SPATIAL BIOMASS PLOT
}

########## SIZE AT AGE PLOT
#' @author Gavin Fay
#' Aug 6 2015, modified from Jan 2015
#' 
#set working directory
#setwd("~/Atlantis/neus_sandbox/")
sizeatageplot <- function(group)
{
#Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
#CodeRelations <- read.csv("coderelations.csv")
#read in biology file
TheData <- read.table("at_biol_neus_v15_working.prm",col.names=1:100,comment.char="",fill=TRUE,header=FALSE)

#find the length-weight parameters from the old prm file, store them
pick <- grep("li_a_",TheData[,1])
xx <- TheData[pick,1:20]
tempmat <- matrix(NA,nrow=nrow(xx),ncol=3)
for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(as.character(xx[igroup,1]),"li_a_")[[1]][2]
tempmat[,2] <- as.numeric(as.character(xx[,2]))
pick <- grep("li_b_",TheData[,1])
tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

#find the ages per cohort parameters from the prm file, store them
pick <- grep("_AgeClassSize",TheData[,1])
xx <- TheData[pick,1:20]
ages.per.cohort <- matrix(NA,nrow=nrow(xx),ncol=2)
for (igroup in 1:nrow(ages.per.cohort)) ages.per.cohort[igroup,1] <- 
  strsplit(as.character(xx[igroup,1]),"_AgeClassSize")[[1]][1]
ages.per.cohort[,2] <- as.numeric(as.character(xx[,2]))

#find the ages at maturity parameters from the prm file, store them
pick <- grep("_age_mat",TheData[,1])
xx <- TheData[pick,1:20]
age.of.maturity <- matrix(NA,nrow=nrow(xx),ncol=2)
for (igroup in 1:nrow(ages.per.cohort)) age.of.maturity[igroup,1] <- 
  strsplit(as.character(xx[igroup,1]),"_age_mat")[[1]][1]
age.of.maturity[,2] <- as.numeric(as.character(xx[,2]))

#For each time step
# For each Atlantis age class, calculate length associated with weight
# Generate length distribution assuming a fixed CV of length at age
# calculate distribution by 1cm
# max length 150cm
upper.bins <- 1:150
#mulen = 20
CVlenage = 0.15
#CVlenage = sqrt(exp(sigma^2) - 1)

require(ncdf)
#~/Desktop/Volumes/MyPassport/NEFSC/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/")

group.names <- read.csv("NeusGroups_v15.csv")
#gb.groups <- c("FPS","FPL","FDS","FDO","FDF","FDD","SHB","SSK","FVB","FDB")
#groups <- group.names$NetCDFName[match(gb.groups,group.names$CODE)]
#groups <- group.names$NetCDFName
#groups <- as.character(t(strsplit(as.character(groups),"_N",fixed=TRUE)))
groups <- group.names$Name

#setwd("~/Desktop/Volumes/MyPassport/NEFSC/ATLANTIS/Scenarios/Test/BASE_ANNUAL/")
setwd("~/Atlantis/atneus/")

res.dir <- "test3/"
res.blurb <- "atneus_v15_Test1_"

library(ncdf)
#library(reshape2)
#library(ggplot2)

my.ncdf <- open.ncdf(paste(res.dir,res.blurb,".nc",sep=""))

#the.files <- list.files()
#ncfile <- the.files[grep("_.nc",the.files)]
#consider providing string as argument to function

#xx2 <- open.ncdf("atneus_v15_Test1_.nc")
#xx2 <- open.ncdf(ncfile)
xx2 <- my.ncdf

names2 <- rep(NA,xx2$nvar)
for (i in 1:xx2$nvar) names2[i] <- xx2$var[[i]]$name

#group = "Pollock"
#get total N
ivar <- grep(paste(group,"_N",sep=""),names2)
groupN <- get.var.ncdf(xx2,ivar)
vol <- get.var.ncdf(xx2,"volume")
TotN <- sum(groupN*vol,na.rm=TRUE)

for (age in 1:10)
{
  ivar <- grep(paste(group,age,"_ResN",sep=""),names2)
  SRN <- get.var.ncdf(xx2,ivar)
  ivar <- grep(paste(group,age,"_StructN",sep=""),names2)
  SRN <- SRN + get.var.ncdf(xx2,ivar)
  ivar <- grep(paste(group,age,"_Nums",sep=""),names2)
  Nums <- get.var.ncdf(xx2,ivar)
  if (age==1) TheN <- SRN*Nums
  if (age>1) TheN <- TheN + SRN*Nums
}

#Lenfreq = array(0,dim=c(length(groups),150,dim(SRN)),
#                dimnames=list(groups=group.names$EMOCCName[match(gb.groups,group.names$CODE)],
 #                             length=upper.bins,depth=1:5,box=0:29,time=1:dim(SRN)[3]))
#Fracperbin = array(0,dim=c(150,dim(SRN)))
#li_a_use = 0.0107
#li_b_use = 2.91
Kwet = 20.0
Redfield_CN = 5.7
ages = 1:10

#groups <- CodeRelations$Child[CodeRelations$IsVertebrate==1]

mulenage <- array(0,dim=c(length(groups),10,dim(SRN)[3]))
muweight <- array(0,dim=c(length(groups),10,dim(SRN)[3]))


#for (group in groups)
#{
  print(group)
  igroup <- which(groups==group)
  
#  ncgroup <- 
 #   group.names$NetCDFName[which(
#      group.names$CODE==as.character(CodeRelations$Parent[igroup]))]
  #ncgroup <- as.character(t(strsplit(as.character(ncgroup),"_N",fixed=TRUE)))
  ncgroup <- group

  #total biomasa by area
  #
  # END GOAL
  # size structure of the catch/pop by area and fleet  (can be in numbers)
  #
  # catches, total catch by fleet and area, apply size structure of pop to get catch at age
  #
  #
  
  
  #biology parameter file
  #weight-at-length parameters
  li_a_use <- 
    as.numeric(tempmat[igroup,2])
  li_b_use <- 
    as.numeric(tempmat[igroup,3])
  
  #calculate length
  #length-weight determined by equation #95 in TechMemo 218
  
  for (age in ages)
  {
    print(age)
    #ivar <- grep(paste(group,age,"_ResN",sep=""),names2)
    SRN <- get.var.ncdf(xx2,paste(ncgroup,age,"_ResN",sep=""))
    #ivar <- grep(paste(group,age,"_StructN",sep=""),names2)
    SRN <- SRN + get.var.ncdf(xx2,paste(ncgroup,age,"_StructN",sep=""))
    #ivar <- grep(paste(group,age,"_Nums",sep=""),names2)
    Nums <- get.var.ncdf(xx2,paste(ncgroup,age,"_Nums",sep=""))
    #if (age==1) TheN <- SRN*Nums
    # if (age>1) TheN <- TheN + SRN*Nums
    
    Nperage <- SRN*Nums
    
    #Nperage2 <- array(dim=dim(Nperage)[-1])
    #for (irow in 1:nrow(Nperage2))
    # Nperage2[irow,] <- colSums(Nperage[,irow,],na.rm=TRUE)
    
    #GET LENGTH FOR THIS AGE CLASS
    Length <- ((Kwet*Redfield_CN*SRN)/(1000*li_a_use))^(1/li_b_use)
    mulen <- rep(0,length(Length[1,1,]))
    for (t in 1:length(mulen))
    {
      numfreq <- Nums[,,t]/sum(Nums[,,t]) 
      mulen[t] <- sum(Length[,,t]*numfreq)      
    }
    
    mulenage[igroup,age,] <- mulen
    muweight[igroup,age,] <- li_a_use*mulen^li_b_use
    #close ages  
  }  
  #close groups
##} 

#setwd("~/Atlantis/neus_sandbox/")
#pdf(file='sizeatage.pdf')
#par(mfrow=c(3,3),mar=c(2,3,3,1),oma=c(3,3,0,0))
#for (igroup in 1:dim(mulenage)[1])
#{
  ages.use <- ages*as.numeric(ages.per.cohort[igroup,2])
  ages.use2 <- c(0,ages.use[-length(ages.use)]) 
  a2 <- (ages.use-ages.use2)/2
  ages.use3 <- ages.use2 + a2
  agemat <- ages.use3[as.numeric(age.of.maturity[igroup,2])]
  plot(ages.use3,mulenage[igroup,,1],type='l',lwd=2,
       main=paste(group," L@A",sep=""),
       xlab="Age",ylab="Length (cm)",ylim=c(0,1.1*max(mulenage[igroup,,])))
  abline(v=agemat,lty=2)
  for(t in 2:52) lines(ages.use3,mulenage[igroup,,t],lwd=0.5,col=gray(0.9))
  lines(ages.use3,mulenage[igroup,,1],lwd=2) 
  mtext("Age",side=1,outer=TRUE)
  mtext("Length",side=2,outer=TRUE) 
#}
#dev.off()

  times <- my.ncdf$var[[which(names2==paste(ncgroup,age,"_Nums",
                                            sep=""))]]$dim[[3]]$vals/(60*60*24*365)
  plot(times,mulenage[igroup,1,],type='l',ylim=c(0,1.1*max(mulenage[igroup,,])),
       axes=FALSE,xlab="time (yr)",ylab="Length(cm)",main=paste(group,
       " L@A over time",sep=""))
  for (age in 2:10) lines(times,mulenage[igroup,age,])
  axis(2)
  axis(1,at=seq(0,floor(times[length(times)]),1))
  box()

 close.ncdf(my.ncdf)
 #close.ncdf(xx2)
}



