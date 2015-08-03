setwd("~/Atlantis/neus_sandbox")
#library(ncdf)

#old.init.nc <- open.ncdf("inneus_2012.nc")

#var.names <- rep(NA,length=old.init.nc$nvar)
#for (i in 1:old.init.nc$nvar) var.names[i] <- old.init.nc$var[[i]]$name

#myvar <- get.var.ncdf(old.init.nc,"Demersal_S_Fish1_StructN")
#myatt <- att.get.ncdf(old.init.nc,"Demersal_S_Fish1_StructN")

#old.init.nc$var[["Demersal_S_Fish1_StructN"]]


olddat <- read.table("gav.junk",header=FALSE,col.names=1:20,fill=TRUE,sep="\t")
olddat <- read.table("gav.junk",header=FALSE,col.names=1:20,
                     colClasses=rep("character",20),fill=TRUE,strip.white=TRUE,sep='\t',quote="]]")
#for (icol in 1:3) olddat[,icol] <- as.character(olddat[,icol])
lastrow <- olddat[nrow(olddat),]
olddat <- olddat[-nrow(olddat),]

myname <- "Demersal_S_Fish1_StructN"

pick <- grep(myname,olddat[,1])
pick2 <- grep(myname,olddat[,2])
pick3 <- grep(myname,olddat[,3])


pick <- grep("// global attributes:",olddat[,1])

newdat <- olddat[1:(pick-1),1:3]
newdat2 <- olddat[(pick:nrow(olddat)),1:3]

#read old groups
old.groups <- read.csv("NeusGroups.csv")
old.groups$IsTurnedOn[48] <- 1
new.groups <- read.csv("NeusGroups_v15.csv")

nold <- which(old.groups$Name=="Prawn")

mapfile <- "coderelations.csv"
CodeRelations <- read.csv(mapfile)


for (iold in 1:nrow(old.groups))
{
  if(old.groups$IsTurnedOn[iold]==1) 
  {
   var2find <- old.groups$Name[iold]
   pick2 <- grep(var2find,newdat[,2])
   pick3 <- grep(var2find,newdat[,3])
   newg <- which(as.character(CodeRelations$Parent)==old.groups$Code[iold])
   reuse <- newdat[c(pick2,pick3),]
   var2put <- new.groups$Name[newg[1]]
   newdat[pick2,2] <- gsub(var2find,var2put,newdat[pick2,2])
   newdat[pick3,3] <- gsub(var2find,var2put,newdat[pick3,3])
   if (length(newg)>1)
   {
     for (igroup in 2:length(newg))
     {
       var2put <- new.groups$Name[newg[igroup]]
       newbit <- reuse
       newbit[,2] <- gsub(var2find,var2put,newbit[,2])
       newbit[,3] <- gsub(var2find,var2put,newbit[,3])
       newdat <- rbind(newdat,newbit)
     }          
   }
}
}

for (iold in 1:nrow(old.groups))
{
  if(old.groups$IsTurnedOn[iold]==0) 
  {
    var2find <- old.groups$Name[iold]
    pick2 <- grep(var2find,newdat[,2])
    pick3 <- grep(var2find,newdat[,3])
    newdat <- newdat[-c(pick2,pick3),]
  }
}

write.table(newdat,file="newinit.junk",col.names=FALSE,row.names=FALSE,sep="",quote=FALSE)

##writeLines(text=paste(newdat[i,],sep=""),con="newinit.junk")

##tempdat <- rep(NA,nrow(newdat))
##for (i in 1:nrow(newdat)) tempdat[i] <- paste(newdat[i,1:3],sep='',collapse='')
##writeLines(text=tempdat[1:20],con="newinit.junk")

for (iold in 1:nrow(old.groups))
{
  if(old.groups$IsTurnedOn[iold]==1) 
  {    
  var2find <- old.groups$Name[iold]
  pick2 <- grep(var2find,newdat2[,1])
  pick3 <- grep(";",newdat2[,1])
  lrows <- rep(NA,length(pick2))
  for (i in 1:length(pick2)) lrows[i] <- min(pick3[pick3>pick2[i]])
  newg <- which(as.character(CodeRelations$Parent)==old.groups$Code[iold])
  reuse <- newdat2[pick2,]
  var2put <- new.groups$Name[newg[1]]
  newdat2[pick2,1] <- gsub(var2find,var2put,newdat2[pick2,1])
  if (length(newg)>1)
  {
   for (igroup in 2:length(newg))
    {
      var2put <- new.groups$Name[newg[igroup]]
      newbit <- reuse
      newbit[,1] <- gsub(var2find,var2put,newbit[,1])      
      for (i in 1:length(pick2))
      {
       newdat2 <- rbind(newdat2,newbit[i,])
       newdat2 <- rbind(newdat2,newdat2[((pick2[i]+1):lrows[i]),])
      }
    }          
  }
 }
}


for (iold in 1:nrow(old.groups))
{
  if(old.groups$IsTurnedOn[iold]==0) 
  {    
    var2find <- old.groups$Name[iold]
    pick2 <- grep(var2find,newdat2[,1])
    pick3 <- grep(";",newdat2[,1])
    lrows <- rep(NA,length(pick2))
    for (i in 1:length(pick2)) lrows[i] <- min(pick3[pick3>pick2[i]])
    to.cut <- NULL
    for (i in 1:length(pick2))
     to.cut <- c(to.cut,(pick2[i]:lrows[i]))  
    newdat2 <- newdat2[-to.cut,]
  }
}

newdat2 <- rbind(newdat2,lastrow)

write.table(newdat2,file="newinit.junk",col.names=FALSE,row.names=FALSE,sep="",
            quote=FALSE,append=TRUE)


# initial scale vectors for run file
init.scales 
tempdat <- read.table('at_run_neus_v15_DE.prm',col.names=1:200,fill=TRUE,
                          blank.lines.skip=FALSE)
irow <- grep('init_scalar',tempdat[,1]) +3

init.scales <- tempdat[irow,which(is.na(tempdat[irow,])==FALSE)]

matches <- match(CodeRelations$Parent,old.groups$Code)

