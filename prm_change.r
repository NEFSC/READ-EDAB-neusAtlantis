##########################################################################################################
# Gavin Fay @nmfs NEFSC May 9th 2014
# R code to create csv file for filling in new values for weight-at-length parameters
#   given a new group structure
# In theory this code could be changed for all parameters of interest.
# Final section then takes the csv file and converts back to format needed for the biology .prm file
#
#  Requires a biology param file and then a lookup file that converts old groups to new groups etc.
##########################################################################################################

#set working directory
setwd("C:/Atlantis/neus_sandbox/Test/test1/")
setwd("~/Atlantis/neus_sandbox/")



#Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
CodeRelations <- read.csv("coderelations.csv")

#read in biology file
TheData <- read.table("at_biol_neus_v15_DE.prm",col.names=1:100,comment.char="",fill=TRUE,header=FALSE)


#find the length-weight parameters from the old prm file, store them
pick <- grep("li_a_",TheData[,1])
xx <- TheData[pick,1:20]
tempmat <- matrix(NA,nrow=nrow(xx),ncol=3)
for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(as.character(xx[igroup,1]),"li_a_")[[1]][2]
tempmat[,2] <- as.numeric(as.character(xx[,2]))
pick <- grep("li_b_",TheData[,1])
tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

#assign the old vals to the new codes, write a csv for data entry with columns to change highlighted and 'parent' group parameter values inserted.
output <- CodeRelations #list(Code=CodeRelations$Child,Parent=CodeRelations$Parent,Change=CodeRelations$Change)
output$li_a <- rep(NA,nrow(output))
output$li_a <- tempmat[match(CodeRelations$Parent,tempmat[,1]),2]
output$li_b <- rep(NA,nrow(output))
output$li_b <- tempmat[match(CodeRelations$Parent,tempmat[,1]),3]
output$source <- ""
output$source[output$Change==0] <- "Atlantis-Neus v1"
#head(output)
write.table(output,file="weightlength.csv",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")

## assuming that the new values have been filled in, put back into format for the .prm file
Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate==1])
temptab <- matrix(NA,nrow=Nverts,ncol=3)
temptab[,1] <- paste("li_a_",output$Child[output$IsVertebrate==1],sep="")
temptab[,2] <- output$li_a[output$IsVertebrate==1]
temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
write.table(temptab,file="newtable.out",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
write(" ",file="newtable.out",append=TRUE)
temptab <- matrix(NA,nrow=Nverts,ncol=3)
temptab[,1] <- paste("li_b_",output$Child[output$IsVertebrate==1],sep="")
temptab[,2] <- output$li_b[output$IsVertebrate==1]
temptab[,3] <- paste("weight at length b parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
write.table(temptab,file="newtable.out",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ",append=TRUE)




change.prm <- function(path,origfile,param.name,outfile,mapfile,param.type,
                       name2=NULL)
 {
  #this function finds a parameter in a prm file, 
  #maps the old values to new group structure,
  #and then writes out the new parameters in a text file,
  #for insertion into a new .prm file

  #set working directory
  setwd(path)

  #Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
  CodeRelations <- read.csv(mapfile)

  #read in orig param file
  TheData <- read.table(origfile,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)


  #find the length-weight parameters from the old prm file, store them
  if (param.name!= "") 
    pick <- grep(param.name,TheData[,1])
  if (length(name2)>0) pick <-pick[grep(name2,TheData[pick,1])]
  if (param.name== "") pick <- grep(name2,TheData[,1])
  xx <- TheData[pick,]
  tempmat <- matrix(NA,nrow=nrow(xx),ncol=2)
  if (param.name!="")
    {
     for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
      strsplit(as.character(xx[igroup,1]),param.name)[[1]][2]
     if (length(name2)>0)
      {
       for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
         strsplit(as.character(tempmat[igroup,1]),name2)[[1]][1]
      }
    }
  if (param.name=="")
   {
    for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
      strsplit(as.character(xx[igroup,1]),name2)[[1]][1]
   }     
  ###if (length(name2)>0) for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(tempmat[igroup,1],name2)[[1]][1]
  tempmat[,2] <- as.numeric(as.character(xx[,2]))
  ##pick <- grep("li_b_",TheData[,1])
  ##tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

#assign the old vals to the new codes, write a csv for data entry with columns to change highlighted and 'parent' group parameter values inserted.
output <- CodeRelations #list(Code=CodeRelations$Child,Parent=CodeRelations$Parent,Change=CodeRelations$Change)
output$x <- rep(NA,nrow(output))
output$x <- tempmat[match(CodeRelations$Parent,tempmat[,1]),2]
#output$li_b <- rep(NA,nrow(output))
#output$li_b <- tempmat[match(CodeRelations$Parent,tempmat[,1]),3]
#output$source <- ""
#output$source[output$Change==0] <- "Atlantis-Neus v1"
#head(output)
#write.table(output,file="weightlength.csv",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")

## assuming that the new values have been filled in, put back into format for the .prm file
if (param.type==1)  #just vertebrates
 {
  Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate==1])
  temptab <- matrix(NA,nrow=Nverts,ncol=2)
  temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],name2,sep="")
  temptab[,2] <- output$x[output$IsVertebrate==1]
  #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
  write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write(" ",file=outfile,append=TRUE)
 }
if (param.type==2)  #all groups
 {
  Ngroups <- nrow(CodeRelations)
  temptab <- matrix(NA,nrow=Ngroups,ncol=2)
  temptab[,1] <- paste(param.name,output$Child,sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child,name2,sep="")
  temptab[,2] <- output$x
  #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
  temptab <- na.omit(temptab)
  write.table(format(temptab,scientific=FALSE),file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write(" ",file=outfile,append=TRUE)
 }
if (param.type==3)  #just invertebrates
 {
  Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate!=1])
  temptab <- matrix(NA,nrow=Nverts,ncol=2)
  temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],name2,sep="")
  temptab[,2] <- output$x[output$IsVertebrate!=1]
  write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write(" ",file=outfile,append=TRUE)
 }


}

#' this function finds a parameter in a prm file, 
#' maps the old values to new group structure,
#' and then writes out the new parameters in a text file,
#' for insertion into a new .prm file
#'  this version is for parameters with dimension and then values on the 2nd line
change.prm2 <- function(path,origfile,param.name,outfile,mapfile,param.type,param.length=4,name2=NULL)
 {
  #this function finds a parameter in a prm file, 
  #maps the old values to new group structure,
  #and then writes out the new parameters in a text file,
  #for insertion into a new .prm file
  # this version is for parameters with dimension and then values on the 2nd line
  
  #set working directory
  setwd(path)
  
  #Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
  CodeRelations <- read.csv(mapfile)
  #read in orig param file
  TheData <- read.table(origfile,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)

  #find the length-weight parameters from the old prm file, store them
  pick <- grep(param.name,TheData[,1])
  xx <- TheData[pick,]
  pick2 <- grep('#',xx[,1])
  xx <- xx[-pick2,]
  yy <- TheData[pick+1,]
  yy <- yy[-pick2,]
  tempmat <- matrix(NA,nrow=nrow(xx),ncol=2)
  for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
    strsplit(as.character(xx[igroup,1]),param.name)[[1]][2]
  if (length(name2)>0) for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(tempmat[igroup,1],name2)[[1]][1]
  tempmat[,2] <- as.numeric(as.character(xx[,2]))
  ##pick <- grep("li_b_",TheData[,1])
  ##tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

  habmap <- c(1,2,2,2,4,5,6,7,8,9,10)
  
  #as.numeric(t(yy[1,1:10]))
  
#assign the old vals to the new codes, write a csv for data entry with columns to change highlighted and 'parent' group parameter values inserted.
output <- CodeRelations #list(Code=CodeRelations$Child,Parent=CodeRelations$Parent,Change=CodeRelations$Change)
output$x <- rep(NA,nrow(output))
output$x <- tempmat[match(CodeRelations$Parent,tempmat[,1]),2]
output$x[] <- 11
#output$li_b <- rep(NA,nrow(output))
#output$li_b <- tempmat[match(CodeRelations$Parent,tempmat[,1]),3]
#output$source <- ""
#output$source[output$Change==0] <- "Atlantis-Neus v1"
#head(output)
#write.table(output,file="weightlength.csv",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")

matches <- match(CodeRelations$Parent,tempmat[,1])
output2 <- matrix(NA,ncol=length(habmap),nrow=nrow(output))
for (irow in 1:nrow(output2))
 {
  output2[irow,] <-  
    as.numeric(t(yy[matches[irow],habmap]))
 }


## assuming that the new values have been filled in, put back into format for the .prm file
if (param.type==1)  #just vertebrates
 {
  Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate==1])
  temptab <- matrix(NA,nrow=Nverts,ncol=2)
  temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],name2,sep="")
  temptab[,2] <- output$x[output$IsVertebrate==1]
  #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
  #write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  #write(" ",file=outfile,append=TRUE)
  rows.use <- which(output$IsVertebrate==1)
  write(" ",file=outfile)
  for (row in rows.use)
  #for (irow in 1:nrow(temptab))
  {
    irow = which(rows.use==row)
    write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                quote=FALSE,sep=" ",append=TRUE)
    write.table(t(output2[row,]),file=outfile,col.names=FALSE,row.names=FALSE,
                quote=FALSE,sep=" ",append=TRUE)
    write(" ",file=outfile,append=TRUE)
  }  
  
 }
if (param.type==2)  #all groups
 {
  Ngroups <- nrow(CodeRelations)
  temptab <- matrix(NA,nrow=Ngroups,ncol=2)
  temptab[,1] <- paste(param.name,output$Child,sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child,name2,sep="")
  temptab[,2] <- output$x
  #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
  write(" ",file=outfile)
  for (irow in 1:nrow(temptab))
  {
   write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
               quote=FALSE,sep=" ",append=TRUE)
   write.table(t(output2[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
               quote=FALSE,sep=" ",append=TRUE)
   write(" ",file=outfile,append=TRUE)
  }
 }
if (param.type==3)  #just invertebrates
 {
  Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate!=1])
  temptab <- matrix(NA,nrow=Nverts,ncol=2)
  temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],name2,sep="")
  temptab[,2] <- output$x[output$IsVertebrate!=1]
  rows.use <- which(output$IsVertebrate!=1)
  write(" ",file=outfile)
  for (row in rows.use)
    #for (irow in 1:nrow(temptab))
   {
    irow = which(rows.use==row)
    write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                quote=FALSE,sep=" ",append=TRUE)
    write.table(t(output2[row,]),file=outfile,col.names=FALSE,row.names=FALSE,
                quote=FALSE,sep=" ",append=TRUE)
    write(" ",file=outfile,append=TRUE)
   
  }
  #write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  #write(" ",file=outfile,append=TRUE)
 }


}

#' this function finds a parameter in a prm file, 
#' maps the old values to new group structure,
#' and then writes out the new parameters in a text file,
#' for insertion into a new .prm file
#'  this version is for parameters with dimension and then values on the 2nd line
change.prm3 <- function(path,origfile,param.name,outfile,mapfile,param.type,
                        name2=NULL)
{
  #this function finds a parameter in a prm file, 
  #maps the old values to new group structure,
  #and then writes out the new parameters in a text file,
  #for insertion into a new .prm file
  # this version is for parameters with dimension and then values on the 2nd line
  
  #set working directory
  setwd(path)
  
  #Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
  CodeRelations <- read.csv(mapfile)
  #read in orig param file
  TheData <- read.table(origfile,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)
  
  
  #find the length-weight parameters from the old prm file, store them
  if (param.name!= "") 
    pick <- grep(param.name,TheData[,1])
  if (length(name2)>0) pick <-pick[grep(name2,TheData[pick,1])]
  if (param.name== "") pick <- grep(name2,TheData[,1])
  xx <- TheData[pick,]
  pick2 <- grep('#',xx[,1])
  if(length(pick2)>0) xx <- xx[-pick2,]
  yy <- TheData[pick+1,]
  if(length(pick2)>0) yy <- yy[-pick2,]
  
  tempmat <- matrix(NA,nrow=nrow(xx),ncol=2)
  if (param.name!="")
  {
    for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
      substr(xx[igroup,1],start=1+str_length(param.name),
             stop=str_length(xx[igroup,1]))
    #strsplit(as.character(xx[igroup,1]),param.name)[[1]][2]
    if (length(name2)>0)
    {
      for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
        #strsplit(as.character(tempmat[igroup,1]),name2)[[1]][1]
        substr(tempmat[igroup,1],stop=str_length(tempmat[igroup,1])-
                 str_length(name2),start=1)
    }
  }
  if (param.name=="")
  {
    for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
      strsplit(as.character(xx[igroup,1]),name2)[[1]][1]
  }     
  ###if (length(name2)>0) for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(tempmat[igroup,1],name2)[[1]][1]
  tempmat[,2] <- as.numeric(as.character(xx[,2]))
  ##pick <- grep("li_b_",TheData[,1])
  ##tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))
  
  
  #as.numeric(t(yy[1,1:10]))
  
  #assign the old vals to the new codes, write a csv for data entry with columns to change highlighted and 'parent' group parameter values inserted.
  output <- CodeRelations #list(Code=CodeRelations$Child,Parent=CodeRelations$Parent,Change=CodeRelations$Change)
  output$x <- rep(NA,nrow(output))
  output$x <- tempmat[match(CodeRelations$Parent,tempmat[,1]),2]
  #output$x[] <- 11
  #output$li_b <- rep(NA,nrow(output))
  #output$li_b <- tempmat[match(CodeRelations$Parent,tempmat[,1]),3]
  #output$source <- ""
  #output$source[output$Change==0] <- "Atlantis-Neus v1"
  #head(output)
  #write.table(output,file="weightlength.csv",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")
  
  matches <- match(CodeRelations$Parent,tempmat[,1])
  numcols <- unique(as.numeric(output$x[which(is.na(output$x)==FALSE)]))
  output2 <- matrix(NA,ncol=numcols,nrow=nrow(output))
  for (irow in 1:nrow(output2))
  {
    output2[irow,] <-  
      as.numeric(as.character(t(yy[matches[irow],1:ncol(output2)])))
  }
  
  
  ## assuming that the new values have been filled in, put back into format for the .prm file
  if (param.type==1)  #just vertebrates
  {
    Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate==1])
    temptab <- matrix(NA,nrow=Nverts,ncol=2)
    temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],sep="")
    if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],name2,sep="")
    temptab[,2] <- output$x[output$IsVertebrate==1]
    #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
    #write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    #write(" ",file=outfile,append=TRUE)
    rows.use <- which(output$IsVertebrate==1)
    write(" ",file=outfile)
    for (row in rows.use)
      #for (irow in 1:nrow(temptab))
    {
      irow = which(rows.use==row)
      if (is.na(temptab[irow,2])==FALSE)
      {
      write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,sep=" ",append=TRUE)
      write.table(t(output2[row,]),file=outfile,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,sep=" ",append=TRUE)
      write(" ",file=outfile,append=TRUE)
      }  
    }  
    
  }
  if (param.type==2)  #all groups
  {
    Ngroups <- nrow(CodeRelations)
    temptab <- matrix(NA,nrow=Ngroups,ncol=2)
    temptab[,1] <- paste(param.name,output$Child,sep="")
    if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child,name2,sep="")
    temptab[,2] <- output$x
    #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
    write(" ",file=outfile)
    for (irow in 1:nrow(temptab))
    {
      if (is.na(temptab[irow,2])==FALSE)
      {
      write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,sep=" ",append=TRUE)
      write.table(t(output2[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,sep=" ",append=TRUE)
      write(" ",file=outfile,append=TRUE)
      }
    }
  }
  if (param.type==3)  #just invertebrates
  {
    Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate!=1])
    temptab <- matrix(NA,nrow=Nverts,ncol=2)
    temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],sep="")
    if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],name2,sep="")
    temptab[,2] <- output$x[output$IsVertebrate!=1]
    rows.use <- which(output$IsVertebrate!=1)
    write(" ",file=outfile)
    for (row in rows.use)
      #for (irow in 1:nrow(temptab))
    {
      if (is.na(temptab[row,2])==FALSE)
      {
        irow = which(rows.use==row)
      write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,sep=" ",append=TRUE)
      write.table(t(output2[row,]),file=outfile,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,sep=" ",append=TRUE)
      write(" ",file=outfile,append=TRUE)
      }
    }
    #write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    #write(" ",file=outfile,append=TRUE)
  }
  
  
}

change.prm4 <- function(path,origfile,param.name,outfile,mapfile,param.type,
                        name2=NULL)
{
  ###### pPREY matrix
  library(stringr)
  #param.name <- "pPREY1"
  #name2 <- "1"
  #set working directory
  setwd(path)
  #Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
  CodeRelations <- read.csv(mapfile)
  oldgroups <- read.csv("NeusGroups.csv")
  
  #read in orig param file
  TheData <- read.table(origfile,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)
  
  #additional columns in pPREY matrix
  
  old.codes <- c(as.character(oldgroups$Code),c("DLsed","DRsed","DCsed","CEPj","PWNj"))
  mapvals <- match(CodeRelations$Parent,oldgroups$Code)
  mapvals2 <- c(mapvals,63:65)
  
  #find the length-weight parameters from the old prm file, store them
  if (param.name!= "") 
    pick <- grep(param.name,TheData[,1])
  if (length(name2)>0) pick <-pick[grep(name2,TheData[pick,1])]
  if (param.name== "") pick <- grep(name2,TheData[,1])
  xx <- TheData[pick,]
  pick2 <- grep('#',xx[,1])
  if(length(pick2)>0) xx <- xx[-pick2,]
  yy <- TheData[pick+1,]
  if(length(pick2)>0) yy <- yy[-pick2,]
  
  tempmat <- matrix(NA,nrow=nrow(xx),ncol=2)
  if (param.name!="")
  {
    for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
      substr(xx[igroup,1],start=1+str_length(param.name),
             stop=str_length(xx[igroup,1]))
    #strsplit(as.character(xx[igroup,1]),param.name)[[1]][2]
    if (length(name2)>0)
    {
      for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
        #strsplit(as.character(tempmat[igroup,1]),name2)[[1]][1]
        substr(tempmat[igroup,1],stop=str_length(tempmat[igroup,1])-
                 str_length(name2),start=1)
    }
  }
  if (param.name=="")
  {
    for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- 
      strsplit(as.character(xx[igroup,1]),name2)[[1]][1]
  }     
  ###if (length(name2)>0) for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(tempmat[igroup,1],name2)[[1]][1]
  tempmat[,2] <- as.numeric(as.character(xx[,2]))
  ##pick <- grep("li_b_",TheData[,1])
  ##tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))
  
  
  #as.numeric(t(yy[1,1:10]))
  
  #assign the old vals to the new codes, write a csv for data entry with columns to change highlighted and 'parent' group parameter values inserted.
  output <- CodeRelations #list(Code=CodeRelations$Child,Parent=CodeRelations$Parent,Change=CodeRelations$Change)
  output$x <- rep(NA,nrow(output))
  #output$x <- tempmat[match(CodeRelations$Parent,tempmat[,1]),2]
  output$x <- nrow(output)
  
  #output$x[] <- 11
  #output$li_b <- rep(NA,nrow(output))
  #output$li_b <- tempmat[match(CodeRelations$Parent,tempmat[,1]),3]
  #output$source <- ""
  #output$source[output$Change==0] <- "Atlantis-Neus v1"
  #head(output)
  #write.table(output,file="weightlength.csv",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")
  
  matches <- match(CodeRelations$Parent,tempmat[,1])
  match2 <- match(CodeRelations$Parent,old.codes)
  numcols <- unique(as.numeric(output$x[which(is.na(output$x)==FALSE)]))
  numcols <- nrow(output)
  output2 <- matrix(NA,ncol=numcols,nrow=nrow(output))
  for (irow in 1:nrow(output2))
  {
    output2[irow,] <-  
      as.numeric(as.character(t(yy[matches[irow],match2])))
  }
  
  ## assuming that the new values have been filled in, put back into format for the .prm file
  if (param.type==1)  #just vertebrates
  {
    Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate==1])
    temptab <- matrix(NA,nrow=Nverts,ncol=2)
    temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],sep="")
    if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],name2,sep="")
    temptab[,2] <- output$x[output$IsVertebrate==1]
    #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
    #write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    #write(" ",file=outfile,append=TRUE)
    rows.use <- which(output$IsVertebrate==1)
    write(" ",file=outfile)
    for (row in rows.use)
      #for (irow in 1:nrow(temptab))
    {
      irow = which(rows.use==row)
      if (is.na(output[row,1])==FALSE)
      {
        write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                    quote=FALSE,sep=" ",append=TRUE)
        write.table(t(output2[row,]),file=outfile,col.names=FALSE,row.names=FALSE,
                    quote=FALSE,sep=" ",append=TRUE)
        write(" ",file=outfile,append=TRUE)
      }  
    }  
    
  }
  if (param.type==2)  #all groups
  {
    Ngroups <- nrow(CodeRelations)
    temptab <- matrix(NA,nrow=Ngroups,ncol=2)
    temptab[,1] <- paste(param.name,output$Child,sep="")
    if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child,name2,sep="")
    temptab[,2] <- output$x
    #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
    write(" ",file=outfile)
    rows.use <- 1:nrow(temptab)
    for (row in rows.use)
    {
      irow = which(rows.use==row)
      if (is.na(output2[row,1])==FALSE)
      {
        write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                    quote=FALSE,sep=" ",append=TRUE)
        write.table(t(output2[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                    quote=FALSE,sep=" ",append=TRUE)
        write(" ",file=outfile,append=TRUE)
      }
    }
  }
  if (param.type==3)  #just invertebrates
  {
    Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate!=1])
    temptab <- matrix(NA,nrow=Nverts,ncol=2)
    temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],sep="")
    if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate!=1],name2,sep="")
    temptab[,2] <- output$x[output$IsVertebrate!=1]
    rows.use <- which(output$IsVertebrate!=1)
    write(" ",file=outfile)
    for (row in rows.use)
      #for (irow in 1:nrow(temptab))
    {
      if (is.na(output2[row,1])==FALSE)
      {
        irow = which(rows.use==row)
        write.table(t(temptab[irow,]),file=outfile,col.names=FALSE,row.names=FALSE,
                    quote=FALSE,sep=" ",append=TRUE)
        write.table(t(output2[row,]),file=outfile,col.names=FALSE,row.names=FALSE,
                    quote=FALSE,sep=" ",append=TRUE)
        write(" ",file=outfile,append=TRUE)
      }
    }
    #write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    #write(" ",file=outfile,append=TRUE)
  }
  
  
}




library(stringr)
outfile <- "newtable.out"
path <- "C:/Atlantis/neus_sandbox/Test/test1/"
path <- "/Volumes/MyPassport/NEFSC/Atlantis/neus_sandbox/Test/test1/"
setwd("~/Atlantis/neus_sandbox")
path <- getwd()
mapfile <- "coderelations.csv"
origfile <- "at_biol_neus_v15_DE.prm"
param.name <- "EPlant_"
name2 <- "_T15"
param.type=3
#change.prm(path,origfile,param.name,outfile,mapfile,param.type)
change.prm(path,origfile,param.name,outfile,mapfile,param.type,name2)
change.prm3(path,origfile,param.name,outfile,mapfile,param.type,name2)
change.prm2(path,origfile,param.name,outfile,mapfile,param.type,name2)
### 4 is for pPREY
change.prm4(path,origfile,param.name,outfile,mapfile,param.type,name2)

change.prm5(path,origfile,param.name,outfile,mapfile,param.type,name2)


########### now do some stuff for the harvest file
setwd("~/Atlantis/neus_sandbox")
path <- getwd()
mapfile <- "coderelations.csv"
origfile <- "at_harvest_neus_v15_DE.prm"
param.name <- "EPlant_"
name2 <- "_T15"
param.type=3







setwd(path)

  #Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
  CodeRelations <- read.csv(mapfile)

temptab <- matrix(NA,nrow=nrow(CodeRelations),ncol=3)
temptab[,1] <- "#"
temptab[,3] <- as.character(CodeRelations$Long.Name)
temptab[,2] <- as.character(CodeRelations$Child)
write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")

###### pPREY matrix

param.name <- "pPREY1"
name2 <- "1"
#set working directory
setwd(path)
#Lookup table with new codes for functional groups along with 'parent' groups from Atlantis-neus v1.
CodeRelations <- read.csv(mapfile)
oldgroups <- read.csv("NeusGroups.csv")
#additional columns in pPREY matrix

old.codes <- c(as.character(oldgroups$Code),c("DLsed","DRsed","DCsed","CEPj","PWNj"))

mapvals <- match(CodeRelations$Parent,oldgroups$Code)
mapvals2 <- c(mapvals,63:65)
#read in orig param file
TheData <- read.table(origfile,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)
#find the length-weight parameters from the old prm file, store them
pick <- grep(param.name,TheData[,1])

xx <- TheData[pick+1,]
xx2 <- TheData[pick,1]
tempmat <- matrix(NA,nrow=length(which(CodeRelations$IsVertebrate==1)),
                  ncol=nrow(CodeRelations))
for (irow in 1:nrow(tempmat)) 
{
 tempmat[irow,] <- xx[irow,mapvals]
 for (i in 1:length(pick))
   {
    child <- strsplit(strsplit(as.character(xx2[i]),
                               paste(param.name,1,sep=""))[[1]][2],1)[[1]]
 
if (length(name2)>0) for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(tempmat[igroup,1],name2)[[1]][1]
tempmat[,2] <- as.numeric(as.character(xx[,2]))
##pick <- grep("li_b_",TheData[,1])
##tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

xx <- TheData[pick,]
tempmat <- matrix(NA,nrow=nrow(xx),ncol=2)
for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(as.character(xx[igroup,1]),param.name)[[1]][2]
if (length(name2)>0) for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(tempmat[igroup,1],name2)[[1]][1]
tempmat[,2] <- as.numeric(as.character(xx[,2]))
##pick <- grep("li_b_",TheData[,1])
##tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

#assign the old vals to the new codes, write a csv for data entry with columns to change highlighted and 'parent' group parameter values inserted.
output <- CodeRelations #list(Code=CodeRelations$Child,Parent=CodeRelations$Parent,Change=CodeRelations$Change)
output$x <- rep(NA,nrow(output))
output$x <- tempmat[match(CodeRelations$Parent,tempmat[,1]),2]
#output$li_b <- rep(NA,nrow(output))
#output$li_b <- tempmat[match(CodeRelations$Parent,tempmat[,1]),3]
#output$source <- ""
#output$source[output$Change==0] <- "Atlantis-Neus v1"
#head(output)
#write.table(output,file="weightlength.csv",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")

## assuming that the new values have been filled in, put back into format for the .prm file
if (param.type==1)  #just vertebrates
{
  Nverts <- length(CodeRelations$IsVertebrate[CodeRelations$IsVertebrate==1])
  temptab <- matrix(NA,nrow=Nverts,ncol=2)
  temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],sep="")
  if (length(name2)>0) temptab[,1] <- paste(param.name,output$Child[output$IsVertebrate==1],name2,sep="")
  temptab[,2] <- output$x[output$IsVertebrate==1]
  #temptab[,3] <- paste("weight at length a parameter for ",output[output$IsVertebrate==1,"Long.Name"],sep="")
  write.table(temptab,file=outfile,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write(" ",file=outfile,append=TRUE)
}




biofile <- "at_biol_neus_v15_working.prm"
TheData <- read.table(biofile,col.names=1:100,comment.char="",fill=TRUE,
                      skip=152,nrow=62,header=FALSE)
mapvals <- match(CodeRelations$Parent,oldgroups$Code)
temp <- cbind(paste("flag",CodeRelations$Child,sep=""),
              oldgroups$IsTurnedOn[mapvals])
write.table(temp,file=outfile,row.names=FALSE,col.names=FALSE,sep="         ",
            quote=FALSE)


## Feb 12 2015
name2 <- "_S1"
write.table(cbind(substr(xx[,1],start=2,stop=str_length(xx[,1])),yy[,1:30]),
            file=outfile,col.names=FALSE,
            row.names=FALSE,quote=FALSE,append=FALSE)
name2 <- "_S2"
write.table(cbind(xx[,1],yy[,1:30]),file=outfile,col.names=FALSE,
            row.names=FALSE,quote=FALSE,append=TRUE)
name2 <- "_S3"
write.table(cbind(xx[,1],yy[,1:30]),file=outfile,col.names=FALSE,
            row.names=FALSE,quote=FALSE,append=TRUE)
name2 <- "_S4"
write.table(cbind(xx[,1],yy[,1:30]),file=outfile,col.names=FALSE,
            row.names=FALSE,quote=FALSE,append=TRUE)


