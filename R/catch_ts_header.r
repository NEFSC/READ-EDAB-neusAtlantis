#' Write header of a catch time series file for a specific box
#' @author Gavin Fay
#' @param dummyfile is the catch time series file to use as a base for new one
#' @param groups the list of atlantis codes for the groups (in order of table)
#' @param outfile is the file that will be written to
#' @param boxnum is the box number this file refers to
write_catch_ts_header <- function(dummyfile="tsfiles/catches/catch1.ts",
                              groups=as.character(read.csv("coderelations.csv",
                              header=TRUE)$Child), outfile="newheader.txt",
                              boxnum=1)
{
  #' read in the existing header file
  old.header <- read.table(dummyfile,col.names=1:11,fill=TRUE,
                           comment.char="",nrow=13)
    
#' read in group structure (order of groups)
#' temporarily read in all groups
#CodeRelations <- read.csv("coderelations.csv",header=TRUE)$Child
#groups <- CodeRelations$Child
num.cols <- length(groups)+1

#' start formatting the new header
#' start with top and Time column
new.header <- old.header
new.header[,8] <- ""
new.header[,11] <- ""
new.header[1,8] <- 2014
new.header[1,11] <- boxnum
new.header[,3] <- as.character(new.header[,3])
new.header[,2] <- as.character(new.header[,2])
new.header[3,3] <- num.cols
#' write top and Time column to "newheader.txt"
write.table(new.header[1:8,],sep=" ",quote=FALSE,col.names=FALSE,
            row.names=FALSE,file=outfile)
#' now start to create the headers for the rest of the column 
header.chunk <- new.header[9:13,1:4]
temp.strings <- strsplit(as.character(header.chunk[-1,2]),"COLUMN",fixed=TRUE)
#' loop over groups
for (group in groups)
  {
  temp.chunk <- header.chunk
  icol=which(groups==group)+1
   for (i in 1:4)
    {
     tempbit <- strsplit(temp.strings[[i]][2],".",fixed=TRUE)[[1]][2]
     temp.chunk[i+1,2] <- paste("COLUMN",icol,".",tempbit,sep="")
    }
    temp.chunk[2:3,3] <- as.character(group)
    #' write the chunk for this particular column
    write.table(temp.chunk,file=outfile,quote=FALSE,col.names=FALSE,
                row.names=FALSE,sep=" ",append=TRUE)
  }
write("##",file=outfile,append=TRUE)
#close function
}
write_catch_ts_header()
