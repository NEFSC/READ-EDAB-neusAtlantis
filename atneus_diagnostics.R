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


