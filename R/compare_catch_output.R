#' Script to compare the age structured output from:
#' neus_outputCATCH.nc vs. neus_outputCatch.txt

library(dplyr)
library(ggplot2)
library(ncdf4)

atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/New_Init_CatchTS_Revert/'

#read CATCH.nc
catch.nc = nc_open(paste0(atl.dir,'neus_outputCATCH.nc'))
#read ANNAGECATCH.nc
# age.catch.nc = nc_open(paste0(atl.dir,'neus_outputANNAGECATCH.nc'))

#read Catch.txt
catch.txt =read.table(paste0(atl.dir,'neus_outputCatch.txt'),header = T, stringsAsFactors = F)

tot.catch.nc = nc_open(paste0(atl.dir,'neus_outputTOTCATCH.nc'))

catch.varnames = names(catch.nc$var)
catch.varnames[grep('Winter',catch.varnames)]
group.name = 'Winter_Skate'
group.code = 'WSK'

group.varnames = catch.varnames[grep(paste0('^',group.name,'.*\\Catch'),catch.varnames)]
  
catch.ls = list()
catch.start = numeric()
for(i in 1:length(group.varnames)){
  cohort = as.numeric(strsplit(group.varnames[i],split = paste0(group.name,'|_Catch'))[[1]][2])
  dat = apply(ncvar_get(catch.nc,group.varnames[i]),2,sum,na.rm=T)
  catch.start[i] = which(dat > 0)[1]
  catch.ls[[i]] = data.frame(Time = 1:length(dat),Cohort = cohort, Catch = dat)
}

catch.df = dplyr::bind_rows(catch.ls)
catch.df$Cohort = as.factor(catch.df$Cohort)

ggplot(data = catch.df, aes(x = Time, y= Catch,10, col = Cohort))+
  geom_line()

# tot.catch = apply(ncvar_get(tot.catch.nc, paste0('Tot_',group.code,'_Catch')),2,sum,na.rm=T)
# tot.catch.df = data.frame(Time = 1:length(tot.catch), tot.catch = tot.catch)
tot.catch.df = catch.df %>% group_by(Time) %>% summarize(tot.catch = sum(Catch,na.rm=T))
plot(tot.catch~Time,tot.catch.df,type='l')

break.catch = catch.df %>%
  left_join(tot.catch.df) %>%
  mutate(age.prop = Catch/tot.catch) %>%
  arrange(Time, Cohort)

ggplot(break.catch,aes(x= Time, y = age.prop, fill = Cohort))+
  geom_area()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,'Set3'))
 
