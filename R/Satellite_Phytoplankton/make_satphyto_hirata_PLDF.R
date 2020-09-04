#Function to pull and format data on Hirata model Diatom:Dinoflagellate ratio (i.e. % Diatom)

#Load Packages
library(dplyr)
library(ggplot2)

#Read in data from CSV
data = read.csv('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/DOY-OCCCI-ATLANTIS_NEUS-HIRATA-DIATOM-PROP.csv',
                header = T, as.is = T)

#Extract DOY
data$DOY = unname(sapply(data$PERIOD,function(x) return(strsplit(x,'_')[[1]][2])))
data$jul.day = as.numeric(data$DOY)

#Pull relevant variables
data = data %>%
  select(DOY,jul.day,SUBAREA,PROD,MED)

diatom.prop = matrix(NA, nrow = 30, ncol = 365)
PL.DF = matrix(NA, nrow = 30, ncol = 365)

fig.diatom.prop = list()
fig.PL.DF = list()

boxes = 0:29
for(b in 1:30){
  dat = data %>% 
    filter(SUBAREA == boxes[b]) %>%
    arrange(DOY)
  dat = reshape2::dcast(dat,DOY~PROD,value.var = 'MED')
  dat$DIATOM.PROP = dat$DIATOM/dat$MICRO
  dat$PLDF = dat$DIATOM/dat$DINOFLAGELLATE
  
  #make Diatom proportion plot
  fig.diatom.prop[[b]] = ggplot(dat,aes(x = as.numeric(DOY), y = DIATOM.PROP))+
    geom_line()+
    xlab('DOY')+
    ylab('Diatom Proportion')+
    ggtitle(paste0('Box ',boxes[b]))+
    theme_bw()+
    theme(plot.title = element_text(hjust = '0.5'))
  
  fig.PL.DF[[b]] = ggplot(dat,aes(x = as.numeric(DOY), y = PLDF))+
    geom_line()+
    xlab('DOY')+
    ylab('Diatom:Dinoflagellate')+
    ggtitle(paste0('Box ',boxes[b]))+
    theme_bw()+
    theme(plot.title = element_text(hjust = '0.5'))
  
  diatom.prop[b,] = dat$DIATOM.PROP
  PL.DF[b,] = dat$PLDF
  
}

#save plots
pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/Hirata_Diatom_Proportion.pdf',width = 16, height = 6, onefile = T)
for(b in 1:length(boxes)){gridExtra::grid.arrange(fig.diatom.prop[[b]])}
dev.off()

pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/Hirata_Diatom_Dinoflagellate.pdf',width = 16, height = 6, onefile = T)
for(b in 1:length(boxes)){gridExtra::grid.arrange(fig.PL.DF[[b]])}
dev.off()

#Save Diatom Proportions and PL:DF
save(diatom.prop,file = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/diatom_proportion_DOY.R')
save(PL.DF,file = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/diatom_dinoflag_ratio_DOY.R')
